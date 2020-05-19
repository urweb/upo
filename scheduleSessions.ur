open Bootstrap4

functor Make(M: sig
                 con sessionKey1 :: Name
                 con sessionKeyT :: Type
                 con sessionKeyR :: {Type}
                 con sessionOther :: {Type}
                 constraint [sessionKey1] ~ sessionKeyR
                 con sessionKey = [sessionKey1 = sessionKeyT] ++ sessionKeyR
                 constraint sessionKey ~ sessionOther
                 table session : (sessionKey ++ sessionOther)
                 val sessionFl : folder sessionKey
                 val session_inj : $(map sql_injectable_prim sessionKey)
                 val session_show : show $sessionKey
                 val session_read : read $sessionKey
                 val session_eq : $(map eq sessionKey)
                 val session_ord : $(map ord sessionKey)

                 con speakerKey1 :: Name
                 con speakerKeyT :: Type
                 con speakerKeyR :: {Type}
                 con speakerOther :: {Type}
                 constraint [speakerKey1] ~ speakerKeyR
                 con speakerKey = [speakerKey1 = speakerKeyT] ++ speakerKeyR
                 constraint speakerKey ~ speakerOther
                 constraint sessionKey ~ speakerKey
                 table speaker : (speakerKey ++ speakerOther)
                 val speakerFl : folder speakerKey
                 val speaker_inj : $(map sql_injectable_prim speakerKey)
                 val speaker_show : show $speakerKey
                 val speaker_read : read $speakerKey

                 con time :: {Type}
                 constraint time ~ sessionKey
                 constraint time ~ speakerKey
                 val timeFl : folder time
                 val time_inj : $(map sql_injectable_prim time)
                 val time_show : show $time
                 val time_read : read $time
                 val time_eq : $(map eq time)
                 val time_ord : $(map ord time)

                 table talk : (sessionKey ++ time ++ map option speakerKey)
             end) = struct
    open M

    val session_eq' : eq $sessionKey = @@Record.eq [sessionKey] session_eq sessionFl
    val time_eq' : eq $time = @@Record.eq [time] time_eq timeFl

    val session_ord' : ord $sessionKey = @@Record.ord [sessionKey] session_ord sessionFl
    val time_ord' : ord $time = @@Record.ord [time] time_ord timeFl

    datatype update =
             NewTime of $sessionKey * $time * string (* string rendering of time *)
           | AssignTime of $sessionKey * $time * string (* string rendering of speaker *)

    table listeners : { Channel : channel update }
                        
    structure AllSessions = struct
        type session = {NewTime : source string,
                        Times : source (list ($time * string (* time string-converted on server *) * source string))}

        type a = {Speakers : list $speakerKey,
                  Sessions : source (list ($sessionKey * session)),
                  Channel : channel update}

        val create =
            let
                fun finalizeSession (ses : $sessionKey, times : list ($time * string)) =
                    times <- List.mapM (fn (tm, speaker) =>
                                           speaker <- source speaker;
                                           return (tm, show tm, speaker)) times;
                    times <- source times;
                    newTime <- source "";
                    return (ses, {NewTime = newTime, Times = times})
                
                fun generate (curSession : option ($sessionKey * list ($time * string)))
                             (acc : list ($sessionKey * session))
                             (rows : list {Session : $sessionKey, Talk : $(map option (time ++ speakerKey))}) =
                    case rows of
                        [] =>
                        (case curSession of
                             None => return acc
                           | Some ses =>
                             ses <- finalizeSession ses;
                             return (ses :: acc))
                      | row :: rows =>
                        case curSession of
                            None => generate (Some (row.Session, [])) acc rows
                          | Some (ses, tms) =>
                            if ses = row.Session then
                                case @Sql.unnull timeFl (row.Talk --- (map option speakerKey)) of
                                    None => error <xml>A no-time row for a session appears after a row for the same session with a time.</xml>
                                  | Some tm =>
                                    let
                                        val speaker = case @Sql.unnull speakerFl (row.Talk --- (map option time)) of
                                                          None => ""
                                                        | Some speaker => show speaker
                                    in
                                        generate (Some (ses, (tm, speaker) :: tms)) acc rows
                                    end
                            else
                                case @Sql.unnull timeFl (row.Talk --- (map option speakerKey)) of
                                    None =>
                                    sesr <- finalizeSession (ses, tms);
                                    generate (Some (row.Session, [])) (sesr :: acc) rows
                                  | Some tm =>
                                    let
                                        val speaker = case @Sql.unnull speakerFl (row.Talk --- (map option time)) of
                                                          None => ""
                                                        | Some speaker => show speaker
                                    in
                                        sesr <- finalizeSession (ses, tms);
                                        generate (Some (row.Session, (tm, speaker) :: [])) (sesr :: acc) rows
                                    end
            in
                ch <- channel;
                dml (INSERT INTO listeners(Channel) VALUES({[ch]}));
                
                speakers <- queryL1 (SELECT speaker.{{speakerKey}}
                                     FROM speaker
                                     ORDER BY {{{@Sql.order_by speakerFl
                                       (@Sql.some_fields [#Speaker] [speakerKey] ! ! speakerFl)
                                       sql_asc}}});
                rows <- queryL (SELECT session.{{sessionKey}}, talk.{{map option time}}, talk.{{map option speakerKey}}
                                FROM {{@@sql_left_join [_] [[Session = sessionKey ++ sessionOther]]
                                  [[Talk = map (fn t => (t, option t)) (sessionKey ++ time)
                                        ++ map (fn t => (option t, option t)) speakerKey]] ! ! !
                                  {Talk = @mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                    (fn [t] (_ : sql_injectable_prim t) => nullify_prim)
                                    (@Folder.concat ! sessionFl timeFl) (session_inj ++ time_inj)
                                    ++ @map0 [fn t => nullify (option t) (option t)]
                                    (fn [t ::_] => nullify_option)
                                    speakerFl}
                                  (FROM session) (FROM talk)
                                  (@@Sql.easy_join [#Talk] [#Session] [sessionKey]
                                     [time ++ map option speakerKey] [sessionOther]
                                     [[]] [_] [[]] ! ! ! ! sessionFl)}}
                                ORDER BY {{{@Sql.order_by
                                             (@Folder.concat ! sessionFl (@Folder.mp timeFl))
                                             (@@Sql.some_fields [#Session] [sessionKey] [sessionOther] [[Talk = map option (time ++ sessionKey ++ speakerKey)]] [[]] [[]] ! ! sessionFl
                                               ++ @@Sql.some_fields [#Talk] [map option time] [map option (speakerKey ++ sessionKey)] [[Session = sessionKey ++ sessionOther]] [[]] [[]] ! ! (@Folder.mp timeFl))
                                             sql_asc}}});
                sessions <- generate None [] rows;
                sessions <- source sessions;
                return {Speakers = speakers, Sessions = sessions, Channel = ch}
            end

        fun onload a =
            let
                fun loop () =
                    m <- recv a.Channel;
                    case m of
                        NewTime (ses, tm, tmS) =>
                        sesl <- get a.Sessions;
                        List.app (fn (ses', {Times = tms, ...}) =>
                                     if ses' <> ses then
                                         return ()
                                     else
                                         tmsV <- get tms;
                                         src <- source "";
                                         set tms (List.sort (fn (a, _, _) (b, _, _) => a > b) ((tm, tmS, src) :: tmsV))) sesl;
                        loop ()
                      | AssignTime (ses, tm, spkrS) =>
                        sesl <- get a.Sessions;
                        List.app (fn (ses', {Times = tms, ...}) =>
                                     if ses' <> ses then
                                         return ()
                                     else
                                         tmsV <- get tms;
                                         List.app (fn (tm', _, s) =>
                                                      if tm' <> tm then
                                                          return ()
                                                      else
                                                          set s spkrS) tmsV) sesl;
                        loop ()
            in
                spawn (loop ())
            end

        fun addTime ses tm =
            @@Sql.easy_insert [sessionKey ++ time ++ map option speakerKey] [_]
              (@mp [sql_injectable_prim] [sql_injectable]
                (fn [t] (_ : sql_injectable_prim t) => _)
                (@Folder.concat ! sessionFl timeFl)
                (session_inj ++ time_inj)
                ++ @mp [sql_injectable_prim] [fn t => sql_injectable (option t)]
                (fn [t] (_ : sql_injectable_prim t) => _) speakerFl speaker_inj)
              (@Folder.concat ! sessionFl
                (@Folder.concat ! timeFl (@Folder.mp speakerFl)))
              talk
              (ses ++ tm ++ @map0 [option] (fn [t ::_] => None) speakerFl);
            queryI1 (SELECT * FROM listeners)
            (fn {Channel = ch} => send ch (NewTime (ses, tm, show tm)))

        fun setSpeaker ses tm spkrS =
            case read spkrS of
                None => error <xml>Invalid speaker</xml>
              | Some spkr =>
                @@Sql.easy_update [sessionKey ++ time] [map option speakerKey] [_] !
                  (@mp [sql_injectable_prim] [sql_injectable]
                    (fn [t] (_ : sql_injectable_prim t) => _)
                    (@Folder.concat ! sessionFl timeFl)
                    (session_inj ++ time_inj))
                  (@mp [sql_injectable_prim] [fn t => sql_injectable (option t)]
                    (fn [t] (_ : sql_injectable_prim t) => _) speakerFl speaker_inj)
                  (@Folder.concat ! sessionFl timeFl)
                  (@Folder.mp speakerFl)
                  talk
                  (ses ++ tm)
                  (@mp [ident] [option] (fn [t] => Some) speakerFl spkr);
                queryI1 (SELECT * FROM listeners)
                        (fn {Channel = ch} => send ch (AssignTime (ses, tm, spkrS)))
            
        fun render _ a = <xml>
          <dyn signal={sesl <- signal a.Sessions;
                       return (List.mapX (fn (ses, r) => <xml>
                         <hr/>
                         <h2>{[ses]}</h2>

                         Add time:
                         <ctextbox class="form-control" source={r.NewTime}/>
                         <button class="btn btn-primary"
                                 value="Add"
                                 onclick={fn _ =>
                                             tm <- get r.NewTime;
                                             case read tm of
                                                 None => error <xml>Invalid time format</xml>
                                               | Some tm => rpc (addTime ses tm)}/>

                         <dyn signal={tms <- signal r.Times;
                                      return (List.mapX (fn (tm, tmS, sp) => <xml>
                                        <div class="card">
                                          <div class="card-header">{[tmS]}</div>

                                          <div class="card-body">
                                            <cselect source={sp}
                                                     onchange={spkrS <- get sp;
                                                               rpc (setSpeaker ses tm spkrS)}>
                                              <coption></coption>
                                              {List.mapX (fn spk => <xml><coption>{[spk]}</coption></xml>) a.Speakers}
                                            </cselect>
                                          </div>
                                        </div>
                                      </xml>) tms)}/>
                       </xml>) sesl)}/>
        </xml>

        fun notification _ = <xml></xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render,
                  Notification = notification}
    end
end
