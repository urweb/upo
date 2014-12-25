open Bootstrap3

functor Make(M : sig
                 con homeKey1 :: Name
                 con homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyRead : read $homeKey
                 val homeKeyEq : eq $homeKey

                 con awayKey1 :: Name
                 con awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyRead : read $awayKey
                 val awayKeyEq : eq $awayKey

                 con timeKey1 :: Name
                 con timeKeyT
                 con timeKeyR :: {Type}
                 constraint [timeKey1] ~ timeKeyR
                 con timeKey = [timeKey1 = timeKeyT] ++ timeKeyR
                 con timeRest :: {Type}
                 constraint timeKey ~ timeRest
                 con timeKeyName :: Name
                 con timeOtherConstraints :: {{Unit}}
                 constraint [timeKeyName] ~ timeOtherConstraints
                 val time : sql_table (timeKey ++ timeRest) ([timeKeyName = map (fn _ => ()) timeKey] ++ timeOtherConstraints)
                 val timeInj : $(map sql_injectable_prim timeKey)
                 val timeKeyFl : folder timeKey
                 val timeKeyShow : show $timeKey
                 val timeKeyEq : eq $timeKey

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
                 constraint awayKey ~ [Channel]
                 constraint (homeKey ++ awayKey) ~ [ByHome]
             end) = struct

    open M

    table meeting : (homeKey ++ timeKey ++ awayKey)
      PRIMARY KEY {{@primary_key [homeKey1] [homeKeyR ++ timeKey ++ awayKey] ! !
                    (homeInj ++ timeInj ++ awayInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}},
      {{one_constraint [#Time] (@Sql.easy_foreign ! ! ! ! ! ! timeKeyFl time)}}

    datatype operation = Add | Del
    type action = { Operation : operation, Home : $homeKey, Away : $awayKey, Time : $timeKey }
    table globalListeners : { Channel : channel action }

    type away_action = { Operation : operation, Home : $homeKey, Time : $timeKey }
    table awayListeners : ([Channel = channel away_action] ++ awayKey)
      PRIMARY KEY {{@primary_key [awayKey1] [awayKeyR] ! ! awayInj}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    table preference : ([ByHome = bool] ++ homeKey ++ awayKey)
      PRIMARY KEY {{@primary_key [#ByHome] [homeKey ++ awayKey] ! !
                    ({ByHome = _} ++ homeInj ++ awayInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    val timeOb [tab] [rest] [tables] [exps] [[tab] ~ tables] [timeKey ~ rest]
        : sql_order_by ([tab = timeKey ++ rest] ++ tables) exps =
        @Sql.order_by timeKeyFl
         (@Sql.some_fields [tab] [timeKey] ! ! timeKeyFl)
         sql_desc

    con all = homeKey ++ awayKey ++ timeKey
    val allFl = @Folder.concat ! homeKeyFl (@Folder.concat ! timeKeyFl awayKeyFl)

    val allInj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim allFl (homeInj ++ awayInj ++ timeInj)
    val homeInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim homeKeyFl homeInj
    val awayInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim awayKeyFl awayInj

    val addMeeting = @@Sql.easy_insert [all] [_] allInj allFl meeting

    fun makeModal bcode titl bod blab = <xml>
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <h4 class="modal-title">{titl}</h4>
          </div>

          <div class="modal-body">
            {bod}
          </div>

          <div class="modal-footer">
            <button class="btn btn-primary"
                    data-dismiss="modal"
                    value={blab}
                    onclick={fn _ => bcode}/>
            <button class="btn btn-default"
                    data-dismiss="modal"
                    value="Cancel"/>
          </div>
        </div>
      </div>
    </xml>

    structure FullGrid = struct
        type awaySet = list $awayKey
        type timeMap = list ($timeKey * awaySet)
        type homeMap = list ($homeKey * timeMap)
        type t = _

        val create =
            allTimes <- queryL1 (SELECT time.{{timeKey}}
                                 FROM time
                                 ORDER BY {{{@Sql.order_by timeKeyFl
                                   (@Sql.some_fields [#Time] [timeKey] ! ! timeKeyFl)
                                   sql_desc}}});

            let
                (* A bit of a little dance to initialize the meeting states,
                 * including blank entries for unused home/time pairs *)
                fun initMap (acc : homeMap)
                            (rows : list $all)
                            (homes : list $homeKey)
                            (times : list $timeKey)
                            (awaysDone : awaySet)
                            (timesDone : timeMap)
                    : homeMap =
                    case homes of
                        [] =>
                        (* Finished with all homes.  Done! *)
                        List.rev acc
                      | home :: homes' =>
                        case times of
                            [] =>
                            (* Finished with one home.  Move to next. *)
                            initMap ((home, List.rev timesDone) :: acc)
                                    rows
                                    homes'
                                    allTimes
                                    []
                                    []
                          | time :: times' =>
                            (* Check if the next row is about this time. *)
                            case rows of
                                [] =>
                                (* Nope. *)
                                initMap acc
                                        []
                                        homes
                                        times'
                                        []
                                        ((time, awaysDone) :: timesDone)
                              | row :: rows' =>
                                if row --- awayKey --- timeKey = home && @eq timeKeyEq (row --- homeKey --- awayKey) time then
                                    (* Aha, a match!  Record this meeting. *)
                                    initMap acc
                                            rows'
                                            homes
                                            times
                                            ((row --- homeKey --- timeKey) :: awaysDone)
                                            timesDone
                                else
                                    (* No match.  On to next time. *)
                                    initMap acc
                                            rows
                                            homes
                                            times'
                                            []
                                            ((time, awaysDone) :: timesDone)
            in
                homes <- queryL1 (SELECT home.{{homeKey}}
                                  FROM home
                                  ORDER BY {{{@Sql.order_by homeKeyFl
                                    (@Sql.some_fields [#Home] [homeKey] ! ! homeKeyFl)
                                    sql_desc}}});
                aways <- queryL1 (SELECT away.{{awayKey}}
                                  FROM away
                                  ORDER BY {{{@Sql.order_by awayKeyFl
                                    (@Sql.some_fields [#Away] [awayKey] ! ! awayKeyFl)
                                    sql_desc}}});
                meetings <- queryL1 (SELECT meeting.{{homeKey}}, meeting.{{timeKey}}, meeting.{{awayKey}}
                                     FROM meeting
                                     ORDER BY {{{@Sql.order_by allFl
                                       (@Sql.some_fields [#Meeting] [all] ! ! allFl)
                                       sql_desc}}});
                meetings <- List.mapM (fn (ho, tms) =>
                                          tms' <- List.mapM (fn (tm, aws) =>
                                                                aws <- source aws;
                                                                return (tm, aws)) tms;
                                          return (ho, tms'))
                                      (initMap []
                                               meetings
                                               homes
                                               allTimes
                                               []
                                               []);
                mid <- fresh;
                modalSpot <- source <xml/>;
                chan <- channel;
                dml (INSERT INTO globalListeners (Channel) VALUES ({[chan]}));
                mf <- source None;
                mt <- source None;
                return {Aways = aways, Times = allTimes, Meetings = meetings,
                        ModalId = mid, ModalSpot = modalSpot, Channel = chan,
                        MovingFrom = mf, MovingTo = mt}
            end

        fun schedule r =
            alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                          FROM meeting
                                          WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                            ! ! allInj allFl r});
            if alreadyScheduled then
                return ()
            else
                addMeeting r;
                queryI1 (SELECT * FROM globalListeners)
                (fn i => send i.Channel {Operation = Add,
                                         Home = r --- awayKey --- timeKey,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
                queryI1 (SELECT * FROM awayListeners
                         WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                           ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Add,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey})

        fun unschedule r =
            alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                          FROM meeting
                                          WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                            ! ! allInj allFl r});
            if not alreadyScheduled then
                return ()
            else
                dml (DELETE FROM meeting
                            WHERE {@@Sql.easy_where [#T] [all] [_] [_] [_] [_]
                              ! ! allInj allFl r});
                queryI1 (SELECT * FROM globalListeners)
                (fn i => send i.Channel {Operation = Del,
                                         Home = r --- awayKey --- timeKey,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
                queryI1 (SELECT * FROM awayListeners
                         WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                           ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Del,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey})

        fun reschedule r =
            alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                          FROM meeting
                                          WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                            ! ! allInj allFl (r.Away ++ r.OldHome ++ r.OldTime)});
            spotUsed <- oneRowE1 (SELECT COUNT( * ) > 0
                                  FROM meeting
                                  WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                    ! ! allInj allFl (r.Away ++ r.NewHome ++ r.NewTime)});
            if not alreadyScheduled || spotUsed then
                return ()
            else
                dml (DELETE FROM meeting
                     WHERE {@@Sql.easy_where [#T] [all] [_] [_] [_] [_]
                       ! ! allInj allFl (r.Away ++ r.OldHome ++ r.OldTime)});
                addMeeting (r.Away ++ r.NewHome ++ r.NewTime);
                queryI1 (SELECT * FROM globalListeners)
                (fn i =>
                    send i.Channel {Operation = Del,
                                    Home = r.OldHome,
                                    Away = r.Away,
                                    Time = r.OldTime};
                    send i.Channel {Operation = Add,
                                    Home = r.NewHome,
                                    Away = r.Away,
                                    Time = r.NewTime});
                queryI1 (SELECT * FROM awayListeners
                         WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                           ! ! awayInj' awayKeyFl r.Away})
                (fn i =>
                    send i.Channel {Operation = Del,
                                    Home = r.OldHome,
                                    Time = r.OldTime};
                    send i.Channel {Operation = Add,
                                    Home = r.NewHome,
                                    Time = r.NewTime})

        fun render t = <xml>
          <div class="modal" id={t.ModalId}>
            <dyn signal={signal t.ModalSpot}/>
          </div>

          <table class="bs3-table table-striped">
            <tr>
              <th/>
              (* Header: one column per time *)
              {List.mapX (fn tm => <xml><th>{[tm]}</th></xml>) t.Times}
            </tr>

            (* One row per home *)
            {List.mapX (fn (ho, tms) => <xml>
              <tr>
                <th>{[ho]}</th>

                (* One column per time *)
                {List.mapX (fn (tm, aws) => <xml>
                  <td dynClass={mf <- signal t.MovingFrom;
                                case mf of
                                    None => return (CLASS "")
                                  | Some _ =>
                                    mt <- signal t.MovingTo;
                                    return (case mt of
                                                None => CLASS ""
                                              | Some mt =>
                                                if mt.Home = ho && mt.Time = tm then
                                                    CLASS "bs3-active"
                                                else
                                                    CLASS "")}
                      onmouseover={fn _ =>
                                      set t.MovingTo (Some {Home = ho, Time = tm})}
                      onclick={fn _ =>
                                  mf <- get t.MovingFrom;
                                  case mf of
                                      None => return ()
                                    | Some mf =>
                                      mt <- get t.MovingTo;
                                      case mt of
                                          None => return ()
                                        | Some mt =>
                                          set t.MovingFrom None;
                                          rpc (reschedule {Away = mf.Away,
                                                           OldHome = mf.Home,
                                                           OldTime = mf.Time,
                                                           NewHome = mt.Home,
                                                           NewTime = mt.Time})}>
                    <active code={selected <- source "";
                                  return <xml>
                                    <dyn signal={awsv <- signal aws;
                                                 (* One button per meeting *)
                                                 return <xml>
                                                   {List.mapX (fn aw => <xml>
                                                     <div dynStyle={mf <- signal t.MovingFrom;
                                                                    return (case mf of
                                                                                None => STYLE "border-style: double; cursor: move"
                                                                              | Some mf =>
                                                                                if mf.Home = ho
                                                                                   && mf.Away = aw
                                                                                   && mf.Time = tm then
                                                                                    STYLE "border-style: double; cursor: move; background-color: green"
                                                                                else
                                                                                    STYLE "border-style: double; cursor: move")}
                                                          onclick={fn _ => stopPropagation;
                                                                      set t.MovingFrom
                                                                          (Some {Home = ho,
                                                                                 Away = aw,
                                                                                 Time = tm})}>
                                                       {[aw]}
                                                       <button class="close"
                                                               data-toggle="modal"
                                                               data-target={"#" ^ show t.ModalId}
                                                               onclick={fn _ =>
                                                                           set t.ModalSpot (makeModal
                                                                             (rpc (unschedule (aw ++ ho ++ tm)))
                                                                             <xml>Are you sure you want to delete the {[tm]} meeting between {[ho]} and {[aw]}?</xml>
                                                                             <xml/>
                                                                             "Yes!")}>
                                                         &times;
                                                       </button>
                                                     </div>
                                                   </xml>) awsv}

                                                   <button class="btn btn-default btn-xs"
                                                           value="+"
                                                           data-toggle="modal"
                                                           data-target={"#" ^ show t.ModalId}
                                                           onclick={fn _ => set t.ModalSpot (makeModal
                                                                              (sel <- get selected;
                                                                               aw <- return (readError sel : $awayKey);
                                                                               rpc (schedule (aw ++ ho ++ tm)))
                                                                              <xml>Adding meeting for {[ho]} at {[tm]}</xml>
                                                                              <xml>
                                                                                <cselect class="form-control"
                                                                                         source={selected}>
                                                                                  {List.mapX (fn aw =>
                                                                                                 if List.mem aw awsv then
                                                                                                     <xml/>
                                                                                                 else
                                                                                                     <xml><coption>{[aw]}</coption></xml>) t.Aways}
                                                                                </cselect>
                                                                              </xml>
                                                                              "Add Meeting")}/>
                                                 </xml>}/>
                                  </xml>}/>
                  </td>
                </xml>) tms}
              </tr>
            </xml>) t.Meetings}
          </table>
        </xml>

        fun tweakMeeting (f : awaySet -> awaySet) (ho : $homeKey) (tm : $timeKey) =
            let
                fun tweakHomes hos =
                    case hos of
                        [] => error <xml>tweakMeeting: unknown home</xml>
                      | (ho', tms) :: hos' =>
                        if ho' = ho then
                            let
                                fun addTimes tms =
                                    case tms of
                                        [] => error <xml>tweakMeeting: unknown time</xml>
                                      | (tm', aws) :: tms' =>
                                        if tm' = tm then
                                            v <- get aws;
                                            set aws (f v)
                                        else
                                            addTimes tms'
                            in
                                addTimes tms
                            end
                        else
                            tweakHomes hos'
            in
                tweakHomes
            end

        fun onload t =
            let
                fun loop () =
                    r <- recv t.Channel;
                    (case r.Operation of
                         Add => tweakMeeting
                                    (fn ls => List.sort (fn x y => show x > show y) (r.Away :: ls))
                                    r.Home r.Time t.Meetings
                       | Del => tweakMeeting
                                    (List.filter (fn aw => aw <> r.Away))
                                    r.Home r.Time t.Meetings);
                    loop ()
            in
                spawn (loop ())
            end
                                                            
    end

    structure OneAway = struct
        type homeSet = list $homeKey
        type timeMap = list ($timeKey * homeSet)
        type t = _

        fun create aw =
            let
                fun doTimes (times : list $timeKey)
                            (rows : list $(timeKey ++ homeKey))
                            (thisTime : homeSet)
                            (acc : timeMap)
                    : timeMap =
                    case times of
                        [] => List.rev acc
                      | tm :: times' =>
                        case rows of
                            [] => doTimes times' [] [] ((tm, List.rev thisTime) :: acc)
                          | row :: rows' =>
                            if row --- homeKey = tm then
                                doTimes times rows' ((row --- timeKey) :: thisTime) acc
                            else
                                doTimes times' rows [] ((tm, List.rev thisTime) :: acc)
            in
                allTimes <- queryL1 (SELECT time.{{timeKey}}
                                     FROM time
                                     ORDER BY {{{@Sql.order_by timeKeyFl
                                       (@Sql.some_fields [#Time] [timeKey] ! ! timeKeyFl)
                                       sql_desc}}});
                meetings <- queryL1 (SELECT meeting.{{timeKey}}, meeting.{{homeKey}}
                                     FROM meeting
                                     ORDER BY {{{@Sql.order_by (@Folder.concat ! timeKeyFl homeKeyFl)
                                       (@Sql.some_fields [#Meeting] [timeKey ++ homeKey] ! !
                                         (@Folder.concat ! timeKeyFl homeKeyFl))
                                       sql_desc}}});
                meetings <- List.mapM (fn (tm, hos) =>
                                          hos <- source hos;
                                          return (tm, hos))
                            (doTimes allTimes meetings [] []);
                ch <- channel;
                @@Sql.easy_insert [[Channel = _] ++ awayKey] [_]
                  ({Channel = _} ++ awayInj')
                  (@Folder.cons [#Channel] [_] ! awayKeyFl)
                  awayListeners
                  ({Channel = ch} ++ aw);
                return {Away = aw, Meetings = meetings, Channel = ch}
            end

        fun render t = <xml>
          <table class="bs3-table table-striped">
            <tr>
              <th>Time</th>
              <th>Meeting</th>
            </tr>

            {List.mapX (fn (tm, hos) => <xml>
              <tr>
                <td>{[tm]}</td>
                <td>
                  <dyn signal={hos <- signal hos;
                               return (case hos of
                                           [] => <xml>&mdash;</xml>
                                         | ho :: hos => <xml>{[ho]}{List.mapX (fn ho => <xml>, {[ho]}</xml>) hos}</xml>)}/>
                                                                                                                                    </td>
              </tr>
            </xml>) t.Meetings}
          </table>
        </xml>

        fun tweakMeeting (f : homeSet -> homeSet) (tm : $timeKey) =
            let
                fun addTimes tms =
                    case tms of
                        [] => error <xml>tweakMeeting[2]: unknown time</xml>
                      | (tm', hos) :: tms' =>
                        if tm' = tm then
                            v <- get hos;
                            set hos (f v)
                        else
                            addTimes tms'
            in
                addTimes
            end

        fun onload t =
            let
                fun loop () =
                    r <- recv t.Channel;
                    (case r.Operation of
                         Add => tweakMeeting
                                    (fn ls => List.sort (fn x y => show x > show y) (r.Home :: ls))
                                    r.Time t.Meetings
                       | Del => tweakMeeting
                                    (List.filter (fn ho => ho <> r.Home))
                                    r.Time t.Meetings);
                    loop ()
            in
                spawn (loop ())
            end

    end

    structure HomePrefs = struct
        type awaySet = list $awayKey
        type t = _

        fun create ho =
            aways <- queryL1 (SELECT away.{{awayKey}}
                              FROM away
                              ORDER BY {{{@Sql.order_by awayKeyFl
                                (@Sql.some_fields [#Away] [awayKey] ! ! awayKeyFl)
                                sql_desc}}});
            prefs <- queryL1 (SELECT preference.{{awayKey}}
                              FROM preference
                              WHERE preference.ByHome
                                AND {@@Sql.easy_where [#Preference] [homeKey] [_] [_] [_] [_]
                                  ! ! homeInj' homeKeyFl ho}
                              ORDER BY {{{@Sql.order_by awayKeyFl
                                (@Sql.some_fields [#Preference] [awayKey] ! ! awayKeyFl)
                                sql_desc}}});
            prefs <- source prefs;
            toAdd <- source "";
            return {Home = ho, Aways = aways, Prefs = prefs, ToAdd = toAdd}

        fun addpref aw ho =
            @@Sql.easy_insert [[ByHome = _] ++ homeKey ++ awayKey] [_] ({ByHome = _} ++ homeInj' ++ awayInj')
              (@Folder.cons [#ByHome] [_] ! (@Folder.concat ! homeKeyFl awayKeyFl))
              preference ({ByHome = True} ++ ho ++ aw)

        fun unpref aw ho =
            dml (DELETE FROM preference
                 WHERE ByHome
                   AND {@@Sql.easy_where [#T] [homeKey ++ awayKey] [_] [_] [_] [_]
                     ! ! (homeInj' ++ awayInj') (@Folder.concat ! homeKeyFl awayKeyFl) (ho ++ aw)})

        fun render t = <xml>
          <table class="bs3-table table-striped">
            <dyn signal={aws <- signal t.Prefs;
                         return <xml>
                           {List.mapX (fn aw => <xml>
                             <tr><td>
                               {[aw]}
                               <button class="close"
                                       onclick={fn _ =>
                                                   rpc (unpref aw t.Home);
                                                   set t.Prefs (List.filter (fn aw' => aw' <> aw) aws)}>
                                 &times;
                               </button>
                             </td></tr>
                             </xml>) aws}

                           <tr><td/></tr>

                           <tr>
                             <td>
                               <cselect class="form-control" source={t.ToAdd}>
                                 {List.mapX (fn aw =>
                                                if List.mem aw aws then
                                                    <xml/>
                                                else
                                                    <xml><coption>{[aw]}</coption></xml>) t.Aways}
                               </cselect>

                               <button class="btn btn-primary"
                                       value="Add Preference"
                                       onclick={fn _ =>
                                                   ta <- get t.ToAdd;
                                                   case ta of
                                                       "" => return ()
                                                     | _ =>
                                                       aw <- return (readError ta);
                                                       rpc (addpref aw t.Home);
                                                       set t.Prefs (List.sort (fn x y => show x > show y) (aw :: aws))}/>
                             </td>
                           </tr>
                         </xml>}/>
          </table>
        </xml>

    end

    structure AwayPrefs = struct
        type homeSet = list $homeKey
        type t = _

        fun create aw =
            homes <- queryL1 (SELECT home.{{homeKey}}
                              FROM home
                              ORDER BY {{{@Sql.order_by homeKeyFl
                                (@Sql.some_fields [#Home] [homeKey] ! ! homeKeyFl)
                                sql_desc}}});
            prefs <- queryL1 (SELECT preference.{{homeKey}}
                              FROM preference
                              WHERE NOT preference.ByHome
                                AND {@@Sql.easy_where [#Preference] [awayKey] [_] [_] [_] [_]
                                  ! ! awayInj' awayKeyFl aw}
                              ORDER BY {{{@Sql.order_by homeKeyFl
                                (@Sql.some_fields [#Preference] [homeKey] ! ! homeKeyFl)
                                sql_desc}}});
            prefs <- source prefs;
            toAdd <- source "";
            return {Away = aw, Homes = homes, Prefs = prefs, ToAdd = toAdd}

        fun addpref aw ho =
            @@Sql.easy_insert [[ByHome = _] ++ homeKey ++ awayKey] [_] ({ByHome = _} ++ homeInj' ++ awayInj')
              (@Folder.cons [#ByHome] [_] ! (@Folder.concat ! homeKeyFl awayKeyFl))
              preference ({ByHome = False} ++ ho ++ aw)

        fun unpref aw ho =
            dml (DELETE FROM preference
                 WHERE NOT ByHome
                   AND {@@Sql.easy_where [#T] [homeKey ++ awayKey] [_] [_] [_] [_]
                     ! ! (homeInj' ++ awayInj') (@Folder.concat ! homeKeyFl awayKeyFl) (ho ++ aw)})

        fun render t = <xml>
          <table class="bs3-table table-striped">
            <dyn signal={hos <- signal t.Prefs;
                         return <xml>
                           {List.mapX (fn ho => <xml>
                             <tr><td>
                               {[ho]}
                               <button class="close"
                                       onclick={fn _ =>
                                                   rpc (unpref t.Away ho);
                                                   set t.Prefs (List.filter (fn ho' => ho' <> ho) hos)}>
                                 &times;
                               </button>
                             </td></tr>
                             </xml>) hos}

                           <tr><td/></tr>

                           <tr>
                             <td>
                               <cselect class="form-control" source={t.ToAdd}>
                                 {List.mapX (fn ho =>
                                                if List.mem ho hos then
                                                    <xml/>
                                                else
                                                    <xml><coption>{[ho]}</coption></xml>) t.Homes}
                               </cselect>

                               <button class="btn btn-primary"
                                       value="Add Preference"
                                       onclick={fn _ =>
                                                   ta <- get t.ToAdd;
                                                   case ta of
                                                       "" => return ()
                                                     | _ =>
                                                       ho <- return (readError ta);
                                                       rpc (addpref t.Away ho);
                                                       set t.Prefs (List.sort (fn x y => show x > show y) (ho :: hos))}/>
                             </td>
                           </tr>
                         </xml>}/>
          </table>
        </xml>

    end

end
