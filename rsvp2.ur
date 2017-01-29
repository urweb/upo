open Bootstrap3

functor Make(M : sig
                 val homeLabel : string
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeData :: {Type}
                 con homeSensitiveData :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeData
                 constraint (homeKey ++ homeRest ++ homeData) ~ homeSensitiveData
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeData ++ homeSensitiveData ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyEq : $(map eq homeKey)
                 val homeDataFl : folder homeData
                 val homeDataShow : $(map show homeData)
                 val homeDataLabels : $(map (fn _ => string) homeData)
                 val homeSensitiveDataFl : folder homeSensitiveData
                 val homeSensitiveDataShow : $(map show homeSensitiveData)
                 val homeSensitiveDataLabels : $(map (fn _ => string) homeSensitiveData)

                 val awayLabel : string
                 con awayKey1 :: Name
                 type awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayData :: {Type}
                 con awaySensitiveData :: {Type}
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 constraint (awayKey ++ awayRest) ~ awayData
                 constraint (awayKey ++ awayRest ++ awayData) ~ awaySensitiveData
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayData ++ awaySensitiveData ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyEq : $(map eq awayKey)
                 val awayDataFl : folder awayData
                 val awayDataShow : $(map show awayData)
                 val awayDataLabels : $(map (fn _ => string) awayData)
                 val awaySensitiveDataFl : folder awaySensitiveData
                 val awaySensitiveDataShow : $(map show awaySensitiveData)
                 val awaySensitiveDataLabels : $(map (fn _ => string) awaySensitiveData)

                 con eventKey1 :: Name
                 type eventKeyT
                 con eventKeyR :: {Type}
                 constraint [eventKey1] ~ eventKeyR
                 con eventKey = [eventKey1 = eventKeyT] ++ eventKeyR
                 con eventData :: {Type} (* Per-guest information to display *)
                 con eventRest :: {Type}
                 constraint eventKey ~ eventRest
                 constraint (eventKey ++ eventRest) ~ eventData
                 con eventKeyName :: Name
                 con eventOtherConstraints :: {{Unit}}
                 constraint [eventKeyName] ~ eventOtherConstraints
                 val event : sql_table (eventKey ++ eventData ++ eventRest) ([eventKeyName = map (fn _ => ()) eventKey] ++ eventOtherConstraints)
                 val render : $eventData -> xbody
                 val eventInj : $(map sql_injectable_prim eventKey)
                 val eventKeyFl : folder eventKey
                 val eventKeyShow : show $eventKey
                 val eventKeyEq : $(map eq eventKey)

                 constraint homeKey ~ eventKey
                 constraint awayKey ~ eventKey

                 val amHome : transaction (option $homeKey)
                 val amPrivilegedHome : transaction (option $homeKey)
                 val amAway : transaction (option $awayKey)

                 val homeMayRsvpTo : $homeKey -> transaction (list $eventKey)
                 val awayMayRsvpTo : $awayKey -> transaction (list $eventKey)
             end) = struct

    open M

    val homeKeyEq : eq $homeKey = @@Record.eq [homeKey] homeKeyEq homeKeyFl
    val awayKeyEq : eq $awayKey = @@Record.eq [awayKey] awayKeyEq awayKeyFl
    val eventKeyEq : eq $eventKey = @@Record.eq [eventKey] eventKeyEq eventKeyFl

    table homeRsvp : (homeKey ++ eventKey)
      PRIMARY KEY {{@primary_key [homeKey1] [homeKeyR ++ eventKey] ! !
                    (homeInj ++ eventInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Event] (@Sql.easy_foreign ! ! ! ! ! ! eventKeyFl event)}}

    table awayRsvp : (awayKey ++ eventKey)
      PRIMARY KEY {{@primary_key [awayKey1] [awayKeyR ++ eventKey] ! !
                    (awayInj ++ eventInj)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}},
      {{one_constraint [#Event] (@Sql.easy_foreign ! ! ! ! ! ! eventKeyFl event)}}

    datatype user h a = Home of ($homeKey * option h) | Away of ($awayKey * option a)
    datatype operation = Add | Del
    type log h a = {Event : $eventKey, User : user h a, Operation : operation}
    table listeners : {Channel : channel (log $homeData $awayData)}
    table privilegedListeners : {Channel : channel (log $(homeData ++ homeSensitiveData) $(awayData ++ awaySensitiveData))}

    val homeInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim homeKeyFl homeInj
    val awayInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim awayKeyFl awayInj
    val eventInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim eventKeyFl eventInj

    functor HomeFn(N : sig
                       con homeShownData :: {Type}
                       val homeShownData : $(homeData ++ homeSensitiveData) -> $homeShownData
                       val homeShownDataFl : folder homeShownData
                       val homeShownDataShow : $(map show homeShownData)
                       val homeShownDataLabels : $(map (fn _ => string) homeShownData)

                       con awayShownData :: {Type}
                       val awayShownData : $(awayData ++ awaySensitiveData) -> $awayShownData
                       val awayShownDataFl : folder awayShownData
                       val awayShownDataShow : $(map show awayShownData)
                       val awayShownDataLabels : $(map (fn _ => string) awayShownData)

                       table my_listeners : {Channel : channel (log $homeShownData $awayShownData)}

                       val amHome : transaction (option $homeKey)
                   end) = struct
        open N

        type home = {Home : $homeKey, Data : $homeShownData}
        type homeEnts = list home
        type away = {Away : $awayKey, Data : $awayShownData}
        type awayEnts = list away
        type event = {Event : $eventKey, Data : $eventData, Home : homeEnts, Away : awayEnts}
        type events = list event
        type input = _
        type a = _

        fun ensure ev ho =
            user <- amHome;
            case user of
                None => error <xml>Must be authenticated to access this page</xml>
              | Some user =>
                if user = ho then
                    allowed <- homeMayRsvpTo ho;
                    if List.mem ev allowed then
                        return ()
                    else
                        error <xml>You are not allowed to RSVP for that event.</xml>
                else
                    error <xml>Wrong user to be accessing this page</xml>

        fun add ev ho =
            ensure ev ho;
            da <- oneRow1 (SELECT home.{{homeData}}, home.{{homeSensitiveData}}
                           FROM home
                           WHERE {@@Sql.easy_where [#Home] [homeKey] [_] [_] [_] [_]
                             ! ! homeInj' homeKeyFl ho});

            @@Sql.easy_insert [eventKey ++ homeKey] [_] (eventInj' ++ homeInj')
              (@Folder.concat ! eventKeyFl homeKeyFl) homeRsvp (ev ++ ho);
            queryI1 (SELECT * FROM listeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Home (ho, Some (da --- homeSensitiveData)),
                                     Operation = Add});
            queryI1 (SELECT * FROM privilegedListeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Home (ho, Some da),
                                     Operation = Add})

        fun del ev ho =
            ensure ev ho;
            dml (DELETE FROM homeRsvp
                 WHERE {@@Sql.easy_where [#T] [eventKey ++ homeKey] [_] [_] [_] [_]
                   ! ! (eventInj' ++ homeInj') (@Folder.concat ! eventKeyFl homeKeyFl) (ev ++ ho)});
            queryI1 (SELECT * FROM listeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Home (ho, None),
                                     Operation = Del});
            queryI1 (SELECT * FROM privilegedListeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Home (ho, None),
                                     Operation = Del})

        fun create ho =
            let
                fun initEvents (allowed : list $eventKey) (acc : events)
                               (evrows : list $(eventKey ++ eventData))
                               (homerows : list {HomeRsvp : $(homeKey ++ eventKey), Home : $homeShownData})
                               (awayrows : list {AwayRsvp : $(awayKey ++ eventKey), Away : $awayShownData})
                               (homes : homeEnts)
                               (aways : awayEnts)
                    : events =
                    case evrows of
                        [] =>
                        (* Processed all events.  Done! *)
                        List.rev acc
                      | ev :: evrows' =>
                        let
                            (* Check to see if the first home row is for this event. *)
                            val opt =
                                case homerows of
                                    [] => None
                                  | r :: homerows' =>
                                    if @eq eventKeyEq (r.HomeRsvp --- homeKey) (ev --- eventData) then
                                        Some ({Home = r.HomeRsvp --- eventKey, Data = r.Home}, homerows')
                                    else
                                        None
                        in
                            case opt of
                                Some (ent, homerows') =>
                                initEvents allowed
                                           acc
                                           evrows
                                           homerows'
                                           awayrows
                                           (ent :: homes)
                                           aways
                              | None =>
                                let
                                    (* Check to see if the first away row is for this event. *)
                                    val opt =
                                        case awayrows of
                                            [] => None
                                          | r :: awayrows' =>
                                            if @eq eventKeyEq (r.AwayRsvp --- awayKey) (ev --- eventData) then
                                                Some ({Away = r.AwayRsvp --- eventKey, Data = r.Away}, awayrows')
                                            else
                                                None
                                in
                                    case opt of
                                        Some (ent, awayrows') =>
                                        initEvents allowed
                                                   acc
                                                   evrows
                                                   homerows
                                                   awayrows'
                                                   homes
                                                   (ent :: aways)
                                      | None =>
                                        (* No matching attendees remain.  Add this event record. *)
                                        initEvents allowed
                                                   (if List.mem ((ev --- eventData) : $eventKey) allowed then
                                                        {Event = ev --- eventData,
                                                         Data = ev --- eventKey,
                                                         Home = List.rev homes,
                                                         Away = List.rev aways} :: acc
                                                    else
                                                        acc)
                                                   evrows'
                                                   homerows
                                                   awayrows
                                                   []
                                                   []
                                end
                        end
            in
                events <- queryL1 (SELECT event.{{eventKey}}, event.{{eventData}}
                                   FROM event
                                   ORDER BY {{{@Sql.order_by eventKeyFl
                                     (@Sql.some_fields [#Event] [eventKey] ! ! eventKeyFl)
                                     sql_asc}}});
                allowed <- homeMayRsvpTo ho;
                homes <- queryL (SELECT homeRsvp.*, home.{{homeData}}, home.{{homeSensitiveData}}
                                 FROM homeRsvp
                                   JOIN home ON {@@Sql.easy_join [#HomeRsvp] [#Home] [homeKey]
                                     [eventKey] [homeData ++ homeSensitiveData ++ homeRest] [_] [_] [_]
                                     ! ! ! ! homeKeyFl}
                                 ORDER BY {{{@Sql.order_by (@Folder.concat ! eventKeyFl homeKeyFl)
                                    (@Sql.some_fields [#HomeRsvp] [eventKey ++ homeKey] ! !
                                     (@Folder.concat ! eventKeyFl homeKeyFl))
                                    sql_asc}}});
                aways <- queryL (SELECT awayRsvp.*, away.{{awayData}}, away.{{awaySensitiveData}}
                                 FROM awayRsvp
                                   JOIN away ON {@@Sql.easy_join [#AwayRsvp] [#Away] [awayKey]
                                     [eventKey] [awayData ++ awaySensitiveData ++ awayRest] [_] [_] [_]
                                     ! ! ! ! awayKeyFl}
                                 ORDER BY {{{@Sql.order_by (@Folder.concat ! eventKeyFl awayKeyFl)
                                    (@Sql.some_fields [#AwayRsvp] [eventKey ++ awayKey] ! !
                                     (@Folder.concat ! eventKeyFl awayKeyFl))
                                    sql_asc}}});
                events <- List.mapM (fn r =>
                                        homes <- source r.Home;
                                        aways <- source r.Away;
                                        return (r -- #Home -- #Away ++ {Home = homes, Away = aways}))
                                    (initEvents allowed [] events
                                                (List.mp (fn r => r -- #Home ++ {Home = homeShownData r.Home}) homes)
                                                (List.mp (fn r => r -- #Away ++ {Away = awayShownData r.Away}) aways)
                                                [] []);
                chan <- channel;
                dml (INSERT INTO my_listeners (Channel) VALUES ({[chan]}));
                return {Self = ho, Events = events, Channel = chan}
            end

        fun render t =
            List.mapX (fn ev => <xml>
              <active code={expanded <- source False;
                            return <xml><div>
                              <h2>
                                <button dynClass={exp <- signal expanded;
                                                  return (if exp then
                                                              CLASS "btn btn-xs glyphicon glyphicon-chevron-up"
                                                          else
                                                              CLASS "btn btn-xs glyphicon glyphicon-chevron-down")}
                                        onclick={fn _ =>
                                                    exp <- get expanded;
                                                    set expanded (not exp)}/>
                                {[@show eventKeyShow ev.Event]}
                              </h2>

                              <dyn signal={exp <- signal expanded;
                                           if not exp then
                                               return <xml/>
                                           else
                                               hos <- signal ev.Home;
                                               aws <- signal ev.Away;
                                               count <- return (List.length hos + List.length aws);
                                               return <xml>
                                                 <div>{M.render ev.Data}</div>
                                                 <div>({[count]} attendee{[if count = 1 then "" else "s"]})</div>
                                                 {if List.exists (fn ho => ho.Home = t.Self) hos then
                                                      <xml>
                                                        <span class="glyphicon glyphicon-ok"/>
                                                        <span>Attending</span>
                                                        <button class="btn btn-primary"
                                                                value="Un-RSVP"
                                                                onclick={fn _ =>
                                                                            rpc (del ev.Event t.Self)}/>
                                                      </xml>
                                                  else
                                                      <xml>
                                                        <span>Not attending</span>
                                                        <button class="btn btn-primary"
                                                                value="RSVP"
                                                                onclick={fn _ =>
                                                                            rpc (add ev.Event t.Self)}/>
                                                      </xml>}

                                                 {case aws of
                                                      [] => <xml/>
                                                    | _ => <xml>
                                                      <h3>{[awayLabel]} ({[List.length aws]})</h3>

                                                      <table class="bs3-table table-striped">
                                                        <tr>
                                                          <th/>
                                                          {@mapX [fn _ => string] [tr]
                                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] label =>
                                                                <xml><th>{[label]}</th></xml>)
                                                            awayShownDataFl awayShownDataLabels}
                                                        </tr>

                                                        {List.mapX (fn aw => <xml><tr>
                                                          <td>{[aw.Away]}</td>
                                                          {@mapX2 [show] [ident] [tr]
                                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) =>
                                                                <xml><td>{[x]}</td></xml>)
                                                            awayShownDataFl awayShownDataShow aw.Data}
                                                        </tr></xml>) aws}
                                                      </table>
                                                    </xml>}

                                                 {case hos of
                                                      [] => <xml/>
                                                    | _ => <xml>
                                                      <h3>{[homeLabel]} ({[List.length hos]})</h3>

                                                      <table class="bs3-table table-striped">
                                                        <tr>
                                                          <th/>
                                                          {@mapX [fn _ => string] [tr]
                                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] label =>
                                                                <xml><th>{[label]}</th></xml>)
                                                            homeShownDataFl homeShownDataLabels}
                                                        </tr>

                                                        {List.mapX (fn ho => <xml><tr>
                                                          <td>{[ho.Home]}</td>
                                                          {@mapX2 [show] [ident] [tr]
                                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) =>
                                                                <xml><td>{[x]}</td></xml>)
                                                            homeShownDataFl homeShownDataShow ho.Data}
                                                        </tr></xml>) hos}
                                                      </table>
                                                    </xml>}
                                               </xml>}/>
                            </div></xml>}/>
            </xml>) t.Events

        fun tweakEvent (f : {Home : homeEnts, Away : awayEnts} -> {Home : homeEnts, Away : awayEnts})
            (ev : $eventKey) =
            let
                fun tweakEvents evs =
                    case evs of
                        [] => error <xml>Rsvp2.tweakEvent: unknown event</xml>
                      | ev' :: evs' =>
                        if ev'.Event = ev then
                            hos <- get ev'.Home;
                            aws <- get ev'.Away;
                            r <- return (f {Home = hos, Away = aws});
                            set ev'.Home r.Home;
                            set ev'.Away r.Away
                        else
                            tweakEvents evs'
            in
                tweakEvents
            end

        fun onload t =
            let
                fun loop () =
                    r <- recv t.Channel;
                    (case r.Operation of
                         Add =>
                         (case r.User of
                              Home (ho, da) =>
                              (case da of
                                   None => error <xml>Rsvp2: no data for new home entry</xml>
                                 | Some da =>
                                   tweakEvent (fn r => r -- #Home
                                                         ++ {Home =
                                                             List.sort (fn x y => show x.Home > show y.Home)
                                                                       ({Home = ho, Data = da} :: r.Home)})
                                              r.Event t.Events)
                            | Away (aw, da) =>
                              (case da of
                                   None => error <xml>Rsvp2: no data for new away entry</xml>
                                 | Some da =>
                                   tweakEvent (fn r => r -- #Away
                                                         ++ {Away =
                                                             List.sort (fn x y => show x.Away > show y.Away)
                                                                       ({Away = aw, Data = da} :: r.Away)})
                                              r.Event t.Events))
                       | Del =>
                         (case r.User of
                              Home (ho, _) =>
                              tweakEvent (fn r => r -- #Home
                                                    ++ {Home = List.filter (fn x => x.Home <> ho) r.Home})
                                         r.Event t.Events
                            | Away (aw, _) =>
                              tweakEvent (fn r => r -- #Away
                                                    ++ {Away = List.filter (fn x => x.Away <> aw) r.Away})
                                         r.Event t.Events));
                    loop ()
            in
                spawn (loop ())
            end

        fun ui x = {
            Create = create x,
            Onload = onload,
            Render = fn _ => render
        }
    end

    structure Home = HomeFn(struct
                                con homeShownData = homeData
                                fun homeShownData r = r --- homeSensitiveData
                                val homeShownDataFl = homeDataFl
                                val homeShownDataShow = homeDataShow
                                val homeShownDataLabels = homeDataLabels

                                con awayShownData = awayData
                                fun awayShownData r = r --- awaySensitiveData
                                val awayShownDataFl = awayDataFl
                                val awayShownDataShow = awayDataShow
                                val awayShownDataLabels = awayDataLabels

                                val my_listeners = listeners
                                val amHome = amHome
                            end)

    structure HomePrivileged = HomeFn(struct
                                          con homeShownData = homeData ++ homeSensitiveData
                                          fun homeShownData r = r
                                          val homeShownDataFl = @Folder.concat ! homeDataFl homeSensitiveDataFl
                                          val homeShownDataShow = homeDataShow ++ homeSensitiveDataShow
                                          val homeShownDataLabels = homeDataLabels ++ homeSensitiveDataLabels

                                          con awayShownData = awayData ++ awaySensitiveData
                                          fun awayShownData r = r
                                          val awayShownDataFl = @Folder.concat ! awayDataFl awaySensitiveDataFl
                                          val awayShownDataShow = awayDataShow ++ awaySensitiveDataShow
                                          val awayShownDataLabels = awayDataLabels ++ awaySensitiveDataLabels

                                          val my_listeners = privilegedListeners
                                          val amHome = amPrivilegedHome
                                      end)

    structure Away = struct
        type event = {Event : $eventKey, Data : $eventData, Registered : source bool}
        type events = list event
        type input = _
        type a = _


        fun ensure ev aw =
            user <- amAway;
            case user of
                None => error <xml>Must be authenticated to access this page</xml>
              | Some user =>
                if user = aw then
                    allowed <- awayMayRsvpTo aw;
                    if List.mem ev allowed then
                        return ()
                    else
                        error <xml>You are not allowed to RSVP for that event.</xml>
                else
                    error <xml>Wrong user to be accessing this page</xml>

        fun add ev aw =
            ensure ev aw;
            da <- oneRow1 (SELECT away.{{awayData}}, away.{{awaySensitiveData}}
                           FROM away
                           WHERE {@@Sql.easy_where [#Away] [awayKey] [_] [_] [_] [_]
                             ! ! awayInj' awayKeyFl aw});

            @@Sql.easy_insert [eventKey ++ awayKey] [_] (eventInj' ++ awayInj')
              (@Folder.concat ! eventKeyFl awayKeyFl) awayRsvp (ev ++ aw);
            queryI1 (SELECT * FROM listeners)
                    (fn r => send r.Channel {Event = ev,
                                             User = Away (aw, Some (da --- awaySensitiveData)),
                                             Operation = Add});
            queryI1 (SELECT * FROM privilegedListeners)
                    (fn r => send r.Channel {Event = ev,
                                             User = Away (aw, Some da),
                                             Operation = Add})

        fun del ev aw =
            ensure ev aw;
            dml (DELETE FROM awayRsvp
                 WHERE {@@Sql.easy_where [#T] [eventKey ++ awayKey] [_] [_] [_] [_]
                   ! ! (eventInj' ++ awayInj') (@Folder.concat ! eventKeyFl awayKeyFl) (ev ++ aw)});
            queryI1 (SELECT * FROM listeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Away (aw, None),
                                     Operation = Del});
            queryI1 (SELECT * FROM privilegedListeners)
            (fn r => send r.Channel {Event = ev,
                                     User = Away (aw, None),
                                     Operation = Del})

        fun create aw =
            events <- List.mapQueryM (SELECT event.{{eventKey}}, event.{{eventData}},
                                        NOT ((SELECT TRUE
                                              FROM awayRsvp
                                              WHERE {@@Sql.easy_where [#AwayRsvp] [awayKey] [_] [_] [_] [_]
                                                  ! ! awayInj' awayKeyFl aw}
                                                AND {@@Sql.easy_join [#Event] [#AwayRsvp] [eventKey]
                                                  [eventData ++ eventRest] [awayKey] [_] [_] [_]
                                                  ! ! ! ! eventKeyFl}) IS NULL) AS Regd
                                      FROM event
                                      ORDER BY {{{@Sql.order_by eventKeyFl
                                                   (@Sql.some_fields [#Event] [eventKey] ! ! eventKeyFl)
                                                   sql_asc}}})
                      (fn r =>
                          regd <- source r.Regd;
                          return {Event = r.Event --- eventData,
                                  Data = r.Event --- eventKey,
                                  Registered = regd});
            allowed <- awayMayRsvpTo aw;
            events <- return (List.filter (fn r => List.mem r.Event allowed) events);
            return {Self = aw, Events = events}

        fun render t =
            List.mapX (fn ev => <xml>
              <active code={expanded <- source False;
                            return <xml><div>
                              <h2>
                                <button dynClass={exp <- signal expanded;
                                                  return (if exp then
                                                              CLASS "btn btn-xs glyphicon glyphicon-chevron-up"
                                                          else
                                                              CLASS "btn btn-xs glyphicon glyphicon-chevron-down")}
                                        onclick={fn _ =>
                                                    exp <- get expanded;
                                                    set expanded (not exp)}/>
                                {[@show eventKeyShow ev.Event]}
                              </h2>

                              <dyn signal={exp <- signal expanded;
                                           if not exp then
                                               return <xml/>
                                           else
                                               regd <- signal ev.Registered;
                                               return <xml>
                                                 <div>{M.render ev.Data}</div>
                                                 {if regd then
                                                      <xml>
                                                        <span class="glyphicon glyphicon-ok"/>
                                                        <span>Attending</span>
                                                        <button class="btn btn-primary"
                                                                value="Un-RSVP"
                                                                onclick={fn _ =>
                                                                            rpc (del ev.Event t.Self);
                                                                            set ev.Registered False}/>
                                                      </xml>
                                                  else
                                                      <xml>
                                                        <span>Not attending</span>
                                                        <button class="btn btn-primary"
                                                                value="RSVP"
                                                                onclick={fn _ =>
                                                                            rpc (add ev.Event t.Self);
                                                                            set ev.Registered True}/>
                                                      </xml>}
                                               </xml>}/>
                            </div></xml>}/>
            </xml>) t.Events

        fun ui x = {
            Create = create x,
            Onload = fn _ => return (),
            Render = fn _ => render
        }

    end

end
