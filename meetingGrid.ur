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
                 constraint (homeKey ++ awayKey) ~ [ByHome, Channel]
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

    type home_action = { Operation : operation, Away : $awayKey, Time : $timeKey }
    table homeListeners : ([Channel = channel home_action] ++ homeKey)
      PRIMARY KEY {{@primary_key [homeKey1] [homeKeyR] ! ! homeInj}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}}

    type away_action = { Operation : operation, Home : $homeKey, Time : $timeKey }
    table awayListeners : ([Channel = channel away_action] ++ awayKey)
      PRIMARY KEY {{@primary_key [awayKey1] [awayKeyR] ! ! awayInj}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    table preference : ([ByHome = bool] ++ homeKey ++ awayKey)
      PRIMARY KEY {{@primary_key [#ByHome] [homeKey ++ awayKey] ! !
                    ({ByHome = _} ++ homeInj ++ awayInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    con all = homeKey ++ awayKey ++ timeKey
    val allFl = @Folder.concat ! homeKeyFl (@Folder.concat ! timeKeyFl awayKeyFl)

    val allInj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim allFl (homeInj ++ awayInj ++ timeInj)
    val homeInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim homeKeyFl homeInj
    val awayInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim awayKeyFl awayInj

    fun addMeeting r =
        @@Sql.easy_insert [all] [_] allInj allFl meeting r;
        queryI1 (SELECT * FROM globalListeners)
                (fn i => send i.Channel {Operation = Add,
                                         Home = r --- awayKey --- timeKey,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
        queryI1 (SELECT * FROM homeListeners
                 WHERE {@@Sql.easy_where [#HomeListeners] [homeKey] [_] [_] [_] [_]
                   ! ! homeInj' homeKeyFl (r --- awayKey --- timeKey)})
                (fn i => send i.Channel {Operation = Add,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
        queryI1 (SELECT * FROM awayListeners
                 WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                   ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Add,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey})

    fun delMeeting r =
        dml (DELETE FROM meeting
             WHERE {@@Sql.easy_where [#T] [all] [_] [_] [_] [_]
               ! ! allInj allFl r});
        queryI1 (SELECT * FROM globalListeners)
                (fn i => send i.Channel {Operation = Del,
                                         Home = r --- awayKey --- timeKey,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
        queryI1 (SELECT * FROM homeListeners
                 WHERE {@@Sql.easy_where [#HomeListeners] [homeKey] [_] [_] [_] [_]
                   ! ! homeInj' homeKeyFl (r --- awayKey --- timeKey)})
                (fn i => send i.Channel {Operation = Del,
                                         Away = r --- homeKey --- timeKey,
                                         Time = r --- awayKey --- homeKey});
        queryI1 (SELECT * FROM awayListeners
                          WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                            ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Del,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey})

    (* This functor helps us abstract over the two directions.
     * We want symmetric functionality for each. *)
    functor Side(N : sig
                     con usKey :: {Type}
                     con usOther :: {Type}
                     con themKey :: {Type}
                     con themOther :: {Type}

                     constraint usKey ~ usOther
                     constraint themKey ~ themOther
                     constraint usKey ~ themKey
                     constraint (usKey ++ themKey) ~ timeKey

                     val localized : { Home : $homeKey, Away : $awayKey }
                                     -> { Us : $usKey, Them : $themKey }
                     val canonical : { Us : $usKey, Them : $themKey }
                                     -> { Home : $homeKey, Away : $awayKey }

                     table us : (usKey ++ usOther)
                     table them : (themKey ++ themOther)

                     table preference : ([ByHome = bool] ++ usKey ++ themKey)

                     table meeting : (usKey ++ themKey ++ timeKey)

                     constraint usKey ~ [Channel]
                     type usChannel
                     val usChannel : usChannel
                                     -> { Operation : operation,
                                          Them : $themKey,
                                          Time : $timeKey }
                     table usListeners : ([Channel = channel usChannel] ++ usKey)


                     val usFl : folder usKey
                     val themFl : folder themKey

                     val usInj : $(map sql_injectable_prim usKey)
                     val themInj : $(map sql_injectable_prim themKey)

                     val usShow : show $usKey
                     val themShow : show $themKey

                     val themRead : read $themKey

                     val usEq : eq $usKey
                     val themEq : eq $themKey
                 end) = struct

        open N

        con all = usKey ++ themKey ++ timeKey
        val allFl = @Folder.concat ! usFl (@Folder.concat ! timeKeyFl themFl)
        val allInj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim allFl (usInj ++ themInj ++ timeInj)
        val usInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim usFl usInj
        val themInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim themFl themInj

        fun canonicalized r =
            let
                val r' = canonical {Us = r --- timeKey --- themKey,
                                    Them = r --- timeKey --- usKey}
            in
                r --- usKey --- themKey ++ r'.Home ++ r'.Away
            end

        val addMeeting r = addMeeting (canonicalized r)
        val delMeeting r = delMeeting (canonicalized r)

        structure FullGrid = struct
            type themSet = list $themKey
            type timeMap = list ($timeKey * themSet)
            type usMap = list ($usKey * timeMap)
            type t = _

            val create =
                allTimes <- queryL1 (SELECT time.{{timeKey}}
                                     FROM time
                                     ORDER BY {{{@Sql.order_by timeKeyFl
                                       (@Sql.some_fields [#Time] [timeKey] ! ! timeKeyFl)
                                       sql_desc}}});

                let
                    (* A bit of a little dance to initialize the meeting states,
                     * including blank entries for unused us/time pairs *)
                    fun initMap (acc : usMap)
                                (rows : list $all)
                                (uses : list $usKey)
                                (times : list $timeKey)
                                (themsDone : themSet)
                                (timesDone : timeMap)
                        : usMap =
                        case uses of
                            [] =>
                            (* Finished with all uses.  Done! *)
                            List.rev acc
                          | us :: uses' =>
                            case times of
                                [] =>
                                (* Finished with one us.  Move to next. *)
                                initMap ((us, List.rev timesDone) :: acc)
                                        rows
                                        uses'
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
                                            uses
                                            times'
                                            []
                                            ((time, List.rev themsDone) :: timesDone)
                                  | row :: rows' =>
                                    if row --- themKey --- timeKey = us && @eq timeKeyEq (row --- usKey --- themKey) time then
                                        (* Aha, a match!  Record this meeting. *)
                                        initMap acc
                                                rows'
                                                uses
                                                times
                                                ((row --- usKey --- timeKey) :: themsDone)
                                                timesDone
                                    else
                                        (* No match.  On to next time. *)
                                        initMap acc
                                                rows
                                                uses
                                                times'
                                                []
                                                ((time, List.rev themsDone) :: timesDone)
                in
                    uses <- queryL1 (SELECT us.{{usKey}}
                                     FROM us
                                     ORDER BY {{{@Sql.order_by usFl
                                        (@Sql.some_fields [#Us] [usKey] ! ! usFl)
                                        sql_desc}}});
                    thems <- queryL1 (SELECT them.{{themKey}}
                                      FROM them
                                      ORDER BY {{{@Sql.order_by themFl
                                        (@Sql.some_fields [#Them] [themKey] ! ! themFl)
                                        sql_desc}}});
                    meetings <- queryL1 (SELECT meeting.{{usKey}}, meeting.{{timeKey}}, meeting.{{themKey}}
                                         FROM meeting
                                         ORDER BY {{{@Sql.order_by allFl
                                           (@Sql.some_fields [#Meeting] [all] ! ! allFl)
                                           sql_desc}}});
                    meetings <- List.mapM (fn (us, tms) =>
                                              tms' <- List.mapM (fn (tm, ths) =>
                                                                    ths <- source ths;
                                                                    return (tm, ths)) tms;
                                              return (us, tms'))
                                          (initMap []
                                                   meetings
                                                   uses
                                                   allTimes
                                                   []
                                                   []);
                    mid <- fresh;
                    modalSpot <- source <xml/>;
                    chan <- channel;
                    dml (INSERT INTO globalListeners (Channel) VALUES ({[chan]}));
                    mf <- source None;
                    mt <- source None;
                    return {Thems = thems, Times = allTimes, Meetings = meetings,
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
                    addMeeting r

            fun unschedule r =
                alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                              FROM meeting
                                              WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                                ! ! allInj allFl r});
                if not alreadyScheduled then
                    return ()
                else
                    delMeeting r

            fun reschedule (r : {Them : $themKey, OldUs : $usKey, OldTime : $timeKey, NewUs : $usKey, NewTime : $timeKey}) =
                alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                              FROM meeting
                                              WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                                ! ! allInj allFl (r.Them ++ r.OldUs ++ r.OldTime)});
                spotUsed <- oneRowE1 (SELECT COUNT( * ) > 0
                                      FROM meeting
                                      WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                        ! ! allInj allFl (r.Them ++ r.NewUs ++ r.NewTime)});
                if not alreadyScheduled || spotUsed then
                    return ()
                else
                    delMeeting (r.Them ++ r.OldUs ++ r.OldTime);
                    addMeeting (r.Them ++ r.NewUs ++ r.NewTime)

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

                (* One row per us *)
                {List.mapX (fn (us, tms) => <xml>
                  <tr>
                    <th>{[us]}</th>

                    (* One column per time *)
                    {List.mapX (fn (tm, ths) => <xml>
                      <td dynClass={mf <- signal t.MovingFrom;
                                    case mf of
                                        None => return (CLASS "")
                                      | Some _ =>
                                        mt <- signal t.MovingTo;
                                        return (case mt of
                                                    None => CLASS ""
                                                  | Some mt =>
                                                    if mt.Us = us && mt.Time = tm then
                                                        CLASS "bs3-active"
                                                    else
                                                        CLASS "")}
                          onmouseover={fn _ =>
                                          set t.MovingTo (Some {Us = us, Time = tm})}
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
                                              rpc (reschedule {Them = mf.Them,
                                                               OldUs = mf.Us,
                                                               OldTime = mf.Time,
                                                               NewUs = mt.Us,
                                                               NewTime = mt.Time})}>
                        <active code={selected <- source "";
                                      deleting <- source False;
                                      return <xml>
                                        <dyn signal={thsv <- signal ths;
                                                     (* One button per meeting *)
                                                     return <xml>
                                                       {List.mapX (fn th => <xml>
                                                         <div dynStyle={mf <- signal t.MovingFrom;
                                                                        return (case mf of
                                                                                    None => STYLE "border-style: double; cursor: move"
                                                                                  | Some mf =>
                                                                                    if mf.Us = us
                                                                                       && mf.Them = th
                                                                                       && mf.Time = tm then
                                                                                        STYLE "border-style: double; cursor: move; background-color: green"
                                                                                    else
                                                                                        STYLE "border-style: double; cursor: move")}
                                                              onclick={fn _ =>
                                                                          del <- get deleting;
                                                                          if del then
                                                                              set deleting False
                                                                          else
                                                                              stopPropagation;
                                                                              set t.MovingFrom
                                                                                  (Some {Us = us,
                                                                                         Them = th,
                                                                                         Time = tm})}>
                                                           {[th]}
                                                           <button class="close"
                                                                   data-toggle="modal"
                                                                   data-target={"#" ^ show t.ModalId}
                                                                   onclick={fn _ =>
                                                                               set deleting True;
                                                                               set t.ModalSpot (Theme.makeModal
                                                                                 (rpc (unschedule (th ++ us ++ tm)))
                                                                                 <xml>Are you sure you want to delete the {[tm]} meeting between {[us]} and {[th]}?</xml>
                                                                                 <xml/>
                                                                                 "Yes!")}>
                                                             &times;
                                                           </button>
                                                         </div>
                                                       </xml>) thsv}

                                                       <button class="btn btn-default btn-xs"
                                                               value="+"
                                                               data-toggle="modal"
                                                               data-target={"#" ^ show t.ModalId}
                                                               onclick={fn _ => set t.ModalSpot (Theme.makeModal
                                                                                  (sel <- get selected;
                                                                                   th <- return (readError sel : $themKey);
                                                                                   rpc (schedule (th ++ us ++ tm)))
                                                                                  <xml>Adding meeting for {[us]} at {[tm]}</xml>
                                                                                  <xml>
                                                                                    <cselect class="form-control"
                                                                                             source={selected}>
                                                                                      {List.mapX (fn th =>
                                                                                                     if List.mem th thsv then
                                                                                                         <xml/>
                                                                                                     else
                                                                                                         <xml><coption>{[th]}</coption></xml>) t.Thems}
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

            fun tweakMeeting (f : themSet -> themSet) (us : $usKey) (tm : $timeKey) =
                let
                    fun tweakUses uses =
                        case uses of
                            [] => error <xml>FullGrid.tweakMeeting: unknown us</xml>
                          | (us', tms) :: uses' =>
                            if us' = us then
                                let
                                    fun addTimes tms =
                                        case tms of
                                            [] => error <xml>FullGrid.tweakMeeting: unknown time</xml>
                                          | (tm', ths) :: tms' =>
                                            if tm' = tm then
                                                v <- get ths;
                                                set ths (f v)
                                            else
                                                addTimes tms'
                                in
                                    addTimes tms
                                end
                            else
                                tweakUses uses'
                in
                    tweakUses
                end

            fun onload t =
                let
                    fun loop () =
                        r <- recv t.Channel;
                        let
                            val r' = localized (r -- #Operation -- #Time)
                        in
                            case r.Operation of
                                Add => tweakMeeting
                                           (fn ls => List.sort (fn x y => show x > show y) (r'.Them :: ls))
                                           r'.Us r.Time t.Meetings
                              | Del => tweakMeeting
                                           (List.filter (fn th => th <> r'.Them))
                                           r'.Us r.Time t.Meetings
                        end;
                        loop ()
                in
                    spawn (loop ())
                end

        end

        structure One = struct
            type themSet = list $themKey
            type timeMap = list ($timeKey * themSet)
            type t = _

            fun create us =
                let
                    fun doTimes (times : list $timeKey)
                                (rows : list $(timeKey ++ themKey))
                                (thisTime : themSet)
                                (acc : timeMap)
                        : timeMap =
                        case times of
                            [] => List.rev acc
                          | tm :: times' =>
                            case rows of
                                [] => doTimes times' [] [] ((tm, List.rev thisTime) :: acc)
                              | row :: rows' =>
                                if row --- themKey = tm then
                                    doTimes times rows' ((row --- timeKey) :: thisTime) acc
                                else
                                    doTimes times' rows [] ((tm, List.rev thisTime) :: acc)
                in
                    allTimes <- queryL1 (SELECT time.{{timeKey}}
                                         FROM time
                                         ORDER BY {{{@Sql.order_by timeKeyFl
                                           (@Sql.some_fields [#Time] [timeKey] ! ! timeKeyFl)
                                           sql_desc}}});
                    meetings <- queryL1 (SELECT meeting.{{timeKey}}, meeting.{{themKey}}
                                         FROM meeting
                                         WHERE {@@Sql.easy_where [#Meeting] [usKey] [_] [_] [_] [_]
                                           ! ! usInj' usFl us}
                                         ORDER BY {{{@Sql.order_by (@Folder.concat ! timeKeyFl themFl)
                                           (@Sql.some_fields [#Meeting] [timeKey ++ themKey] ! !
                                             (@Folder.concat ! timeKeyFl themFl))
                                           sql_desc}}});
                    meetings <- List.mapM (fn (tm, ths) =>
                                              ths <- source ths;
                                              return (tm, ths))
                                (doTimes allTimes meetings [] []);
                    ch <- channel;
                    @@Sql.easy_insert [[Channel = _] ++ usKey] [_]
                      ({Channel = _} ++ usInj')
                      (@Folder.cons [#Channel] [_] ! usFl)
                      usListeners
                      ({Channel = ch} ++ us);
                    return {Us = us, Meetings = meetings, Channel = ch}
                end

            fun render t = <xml>
              <table class="bs3-table table-striped">
                <tr>
                  <th>Time</th>
                  <th>Meeting</th>
                </tr>

                {List.mapX (fn (tm, ths) => <xml>
                  <tr>
                    <td>{[tm]}</td>
                    <td>
                      <dyn signal={ths <- signal ths;
                                   return (case ths of
                                               [] => <xml>&mdash;</xml>
                                             | th :: ths => <xml>{[th]}{List.mapX (fn th => <xml>, {[th]}</xml>) ths}</xml>)}/>
                                                                                                                                        </td>
                  </tr>
                </xml>) t.Meetings}
              </table>
            </xml>

            fun tweakMeeting (f : themSet -> themSet) (tm : $timeKey) =
                let
                    fun addTimes tms =
                        case tms of
                            [] => error <xml>One.tweakMeeting: unknown time</xml>
                          | (tm', ths) :: tms' =>
                            if tm' = tm then
                                v <- get ths;
                                set ths (f v)
                            else
                                addTimes tms'
                in
                    addTimes
                end

            fun onload t =
                let
                    fun loop () =
                        r <- recv t.Channel;
                        let
                            val r = usChannel r
                        in
                            case r.Operation of
                                Add => tweakMeeting
                                           (fn ls => List.sort (fn x y => show x > show y) (r.Them :: ls))
                                           r.Time t.Meetings
                              | Del => tweakMeeting
                                           (List.filter (fn th => th <> r.Them))
                                           r.Time t.Meetings
                        end;
                        loop ()
                in
                    spawn (loop ())
                end

        end
    end

    structure Home = Side(struct
                              con usKey = homeKey
                              con themKey = awayKey

                              fun localized r = {Us = r.Home, Them = r.Away}
                              fun canonical r = {Home = r.Us, Away = r.Them}

                              val us = home
                              val them = away

                              val preference = preference

                              val meeting = meeting

                              fun usChannel r = r -- #Away ++ {Them = r.Away}
                              val usListeners = homeListeners

                              val usInj = homeInj
                              val themInj = awayInj

                              val usFl = homeKeyFl
                              val themFl = awayKeyFl
                          end)

    structure Away = Side(struct
                              con usKey = awayKey
                              con themKey = homeKey

                              fun localized r = {Us = r.Away, Them = r.Home}
                              fun canonical r = {Away = r.Us, Home = r.Them}

                              val us = away
                              val them = home

                              val preference = preference

                              val meeting = meeting

                              fun usChannel r = r -- #Home ++ {Them = r.Home}
                              val usListeners = awayListeners

                              val usInj = awayInj
                              val themInj = homeInj

                              val usFl = awayKeyFl
                              val themFl = homeKeyFl
                          end)


    structure HomePrefs = ChooseForeign.Make(struct
                                                 val const = {ByHome = True}
                                                 con given = homeKey
                                                 con chosen = awayKey

                                                 val choices = preference
                                                 val options = away
                                                 val buttonLabel = "Add Preference"

                                                 val givenInj = homeInj'
                                                 val chosenInj = awayInj'

                                                 val givenFl = homeKeyFl
                                                 val chosenFl = awayKeyFl
                                             end)

    structure AwayPrefs = ChooseForeign.Make(struct
                                                 val const = {ByHome = False}
                                                 con given = awayKey
                                                 con chosen = homeKey

                                                 val choices = preference
                                                 val options = home
                                                 val buttonLabel = "Add Preference"

                                                 val givenInj = awayInj'
                                                 val chosenInj = homeInj'

                                                 val givenFl = awayKeyFl
                                                 val chosenFl = homeKeyFl
                                             end)

end
