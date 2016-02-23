style meeting_default
style meeting_selected
style meeting_conflict

open Bootstrap3

functor Make(M : sig
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeOffice :: {Type}
                 con homeHardConst :: {Type}
                 con homeSoftConst :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeOffice
                 constraint (homeKey ++ homeRest ++ homeOffice) ~ homeSoftConst
                 constraint (homeKey ++ homeRest ++ homeOffice ++ homeSoftConst) ~ homeHardConst
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeOffice ++ homeSoftConst ++ homeHardConst ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyRead : read $homeKey
                 val homeKeyEq : $(map eq homeKey)
                 val homeKeyOrd : $(map ord homeKey)
                 val officeFl : folder homeOffice
                 val officeShow : show $homeOffice
                 val homeSoftConstFl : folder homeSoftConst
                 val homeSoftConstInj : $(map sql_injectable homeSoftConst)
                 val homeSoftConst : $homeSoftConst
                 val homeHardConstFl : folder homeHardConst
                 val homeHardConstInj : $(map sql_injectable homeHardConst)
                 val homeHardConst : $homeHardConst

                 con awayKey1 :: Name
                 type awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayConst :: {Type}
                 con awayFilter :: {Type}
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 constraint (awayKey ++ awayRest) ~ awayConst
                 constraint (awayKey ++ awayRest ++ awayConst) ~ awayFilter
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayConst ++ awayFilter ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyRead : read $awayKey
                 val awayKeyEq : $(map eq awayKey)
                 val awayKeyOrd : $(map ord awayKey)
                 val awayConstFl : folder awayConst
                 val awayConstInj : $(map sql_injectable awayConst)
                 val awayConst : $awayConst
                 val awayFilterFl : folder awayFilter

                 con timeKey1 :: Name
                 type timeKeyT
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
                 val timeKeyRead : read $timeKey
                 val timeKeyEq : $(map eq timeKey)

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
                 constraint homeOffice ~ timeKey
                 constraint (homeKey ++ awayKey) ~ [ByHome, Channel]

                 val amHome : transaction (option $homeKey)
                 val amAway : transaction (option $awayKey)
             end) = struct

    open M

    val homeKeyEq : eq $homeKey = @@Record.eq [homeKey] homeKeyEq homeKeyFl
    val awayKeyEq : eq $awayKey = @@Record.eq [awayKey] awayKeyEq awayKeyFl
    val timeKeyEq : eq $timeKey = @@Record.eq [timeKey] timeKeyEq timeKeyFl

    val homeKeyOrd : ord $homeKey = @@Record.ord [homeKey] homeKeyOrd homeKeyFl
    val awayKeyOrd : ord $awayKey = @@Record.ord [awayKey] awayKeyOrd awayKeyFl

    table meeting : (homeKey ++ timeKey ++ awayKey)
      PRIMARY KEY {{@primary_key [homeKey1] [homeKeyR ++ timeKey ++ awayKey] ! !
                    (homeInj ++ timeInj ++ awayInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}},
      {{one_constraint [#Time] (@Sql.easy_foreign ! ! ! ! ! ! timeKeyFl time)}}

    datatype operation = Add | Del
    type action = { Operation : operation, Home : $homeKey, Away : $awayKey, Time : $timeKey }
    table globalListeners : { Channel : channel action }

    type home_action = { Operation : operation, Away : $awayKey, Time : $timeKey, Place : option unit }
    table homeListeners : ([Channel = channel home_action] ++ homeKey)
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}}

    type away_action = { Operation : operation, Home : $homeKey, Time : $timeKey, Place : option $homeOffice }
    table awayListeners : ([Channel = channel away_action] ++ awayKey)
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    table preference : ([ByHome = bool] ++ homeKey ++ awayKey)
      PRIMARY KEY {{@primary_key [#ByHome] [homeKey ++ awayKey] ! !
                    ({ByHome = _} ++ homeInj ++ awayInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}}

    table homeUnavailable : (homeKey ++ timeKey)
      PRIMARY KEY {{@primary_key [homeKey1] [homeKeyR ++ timeKey] ! !
                    (homeInj ++ timeInj)}},
      {{one_constraint [#Home] (@Sql.easy_foreign ! ! ! ! ! ! homeKeyFl home)}},
      {{one_constraint [#Time] (@Sql.easy_foreign ! ! ! ! ! ! timeKeyFl time)}}

    table awayUnavailable : (awayKey ++ timeKey)
      PRIMARY KEY {{@primary_key [awayKey1] [awayKeyR ++ timeKey] ! !
                    (awayInj ++ timeInj)}},
      {{one_constraint [#Away] (@Sql.easy_foreign ! ! ! ! ! ! awayKeyFl away)}},
      {{one_constraint [#Time] (@Sql.easy_foreign ! ! ! ! ! ! timeKeyFl time)}}

    con all = homeKey ++ awayKey ++ timeKey
    val allFl = @Folder.concat ! homeKeyFl (@Folder.concat ! timeKeyFl awayKeyFl)

    val allInj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim allFl (homeInj ++ awayInj ++ timeInj)
    val homeInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim homeKeyFl homeInj
    val awayInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim awayKeyFl awayInj
    val timeInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim timeKeyFl timeInj

    fun addMeeting r =
        office <- oneRow1 (SELECT home.{{homeOffice}}
                           FROM home
                           WHERE {@@Sql.easy_where [#Home] [homeKey] [_] [_] [_] [_]
                             ! ! homeInj' homeKeyFl (r --- awayKey --- timeKey)});

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
                                         Time = r --- awayKey --- homeKey,
                                         Place = Some ()});
        queryI1 (SELECT * FROM awayListeners
                 WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                   ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Add,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey,
                                         Place = Some office})

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
                                         Time = r --- awayKey --- homeKey,
                                         Place = None});
        queryI1 (SELECT * FROM awayListeners
                          WHERE {@@Sql.easy_where [#AwayListeners] [awayKey] [_] [_] [_] [_]
                            ! ! awayInj' awayKeyFl (r --- homeKey --- timeKey)})
                (fn i => send i.Channel {Operation = Del,
                                         Home = r --- awayKey --- timeKey,
                                         Time = r --- awayKey --- homeKey,
                                         Place = None})

    (* This functor helps us abstract over the two directions.
     * We want symmetric functionality for each. *)
    functor Side(N : sig
                     con usKey :: {Type}
                     con usOffice :: {Type}
                     con usSoftConst :: {Type}
                     con usHardConst :: {Type}
                     con usOther :: {Type}
                     con themKey :: {Type}
                     con themOffice :: {Type}
                     con themSoftConst :: {Type}
                     con themHardConst :: {Type}
                     con themOther :: {Type}

                     (* Note: the "office" fields here do double-duty as "filter" fields in the eventual invocation! *)

                     constraint usKey ~ usOther
                     constraint (usKey ++ usOther) ~ usOffice
                     constraint (usKey ++ usOther ++ usOffice) ~ usSoftConst
                     constraint (usKey ++ usOther ++ usOffice ++ usSoftConst) ~ usHardConst
                     constraint themKey ~ themOther
                     constraint (themKey ++ themOther) ~ themOffice
                     constraint (themKey ++ themOther ++ themOffice) ~ themSoftConst
                     constraint (themKey ++ themOther ++ themOffice ++ themSoftConst) ~ themHardConst
                     constraint usKey ~ themKey
                     constraint (usKey ++ themKey) ~ timeKey
                     constraint themOffice ~ timeKey
                     constraint (usKey ++ themKey) ~ [ByHome]

                     val localized : { Home : $homeKey, Away : $awayKey }
                                     -> { Us : $usKey, Them : $themKey }
                     val canonical : { Us : $usKey, Them : $themKey }
                                     -> { Home : $homeKey, Away : $awayKey }

                     table us : (usKey ++ usOffice ++ usSoftConst ++ usHardConst ++ usOther)
                     table them : (themKey ++ themOffice ++ themSoftConst ++ themHardConst ++ themOther)

                     table preference : ([ByHome = bool] ++ usKey ++ themKey)
                     table unavailable : (usKey ++ timeKey)

                     table meeting : (usKey ++ themKey ++ timeKey)

                     constraint usKey ~ [Channel]
                     type usChannel
                     val usChannel : usChannel
                                     -> { Operation : operation,
                                          Them : $themKey,
                                          Time : $timeKey,
                                          Place : option $themOffice }
                     table usListeners : ([Channel = channel usChannel] ++ usKey)


                     val usFl : folder usKey
                     val themFl : folder themKey

                     val usInj : $(map sql_injectable_prim usKey)
                     val themInj : $(map sql_injectable_prim themKey)

                     val usShow : show $usKey
                     val themShow : show $themKey

                     val themRead : read $themKey

                     val usEq : eq $usKey
                     val usOrd : ord $usKey
                     val themEq : eq $themKey

                     val usOfficeFl : folder usOffice
                     val usOfficeShow : show $usOffice
                     val themOfficeShow : show $themOffice

                     val usSoftConstFl : folder usSoftConst
                     val usSoftConstInj : $(map sql_injectable usSoftConst)
                     val usSoftConst : $usSoftConst
                     val usHardConstFl : folder usHardConst
                     val usHardConstInj : $(map sql_injectable usHardConst)
                     val usHardConst : $usHardConst

                     val themSoftConstFl : folder themSoftConst
                     val themSoftConstInj : $(map sql_injectable themSoftConst)
                     val themSoftConst : $themSoftConst
                     val themHardConstFl : folder themHardConst
                     val themHardConstInj : $(map sql_injectable themHardConst)
                     val themHardConst : $themHardConst

                     val byHome : bool

                     val amUs : transaction (option $usKey)
                 end) : sig
        (* Display a full, editable grid of all meetings (rows: aways, columns: times).
         * The signal function can be used to control which rows are displayed. *)
        structure FullGrid : Ui.S where type input = $(N.usKey ++ N.usOffice) -> signal bool

        (* Display a read-only record of all records for an away. *)
        structure One : Ui.S where type input = $N.usKey

        (* Inputing meeting preferences for an away *)
        structure Prefs : Ui.S where type input = $N.usKey

        (* Inputing schedule constraints for an away *)
        structure Unavail : Ui.S where type input = $N.usKey

        (* Delete all of this person's meetings. *)
        val deleteFor : $N.usKey -> transaction unit
    end = struct

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
            type themSet = list ($themKey * int (* number of meetings this them has at this time *))
            type timeMap = list ($timeKey * bool (* available? *) * themSet)
            type usMap = list ($usKey * $usOffice * timeMap)
            type input = _
            type a = _

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
                                (rows : list {Meeting : $all, ThisTime : option int})
                                (uses : list $(usKey ++ usOffice))
                                (times : list $timeKey)
                                (unavails : list $(usKey ++ timeKey))
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
                                initMap ((us --- usOffice, us --- usKey, List.rev timesDone) :: acc)
                                        rows
                                        uses'
                                        allTimes
                                        unavails
                                        []
                                        []
                              | time :: times' =>
                                (* Check if the next row is about this time. *)
                                case rows of
                                    [] =>
                                    (* Nope. *)
                                    let
                                        val (available, unavails') =
                                            case unavails of
                                                [] => (True, [])
                                              | r :: unavails' =>
                                                if r --- timeKey = us --- usOffice
                                                   && @eq timeKeyEq (r --- usKey) time then
                                                    (False, unavails')
                                                else
                                                    (True, unavails)
                                    in
                                        initMap acc
                                                []
                                                uses
                                                times'
                                                unavails'
                                                []
                                                ((time, available, List.rev themsDone) :: timesDone)
                                    end
                                  | {Meeting = row, ThisTime = tt} :: rows' =>
                                    if row --- themKey --- timeKey < us --- usOffice then
                                        (* We've encountered a meeting for an invalid us.  Skip it! *)
                                        initMap acc rows' uses times unavails themsDone timesDone
                                    else if row --- themKey --- timeKey = us --- usOffice
                                       && @eq timeKeyEq (row --- usKey --- themKey) time then
                                        (* Aha, a match!  Record this meeting. *)
                                        initMap acc
                                                rows'
                                                uses
                                                times
                                                unavails
                                                ((row --- usKey --- timeKey, Option.get 1 tt) :: themsDone)
                                                timesDone
                                    else
                                        (* No match.  On to next time. *)
                                        let
                                            val (available, unavails') =
                                                case unavails of
                                                    [] => (True, [])
                                                  | r :: unavails' =>
                                                    if r --- timeKey = us --- usOffice && @eq timeKeyEq (r --- usKey) time then
                                                        (False, unavails')
                                                    else
                                                        (True, unavails)
                                        in
                                            initMap acc
                                                    rows
                                                    uses
                                                    times'
                                                    unavails'
                                                    []
                                                    ((time, available, List.rev themsDone) :: timesDone)
                                        end
                in
                    uses <- queryL1 (SELECT us.{{usKey}}, us.{{usOffice}}
                                     FROM us
                                     WHERE {@@Sql.easy_where [#Us] [usHardConst ++ usSoftConst]
                                       [usKey ++ usOffice ++ usOther] [_] [_] [_]
                                       ! ! (usHardConstInj ++ usSoftConstInj)
                                       (@Folder.concat ! usHardConstFl usSoftConstFl)
                                       (usHardConst ++ usSoftConst)}
                                     ORDER BY {{{@Sql.order_by usFl
                                        (@Sql.some_fields [#Us] [usKey] ! ! usFl)
                                        sql_desc}}});
                    thems <- queryL1 (SELECT them.{{themKey}}
                                      FROM them
                                      WHERE {@@Sql.easy_where [#Them] [themHardConst ++ themSoftConst]
                                        [themKey ++ themOffice ++ themOther] [_] [_] [_]
                                        ! ! (themHardConstInj ++ themSoftConstInj)
                                        (@Folder.concat ! themHardConstFl themSoftConstFl)
                                        (themHardConst ++ themSoftConst)}
                                      ORDER BY {{{@Sql.order_by themFl
                                        (@Sql.some_fields [#Them] [themKey] ! ! themFl)
                                        sql_desc}}});
                    unavails <- queryL1 (SELECT unavailable.{{usKey}}, unavailable.{{timeKey}}
                                         FROM unavailable JOIN us
                                           ON {@@Sql.easy_join [#Unavailable] [#Us]
                                             [usKey] [timeKey] [usOffice ++ usSoftConst ++ usHardConst ++ usOther]
                                             [_] [_] [_] ! ! ! ! usFl}
                                         WHERE {@@Sql.easy_where [#Us] [usHardConst ++ usSoftConst]
                                           [usKey ++ usOffice ++ usOther] [_] [_] [_]
                                           ! ! (usHardConstInj ++ usSoftConstInj)
                                           (@Folder.concat ! usHardConstFl usSoftConstFl)
                                           (usHardConst ++ usSoftConst)}
                                         ORDER BY {{{@Sql.order_by (@Folder.concat ! usFl timeKeyFl)
                                           (@Sql.some_fields [#Unavailable] [usKey ++ timeKey] ! !
                                             (@Folder.concat ! usFl timeKeyFl))
                                           sql_desc}}});
                    meetings <- queryL (SELECT meeting.{{usKey}}, meeting.{{timeKey}}, meeting.{{themKey}},
                                          (SELECT COUNT( * )
                                           FROM meeting AS ByThem
                                           WHERE {@@Sql.easy_join [#ByThem] [#Meeting]
                                             [themKey] [usKey ++ timeKey] [usKey ++ timeKey]
                                             [_] [_] [_] ! ! ! ! themFl}
                                             AND {@@Sql.easy_join [#ByThem] [#Meeting]
                                             [timeKey] [usKey ++ themKey] [usKey ++ themKey]
                                             [_] [_] [_] ! ! ! ! timeKeyFl}) AS ThisTime
                                        FROM meeting
                                        ORDER BY {{{@Sql.order_by allFl
                                          (@Sql.some_fields [#Meeting] [all] ! ! allFl)
                                          sql_desc}}});
                    meetings <- List.mapM (fn (us, off, tms) =>
                                              tms' <- List.mapM (fn (tm, avail, ths) =>
                                                                    ths <- source ths;
                                                                    return (tm, avail, ths)) tms;
                                              return (us, off, tms'))
                                          (initMap []
                                                   meetings
                                                   uses
                                                   allTimes
                                                   unavails
                                                   []
                                                   []);
                    chan <- channel;
                    dml (INSERT INTO globalListeners (Channel) VALUES ({[chan]}));
                    mf <- source None;
                    mt <- source None;
                    return {Thems = thems, Times = allTimes, Meetings = meetings,
                            Channel = chan, MovingFrom = mf, MovingTo = mt}
                end

            val ensureUser =
                user <- amUs;
                case user of
                    None => error <xml>Must be authenticated to access this page</xml>
                  | Some us' => return ()

            fun schedule r =
                ensureUser;
                alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                              FROM meeting
                                              WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                                ! ! allInj allFl r});
                if alreadyScheduled then
                    return ()
                else
                    addMeeting r

            fun unschedule r =
                ensureUser;
                alreadyScheduled <- oneRowE1 (SELECT COUNT( * ) > 0
                                              FROM meeting
                                              WHERE {@@Sql.easy_where [#Meeting] [all] [_] [_] [_] [_]
                                                ! ! allInj allFl r});
                if not alreadyScheduled then
                    return ()
                else
                    delMeeting r

            fun reschedule (r : {Them : $themKey, OldUs : $usKey, OldTime : $timeKey, NewUs : $usKey, NewTime : $timeKey}) =
                ensureUser;
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

            fun render filt ctx t = <xml>
              <table class="bs3-table table-striped">
                <tr>
                  <th/>
                  (* Header: one column per time *)
                  {List.mapX (fn tm => <xml><th>{[tm]}</th></xml>) t.Times}
                </tr>

                (* One row per us *)
                {List.mapX (fn (us, off, tms) => <xml>
                  <tr dynStyle={b <- filt (us ++ off); return (if b then STYLE "" else STYLE "display: none")}>
                    <th>{[us]}{[off]}</th>

                    (* One column per time *)
                    {List.mapX (fn (tm, avail, ths) => <xml>
                      <td dynClass={mf <- signal t.MovingFrom;
                                    cls <- (case mf of
                                                None => return (CLASS "")
                                              | Some _ =>
                                                mt <- signal t.MovingTo;
                                                return (case mt of
                                                            None => CLASS ""
                                                          | Some mt =>
                                                            if mt.Us = us && mt.Time = tm then
                                                                CLASS "bs3-active"
                                                            else
                                                                CLASS ""));
                                   return (if avail then cls else classes cls danger)}
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
                                                       {List.mapX (fn (th, tt) => <xml>
                                                         <div dynClass={mf <- signal t.MovingFrom;
                                                                        default <- return (case thsv of
                                                                                               _ :: _ :: _ => meeting_conflict
                                                                                             | _ => if tt > 1 then meeting_conflict else meeting_default);
                                                                        return (case mf of
                                                                                    None => default
                                                                                  | Some mf =>
                                                                                    if mf.Us = us
                                                                                       && mf.Them = th
                                                                                       && mf.Time = tm then
                                                                                        meeting_selected
                                                                                    else
                                                                                        default)}
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
                                                           {Ui.modalButton ctx (CLASS "close")
                                                                           <xml>&times;</xml>
                                                                           (set deleting True;
                                                                            return (Ui.modal
                                                                                 (rpc (unschedule (th ++ us ++ tm)))
                                                                                 <xml>Are you sure you want to delete the {[tm]} meeting between {[us]} and {[th]}?</xml>
                                                                                 <xml/>
                                                                                 <xml>Yes!</xml>))}
                                                         </div>
                                                       </xml>) thsv}

                                                       {Ui.modalButton ctx (CLASS "btn btn-default btn-xs")
                                                               <xml>+</xml>
                                                               (return (Ui.modal
                                                                            (sel <- get selected;
                                                                             th <- return (readError sel : $themKey);
                                                                             rpc (schedule (th ++ us ++ tm)))
                                                                            <xml>Adding meeting for {[us]} at {[tm]}</xml>
                                                                            <xml>
                                                                              <cselect class="form-control"
                                                                                       source={selected}>
                                                                                {List.mapX (fn th =>
                                                                                               if List.exists (fn (th', _) => th' = th) thsv then
                                                                                                   <xml/>
                                                                                               else
                                                                                                   <xml><coption>{[th]}</coption></xml>) t.Thems}
                                                                              </cselect>
                                                                            </xml>
                                                                            <xml>Add Meeting</xml>))}
                                                     </xml>}/>
                                      </xml>}/>
                      </td>
                    </xml>) tms}
                  </tr>
                </xml>) t.Meetings}
              </table>
            </xml>

            fun tweakMeeting (f : themSet -> themSet) (g : int -> int) (us : $usKey) (th : $themKey) (tm : $timeKey) =
                List.app (fn (us', _, tms) =>
                             (if us' = us then
                                 let
                                     fun addTimes tms =
                                         case tms of
                                             [] => return ()
                                           | (tm', _, ths) :: tms' =>
                                             if tm' = tm then
                                                 v <- get ths;
                                                 set ths (f v)
                                             else
                                                 addTimes tms'
                                 in
                                     addTimes tms
                                 end
                             else
                                 return ());

                             List.app (fn (tm', _, ths) =>
                                          if tm = tm' then
                                              thsv <- get ths;
                                              set ths (List.mp (fn (th', count) =>
                                                                   (th', if th' = th then
                                                                             g count
                                                                         else
                                                                             count)) thsv)
                                          else
                                              return ()) tms)

            fun onload t =
                let
                    fun loop () =
                        r <- recv t.Channel;
                        let
                            val r' = localized (r -- #Operation -- #Time)

                            fun findExistingMeeting uses =
                                case uses of
                                    [] => return 0
                                  | (_, _, tms) :: uses' =>
                                    let
                                        fun fem tms =
                                            case tms of
                                                [] => findExistingMeeting uses'
                                              | (tm, _, ths) :: tms' =>
                                                if tm <> r.Time then
                                                    fem tms'
                                                else
                                                    thsv <- get ths;
                                                    case List.find (fn (th, _) => th = r'.Them) thsv of
                                                        None => fem tms'
                                                      | Some (_, count) => return count
                                    in
                                        fem tms
                                    end
                        in
                            case r.Operation of
                                Add =>
                                count <- findExistingMeeting t.Meetings;
                                tweakMeeting
                                    (fn ls => List.sort (fn (x, _) (y, _) => show x > show y)
                                                        ((r'.Them, count) :: ls))
                                    (fn n => n + 1)
                                    r'.Us r'.Them r.Time t.Meetings
                              | Del => tweakMeeting
                                           (List.filter (fn (th, _) => th <> r'.Them))
                                           (fn n => n - 1)
                                           r'.Us r'.Them r.Time t.Meetings
                        end;
                        loop ()
                in
                    spawn (loop ())
                end

            fun ui filt = {
                Create = create,
                Onload = onload,
                Render = render filt
            }

        end

        structure One = struct
            type themSet = list ($themKey * $themOffice)
            type timeMap = list ($timeKey * themSet)
            type input = $usKey
            type a = {Us : $usKey,
                      Meetings : list ($timeKey * source themSet),
                      Channel : channel usChannel}

            fun create us =
                let
                    fun doTimes (times : list $timeKey)
                                (rows : list {Meeting : $(timeKey ++ themKey), Them : $themOffice})
                                (thisTime : themSet)
                                (acc : timeMap)
                        : timeMap =
                        case times of
                            [] => List.rev acc
                          | tm :: times' =>
                            case rows of
                                [] => doTimes times' [] [] ((tm, List.rev thisTime) :: acc)
                              | row :: rows' =>
                                if row.Meeting --- themKey = tm then
                                    doTimes times rows' ((row.Meeting --- timeKey, row.Them) :: thisTime) acc
                                else
                                    doTimes times' rows [] ((tm, List.rev thisTime) :: acc)
                in
                    allTimes <- queryL1 (SELECT time.{{timeKey}}
                                         FROM time
                                         ORDER BY {{{@Sql.order_by timeKeyFl
                                           (@Sql.some_fields [#Time] [timeKey] ! ! timeKeyFl)
                                           sql_desc}}});
                    meetings <- queryL (SELECT meeting.{{timeKey}}, meeting.{{themKey}}, them.{{themOffice}}
                                        FROM meeting
                                          JOIN them ON {@@Sql.easy_join [#Meeting] [#Them] [themKey]
                                                [usKey ++ timeKey]
                                                [themOffice ++ themHardConst ++ themSoftConst ++ themOther]
                                                [_] [_] [_]
                                                ! ! ! ! themFl}
                                        WHERE {@@Sql.easy_where [#Meeting] [usKey] [_] [_] [_] [_]
                                          ! ! usInj' usFl us}
                                          AND {@@Sql.easy_where [#Them] [themHardConst ++ themSoftConst]
                                            [themKey ++ themOffice ++ themOther] [_] [_] [_]
                                            ! ! (themHardConstInj ++ themSoftConstInj)
                                            (@Folder.concat ! themHardConstFl themSoftConstFl)
                                            (themHardConst ++ themSoftConst)}
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

            (* Note: with a new improvement to Ur/Web type inference, this annotation becomes necessary.
             * Should probably look further into why, some day. *)
            fun render (t : a) = <xml>
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
                                             | (th, off) :: ths => <xml>{[th]}{[off]}{List.mapX (fn (th, off) => <xml>, {[th]}{[off]}</xml>) ths}</xml>)}/>
                                                                                                                                        </td>
                  </tr>
                </xml>) t.Meetings}
              </table>
            </xml>

            fun tweakMeeting (f : themSet -> themSet) (tm : $timeKey) =
                let
                    fun addTimes tms =
                        case tms of
                            [] => return ()
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
                                Add =>
                                (case r.Place of
                                     None => error <xml>One.onload: missing place</xml>
                                   | Some off =>
                                     tweakMeeting
                                         (fn ls => List.sort (fn (x, _) (y, _) => show x > show y) ((r.Them, off) :: ls))
                                         r.Time t.Meetings)
                              | Del => tweakMeeting
                                           (List.filter (fn (th, _) => th <> r.Them))
                                           r.Time t.Meetings
                        end;
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

        structure Prefs = ChooseForeign.Make(struct
                                                 val const = {ByHome = byHome}
                                                 val optionsConst = themHardConst
                                                 con given = usKey
                                                 con chosen = themKey

                                                 val choices = preference
                                                 val options = them
                                                 val buttonLabel = "Add Preference"

                                                 val givenInj = usInj'
                                                 val chosenInj = themInj'

                                                 val givenFl = usFl
                                                 val chosenFl = themFl

                                                 val amGiven = amUs

                                                 val optionsConstInj = themHardConstInj
                                                 val optionsConstFl = themHardConstFl
                                             end)

        structure Unavail = ChooseForeign.Make(struct
                                                   val const = {}
                                                   val optionsConst = {}
                                                   con given = usKey
                                                   con chosen = timeKey

                                                   val choices = unavailable
                                                   val options = time
                                                   val buttonLabel = "Add Constraint"

                                                   val givenInj = usInj'
                                                   val chosenInj = timeInj'

                                                   val givenFl = usFl
                                                   val chosenFl = timeKeyFl

                                                   val amGiven = amUs
                                               end)

        fun deleteFor r =
            queryI1 (SELECT meeting.{{themKey}}, meeting.{{timeKey}}
                     FROM meeting
                     WHERE {@@Sql.easy_where [#Meeting] [usKey] [_] [_] [_] [_]
                       ! ! usInj' usFl r})
                    (fn r' => delMeeting (r ++ r'));

            dml (DELETE FROM meeting
                        WHERE {@@Sql.easy_where [#T] [usKey] [_] [_] [_] [_]
                          ! ! usInj' usFl r})
    end

    val show_unit : show unit = mkShow (fn () => "")

    structure Home = struct
        open Side(struct
                      con usKey = homeKey
                      con usOffice = homeOffice
                      con usHardConst = homeHardConst
                      con usSoftConst = homeSoftConst
                      con themKey = awayKey
                      con themOffice = []
                      con themHardConst = []
                      con themSoftConst = awayConst

                      fun localized r = {Us = r.Home, Them = r.Away}
                      fun canonical r = {Home = r.Us, Away = r.Them}

                      val us = home
                      val them = away

                      val preference = preference
                      val byHome = True

                      val meeting = meeting

                      fun usChannel r = r -- #Away ++ {Them = r.Away}
                      val usListeners = homeListeners
                      val unavailable = homeUnavailable

                      val usInj = homeInj
                      val themInj = awayInj

                      val usFl = homeKeyFl
                      val themFl = awayKeyFl
                      val usOfficeFl = officeFl

                      val amUs = amHome

                      val usHardConst = homeHardConst
                      val usSoftConst = homeSoftConst
                      val themHardConst = {}
                      val themSoftConst = awayConst

                      val usHardConstInj = homeHardConstInj
                      val usHardConstFl = homeHardConstFl
                      val usSoftConstInj = homeSoftConstInj
                      val usSoftConstFl = homeSoftConstFl
                      val themSoftConstInj = awayConstInj
                      val themSoftConstFl = awayConstFl
                  end)

        structure FullGrid = struct
            open FullGrid
            val ui = ui (fn _ => return True)
        end
    end

    structure Away = Side(struct
                              con usKey = awayKey
                              con usOffice = awayFilter
                              con usHardConst = []
                              con usSoftConst = awayConst
                              con themKey = homeKey
                              con themOffice = homeOffice
                              con themHardConst = homeHardConst
                              con themSoftConst = homeSoftConst

                              fun localized r = {Us = r.Away, Them = r.Home}
                              fun canonical r = {Away = r.Us, Home = r.Them}

                              val us = away
                              val them = home

                              val preference = preference
                              val byHome = False

                              val meeting = meeting

                              fun usChannel r = r -- #Home ++ {Them = r.Home}
                              val usListeners = awayListeners
                              val unavailable = awayUnavailable

                              val usInj = awayInj
                              val themInj = homeInj

                              val usFl = awayKeyFl
                              val themFl = homeKeyFl

                              val amUs = amAway

                              val usHardConst = {}
                              val usSoftConst = awayConst
                              val themSoftConst = homeSoftConst
                              val themHardConst = homeHardConst

                              val usSoftConstInj = awayConstInj
                              val usSoftConstFl = awayConstFl
                              val themHardConstInj = homeHardConstInj
                              val themHardConstFl = homeHardConstFl
                              val themSoftConstInj = homeSoftConstInj
                              val themSoftConstFl = homeSoftConstFl

                              val usOfficeShow = mkShow (fn _ => "")
                              val usOfficeFl = awayFilterFl
                          end)

    val scheduleSome =
        (* Loop randomly over all preferences, disregarding sidedness. *)
        queryI1 (SELECT preference.{{homeKey}}, preference.{{awayKey}}
                 FROM preference
                 ORDER BY RANDOM())
        (fn r =>
            (* First, check if this pair already have a scheduled meeting. *)
            areMeeting <- oneRowE1 (SELECT COUNT( * ) > 0
                                    FROM meeting
                                    WHERE {@@Sql.easy_where [#Meeting] [homeKey ++ awayKey] [_] [_] [_] [_]
                                      ! ! (homeInj' ++ awayInj')
                                      (@Folder.concat ! homeKeyFl awayKeyFl) r});

            if areMeeting then
                (* Already covered! *)
                return ()
            else
                (* Try to find a compatible open slot. *)
                slot <- oneOrNoRows1 (SELECT time.{{timeKey}}
                                      FROM time
                                      WHERE (SELECT TRUE
                                             FROM meeting
                                             WHERE {@@Sql.easy_where [#Meeting] [homeKey] [_] [_] [_] [_]
                                                 ! ! homeInj' homeKeyFl (r --- awayKey)}
                                               AND {@@Sql.easy_join [#Meeting] [#Time] [timeKey]
                                                 [homeKey ++ awayKey] [timeRest] [_] [_] [_]
                                                 ! ! ! ! timeKeyFl}) IS NULL
                                        AND (SELECT TRUE
                                             FROM meeting
                                             WHERE {@@Sql.easy_where [#Meeting] [awayKey] [_] [_] [_] [_]
                                                 ! ! awayInj' awayKeyFl (r --- homeKey)}
                                               AND {@@Sql.easy_join [#Meeting] [#Time] [timeKey]
                                                 [homeKey ++ awayKey] [timeRest] [_] [_] [_]
                                                 ! ! ! ! timeKeyFl}) IS NULL
                                        AND (SELECT TRUE
                                             FROM homeUnavailable
                                             WHERE {@@Sql.easy_where [#HomeUnavailable] [homeKey] [_] [_] [_] [_]
                                                 ! ! homeInj' homeKeyFl (r --- awayKey)}
                                               AND {@@Sql.easy_join [#HomeUnavailable] [#Time] [timeKey]
                                                 [homeKey] [timeRest] [_] [_] [_]
                                                 ! ! ! ! timeKeyFl}) IS NULL
                                        AND (SELECT TRUE
                                             FROM awayUnavailable
                                             WHERE {@@Sql.easy_where [#AwayUnavailable] [awayKey] [_] [_] [_] [_]
                                                 ! ! awayInj' awayKeyFl (r --- homeKey)}
                                               AND {@@Sql.easy_join [#AwayUnavailable] [#Time] [timeKey]
                                                 [awayKey] [timeRest] [_] [_] [_]
                                                 ! ! ! ! timeKeyFl}) IS NULL
                                        AND NOT ((SELECT TRUE
                                                  FROM home
                                                  WHERE {@@Sql.easy_where [#Home] [homeKey] [_] [_] [_] [_]
                                                    ! ! homeInj' homeKeyFl (r --- awayKey)}
                                                    AND {@@Sql.easy_where [#Home] [homeHardConst ++ homeSoftConst]
                                                    [_] [_] [_] [_]
                                                    ! ! (homeHardConstInj ++ homeSoftConstInj)
                                                    (@Folder.concat ! homeHardConstFl homeSoftConstFl)
                                                    (homeHardConst ++ homeSoftConst)}) IS NULL)
                                        AND NOT ((SELECT TRUE
                                                  FROM away
                                                  WHERE {@@Sql.easy_where [#Away] [awayKey] [_] [_] [_] [_]
                                                    ! ! awayInj' awayKeyFl (r --- homeKey)}
                                                    AND {@@Sql.easy_where [#Away] [awayConst] [_] [_] [_] [_]
                                                    ! ! awayConstInj awayConstFl awayConst}) IS NULL)
                                      LIMIT 1);
                case slot of
                    None =>
                    (* No suitable openings.  Better luck next time! *)
                    return ()
                  | Some slot =>
                    (* Found one!  Schedule it. *)
                    addMeeting (r ++ slot))

end
