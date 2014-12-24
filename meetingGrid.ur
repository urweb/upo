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

    val timeOb [tab] [rest] [tables] [exps] [[tab] ~ tables] [timeKey ~ rest]
        : sql_order_by ([tab = timeKey ++ rest] ++ tables) exps =
        @Sql.order_by timeKeyFl
         (@Sql.some_fields [tab] [timeKey] ! ! timeKeyFl)
         sql_desc

    con all = homeKey ++ awayKey ++ timeKey
    val allFl = @Folder.concat ! homeKeyFl (@Folder.concat ! timeKeyFl awayKeyFl)

    val allInj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim allFl (homeInj ++ awayInj ++ timeInj)

    val addMeeting = @@Sql.easy_insert [all] [_] allInj allFl meeting


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
                return {Aways = aways, Times = allTimes, Meetings = meetings,
                        ModalId = mid, ModalSpot = modalSpot, Channel = chan}
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
                                         Time = r --- awayKey --- homeKey})

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
                  <td>
                    <active code={selected <- source "";
                                  return <xml>
                                    <dyn signal={awsv <- signal aws;
                                                 (* One button per meeting *)
                                                 return <xml>
                                                   {List.mapX (fn aw => <xml>
                                                     <div style="border-style: double">
                                                       {[aw]}
                                                       <button class="close"
                                                               onclick={fn _ =>
                                                                           rpc (unschedule (aw ++ ho ++ tm))}>
                                                         &times;
                                                       </button>
                                                     </div>
                                                   </xml>) awsv}

                                                   <button class="btn btn-default btn-xs"
                                                           value="+"
                                                           data-toggle="modal"
                                                           data-target={"#" ^ show t.ModalId}
                                                           onclick={fn _ => set t.ModalSpot <xml>
                                                             <div class="modal-dialog">
                                                               <div class="modal-content">
                                                                 <div class="modal-header">
                                                                   <h4 class="modal-title">Adding meeting for {[ho]} at {[tm]}</h4>
                                                                 </div>

                                                                 <div class="modal-body">
                                                                   <cselect class="form-control"
                                                                            source={selected}>
                                                                     {List.mapX (fn aw =>
                                                                                    if List.mem aw awsv then
                                                                                        <xml/>
                                                                                    else
                                                                                        <xml><coption>{[aw]}</coption></xml>) t.Aways}
                                                                   </cselect>
                                                                 </div>

                                                                 <div class="modal-footer">
                                                                   <button class="btn btn-primary"
                                                                           data-dismiss="modal"
                                                                           value="Add Meeting"
                                                                           onclick={fn _ =>
                                                                                       sel <- get selected;
                                                                                       aw <- return (readError sel : $awayKey);
                                                                                       rpc (schedule (aw ++ ho ++ tm))}/>
                                                                   <button class="btn btn-default"
                                                                           data-dismiss="modal"
                                                                           value="Cancel"/>

                                                                 </div>
                                                               </div>
                                                             </div>
                                                           </xml>}/>
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

end
