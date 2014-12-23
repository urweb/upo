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

    val timeOb [tab] [rest] [tables] [exps] [[tab] ~ tables] [timeKey ~ rest]
        : sql_order_by ([tab = timeKey ++ rest] ++ tables) exps =
        @Sql.order_by timeKeyFl
         (@Sql.some_fields [tab] [timeKey] ! ! timeKeyFl)
         sql_desc

    val combinedFl = @Folder.concat ! homeKeyFl (@Folder.concat ! timeKeyFl awayKeyFl)

    val addMeeting = @@Sql.easy_insert [homeKey ++ awayKey ++ timeKey] [_]
                       (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim combinedFl (homeInj ++ awayInj ++ timeInj))
                       combinedFl
                       meeting


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
                            (rows : list $(homeKey ++ timeKey ++ awayKey))
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
                                     ORDER BY {{{@Sql.order_by combinedFl
                                       (@Sql.some_fields [#Meeting] [homeKey ++ timeKey ++ awayKey] ! ! combinedFl)
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
                return {Aways = aways, Times = allTimes, Meetings = meetings}
            end

        val schedule = addMeeting

        fun render t = <xml>
          <table>
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
                    <active code={expanded <- source False;
                                  selected <- source "";
                                  return <xml>
                                    <dyn signal={awsv <- signal aws;
                                                 (* One button per meeting *)
                                                 return <xml>
                                                   {List.mapX (fn aw => <xml>
                                                     <div>{[aw]}</div>
                                                   </xml>) awsv}
                                                   <dyn signal={exp <- signal expanded;
                                                                return <xml>
                                                                  <button value={if exp then "-" else "+"}
                                                                          onclick={fn _ => set expanded (not exp)}/>
                                                                </xml>}/>
                                                   <dyn signal={exp <- signal expanded;
                                                                if not exp then
                                                                    return <xml/>
                                                                else
                                                                    return <xml>
                                                                      <cselect source={selected}>
                                                                        {List.mapX (fn aw =>
                                                                                       if List.mem aw awsv then
                                                                                           <xml/>
                                                                                       else
                                                                                           <xml><coption>{[aw]}</coption></xml>) t.Aways}
                                                                      </cselect>

                                                                      <button value="Add"
                                                                              onclick={fn _ =>
                                                                                          set expanded False;
                                                                                          sel <- get selected;
                                                                                          aw <- return (readError sel);
                                                                                          rpc (schedule (aw ++ ho ++ tm));
                                                                                          set aws (List.sort (fn x y => show x > show y) (aw :: awsv))}/>
                                                                    </xml>}/>
                                                 </xml>}/>
                                  </xml>}/>
                  </td>
                </xml>) tms}
              </tr>
            </xml>) t.Meetings}
          </table>
        </xml>
    end

end
