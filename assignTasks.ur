open Bootstrap3

functor Make(M : sig
                 con assignable :: {Type}
                 con assigned :: Name
                 constraint assignable ~ [assigned]
                 val assignableFl : folder assignable
                 val show_assignable : show $assignable
                 val inj_assignable : $(map sql_injectable_prim assignable)

                 table assignments : (assignable ++ [assigned = string])
                 val eligibleAssignees : transaction (list string)

                 type filter
                 val allFilters : transaction (list filter)
                 val filter : filter -> sql_query [] [] [] assignable
                 val show_filter : show filter
                 val read_filter : read filter
             end) = struct

    open M

    fun byFilter f =
        queryL ({{{@sql_query1 [[]] ! ! ! !
                    {Distinct = False,
                     From = @@sql_left_join [[]] [[T = assignable]] [[Assignments = map (fn t => (t, option t)) (assignable ++ [assigned = string])]] ! ! !
                              {Assignments = @mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                              @@nullify_prim assignableFl inj_assignable ++ {assigned = nullify_prim}}
                              (sql_from_query [#T] (filter f))
                              (sql_from_table [#Assignments] assignments)
                              (@@Sql.easy_join [#T] [#Assignments] [assignable] [[]] [[assigned = _]]
                                 [[]] [[]] [[]] ! ! ! ! assignableFl),
                     Where = (WHERE TRUE),
                     GroupBy = sql_subset_all [_],
                     Having = (WHERE TRUE),
                     SelectFields = sql_subset [[T = (assignable, _), Assignments = ([assigned = _], _)]],
                     SelectExps = {}}}}}
                 ORDER BY {{{@Sql.order_by assignableFl (@Sql.some_fields [#T] [assignable] ! ! assignableFl) sql_desc}}})

    val inj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim assignableFl inj_assignable

    fun remove k =
        dml (DELETE FROM assignments
             WHERE {@Sql.easy_where [#T] ! ! inj assignableFl k})

    fun assign k u =
        remove k;
        @@Sql.easy_insert [assignable ++ [assigned = string]] [_]
          ({assigned = _} ++ inj) (@Folder.cons [assigned] [_] ! assignableFl) assignments (k ++ {assigned = u})

    structure MakeAssignments = struct
        type a = {AllFilters : list filter,
                  Eligible : list string,
                  Filter : source string,
                  Assignables : source (list ($assignable * source string))}

        val create =
            filts <- allFilters;
            eligible <- eligibleAssignees;
            filt <- source "";
            asbls <- source [];
            return {AllFilters = filts,
                    Eligible = eligible,
                    Filter = filt,
                    Assignables = asbls}

        fun onload _ = return ()

        fun render _ a = <xml>
          <cselect source={a.Filter}
                   onchange={filt <- get a.Filter;
                             case filt of
                                 "" => set a.Assignables []
                               | _ =>
                                 case read filt of
                                     None => error <xml>AssignTasks: Error deserializing task description</xml>
                                   | Some f =>
                                     asbls <- rpc (byFilter f);
                                     asbls <- List.mapM (fn r =>
                                                            u <- source (case r.Assignments.assigned of
                                                                             None => ""
                                                                           | Some s => s);
                                                            return (r.T, u)) asbls;
                                     set a.Assignables asbls}>
            <coption value="">Select a filter:</coption>
            {List.mapX (fn filt => <xml><coption>{[filt]}</coption></xml>) a.AllFilters}
          </cselect>

          <table class="bs3-table table-striped">
            <tr> <th>Task</th> <th>Assigned to</th> </tr>

            <dyn signal={asbls <- signal a.Assignables;
                         return (List.mapX (fn (k, u) => <xml>
                           <tr>
                             <td>{[k]}</td>
                             <td><cselect source={u}
                                          onchange={u <- get u;
                                                    case u of
                                                        "" => rpc (remove k)
                                                      | _ => rpc (assign k u)}>
                               <coption/>
                               {List.mapX (fn usr => <xml><coption>{[usr]}</coption></xml>) a.Eligible}
                             </cselect></td>
                           </tr>
                         </xml>) asbls)}/>
          </table>
        </xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render}
    end

end
