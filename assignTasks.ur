open Bootstrap

functor Make(M : sig
                 con assignable :: {Type}
                 con assigned :: Name
                 constraint assignable ~ [assigned]
                 val assignableFl : folder assignable
                 val show_assignable : show $assignable
                 val inj_assignable : $(map sql_injectable_prim assignable)

                 table assignments : (assignable ++ [assigned = string])
                 val eligibleAssignees : transaction (list (string * list string))

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
                 ORDER BY {{{@Sql.order_by assignableFl (@Sql.some_fields [#T] [assignable] ! ! assignableFl) sql_asc}}})

    val inj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim assignableFl inj_assignable

    fun remove k =
        dml (DELETE FROM assignments
             WHERE {@Sql.easy_where [#T] ! ! inj assignableFl k})

    fun assign k u =
        remove k;
        @@Sql.easy_insert [assignable ++ [assigned = string]] [_]
          ({assigned = _} ++ inj) (@Folder.cons [assigned] [_] ! assignableFl) assignments (k ++ {assigned = u})

    fun random f us =
        _ <- query ({{{@sql_query1 [[Assignments]] ! ! ! !
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
                         SelectFields = sql_subset [[T = (assignable, _), Assignments = ([], _)]],
                         SelectExps = {}}}}}
                    ORDER BY RANDOM)
             (fn r (ls1, ls2) =>
                 let
                     val (u, ls1, ls2) =
                         case ls1 of
                             [] =>
                             let
                                 val ls2 = List.rev ls2
                             in
                                 case ls2 of
                                     [] => error <xml>AssignTasks: No users selected!</xml>
                                   | u :: ls2 => (u, ls2, u :: [])
                             end
                           | u :: ls1 => (u, ls1, u :: ls2)
                 in
                     assign r.T u;
                     return (ls1, ls2)
                 end) (us, []);
        byFilter f

    structure MakeAssignments = struct
        type a = {AllFilters : list filter,
                  Eligible : list (string * list string),
                  Filter : source string,
                  Assignables : source (list ($assignable * source string)),
                  RandomAssignees : source (option (list (string * source bool)))}

        val create =
            filts <- allFilters;
            eligible <- eligibleAssignees;
            filt <- source "";
            asbls <- source [];
            ras <- source None;
            return {AllFilters = filts,
                    Eligible = eligible,
                    Filter = filt,
                    Assignables = asbls,
                    RandomAssignees = ras}

        fun onload _ = return ()

        fun makeAssignables a asbls =
            asbls <- List.mapM (fn r =>
                                   u <- source (case r.Assignments.assigned of
                                                    None => ""
                                                  | Some s => s);
                                   return (r.T, u)) asbls;
            set a.Assignables asbls

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
                                     makeAssignables a asbls}>
            <coption value="">Select a filter:</coption>
            {List.mapX (fn filt => <xml><coption>{[filt]}</coption></xml>) a.AllFilters}
          </cselect>

          <dyn signal={asbls <- signal a.Assignables;
                       allAssigned <- List.foldlM (fn (_, s) b =>
                                                      u <- signal s;
                                                      return (u <> "" && b)) True asbls;
                       return (if allAssigned then
                                   <xml></xml>
                               else
                                   <xml>
                                     <dyn signal={ras <- signal a.RandomAssignees;
                                                  return (case ras of
                                                              None => <xml>
                                                                <button class="btn btn-secondary"
                                                                        onclick={fn _ =>
                                                                                    case a.Eligible of
                                                                                        [] => error <xml>AssignTasks: no eligibility categories</xml>
                                                                                      | (_, ls) :: _ =>
                                                                                        us <- List.mapM (fn u =>
                                                                                                            sel <- source False;
                                                                                                            return (u, sel)) ls;
                                                                                        set a.RandomAssignees (Some us)}>
                                                                  <span class="glyphicon glyphicon-menu-down"/> Random Assignment
                                                                </button>
                                                              </xml>
                                                            | Some ls => <xml>
                                                                <button class="btn btn-secondary"
                                                                        onclick={fn _ => set a.RandomAssignees None}>
                                                                  <span class="glyphicon glyphicon-menu-up"/> Close
                                                                </button>

                                                                <button class="btn btn-secondary"
                                                                        onclick={fn _ => List.app (fn (_, sel) => set sel False) ls}>
                                                                  Nobody
                                                                </button>

                                                                {List.mapX (fn (nm, ls') => <xml>
                                                                  <button class="btn btn-secondary"
                                                                          onclick={fn _ => List.app (fn (u, sel) => set sel (List.mem u ls')) ls}>
                                                                    {[nm]}
                                                                  </button>
                                                                </xml>) a.Eligible}

                                                                <button class="btn btn-default"
                                                                        onclick={fn _ =>
                                                                                    us <- List.foldlM (fn (u, sel) ls =>
                                                                                                          b <- get sel;
                                                                                                          return (if b then u :: ls else ls)) [] ls;
                                                                                    case us of
                                                                                        [] => alert "Please select some people to assign tasks to."
                                                                                      | _ =>
                                                                                        filt <- get a.Filter;
                                                                                        set a.RandomAssignees None;
                                                                                        asbls <- rpc (random (readError filt) us);
                                                                                        makeAssignables a asbls}>
                                                                  Make Random Assignment
                                                                </button>

                                                                <div>
                                                                  {List.mapX (fn (u, sel) => <xml>
                                                                    <ccheckbox source={sel}/> {[u]}<br/>
                                                                    </xml>) ls}
                                                                </div>
                                                              </xml>)}/>
                                   </xml>)}/>

          <table class="bs-table">
            <thead><tr> <th>Task</th> <th>Assigned to</th> </tr></thead>

            <tbody>
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
                                 {List.mapX (fn usr => <xml><coption>{[usr]}</coption></xml>) (case a.Eligible of
                                                                                                   [] => error <xml>AssignTasks: no eligibility categories</xml>
                                                                                                 | (_, ls) :: _ => ls)}
                               </cselect></td>
                             </tr>
                             </xml>) asbls)}/>
            </tbody>
          </table>
        </xml>

        fun notification _ _ = <xml></xml>
        fun buttons _ _ = <xml></xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render,
                  Notification = notification,
                  Buttons = buttons}
    end

end
