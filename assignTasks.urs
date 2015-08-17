(* Assigning users to complete tasks that are connected to existence of certain database rows *)

functor Make(M : sig
                 con assignable :: {Type}
                 (* The key of an assignable task *)
                 con assigned :: Name
                 (* Name of column recording which user gets assigned *)
                 constraint assignable ~ [assigned]
                 val assignableFl : folder assignable
                 val show_assignable : show $assignable
                 val inj_assignable : $(map sql_injectable_prim assignable)

                 table assignments : (assignable ++ [assigned = string])
                 (* Table recording decisions of who should do what *)
                 val eligibleAssignees : transaction (list (string * list string))
                 (* Which users are eligible to be assigned these tasks?
                  * Group them into named categories, where the first category should include all eligible users. *)

                 type filter
                 (* Search term used to narrow down the space of assignables *)
                 val allFilters : transaction (list filter)
                 (* Get a list of all legal filter terms, given the current database state. *)
                 val filter : filter -> sql_query [] [] [] assignable
                 (* Interpret a filter as a pseudotable listing all matching assignables. *)
                 val show_filter : show filter
                 val read_filter : read filter
             end) : sig
    structure MakeAssignments : Ui.S0
end
