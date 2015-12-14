(* Displaying selected fields of a table (subject to a filtering condition),
 * allowing some to be edited with widgets *)

functor Make(M : sig
                 con hidden :: {Type}
                 con static :: {Type}
                 con dynamic :: {(Type * Type * Type)}
                 constraint hidden ~ static
                 constraint (hidden ++ static) ~ dynamic

                 table t : (hidden ++ static ++ map fst3 dynamic)
                 val shows : $(map show static)
                 val widgets : $(map Widget.t' dynamic)
                 val labels : $(map (fn _ => string) static ++ map (fn _ => string) dynamic)

                 val fls : folder static
                 val fld : folder dynamic
                 val injs : $(map sql_injectable static)
                 val injd : $(map (fn p => sql_injectable p.1) dynamic)

                 val amAuthorized : transaction bool (* Is this user allowed to use this interface? *)
             end) : Ui.S where type input = sql_exp [T = M.hidden ++ M.static ++ map fst3 M.dynamic] [] [] bool

(* Warning: [static] should really serve as a key for the table, or hijinks will ensue! *)
