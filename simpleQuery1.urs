(* Simple tabular rendering of query results *)

functor Make(M : sig
                 con fs :: {Type}
                 con tab :: Name
                 val query : sql_query [] [] [tab = fs] []
                 val fl : folder fs
                 val show : $(map show fs)
                 val labels : $(map (fn _ => string) fs)
             end) : Ui.S0
