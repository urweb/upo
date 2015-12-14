(* Simple tabular rendering of query results, based on some input parameter *)

functor Make(M : sig
                 type input
                 con fs :: {Type}
                 val query : input -> sql_query [] [] [] fs
                 val fl : folder fs
                 val show : $(map show fs)
                 val labels : $(map (fn _ => string) fs)
             end) : Ui.S where type input = M.input

