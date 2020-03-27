(* A flexible way to add rows to tables, combining custom logic for some columns with automatic widgetry for the rest *)

functor Make(M : sig
                 con custom :: {Type}
                 con ws :: {(Type * Type * Type)}
                 constraint custom ~ ws
                 val widgets : $(map Widget.t' ws)
                 val labels : $(map (fn _ => string) ws)
                 val cfl : folder custom
                 val wfl : folder ws
                 val injs : $(map sql_injectable (custom ++ map fst3 ws))

                 table tab : (map fst3 ws ++ custom)
                 val custom : $(map fst3 ws) -> transaction $custom
             end) : Ui.S0
