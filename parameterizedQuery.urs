(* Running SQL queries based on some parameters entered via widgets *)

functor Make(M : sig
                 con params :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' params)
                 val paramsFl : folder params
                 val paramLabels : $(map (fn _ => string) params)

                 con buttons :: {Unit}
                 val buttonsFl : folder buttons
                 con results :: {Type}
                 val resultsFl : folder results
                 val resultLabels : $(map (fn _ => string) results)
                 val query : $(map fst3 params) -> sql_query [] [] [] results
                 val shows : $(map show results)
             end) : Ui.S where type input = $(mapU ($M.results -> string (* label *) * url) M.buttons)
