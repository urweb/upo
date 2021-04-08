(* Running SQL queries based on some parameters entered via widgets *)

signature S = sig
    con params :: {(Type * Type * Type)}
    val widgets : $(map Widget.t' params)
    val paramsFl : folder params
    val paramLabels : $(map (fn _ => string) params)

    val authorized : transaction bool
end

functor Html(M : sig
                 include S

                 con results :: {(Type * Type * Type)}
                 val resultsFl : folder results
                 val resultLabels : $(map (fn _ => string) results)
                 val query : $(map fst3 params) -> transaction (sql_query [] [] [] (map fst3 results))
                 val resultWidgets : $(map Widget.t' results)

                 con buttons :: {Unit}
                 val buttonsFl : folder buttons

                 val onResult : option ($(map fst3 results) -> transaction unit)
                 (* If present, include a button to run this function
                  * server-side on every result. *)
             end) : Ui.S where type input = $(mapU ($(map fst3 M.params) -> $(map fst3 M.results) -> string (* label *) * url) M.buttons)

functor Csv(M : sig
                include S

                con results :: {Type}
                val resultsFl : folder results
                val resultLabels : $(map (fn _ => string) results)
                val query : $(map fst3 params) -> transaction (sql_query [] [] [] results)
                val shows : $(map show results)

                val filename : string
            end) : Ui.S0
