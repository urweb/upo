(* Running SQL queries based on some parameters entered via widgets *)

signature S = sig
    con params :: {(Type * Type * Type)}
    val widgets : $(map Widget.t' params)
    val paramsFl : folder params
    val paramLabels : $(map (fn _ => string) params)

    con results :: {Type}
    val resultsFl : folder results
    val resultLabels : $(map (fn _ => string) results)
    val query : $(map fst3 params) -> sql_query [] [] [] results
    val shows : $(map show results)

    val authorized : transaction bool
end

functor Html(M : sig
                 include S
                 con buttons :: {Unit}
                 val buttonsFl : folder buttons
             end) : Ui.S where type input = $(mapU ($M.results -> string (* label *) * url) M.buttons)

functor Csv(M : sig
                include S
                val filename : string
            end) : Ui.S0
