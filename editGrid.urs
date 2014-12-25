(* A simple grid for editing a whole table, when its column values can all be represented in textboxes *)

functor Make(M : sig
                 con key :: {Type}
                 con rest :: {Type}
                 constraint key ~ rest

                 table tab : (key ++ rest)

                 val keyFl : folder key
                 val restFl : folder rest

                 val keyShow : $(map show key)
                 val keyRead : $(map read key)
                 val keyEq : $(map eq key)
                 val keyInj : $(map sql_injectable key)

                 val restShow : $(map show rest)
                 val restRead : $(map read rest)
                 val restInj : $(map sql_injectable rest)

                 val labels : $(map (fn _ => string) (key ++ rest))

                 val authorized : transaction bool
             end) : sig
    type t
    val create : transaction t
    val render : t -> xbody
end
