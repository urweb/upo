(* Widget to help the user choose a subset of applicable rows of a table.
 * We work with a few different kinds of DB columns:
 *  - [const]: columns whose values are fixed at compile time
 *  - [given]: columns whose values are fixed at runtime
 *  - [chosen]: columns where the user has some say at runtime
 *)

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con chosen :: {Type}
                 constraint const ~ given
                 constraint (const ++ given) ~ chosen

                 val const : $const

                 table choices : (const ++ given ++ chosen)

                 con optionsConst :: {Type}
                 con others :: {Type}
                 constraint others ~ chosen
                 constraint (others ++ chosen) ~ optionsConst
                 table options : (optionsConst ++ chosen ++ others)
                 val optionsConst : $optionsConst

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen
                 val optionsConstFl : folder optionsConst

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val chosenInj : $(map sql_injectable chosen)
                 val optionsConstInj : $(map sql_injectable optionsConst)

                 val chosenShow : show $chosen
                 val chosenRead : read $chosen
                 val chosenEq : eq $chosen

                 val givenEq : eq $given

                 val buttonLabel : string

                 (* Authentication *)
                 val amGiven : transaction (option $given)
             end) : sig

    type t
    val create : $M.given -> transaction t
    val render : t -> xbody

end
