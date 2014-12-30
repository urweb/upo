(* Widget to display some textual fields to a user, with the option to edit *)

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con fixed :: {Type}
                 con chosen :: {Unit}
                 constraint const ~ given
                 constraint (const ++ given) ~ fixed
                 constraint (const ++ given ++ fixed) ~ chosen

                 val const : $const

                 table tab : (const ++ given ++ fixed ++ mapU string chosen)

                 val chosenLabels : $(mapU string chosen)

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val givenEq : eq $given

                 val textLabel : string

                 (* Authentication *)
                 val amGiven : transaction (option $given)
             end) : Ui.S where type input = $M.given
