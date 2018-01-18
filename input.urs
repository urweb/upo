(* Widget to display some arbitrarily typed fields to a user, with the option to edit *)

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con fixed :: {Type}
                 con chosen :: {(Type * Type * Type)}
                 constraint const ~ given
                 constraint (const ++ given) ~ fixed
                 constraint (const ++ given ++ fixed) ~ chosen

                 val const : $const

                 table tab : (const ++ given ++ fixed ++ map fst3 chosen)

                 val chosenLabels : $(map (fn _ => string) chosen)

                 val widgets : $(map Widget.t' chosen)

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val givenEq : $(map eq given)
                 val chosenInj : $(map (fn p => sql_injectable p.1) chosen)

                 val textLabel : string

                 (* Authentication *)
                 val amGiven : transaction (option $given)
             end) : Ui.S where type input = $M.given
