(* Let the user fill in fields of his own database row. *)

functor Make(M : sig
                 con key :: {Type}
                 con chosen :: {(Type * Type * Type)}
                 con rest :: {Type}
                 constraint key ~ chosen
                 constraint key ~ rest
                 constraint chosen ~ rest
                 table t : (key ++ map fst3 chosen ++ rest)

                 val widgets : $(map Widget.t' chosen)
                 val key_inj : $(map sql_injectable key)
                 val chosen_inj : $(map (fn p => sql_injectable p.1) chosen)
                 val kfl : folder key
                 val cfl : folder chosen
                 val labels : $(map (fn _ => string) chosen)

                 val whoami : transaction (option $key)
             end) : Ui.S where type input = $M.key
