(* Allowing visitors to submit a web form to add a row to a table *)

functor Make(M : sig
                 con widgets :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' widgets)
                 val widgetsFl : folder widgets
                 val widgetsInj : $(map (fn p => sql_injectable p.1) widgets)
                 val labels : $(map (fn _ => string) widgets)

                 type context
                 val context : transaction (option context)
                             (* Return [None] if this user may not submit the form. *)
                      
                 con others :: {Type}
                 val others : context -> $(map (sql_exp [] [] []) others)
                 constraint widgets ~ others
                     
                 table tab : $(map fst3 widgets ++ others)
             end) : Ui.S0
