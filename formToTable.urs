(* Allowing visitors to submit a web form to add a row to a table *)

functor Make(M : sig
                 con widgets :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' widgets)
                 val widgetsFl : folder widgets
                 val widgetsInj : $(map (fn p => sql_injectable p.1) widgets)
                 val labels : $(map (fn _ => string) widgets)

                 con constants :: {Type}
                 val constants : $constants
                 val constantsFl : folder constants
                 val constantsInj : $(map sql_injectable constants)

                 constraint constants ~ widgets
                                    
                 type context
                 val context : transaction (option context)
                             (* Return [None] if this user may not submit the form. *)
                      
                 con others :: {Type}
                 val others : context -> $(map (sql_exp [] [] []) others)
                 constraint widgets ~ others
                 constraint constants ~ others
                     
                 table tab : $(map fst3 widgets ++ constants ++ others)
             end) : Ui.S where type input = $(map (fn p => option p.1) M.widgets)
