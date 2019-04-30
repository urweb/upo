(* Allowing visitors to approve rows of one table to be copied (with additions) to another table *)

functor Make(M : sig
                 con key :: {(Type * Type * Type)}
                 val keyFl : folder key
                 val keyInj : $(map (fn p => sql_injectable p.1) key)

                 con otherIn :: {(Type * Type * Type)}
                 val otherInFl : folder otherIn
                 val otherInInj : $(map (fn p => sql_injectable p.1) otherIn)

                 constraint key ~ otherIn

                 con inKeyName :: Name
                 con inOtherConstraints :: {{Unit}}
                 constraint [inKeyName] ~ inOtherConstraints
                 val tableIn : sql_table (map fst3 key ++ map fst3 otherIn) ([inKeyName = map (fn _ => ()) key] ++ inOtherConstraints)
                 val labelsIn :$(map (fn _ => string) (key ++ otherIn))
                 val widgetsIn : $(map Widget.t' (key ++ otherIn))
                                  
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
                 val context : $(map fst3 key) -> transaction (option context)
                             (* Return [None] if this user may not submit the form. *)
                      
                 con others :: {Type}
                 val others : context -> $(map (sql_exp [] [] []) others)
                 constraint widgets ~ others
                 constraint constants ~ others

                 con ported :: {Type}
                 constraint widgets ~ ported
                 constraint constants ~ ported
                 constraint others ~ ported
                 val portedFl : folder ported
                 val portedInj : $(map sql_injectable ported)
                 val port : $(map fst3 (key ++ otherIn)) -> $ported
                     
                 table tableOut : $(ported ++ map fst3 widgets ++ constants ++ others)
             end) : Ui.S where type input = $(map fst3 M.key)
