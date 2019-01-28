(* Simple rendering of single rows using widgets, with lookup by key *)

functor Make(M : sig
                 con key :: {(Type * Type * Type)}
                 con r :: {(Type * Type * Type)}
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint key ~ r
                 constraint [keyName] ~ otherConstraints

                 val t : sql_table (map fst3 (key ++ r)) ([keyName = map (fn _ => ()) key] ++ otherConstraints)
                            
                 val labels : $(map (fn _ => string) (key ++ r))
                 val ws : $(map Widget.t' (key ++ r))
                 val kfl : folder key
                 val rfl : folder r
                 val injs : $(map sql_injectable (map fst3 key))
             end) : Ui.S where type input = $(map fst3 M.key)
