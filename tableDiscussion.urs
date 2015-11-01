(* Specialization of Discussion using foreign keys *)

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 constraint key ~ [Thread, When, Who, Text]
                 val fl : folder key
                 val kinj : $(map sql_injectable_prim key)
                 con rest :: {Type}
                 constraint rest ~ key
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val parent : sql_table (key ++ rest) ([keyName = map (fn _ => ()) key] ++ otherConstraints)

                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 val access : $key -> transaction Discussion.access
             end) : Ui.S where type input = $M.key
