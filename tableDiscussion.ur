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
             end) = struct

    open M

    table message : (key ++ [Thread = time, When = time, Who = string, Text = string])
      PRIMARY KEY {{@primary_key [key1] [keyR ++ [Thread = _, When = _]] ! !
                     (kinj ++ _)}},
      {{one_constraint [#Parent] (@Sql.easy_foreign ! ! ! ! ! ! fl parent)}}

    open Discussion.Make(struct
                             open M

                             con message_hidden_constraints = _
                             con empty :: {{Unit}} = []
                             constraint empty ~ message_hidden_constraints
                             val message = message

                             val kinj = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim fl kinj
                         end)

end
