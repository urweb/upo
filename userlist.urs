(* List all users who have the app loaded now. *)

functor Make(M : sig
                 con key :: Name
                 con rest :: {Type}
                 constraint [key] ~ rest
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val user : sql_table ([key = string] ++ rest) ([keyName = [key]] ++ otherKeys)

                 val whoami : transaction (option string)
             end) : Ui.S0
