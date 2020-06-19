(* Fill in optional time columns in some rows of a table. *)

functor Make(M : sig
                 con key :: Name
                 type keyT
                 con tm :: Name
                 con others :: {Type}
                 constraint [key] ~ [tm]
                 constraint [key, tm] ~ others
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val t : sql_table ([key = keyT, tm = option time] ++ others) ([keyName = [key]] ++ otherKeys)

                 val eq_key : eq keyT
                 val read_key : read keyT
                 val show_key : show keyT
                 val inj_key : sql_injectable keyT

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([key = keyT, tm = option time] ++ others)
             end) : Ui.S0
