(* Let the current user mark which times from a table of possibilities he likes,
 * using a nice calendar GUI. *)

functor Make(M : sig
                 con key :: Name
                 con kother :: {Type}
                 constraint [key] ~ kother
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val times : sql_table ([key = time] ++ kother) ([keyName = [key]] ++ otherKeys)

                 con user :: Name
                 con tcol :: Name
                 con preferred :: Name
                 constraint [user] ~ [tcol]
                 constraint [user, tcol] ~ [preferred]
                 table prefs : {user : string, tcol : time, preferred : bool}
                 val title : string

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t [key = time]
                 val slotDuration : option string
             end) : Ui.S where type input = string
