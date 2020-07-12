(* Use a nice calendar GUI to assign times to events. *)

functor Make(M : sig
                 con key :: Name
                 con others :: {Type}
                 constraint [key] ~ others
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val times : sql_table ([key = time] ++ others) ([keyName = [key]] ++ otherKeys)
                 val others : time -> $others
                 val inj_others : $(map sql_injectable others)
                 val ofl : folder others

                 con this :: Name
                 con thisT :: Type
                 con ttime :: Name
                 con r :: {Type}
                 constraint [this] ~ [ttime]
                 constraint [this, ttime] ~ r
                 table t : ([this = thisT, ttime = option time] ++ r)
                 val tTitle : string

                 val show_this : show thisT
                 val read_this : read thisT
                 val eq_this : eq thisT
                 val inj_this : sql_injectable thisT

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([this = thisT, ttime = option time] ++ r)
                 val schedAddon : SchedulingAddons.t thisT
             end) : Ui.S0
