(* Given information on time availability of users assigned events,
 * use a nice calendar GUI to assign times to events. *)

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

                 con btime :: Name
                 con user :: Name
                 con preferred :: Name
                 constraint [btime] ~ [user]
                 constraint [btime, user] ~ [preferred]
                 table bid : {btime : time, user : string, preferred : bool}
                 val bidTitle : string

                 con this :: Name
                 con thisT :: Type
                 con ttime :: Name
                 con assignees :: {Unit}
                 con r :: {Type}
                 constraint [this] ~ [ttime]
                 constraint [this, ttime] ~ assignees
                 constraint [this, ttime] ~ r
                 constraint assignees ~ r
                 constraint [T] ~ [Times]
                 constraint [T, Times] ~ [Preferred]
                 constraint [T, Times, Preferred] ~ assignees
                 table t : ([this = thisT, ttime = option time] ++ mapU (option string) assignees ++ r)
                 val tTitle : string

                 val fl : folder assignees
                 val show_this : show thisT
                 val read_this : read thisT
                 val eq_this : eq thisT
                 val inj_this : sql_injectable thisT
                 val assignees : $(mapU string assignees)

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([this = thisT, ttime = option time] ++ mapU (option string) assignees ++ r)
             end) : Ui.S0
