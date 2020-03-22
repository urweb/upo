(* Building on a process where users indicate preferences (perhaps using the
 * [Preferences] concept), finalize assignment of users to items. *)

functor Make(M : sig
                 (* Choices are what the users indicate preferences for. *)
                 con choice :: Name
                 type choiceT
                 con choiceR :: {Type}
                 constraint [choice] ~ choiceR
                 table choice : ([choice = choiceT] ++ choiceR)
                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val eq_choiceT : eq choiceT
                 val inj_choiceT : sql_injectable_prim choiceT
                 val inj_choiceR : $(map sql_injectable_prim choiceR)
                 val cfl : folder choiceR

                 (* Preferences are per-user. *)
                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 con users :: {{{Unit}}}
                 val prefs : $(map (sql_table [user = string, slot = choiceT, preferred = bool]) users)

                 (* This relation links users to their assigned choices. *)
                 constraint [choice] ~ users
                 constraint choiceR ~ users
                 con akeys :: {{Unit}}
                 val assignment : sql_table ([choice = choiceT] ++ map (fn _ => string) users ++ choiceR) akeys
                 val fl : folder users
                 val labels : $(map (fn _ => string) users)

                 (* Is this user allowed to perform assignments? *)
                 val whoami : transaction (option string)
             end) : Ui.S0
