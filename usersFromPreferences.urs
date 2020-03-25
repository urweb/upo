(* Building on a process where users indicate preferences (perhaps using the
 * [Preferences] concept), finalize assignment of users to items. *)

functor Make(M : sig
                 (* Choices are what the users indicate preferences for. *)
                 con choice :: Name
                 type choiceT
                 con users :: {{{Unit}}}
                 con choiceR :: {Type}
                 constraint [choice] ~ users
                 constraint [choice] ~ choiceR
                 constraint users ~ choiceR
                 con ckeys :: {{Unit}}
                 val choice : sql_table ([choice = choiceT] ++ map (fn _ => option string) users ++ choiceR) ckeys

                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val eq_choiceT : eq choiceT
                 val inj_choiceT : sql_injectable_prim choiceT
                 val cfl : folder choiceR
                 val fl : folder users
                 val labels : $(map (fn _ => string) users)

                 (* Preferences are per-user. *)
                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 val prefs : $(map (sql_table [user = string, slot = choiceT, preferred = bool]) users)

                 (* Is this user allowed to perform assignments? *)
                 val whoami : transaction (option string)
             end) : Ui.S0
