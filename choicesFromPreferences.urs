(* Building on a process where users indicate preferences (perhaps using the
 * [Preferences] concept), finalize assignment of choices compatible with
 * preferences. *)

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

                 (* Preferences are per-user. *)
                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 table pref : {user : string, slot : choiceT, preferred : bool}

                 (* Items are the ones we must assign choices to, based on user preferences. *)
                 con item :: Name
                 type itemT
                 con ichoice :: Name (* field name recording eventual choice *)
                 con users :: {Unit} (* These are the named users whose
                                      * preferences we should consider. *)
                 con itemR :: {(Type * Type)}
                 constraint [item] ~ [ichoice]
                 constraint [item, ichoice] ~ users
                 constraint [item, ichoice] ~ itemR
                 constraint users ~ itemR
                 table item : ([item = itemT, ichoice = option choiceT] ++ mapU (option string) users ++ map fst itemR)
                 val fl : folder users
                 val show_itemT : show itemT
                 val eq_itemT : eq itemT
                 val inj_itemT : sql_injectable_prim itemT
                 val nullify_itemR : $(map (fn p => nullify p.1 p.2) itemR)
                 val labels : $(mapU string users)

                 (* Is this user allowed to perform assignments? *)
                 val authorize : transaction bool
             end) : Ui.S0
