(* Let a user indicate preferences for each of a set of options. *)

functor Make(M : sig
                 con choice :: Name
                 type choiceT
                 con choiceR :: {Type}
                 constraint [choice] ~ choiceR
                 table choice : ([choice = choiceT] ++ choiceR)
                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val inj_choiceT : sql_injectable_prim choiceT

                 con user :: Name
                 con slot :: Name
                 con available :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [available]
                 constraint [user, slot, available] ~ [preferred]
                 table pref : {user : string, slot : choiceT,
                               available : bool, preferred : bool}
                 
                 val whoami : transaction (option string)
             end) : Ui.S where type input = string
