(* Basic abstraction for collaborative modification of a database table *)

type permission = {Add : bool,
                   Delete : bool,
                   Modify : bool}

functor Make(M : sig
                 con fs :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' fs)
                 table tab : $(map fst3 fs)
                 val title : string
                 val fl : folder fs
                 val eqs : $(map eq (map fst3 fs))
                 val ords : $(map ord (map fst3 fs))
                 val injs : $(map sql_injectable (map fst3 fs))

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission

                 val onAdd : $(map fst3 fs) -> transaction unit
                 val onDelete : $(map fst3 fs) -> transaction unit
                 val onModify : {Old : $(map fst3 fs), New : $(map fst3 fs)} -> transaction unit
             end) : Ui.S0
