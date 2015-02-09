(* Basic abstraction for collaborative modification of a database table *)

type permission = {Add : bool,
                   Delete : bool,
                   Modify : bool}

functor Make(M : sig
                 con fs :: {(Type * Type)}
                 val widgets : $(map Widget.t' fs)
                 table tab : $(map fst fs)
                 val fl : folder fs
                 val eqs : $(map eq (map fst fs))
                 val ords : $(map ord (map fst fs))
                 val injs : $(map sql_injectable (map fst fs))

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission

                 val onAdd : $(map fst fs) -> transaction unit
                 val onDelete : $(map fst fs) -> transaction unit
                 val onModify : {Old : $(map fst fs), New : $(map fst fs)} -> transaction unit
             end) : Ui.S0
