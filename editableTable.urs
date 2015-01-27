(* Basic abstraction for collaborative modification of a database table *)

type permission = {Add : bool,
                   Delete : bool,
                   Modify : bool}

functor Make(M : sig
                 con fs :: {(Type * Type)}
                 val widgets : $(map Widget.t' fs)
                 table tab : $(map fst fs)
                 val fl : folder fs
                 val eqs : $(map (fn p => eq p.1) fs)
                 val ords : $(map (fn p => ord p.1) fs)
                 val injs : $(map (fn p => sql_injectable p.1) fs)

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission
             end) : Ui.S0
