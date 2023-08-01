(* Simple graph-based rendering of query results *)

class numeric :: Type -> Type
val numeric_int : numeric int
val numeric_float : numeric float
val numeric_option : t ::: Type -> numeric t -> numeric (option t)

functor Make(M : sig
                 con xName :: Name
                 type xType
                 con y :: {Type}
                 constraint [xName] ~ y
                 val query : sql_query [] [] [] ([xName = xType] ++ y)
                 val fl : folder y
                 val label : show xType
                 val numerics : $(map numeric y)
             end) : Ui.S0
