(* Simple graph-based rendering of query results *)

class numeric :: Type -> Type
val numeric_int : numeric int
val numeric_float : numeric float
val numeric_option : t ::: Type -> numeric t -> numeric (option t)

datatype graphType = Bar | StackedBar | Line | Pie | Doughnut | PolarArea | Radar

functor Make(M : sig
                 con xName :: Name
                 type xType
                 con y :: {Type}
                 constraint [xName] ~ y
                 val query : sql_query [] [] [] ([xName = xType] ++ y)
                 val fl : folder y
                 val label : show xType
                 val numerics : $(map numeric y)
                 val labels : $(map (fn _ => string) ([xName = xType] ++ y))
                 val graphType : graphType
             end) : Ui.S0
