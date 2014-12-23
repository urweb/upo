(* Metaprogramming utilities for SQL *)

(* Build an 'ORDER BY' clause programmatically, with the same sort order for each expression. *)
val order_by : tables ::: {{Type}} -> exps ::: {Type} -> dummy ::: {Type}
               -> folder dummy
               -> $(map (sql_exp tables [] exps) dummy)
               -> sql_direction
               -> sql_order_by tables exps

(* Build a record of expressions for a subset of the fields of a table. *)
val some_fields : tab :: Name -> keep :: {Type} -> drop ::: {Type} -> others ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                  -> [keep ~ drop] => [[tab] ~ others] => folder keep
                  -> $(map (sql_exp ([tab = keep ++ drop] ++ others) agg exps) keep)
