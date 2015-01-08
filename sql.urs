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

(* Construct a trivial matching, for foreign-key purposes *)
val easy_matching : fs ::: {Type} -> folder fs -> matching fs fs

(* Trivial foreign-key match-up *)
val easy_foreign : nm ::: Name -> fnm ::: Name -> ft ::: Type -> fs ::: {Type} -> munused ::: {Type} -> funused ::: {Type} -> uniques ::: {{Unit}}
                   -> [[fnm] ~ fs] => [[fnm] ~ munused] => [fs ~ munused] => [[fnm] ~ funused] => [fs ~ funused] => [[nm] ~ uniques] => folder ([fnm = ft] ++ fs)
                   -> sql_table ([fnm = ft] ++ fs ++ funused) ([nm = map (fn _ => ()) ([fnm = ft] ++ fs)] ++ uniques)
                   -> sql_constraint ([fnm = ft] ++ fs ++ munused) []

(* Easy table insert with constants *)
val easy_insert : fields ::: {Type} -> uniques ::: {{Unit}}
                  -> $(map sql_injectable fields)
                  -> folder fields
                  -> sql_table fields uniques
                  -> $fields
                  -> transaction unit

(* Like above, but does update instead if rows exist with matching keys *)
val easy_insertOrUpdate : keys :: {Type} -> fields ::: {Type} -> uniques ::: {{Unit}}
                          -> [keys ~ fields]
                          => $(map sql_injectable keys)
                          -> $(map sql_injectable fields)
                          -> folder keys
                          -> folder fields
                          -> sql_table (keys ++ fields) uniques
                          -> $(keys ++ fields)
                          -> transaction unit

(* Like above, but does nothing if rows exist with matching keys *)
val easy_insertOrSkip : keys :: {Type} -> fields ::: {Type} -> uniques ::: {{Unit}}
                        -> [keys ~ fields]
                        => $(map sql_injectable keys)
                        -> $(map sql_injectable fields)
                        -> folder keys
                        -> folder fields
                        -> sql_table (keys ++ fields) uniques
                        -> $(keys ++ fields)
                        -> transaction unit

(* Easy table update with constants *)
val easy_update : key ::: {Type} -> fields ::: {Type} -> uniques ::: {{Unit}}
                  -> [key ~ fields]
                  => $(map sql_injectable key)
                  -> $(map sql_injectable fields)
                  -> folder key
                  -> folder fields
                  -> sql_table (key ++ fields) uniques
                  -> $key
                  -> $fields
                  -> transaction unit

val easy_update' : key ::: {Type} -> fields ::: {Type} -> uniques ::: {{Unit}}
                  -> [key ~ fields]
                  => $(map sql_injectable key)
                  -> $(map sql_injectable fields)
                  -> folder key
                  -> folder fields
                  -> sql_table (key ++ fields) uniques
                  -> $key
                  -> $(key ++ fields)
                  -> transaction unit

val easy_update'' : key ::: {Type} -> fields ::: {Type} -> uniques ::: {{Unit}}
                    -> leftAlone ::: {Type}
                    -> [key ~ fields]
                    => [key ++ fields ~ leftAlone]
                    => $(map sql_injectable key)
                    -> $(map sql_injectable fields)
                    -> folder key
                    -> folder fields
                    -> sql_table (key ++ fields ++ leftAlone) uniques
                    -> $key
                    -> $fields
                    -> transaction unit

(* Build a WHERE clause equating fields of a table to constant values. *)
val easy_where : tab :: Name -> using ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                 -> [using ~ notUsing] => [[tab] ~ otherTables]
                 => $(map sql_injectable using) -> folder using
                 -> $using
                 -> sql_exp ([tab = using ++ notUsing] ++ otherTables) agg exps bool

(* Build a WHERE clause joining identical columns between two tables. *)
val easy_join : tab1 :: Name -> tab2 :: Name -> using ::: {Type}
                -> notUsing1 ::: {Type} -> notUsing2 ::: {Type}
                -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                -> [[tab1] ~ [tab2]] => [using ~ notUsing1] => [using ~ notUsing2]
                => [[tab1, tab2] ~ otherTables]
                => folder using
                -> sql_exp ([tab1 = using ++ notUsing1, tab2 = using ++ notUsing2] ++ otherTables) agg exps bool
