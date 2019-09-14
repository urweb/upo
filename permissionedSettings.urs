(* Based on current user, make available different tweakable database fields. *)

con t :: Type -> Type -> Type
(* Parameters are private state types. *)

con singleRowTable1 :: {(Type * Type * Type)} -> Type
con singleRowTable2 :: {(Type * Type * Type)} -> Type
val singleRowTable : u ::: Name -> group :: Name -> auth ::: {Type} -> aks ::: {{Unit}}
                     -> fs ::: {(Type * Type * Type)} -> fks ::: {{Unit}}
                     -> [[u] ~ auth] => [[group] ~ [u = string] ++ auth]
                     => folder fs
                     -> $(map Widget.t' fs)
                     -> $(map (fn p => sql_injectable p.1) fs)
                     -> string (* title *)
                     -> sql_table ([u = string, group = bool] ++ auth) aks
                        (* Users table, to check if this user belongs to the named group *)
                     -> sql_table (map fst3 fs) fks
                        (* The eponymous single-row table *)
                     -> $(map (fn _ => string) fs) (* labels *)
                        -> t (singleRowTable1 fs) (singleRowTable2 fs)

con tableWithAcl1 :: Type -> {(Type * Type * Type)} -> Type
con tableWithAcl2 :: Type -> {(Type * Type * Type)} -> Type
val tableWithAcl : u ::: Name -> k ::: Name -> kt ::: Type -> auth ::: {Type} -> aks ::: {{Unit}}
                  -> k2 ::: Name -> fs ::: {(Type * Type * Type)} -> fks ::: {{Unit}}
                  -> [[u] ~ [k]] => [[u = string, k = kt] ~ auth] => [[k2] ~ fs]
                  => sql_injectable_prim kt
                  -> folder fs
                  -> $(map Widget.t' fs)
                  -> $(map (fn p => sql_injectable_prim p.1) fs)
                  -> (kt -> string) (* title *)
                  -> sql_table ([u = string, k = kt] ++ auth) aks
                     (* Access control list, to check which users may change which resources *)
                  -> sql_table ([k2 = kt] ++ map fst3 fs) fks
                     (* The table to be modified *)
                  -> $(map (fn _ => string) fs) (* labels *)
                  -> t (tableWithAcl1 kt fs) (tableWithAcl2 kt fs)

con compose1 :: Type -> Type -> Type
con compose2 :: Type -> Type -> Type
val compose : a1 ::: Type -> b1 ::: Type -> a2 ::: Type -> b2 ::: Type
              -> t a1 a2 -> t b1 b2 -> t (compose1 a1 b1) (compose2 a2 b2)
                        
functor Make(M : sig
                 type p1
                 type p2
                 val t : t p1 p2

                 val whoami : transaction (option string)
             end) : Ui.S0
