(* Interface for exploring a graph of relations, one page per row, viewing and editing *)

con t :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)} -> Type
(* Each table gets:
 * (1) Key type
 * (2) Full schema
 * (3) Part of schema that has already been tiled with input widgets
 * (4) Constraints, present just to plug into [sql_table] below
 * (5) Internal Explorer implementation type #1
 * (5) Internal Explorer implementation type #2 *)

type base1
type base2
(* Default implementation types *)

val none : t []

val one : tname :: Name -> key :: Name -> keyT ::: Type -> rest ::: {Type} -> cstrs ::: {{Unit}}
          -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> [[key] ~ rest] => [[tname] ~ old] => sql_table ([key = keyT] ++ rest) cstrs -> string
          -> show keyT -> sql_injectable keyT -> $(map sql_injectable rest)
          -> folder rest
          -> t old
          -> t ([tname = (keyT, [key = keyT] ++ rest, [], cstrs, base1, base2)] ++ old)

val two : tname :: Name -> key1 :: Name -> key2 :: Name -> keyT1 ::: Type -> keyT2 ::: Type
          -> rest ::: {Type} -> cstrs ::: {{Unit}} -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> [[key1] ~ [key2]] => [[key1, key2] ~ rest] => [[tname] ~ old]
          => sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs -> string
          -> show (keyT1 * keyT2) -> sql_injectable keyT1 -> sql_injectable keyT2
          -> $(map sql_injectable rest)
          -> folder rest
          -> t old
          -> t ([tname = (keyT1 * keyT2, [key1 = keyT1, key2 = keyT2] ++ rest, [], cstrs, base1, base2)] ++ old)

con text1 :: Type -> Type
con text2 :: Type -> Type

val text : tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
           -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
           -> impl1 ::: Type -> impl2 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
           -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
           => string
           -> show colT
           -> read colT
           -> t ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2)] ++ old)
           -> t ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, text1 impl1, text2 impl2)] ++ old)

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type)}
                 val t : t (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5)) tables)
                 val fl : folder tables
             end) : sig
    val index : variant (map (fn _ => unit) M.tables) -> transaction page
    val create : variant (map (fn _ => unit) M.tables) -> transaction page
end
