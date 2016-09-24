(* Interface for exploring a graph of relations, one page per row, viewing and editing *)

con t :: {(Type * {Type} * {{Unit}} * Type)} -> Type
(* Each table gets:
 * (1) Key type
 * (2) Full schema
 * (3) Constraints, present just to plug into [sql_table] below
 * (4) Internal Explorer implementation type *)

type base
(* Default implementation type *)

val none : t []

val one : tname :: Name -> key :: Name -> keyT ::: Type -> rest ::: {Type} -> cstrs ::: {{Unit}}
          -> old ::: {(Type * {Type} * {{Unit}} * Type)}
          -> [[key] ~ rest] => [[tname] ~ old] => sql_table ([key = keyT] ++ rest) cstrs -> string
          -> show keyT -> sql_injectable keyT
          -> t old
          -> t ([tname = (keyT, [key = keyT] ++ rest, cstrs, base)] ++ old)

val two : tname :: Name -> key1 :: Name -> key2 :: Name -> keyT1 ::: Type -> keyT2 ::: Type
          -> rest ::: {Type} -> cstrs ::: {{Unit}} -> old ::: {(Type * {Type} * {{Unit}} * Type)}
          -> [[key1] ~ [key2]] => [[key1, key2] ~ rest] => [[tname] ~ old]
          => sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs -> string
          -> show (keyT1 * keyT2) -> sql_injectable keyT1 -> sql_injectable keyT2
          -> t old
          -> t ([tname = (keyT1 * keyT2, [key1 = keyT1, key2 = keyT2] ++ rest, cstrs, base)] ++ old)

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type)}
                 val t : t tables
                 val fl : folder tables
             end) : sig
    val index : variant (map (fn _ => unit) M.tables) -> transaction page
end
