(* Interface for exploring a graph of relations, one page per row, viewing and editing *)

con t :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
    -> {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
    -> Type
(* Each table gets:
 * (1) Key type
 * (2) Full schema
 * (3) Part of schema that has already been tiled with input widgets
 * (4) Constraints, present just to plug into [sql_table] below
 * (5) Internal Explorer implementation type #1
 * (5) Internal Explorer implementation type #2
 *
 * The first argument is the final schema, prognosticated before we start adding concrete tables.
 * The second argument only includes the tables we have added already.
 * The two must match when we are ready to build a concrete Web component. *)

type base1
type base2
(* Default implementation types *)

val none : full ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
           -> t full []

val one : full ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> tname :: Name -> key :: Name -> keyT ::: Type -> rest ::: {Type} -> cstrs ::: {{Unit}}
          -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> [[key] ~ rest] => [[tname] ~ old] => sql_table ([key = keyT] ++ rest) cstrs -> string
          -> show keyT -> sql_injectable keyT -> $(map sql_injectable rest)
          -> folder rest -> folder old
          -> t full old
          -> t full ([tname = (keyT, [key = keyT] ++ rest, [], cstrs, base1, base2)] ++ old)

val two : full ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> tname :: Name -> key1 :: Name -> key2 :: Name -> keyT1 ::: Type -> keyT2 ::: Type
          -> rest ::: {Type} -> cstrs ::: {{Unit}} -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
          -> [[key1] ~ [key2]] => [[key1, key2] ~ rest] => [[tname] ~ old]
          => sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs -> string
          -> show (keyT1 * keyT2) -> sql_injectable keyT1 -> sql_injectable keyT2
          -> $(map sql_injectable rest)
          -> folder rest -> folder old
          -> t full old
          -> t full ([tname = (keyT1 * keyT2, [key1 = keyT1, key2 = keyT2] ++ rest, [], cstrs, base1, base2)] ++ old)

con text1 :: Type -> Type
con text2 :: Type -> Type

val text : full ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
           -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
           -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
           -> impl1 ::: Type -> impl2 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
           -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
           => string
           -> show colT
           -> read colT
           -> t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2)] ++ old)
           -> t full ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, text1 impl1, text2 impl2)] ++ old)

con foreign1 :: Type -> Type -> Type
con foreign2 :: Type -> Type -> Type

val foreign : full ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
              -> ft1 ::: {Type} -> ft2 ::: {Type} -> ft3 ::: {{Unit}} -> ft4 ::: Type -> ft5 ::: Type
              -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
              -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
              -> impl1 ::: Type -> impl2 ::: Type
              -> ftname :: Name -> fcol :: Name
              -> fcols ::: {Type} -> fcolsDone ::: {Type} -> fcstrs ::: {{Unit}}
              -> fimpl1 ::: Type -> fimpl2 ::: Type
              -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}
              -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
              => [[fcol] ~ fcols] => [[fcol] ~ fcolsDone] => [[ftname] ~ old]
              => [[tname] ~ [ftname]] => [[ftname] ~ full]
              => string
              -> show colT
              -> read colT
              -> t ([ftname = (colT, ft1, ft2, ft3, ft4, ft5)] ++ full)
                   ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2),
                     ftname = (colT, [fcol = colT] ++ fcols, fcolsDone, fcstrs, fimpl1, fimpl2)] ++ old)
              -> t ([ftname = (colT, ft1, ft2, ft3, ft4, ft5)] ++ full)
                   ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, foreign1 impl1 colT, foreign2 impl2 colT),
                     ftname = (colT, [fcol = colT] ++ fcols, fcolsDone, fcstrs, fimpl1, fimpl2)] ++ old)

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type)}
                 val t : t (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5)) tables)
                           (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5)) tables)
                 val fl : folder tables
             end) : sig
    val index : variant (map (fn _ => unit) M.tables) -> transaction page
    val create : variant (map (fn _ => unit) M.tables) -> transaction page
end
