(* Interface for exploring a graph of relations, one page per row, viewing and editing *)

con t :: {Type}
    -> {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
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
 * The two must match (taking the first position from each record) when we are ready to build a concrete Web component. *)

type base1
type base2
type base3
(* Default implementation types *)

val none : full ::: {Type} -> t full []

val one : full ::: {Type}
          -> tname :: Name -> key :: Name -> keyT ::: Type -> rest ::: {Type} -> cstrs ::: {{Unit}}
          -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
          -> [[key] ~ rest] => [[tname] ~ old] => sql_table ([key = keyT] ++ rest) cstrs -> string
          -> show keyT -> sql_injectable keyT -> $(map sql_injectable rest)
          -> folder rest -> folder old
          -> t full old
          -> t full ([tname = (keyT, [key = keyT] ++ rest, [], cstrs, base1, base2, base3)] ++ old)

val two : full ::: {Type}
          -> tname :: Name -> key1 :: Name -> key2 :: Name -> keyT1 ::: Type -> keyT2 ::: Type
          -> rest ::: {Type} -> cstrs ::: {{Unit}} -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
          -> [[key1] ~ [key2]] => [[key1, key2] ~ rest] => [[tname] ~ old]
          => sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs -> string
          -> show (keyT1 * keyT2) -> sql_injectable keyT1 -> sql_injectable keyT2
          -> $(map sql_injectable rest)
          -> folder rest -> folder old
          -> t full old
          -> t full ([tname = (keyT1 * keyT2, [key1 = keyT1, key2 = keyT2] ++ rest, [], cstrs, base1, base2, base3)] ++ old)

con text1 :: Type -> Type
con text2 :: Type -> Type
con text3 :: Type -> Type

val text : full ::: {Type}
           -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
           -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
           -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
           -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
           => string
           -> show colT
           -> read colT
           -> t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
           -> t full ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, text1 impl1, text2 impl2, text3 impl3)] ++ old)

con foreign1 :: Type -> Type -> Type -> Type
con foreign2 :: Type -> Type -> Type -> Type
con foreign3 :: Type -> Type -> Type -> Type

val foreign : full ::: {Type}
              -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
              -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
              -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type
              -> ftname :: Name -> fcol :: Name
              -> fcols ::: {Type} -> fcolsDone ::: {Type} -> fcstrs ::: {{Unit}}
              -> fimpl1 ::: Type -> fimpl2 ::: Type -> fimpl3 ::: Type
              -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
              -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
              => [[fcol] ~ fcols] => [[ftname] ~ old]
              => [[tname] ~ [ftname]] => [[tname, ftname] ~ full]
              => string
              -> string
              -> show colT
              -> read colT
              -> sql_injectable colT
              -> t ([tname = key, ftname = colT] ++ full)
                   ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3),
                     ftname = (colT, [fcol = colT] ++ fcols, fcolsDone, fcstrs, fimpl1, fimpl2, fimpl3)] ++ old)
              -> t ([tname = key, ftname = colT] ++ full)
                   ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, foreign1 impl1 key colT, foreign2 impl2 key colT, impl3),
                     ftname = (colT, [fcol = colT] ++ fcols, fcolsDone, fcstrs, fimpl1, fimpl2, foreign3 fimpl3 key colT)] ++ old)

con manyToMany11 :: Type -> Type -> Type -> Type
con manyToMany12 :: Type -> Type -> Type -> Type
con manyToMany13 :: Type -> Type -> Type -> Type
con manyToMany21 :: Type -> Type -> Type -> Type
con manyToMany22 :: Type -> Type -> Type -> Type
con manyToMany23 :: Type -> Type -> Type -> Type

val manyToMany : full ::: {Type}
                 -> tname1 :: Name -> key1 ::: Type -> col1 :: Name
                 -> cols1 ::: {Type} -> colsDone1 ::: {Type} -> cstrs1 ::: {{Unit}}
                 -> impl11 ::: Type -> impl12 ::: Type -> impl13 ::: Type
                 -> tname2 :: Name -> key2 ::: Type -> col2 :: Name
                 -> cols2 ::: {Type} -> colsDone2 ::: {Type} -> cstrs2 ::: {{Unit}}
                 -> impl21 ::: Type -> impl22 ::: Type -> impl23 ::: Type
                 -> cstrs ::: {{Unit}}
                 -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
                 -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ old] => [[tname1, tname2] ~ full]
                 => [[col1] ~ cols1] => [[col2] ~ cols2] => [[col1] ~ [col2]]
                 => sql_table [col1 = key1, col2 = key2] cstrs
                 -> string
                 -> string
                 -> eq key1
                 -> ord key1
                 -> show key1
                 -> read key1
                 -> sql_injectable key1
                 -> eq key2
                 -> ord key2
                 -> show key2
                 -> read key2
                 -> sql_injectable key2
                 -> t ([tname1 = key1, tname2 = key2] ++ full)
                      ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                        tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)
                 -> t ([tname1 = key1, tname2 = key2] ++ full)
                      ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, manyToMany11 impl11 key1 key2, manyToMany12 impl12 key1 key2, manyToMany13 impl13 key1 key2),
                        tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, manyToMany21 impl21 key1 key2, manyToMany22 impl22 key1 key2, manyToMany23 impl23 key1 key2)] ++ old)

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type * Type)}
                 val t : t (map (fn p => p.1) tables)
                           (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5, p.6)) tables)
                 val fl : folder tables
             end) : sig
    val index : variant (map (fn _ => unit) M.tables) -> transaction page
    val create : variant (map (fn _ => unit) M.tables) -> transaction page
end
