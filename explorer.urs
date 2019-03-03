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
 * (6) Internal Explorer implementation type #2
 * (7) Internal Explorer implementation type #3
 *
 * The first argument is the final schema, prognosticated before we start adding concrete tables.
 * The second argument only includes the tables we have added already.
 * The two must match (taking the first position from each record) when we are ready to build a concrete Web component. *)

type base1
type base2
type base3
(* Default implementation types *)

val none : full ::: {Type} -> t full []

datatype index_style exp row =
         Default of exp
       | Custom of transaction (list row)

val one : full ::: {Type}
          -> tname :: Name -> key :: Name -> keyT ::: Type -> rest ::: {Type} -> cstrs ::: {{Unit}}
          -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
          -> [[key] ~ rest] => [[tname] ~ old] => sql_table ([key = keyT] ++ rest) cstrs -> string
          -> transaction xbody (* Extra content to include at top of index page *)
          -> index_style (sql_exp [Tab = [key = keyT] ++ rest] [] [] bool (* Filter condition for inclusion on index page *))
             (keyT * xbody)
          -> show keyT -> eq keyT -> ord keyT -> sql_injectable keyT -> $(map sql_injectable rest)
          -> folder rest -> folder old
          -> t full old
          -> t full ([tname = (keyT, [key = keyT] ++ rest, [], cstrs, base1, base2, base3)] ++ old)

val two : full ::: {Type}
          -> tname :: Name -> key1 :: Name -> key2 :: Name -> keyT1 ::: Type -> keyT2 ::: Type
          -> rest ::: {Type} -> cstrs ::: {{Unit}} -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
          -> [[key1] ~ [key2]] => [[key1, key2] ~ rest] => [[tname] ~ old]
          => sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs -> string
          -> transaction xbody (* Extra content to include at top of index page *)
          -> index_style (sql_exp [Tab = [key1 = keyT1, key2 = keyT2] ++ rest] [] [] bool)
             (keyT1 * keyT2 * xbody)
          -> show (keyT1 * keyT2) -> eq (keyT1 * keyT2) -> ord (keyT1 * keyT2)
          -> sql_injectable keyT1 -> sql_injectable keyT2
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

val hyperref : full ::: {Type}
               -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
               -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
               -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
               -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
               => string
               -> show colT
               -> read colT
               -> t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
               -> t full ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, text1 impl1, text2 impl2, text3 impl3)] ++ old)

val image : full ::: {Type}
            -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
            -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
            -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
            -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
            => string
            -> show colT
            -> read colT
            -> css_class
            -> t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
            -> t full ([tname = (key, [col = colT] ++ cols, [col = colT] ++ colsDone, cstrs, text1 impl1, text2 impl2, text3 impl3)] ++ old)

val textOpt : full ::: {Type}
              -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
              -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
              -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
              -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
              => string
              -> show colT
              -> read colT
              -> t full ([tname = (key, [col = option colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
              -> t full ([tname = (key, [col = option colT] ++ cols, [col = option colT] ++ colsDone, cstrs, text1 impl1, text2 impl2, text3 impl3)] ++ old)

con checkbox1 :: Type -> Type
con checkbox2 :: Type -> Type
con checkbox3 :: Type -> Type

val checkbox : full ::: {Type}
               -> tname :: Name -> key ::: Type -> col :: Name
               -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
               -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
               -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
               => string
               -> t full ([tname = (key, [col = bool] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
               -> t full ([tname = (key, [col = bool] ++ cols, [col = bool] ++ colsDone, cstrs, checkbox1 impl1, checkbox2 impl2, checkbox3 impl3)] ++ old)

con htmlbox1 :: Type -> Type
con htmlbox2 :: Type -> Type
con htmlbox3 :: Type -> Type

val htmlbox : full ::: {Type}
              -> tname :: Name -> key ::: Type -> col :: Name
              -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
              -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
              -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
              => string
              -> t full ([tname = (key, [col = string] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
              -> t full ([tname = (key, [col = string] ++ cols, [col = string] ++ colsDone, cstrs, htmlbox1 impl1, htmlbox2 impl2, htmlbox3 impl3)] ++ old)

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

con manyToMany11 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type
con manyToMany12 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type
con manyToMany13 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type
con manyToMany21 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type
con manyToMany22 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type
con manyToMany23 :: Type -> Type -> Type -> {(Type * Type * Type)} -> Type

val manyToMany : full ::: {Type}
                 -> tname1 :: Name -> key1 ::: Type -> col1 :: Name -> colR1 :: Name
                 -> cols1 ::: {Type} -> colsDone1 ::: {Type} -> cstrs1 ::: {{Unit}}
                 -> impl11 ::: Type -> impl12 ::: Type -> impl13 ::: Type
                 -> tname2 :: Name -> key2 ::: Type -> col2 :: Name -> colR2 :: Name
                 -> cols2 ::: {Type} -> colsDone2 ::: {Type} -> cstrs2 ::: {{Unit}}
                 -> impl21 ::: Type -> impl22 ::: Type -> impl23 ::: Type
                 -> cstrs ::: {{Unit}}
                 -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
                 -> others ::: {(Type * Type * Type)}
                 -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ old] => [[tname1, tname2] ~ full]
                 => [[col1] ~ cols1] => [[col2] ~ cols2] => [[col1] ~ [col2]] => [[colR1] ~ [colR2]]
                 => [others ~ [colR1, colR2]]
                 => sql_table ([colR1 = key1, colR2 = key2] ++ map fst3 others) cstrs
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
                 -> folder others
                 -> $(map Widget.t' others)
                 -> $(map sql_injectable (map fst3 others))
                 -> $(map (fn _ => string) others)
                 -> t ([tname1 = key1, tname2 = key2] ++ full)
                      ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                        tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)
                 -> t ([tname1 = key1, tname2 = key2] ++ full)
                      ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, manyToMany11 impl11 key1 key2 others, manyToMany12 impl12 key1 key2 others, manyToMany13 impl13 key1 key2 others),
                        tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, manyToMany21 impl21 key1 key2 others, manyToMany22 impl22 key1 key2 others, manyToMany23 impl23 key1 key2 others)] ++ old)

(* Now let's support one file attached to each row of the connecting table. *)
functor ManyToManyWithFile(M : sig
                               con full :: {Type}
                               con tname1 :: Name
                               con key1 :: Type
                               con col1 :: Name
                               con colR1 :: Name
                               con cols1 :: {Type}
                               con colsDone1 :: {Type}
                               con cstrs1 :: {{Unit}}
                               con impl11 :: Type
                               con impl12 :: Type
                               con impl13 :: Type
                               con tname2 :: Name
                               con key2 :: Type
                               con col2 :: Name
                               con colR2 :: Name
                               con cols2 :: {Type}
                               con colsDone2 :: {Type}
                               con cstrs2 :: {{Unit}}
                               con impl21 :: Type
                               con impl22 :: Type
                               con impl23 :: Type
                               con cstrs :: {{Unit}}
                               con old :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
                               con others :: {(Type * Type * Type)}
                               constraint [tname1] ~ [tname2]
                               constraint [tname1, tname2] ~ old
                               constraint [tname1, tname2] ~ full
                               constraint [col1] ~ cols1
                               constraint [col2] ~ cols2
                               constraint [col1] ~ [col2]
                               constraint [colR1] ~ [colR2]
                               constraint others ~ [colR1, colR2]
                               constraint [FileName, FileType, FileData] ~ [colR1, colR2]
                               constraint [FileName, FileType, FileData] ~ others
                               val rel : sql_table ([colR1 = key1, colR2 = key2, FileName = string, FileType = string, FileData = blob] ++ map fst3 others) cstrs
                               val lab1 : string
                               val lab2 : string
                               val eq_key1 : eq key1
                               val ord_key1 : ord key1
                               val show_key1 : show key1
                               val read_key1 : read key1
                               val inj_key1 : sql_injectable key1
                               val eq_key2 : eq key2
                               val ord_key2 : ord key2
                               val show_key2 : show key2
                               val read_key2 : read key2
                               val inj_key2 : sql_injectable key2
                               val fl : folder others
                               val ws : $(map Widget.t' others)
                               val injs : $(map sql_injectable (map fst3 others))
                               val labels : $(map (fn _ => string) others)

                               val authorize : key1 -> key2 -> transaction bool
                           end) : sig
    type manyToManyWithFile11
    type manyToManyWithFile12
    type manyToManyWithFile13
    type manyToManyWithFile21
    type manyToManyWithFile22
    type manyToManyWithFile23
    
    val make : t ([M.tname1 = M.key1, M.tname2 = M.key2] ++ M.full)
                 ([M.tname1 = (M.key1, [M.col1 = M.key1] ++ M.cols1, M.colsDone1, M.cstrs1, M.impl11, M.impl12, M.impl13),
                   M.tname2 = (M.key2, [M.col2 = M.key2] ++ M.cols2, M.colsDone2, M.cstrs2, M.impl21, M.impl22, M.impl23)] ++ M.old)
               -> t ([M.tname1 = M.key1, M.tname2 = M.key2] ++ M.full)
                    ([M.tname1 = (M.key1, [M.col1 = M.key1] ++ M.cols1, M.colsDone1, M.cstrs1, manyToManyWithFile11, manyToManyWithFile12, manyToManyWithFile13),
                      M.tname2 = (M.key2, [M.col2 = M.key2] ++ M.cols2, M.colsDone2, M.cstrs2, manyToManyWithFile21, manyToManyWithFile22, manyToManyWithFile23)] ++ M.old)
end

(* For this version, the ordering applies in listing all [tname2] entries for [tname1]. *)
val manyToManyOrdered : full ::: {Type}
                        -> tname1 :: Name -> key1 ::: Type -> col1 :: Name -> colR1 :: Name
                        -> cols1 ::: {Type} -> colsDone1 ::: {Type} -> cstrs1 ::: {{Unit}}
                        -> impl11 ::: Type -> impl12 ::: Type -> impl13 ::: Type
                        -> tname2 :: Name -> key2 ::: Type -> col2 :: Name -> colR2 :: Name
                        -> cols2 ::: {Type} -> colsDone2 ::: {Type} -> cstrs2 ::: {{Unit}}
                        -> impl21 ::: Type -> impl22 ::: Type -> impl23 ::: Type
                        -> cstrs ::: {{Unit}}
                        -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
                        -> others ::: {(Type * Type * Type)}
                        -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ old] => [[tname1, tname2] ~ full]
                        => [[col1] ~ cols1] => [[col2] ~ cols2] => [[col1] ~ [col2]] => [[col1, col2] ~ [SeqNum]]
                        => [[colR1] ~ [colR2]] => [[colR1, colR2] ~ [SeqNum]] => [others ~ [colR1, colR2, SeqNum]]
                        => sql_table ([colR1 = key1, colR2 = key2, SeqNum = int] ++ map fst3 others) cstrs
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
                        -> folder others
                        -> $(map Widget.t' others)
                        -> $(map (fn p => sql_injectable p.1) others)
                        -> $(map (fn _ => string) others)
                        -> t ([tname1 = key1, tname2 = key2] ++ full)
                             ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                               tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)
                        -> t ([tname1 = key1, tname2 = key2] ++ full)
                             ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, manyToMany11 impl11 key1 key2 others, manyToMany12 impl12 key1 key2 others, manyToMany13 impl13 key1 key2 others),
                               tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, manyToMany21 impl21 key1 key2 others, manyToMany22 impl22 key1 key2 others, manyToMany23 impl23 key1 key2 others)] ++ old)

con custom1 :: Type -> Type -> Type
con custom2 :: Type -> Type -> Type
con custom3 :: Type -> Type -> Type

(* Treat a column as a foreign key, where in read mode we display it with custom logic. *)
val custom : full ::: {Type}
              -> tname :: Name -> key ::: Type -> col :: Name -> colT ::: Type
              -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
              -> stash ::: Type
              -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
              -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
              => string
              -> show colT
              -> read colT
              -> (colT -> transaction (option stash))
              -> (stash -> xtable)
              -> (colT -> transaction unit) (* Run each time we update the database. *)
              -> t full ([tname = (key, [col = option colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
              -> t full ([tname = (key, [col = option colT] ++ cols, [col = option colT] ++ colsDone, cstrs, custom1 stash impl1, custom2 stash impl2, custom3 stash impl3)] ++ old)

datatype action tab key =
         Read of tab
       | Create of tab
       | Update of key
       | Delete of key

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type * Type)}
                 val t : t (map (fn p => p.1) tables)
                           (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5, p.6)) tables)
                 val fl : folder tables

                 val authorize : action (variant (map (fn _ => unit) tables)) (variant (map (fn p => p.1) tables)) -> transaction bool

                 (* Other tabs to include, beside those associated with these tables. *)
                 con preTabs :: {Unit} (* To appear *before* the tables *)
                 con postTabs :: {Unit}
                 con hiddenTabs :: {Unit} (* Available pages without navbar links *)
                 constraint preTabs ~ postTabs
                 constraint (preTabs ++ postTabs) ~ hiddenTabs
                 val preTabs : $(mapU (string (* page title *) * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) preTabs)
                 val preFl : folder preTabs
                 val postTabs : $(mapU (string * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) postTabs)
                 val postFl : folder postTabs
                 val hiddenTabs : $(mapU (string * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) hiddenTabs)
                 val hiddenFl : folder hiddenTabs
                 constraint (preTabs ++ postTabs ++ hiddenTabs) ~ tables
             end) : sig
    val index : variant (map (fn _ => unit) M.tables) -> transaction page
    val create : variant (map (fn _ => unit) M.tables) -> transaction page
    val page : variant (map (fn _ => unit) M.tables ++ mapU unit (M.preTabs ++ M.postTabs ++ M.hiddenTabs)) -> transaction page

    val tableNames : $(map (fn _ => string) M.tables)

    include Ui.S where type input = variant (map (fn p => p.1) M.tables)
end
