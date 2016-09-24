type t1 (p :: (Type * {Type} * {{Unit}} * Type)) =
     {Title : string,
      Table : sql_table p.2 p.3,
      KeyIs : nm :: Name -> p.1 -> sql_exp [nm = p.2] [] [] bool,
      Show : show p.1,
      Config : transaction p.4,
      Render : p.4 -> $p.2 -> xtable}
                                         
type t (tables :: {(Type * {Type} * {{Unit}} * Type)}) =
    $(map t1 tables)

type base = unit

val none = {}

fun one [tname :: Name] [key :: Name] [keyT ::: Type] [rest ::: {Type}] [cstrs ::: {{Unit}}]
        [old ::: {(Type * {Type} * {{Unit}} * Type)}]
        [[key] ~ rest] [[tname] ~ old]
        (tab : sql_table ([key = keyT] ++ rest) cstrs) (title : string)
        (sh : show keyT) (_ : sql_injectable keyT) (old : t old) =
    {tname = {Title = title,
              Table = tab,
              KeyIs = fn [nm ::_] v => (WHERE {{nm}}.{key} = {[v]}),
              Show = sh,
              Config = return (),
              Render = fn () _ => <xml/>}} ++ old

fun two [tname :: Name] [key1 :: Name] [key2 :: Name] [keyT1 ::: Type] [keyT2 ::: Type]
    [rest ::: {Type}] [cstrs ::: {{Unit}}] [old ::: {(Type * {Type} * {{Unit}} * Type)}]
    [[key1] ~ [key2]] [[key1, key2] ~ rest] [[tname] ~ old]
    (tab: sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs) (title : string)
    (sh : show (keyT1 * keyT2)) (_ : sql_injectable keyT1) (_ : sql_injectable keyT2)
    (old : t old) =
    {tname = {Title = title,
              Table = tab,
              KeyIs = fn [nm ::_] (v1, v2) => (WHERE {{nm}}.{key1} = {[v1]}
                                                 AND {{nm}}.{key2} = {[v2]}),
              Show = sh,
              Config = return (),
              Render = fn () _ => <xml/>}} ++ old

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type)}
                 val t : t tables
                 val fl : folder tables
             end) = struct
    open M
    open Ui.Make(Theme)

    type tag = variant (map (fn _ => unit) tables)
    val eq_tag : eq tag = @Variant.eqU (@@Folder.mp [fn _ => ()] [_] fl)

    fun index (which : tag) =
        let
            fun titleOf v =
                @Variant.destrR [fn _ => unit] [t1]
                 (fn [p ::_] () r => r.Title)
                 fl v t
        in
            @@tabbedStatic [map (fn _ => ()) tables] (@Folder.mp fl)
              title
              (@@Variant.mp [map (fn _ => ()) tables] [_] (@Folder.mp fl)
                 (fn v => (titleOf v, v = which, url (index v))))
              <xml>Nothing here yet</xml>
        end

end
