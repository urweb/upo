open Bootstrap3

type t1 (p :: (Type * {Type} * {Type} * {{Unit}} * Type * Type)) =
     {Title : string,
      Table : sql_table p.2 p.4,
      Insert : $p.2 -> transaction unit,
      List : transaction (list p.1),
      KeyIs : nm :: Name -> p.1 -> sql_exp [nm = p.2] [] [] bool,
      Show : show p.1,
      Config : transaction p.5,
      Render : p.5 -> $p.2 -> xtable,
      FreshWidgets : transaction p.6,
      RenderWidgets : p.5 -> p.6 -> xbody,
      ReadWidgets : p.6 -> signal $p.3}
                                         
type t (tables :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}) =
    $(map t1 tables)

type base1 = unit
type base2 = unit

val none = {}

fun one [tname :: Name] [key :: Name] [keyT ::: Type] [rest ::: {Type}] [cstrs ::: {{Unit}}]
        [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}]
        [[key] ~ rest] [[tname] ~ old]
        (tab : sql_table ([key = keyT] ++ rest) cstrs) (title : string)
        (sh : show keyT) (inj : sql_injectable keyT) (injs : $(map sql_injectable rest))
        (fl : folder ([key = keyT] ++ rest)) (old : t old) =
    {tname = {Title = title,
              Table = tab,
              Insert = @@Sql.easy_insert [[key = _] ++ rest] [_] ({key = inj} ++ injs) fl tab,
              List = List.mapQuery (SELECT tab.{key}
                                    FROM tab
                                    ORDER BY tab.{key})
                                   (fn {Tab = r} => r.key),
              KeyIs = fn [nm ::_] v => (WHERE {{nm}}.{key} = {[v]}),
              Show = sh,
              Config = return (),
              Render = fn () _ => <xml></xml>,
              FreshWidgets = return (),
              RenderWidgets = fn () () => <xml></xml>,
              ReadWidgets = fn () => return ()}} ++ old

fun two [tname :: Name] [key1 :: Name] [key2 :: Name] [keyT1 ::: Type] [keyT2 ::: Type]
    [rest ::: {Type}] [cstrs ::: {{Unit}}] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}]
    [[key1] ~ [key2]] [[key1, key2] ~ rest] [[tname] ~ old]
    (tab: sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs) (title : string)
    (sh : show (keyT1 * keyT2)) (inj1 : sql_injectable keyT1) (inj2 : sql_injectable keyT2)
    (injs : $(map sql_injectable rest)) (fl : folder ([key1 = keyT1, key2 = keyT2] ++ rest))
    (old : t old) =
    {tname = {Title = title,
              Table = tab,
              Insert = @@Sql.easy_insert [[key1 = _, key2 = _] ++ rest] [_] ({key1 = inj1, key2 = inj2} ++ injs) fl tab,
              List = List.mapQuery (SELECT tab.{key1}, tab.{key2}
                                    FROM tab
                                    ORDER BY tab.{key1}, tab.{key2})
                                   (fn {Tab = r} => (r.key1, r.key2)),
              KeyIs = fn [nm ::_] (v1, v2) => (WHERE {{nm}}.{key1} = {[v1]}
                                                 AND {{nm}}.{key2} = {[v2]}),
              Show = sh,
              Config = return (),
              Render = fn () _ => <xml></xml>,
              FreshWidgets = return (),
              RenderWidgets = fn () () => <xml></xml>,
              ReadWidgets = fn () => return ()}} ++ old

type text1 t = t
type text2 t = source string * t

fun text [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
    [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
    [impl1 ::: Type] [impl2 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type)}]
    [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
    (lab : string) (_ : show colT) (_ : read colT)
    (old : t ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2)] ++ old)) =
    old -- tname
        ++ {tname = old.tname
                        -- #Render -- #FreshWidgets -- #RenderWidgets -- #ReadWidgets
                        ++ {Render = fn cfg r =>
                                        <xml>
                                          {old.tname.Render cfg r}
                                          <tr>
                                            <th>{[lab]}</th>
                                            <td>{[r.col]}</td>
                                          </tr>
                                        </xml>,
                            FreshWidgets =
                               s <- source "";
                               ws <- old.tname.FreshWidgets;
                               return (s, ws),
                            RenderWidgets = fn cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             wsv <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv)}}

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type)}
                 val t : t (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5)) tables)
                 val fl : folder tables
             end) = struct
    open M
    open Ui.Make(Theme)

    type tag = variant (map (fn _ => unit) tables)
    val eq_tag : eq tag = @Variant.eqU (@@Folder.mp [fn _ => ()] [_] fl)

    con dupF (p :: (Type * {Type} * {{Unit}} * Type * Type)) = (p.1, p.2, p.2, p.3, p.4, p.5)
    con dup = map dupF tables

    fun titleOf v =
        @@Variant.destrR [fn _ => unit] [t1] [string]
          (fn [p ::_] () r => r.Title)
          [dup] (@Folder.mp fl) v t

    fun tabbed f which =
        @@tabbedStatic [map (fn _ => ()) tables] (@Folder.mp fl)
          title
          (@@Variant.mp [map (fn _ => ()) tables] [_] (@Folder.mp fl)
             (fn v => (titleOf v, @eq eq_tag v which, url (f v))))

    fun index (which : tag) =
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type) -> Type) -> tf p -> variant (map tf tables)) () r =>
              rows <- r.List;
              return <xml>
                <table class="bs3-table table-striped">
                  {List.mapX (fn k => <xml><tr><td>{[@show r.Show k]}</td></tr></xml>) rows}
                </table>

                <a class="btn btn-primary" link={create which}>New Entry</a>
              </xml>)
          fl which t;
        tabbed index which bod

    and create (which : tag) =
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type) -> Type) -> tf p -> variant (map tf tables)) () r =>
              cfg <- r.Config;
              ws <- r.FreshWidgets;
              return <xml>
                <h1>Create {[r.Title]}</h1>

                {r.RenderWidgets cfg ws}

                <button value="Create" class="btn btn-primary"
                        onclick={fn _ =>
                                    vs <- current (r.ReadWidgets ws);
                                    rpc (doCreate (maker [fn p => $p.2] vs));
                                    redirect (url (index which))}/>
              </xml>)
          fl which t;
        tabbed create which bod

    and doCreate (which : variant (map (fn p => $p.2) dup)) =
        @@Variant.destrR [fn p => $p.2] [t1] [transaction unit]
          (fn [p ::_] (vs : $p.2) r =>
              r.Insert vs)
          [dup] (@Folder.mp fl) which t

end
