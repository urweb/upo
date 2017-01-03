open Bootstrap3

type t1 (full :: {Type}) (p :: (Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)) =
     {Title : string,
      Table : sql_table p.2 p.4,
      Insert : $p.2 -> transaction unit,
      Update : $p.2 -> transaction unit,
      List : sql_exp [Tab = p.2] [] [] bool -> transaction (list p.1),
      KeyIs : nm :: Name -> p.1 -> sql_exp [nm = p.2] [] [] bool,
      Show : show p.1,
      Config : transaction p.5,
      Auxiliary : p.1 -> transaction p.7,
      Render : (variant full -> string -> xbody) -> p.7 -> $p.2 -> xtable,
      FreshWidgets : transaction p.6,
      WidgetsFrom : $p.2 -> transaction p.6,
      RenderWidgets : p.5 -> p.6 -> xbody,
      ReadWidgets : p.6 -> signal $p.3}

type t (full :: {Type}) (tables :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}) =
    $(map (t1 full) tables)

type base1 = unit
type base2 = unit
type base3 = unit

val none [full ::: {Type}] = {}

fun one [full ::: {Type}]
        [tname :: Name] [key :: Name] [keyT ::: Type] [rest ::: {Type}] [cstrs ::: {{Unit}}]
        [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
        [[key] ~ rest] [[tname] ~ old]
        (tab : sql_table ([key = keyT] ++ rest) cstrs) (title : string)
        (sh : show keyT) (inj : sql_injectable keyT) (injs : $(map sql_injectable rest))
        (fl : folder rest) (ofl : folder old) (old : t full old) =
    {tname = {Title = title,
              Table = tab,
              Insert = @@Sql.easy_insert [[key = _] ++ rest] [_] ({key = inj} ++ injs) (@Folder.cons [_] [_] ! fl) tab,
              Update = fn r => @@Sql.easy_update [[key = _]] [rest] [_] ! {key = inj} injs _ fl tab (r --- rest) (r -- key),
              List = fn wher => List.mapQuery (SELECT tab.{key}
                                               FROM tab
                                               WHERE {wher}
                                               ORDER BY tab.{key})
                                              (fn {Tab = r} => r.key),
              KeyIs = fn [nm ::_] v => (WHERE {{nm}}.{key} = {[v]}),
              Show = sh,
              Config = return (),
              Auxiliary = fn _ => return (),
              Render = fn _ _ _ => <xml></xml>,
              FreshWidgets = return (),
              WidgetsFrom = fn _ => return (),
              RenderWidgets = fn () () => <xml></xml>,
              ReadWidgets = fn () => return ()}} ++ old

fun two [full ::: {Type}]
        [tname :: Name] [key1 :: Name] [key2 :: Name] [keyT1 ::: Type] [keyT2 ::: Type]
        [rest ::: {Type}] [cstrs ::: {{Unit}}] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
        [[key1] ~ [key2]] [[key1, key2] ~ rest] [[tname] ~ old]
        (tab: sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs) (title : string)
        (sh : show (keyT1 * keyT2)) (inj1 : sql_injectable keyT1) (inj2 : sql_injectable keyT2)
        (injs : $(map sql_injectable rest)) (fl : folder rest) (ofl : folder old)
        (old : t full old) =
    {tname = {Title = title,
              Table = tab,
              Insert = @@Sql.easy_insert [[key1 = _, key2 = _] ++ rest] [_] ({key1 = inj1, key2 = inj2} ++ injs) (@Folder.cons [_] [_] ! (@Folder.cons [_] [_] ! fl)) tab,
              Update = fn r => @@Sql.easy_update [[key1 = _, key2 = _]] [rest] [_] ! {key1 = inj1, key2 = inj2} injs _ fl tab (r --- rest) (r -- key1 -- key2),
              List = fn wher => List.mapQuery (SELECT tab.{key1}, tab.{key2}
                                               FROM tab
                                               WHERE {wher}
                                               ORDER BY tab.{key1}, tab.{key2})
                                   (fn {Tab = r} => (r.key1, r.key2)),
              KeyIs = fn [nm ::_] (v1, v2) => (WHERE {{nm}}.{key1} = {[v1]}
                                                 AND {{nm}}.{key2} = {[v2]}),
              Show = sh,
              Config = return (),
              Auxiliary = fn _ => return (),
              Render = fn _ _ _ => <xml></xml>,
              FreshWidgets = return (),
              WidgetsFrom = fn _ => return (),
              RenderWidgets = fn () () => <xml></xml>,
              ReadWidgets = fn () => return ()}} ++ old

type text1 t = t
type text2 t = source string * t
type text3 t = t

fun text [full ::: {Type}]
         [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
         [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
         [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
         [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
         (lab : string) (_ : show colT) (_ : read colT)
         (old : t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
    old -- tname
        ++ {tname = old.tname
                        -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                        ++ {Render = fn entry aux r =>
                                        <xml>
                                          {old.tname.Render entry aux r}
                                          <tr>
                                            <th>{[lab]}</th>
                                            <td>{[r.col]}</td>
                                          </tr>
                                        </xml>,
                            FreshWidgets =
                               s <- source "";
                               ws <- old.tname.FreshWidgets;
                               return (s, ws),
                            WidgetsFrom = fn r =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom r;
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

type foreign1 t key colT = list colT * t
type foreign2 t key colT = source string * t
type foreign3 t key colT = list key * t

fun foreign [full ::: {Type}]
            [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
            [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
            [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type]
            [ftname :: Name] [fcol :: Name]
            [fcols ::: {Type}] [fcolsDone ::: {Type}] [fcstrs ::: {{Unit}}]
            [fimpl1 ::: Type] [fimpl2 ::: Type] [fimpl3 ::: Type]
            [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
            [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
            [[fcol] ~ fcols] [[ftname] ~ old]
            [[tname] ~ [ftname]] [[tname, ftname] ~ full]
            (lab : string) (clab : string) (_ : show colT) (_ : read colT) (_ : sql_injectable colT)
            (old : t ([tname = key, ftname = colT] ++ full)
                     ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3),
                       ftname = (colT, [fcol = colT] ++ fcols, fcolsDone, fcstrs, fimpl1, fimpl2, fimpl3)] ++ old)) =
    old -- tname -- ftname
        ++ {tname = old.tname
                        -- #Config -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                        ++ {Config =
                            let
                                val tab = old.ftname.Table
                            in
                                keys <- List.mapQuery (SELECT DISTINCT tab.{fcol}
                                                       FROM tab
                                                       ORDER BY tab.{fcol})
                                                      (fn r => r.Tab.fcol);
                                cfg <- old.tname.Config;
                                return (keys, cfg)
                            end,
                            Render = fn entry aux r =>
                                        <xml>
                                          {old.tname.Render entry aux r}
                                          <tr>
                                            <th>{[lab]}</th>
                                            <td>{entry (make [ftname] r.col) (show r.col)}</td>
                                          </tr>
                                        </xml>,
                            FreshWidgets =
                               s <- source "";
                               ws <- old.tname.FreshWidgets;
                               return (s, ws),
                            WidgetsFrom = fn r =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom r;
                               return (s, ws),
                            RenderWidgets = fn (cfg1, cfg2) (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets cfg2 ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <cselect class="form-control" source={s}>
                                                     {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                   </cselect>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             wsv <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv)},
           ftname = old.ftname
                        -- #Auxiliary -- #Render
                        ++ {Auxiliary = fn fkey =>
                                           let
                                               val tab = old.tname.Table
                                           in
                                               keys <- old.tname.List (WHERE tab.{col} = {[fkey]});
                                               aux <- old.ftname.Auxiliary fkey;
                                               return (keys, aux)
                                           end,
                           Render = fn entry (children, aux) r =>
                                       let
                                           val _ : show key = old.tname.Show
                                       in
                                           <xml>
                                             {old.ftname.Render entry aux r}
                                             <tr>
                                               <th>{[clab]}</th>
                                               <td>{List.mapX (fn key => <xml>{entry (make [tname] key) (show key)}<br/></xml>) children}</td>
                                             </tr>
                                           </xml>
                                       end}}

functor Make(M : sig
                 structure Theme : Ui.THEME

                 val title : string
                 con tables :: {(Type * {Type} * {{Unit}} * Type * Type * Type)}
                 val t : t (map (fn p => p.1) tables)
                           (map (fn p => (p.1, p.2, p.2, p.3, p.4, p.5, p.6)) tables)
                 val fl : folder tables
             end) = struct
    open M
    open Ui.Make(Theme)

    type tag = variant (map (fn _ => unit) tables)
    val eq_tag : eq tag = @Variant.eqU (@@Folder.mp [fn _ => ()] [_] fl)

    con dupF (p :: (Type * {Type} * {{Unit}} * Type * Type * Type)) = (p.1, p.2, p.2, p.3, p.4, p.5, p.6)
    con dup = map dupF tables

    con tables' = map (fn p => p.1) tables

    fun titleOf v =
        @@Variant.destrR [fn _ => unit] [t1 tables'] [string]
          (fn [p ::_] () r => r.Title)
          [dup] (@Folder.mp fl) v t

    fun tabbed f which =
        @@tabbedStatic [map (fn _ => ()) tables] (@Folder.mp fl)
          title
          (@@Variant.mp [map (fn _ => ()) tables] [_] (@Folder.mp fl)
             (fn v => (titleOf v, @eq eq_tag v which, url (f v))))

    datatype editingState row widgets =
             NotEditing of row
           | Editing of row * widgets

    fun index (which : tag) =
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) () r =>
              rows <- r.List (WHERE TRUE);
              return <xml>
                <table class="bs3-table table-striped">
                  {List.mapX (fn k => <xml><tr><td><a link={entry (maker [fn p => p.1] k)}>{[@show r.Show k]}</a></td></tr></xml>) rows}
                </table>

                <a class="btn btn-primary" link={create which}>New Entry</a>
              </xml>)
          fl which t;
        tabbed index which (fn _ => bod)

    and create (which : tag) =
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) () r =>
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
        tabbed create which (fn _ => bod)

    and doCreate (which : variant (map (fn p => $p.2) dup)) =
        @@Variant.destrR [fn p => $p.2] [t1 tables'] [transaction unit]
          (fn [p ::_] (vs : $p.2) r =>
              r.Insert vs)
          [dup] (@Folder.mp fl) which t

    and entry (which : variant (map (fn p => p.1) tables)) =
        (ctx : source (option Ui.context)) <- source None;
        bod <- @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) (k : p.1) (r : t1 tables' (dupF p)) =>
              let
                  val tab = r.Table
              in
                  cfg <- r.Config;
                  aux <- r.Auxiliary k;
                  row <- oneRow1 (SELECT *
                                  FROM tab
                                  WHERE {r.KeyIs [#Tab] k});
                  est <- source (NotEditing row);
                  return <xml>
                    <dyn signal={esta <- signal est;
                                 case esta of
                                     NotEditing row =>
                                     ctx <- signal ctx;
                                     (case ctx of
                                         None => return <xml></xml>
                                       | Some ctx => return <xml>
                                         <p>
                                           <button class="btn btn-primary"
                                                   onclick={fn _ => ws <- r.WidgetsFrom row; set est (Editing (row, ws))}>Edit</button>
                                           {Ui.modalButton ctx (CLASS "btn") <xml>Delete</xml>
                                                           (return (Ui.modal (rpc (delete which); redirect (url (index (maker [fn _ => unit] ()))))
                                                                             <xml>Are you sure you want to delete this entry?</xml>
                                                                             <xml></xml>
                                                                             <xml>Yes, delete it.</xml>))}
                                           </p>

                                         <table class="bs3-table table-striped">
                                           {r.Render (fn key text => <xml><a link={entry key}>{[text]}</a></xml>) aux row}
                                         </table>
                                      </xml>)
                                   | Editing (row, ws) => return <xml>
                                     <p>
                                       <button class="btn btn-primary"
                                               onclick={fn _ =>
                                                           debug "Ahoy";
                                                           row' <- current (r.ReadWidgets ws);
                                                           debug "Ahi";
                                                           rpc (save (maker [fn p => $p.2] row'));
                                                           set est (NotEditing row')}>Save</button>
                                       <button class="btn"
                                               onclick={fn _ => set est (NotEditing row)}>Cancel</button>
                                     </p>

                                     {r.RenderWidgets cfg ws}
                                   </xml>}/>
                  </xml>
              end)
          fl which t;

        tabbed index (@Variant.erase (@Folder.mp fl) which) (fn ctxv => <xml>
          <active code={set ctx (Some ctxv); return <xml></xml>}/>
          {bod}
        </xml>)

    and save (which : variant (map (fn p => $p.2) tables)) =
        @@Variant.destrR [fn p => $p.2] [fn p => t1 tables' (dupF p)] [transaction unit]
          (fn [p ::_] (vs : $p.2) r =>
              r.Update vs)
          [tables] fl which t

    and delete (which : variant (map (fn p => p.1) tables)) =
        @@Variant.destrR [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction unit]
          (fn [p ::_] (k : p.1) (r : t1 tables' (dupF p)) =>
              let
                  val tab = r.Table
              in
                  dml (DELETE FROM tab
                       WHERE {r.KeyIs [#T] k})
              end)
          [tables] fl which t

end
