open Bootstrap4

type t1 (full :: {Type}) (p :: (Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)) =
     {Title : string,
      Extra : transaction xbody,
      Table : sql_table p.2 p.4,
      Insert : $p.2 -> p.7 -> transaction unit,
      Update : p.1 -> $p.2 -> p.7 -> transaction unit,
      Delete : p.1 -> transaction unit,
      List : sql_exp [Tab = p.2] [] [] bool -> transaction (list p.1),
      KeyIs : nm :: Name -> p.1 -> sql_exp [nm = p.2] [] [] bool,
      Show : show p.1,
      Eq : eq p.1,
      Ord : ord p.1,
      Config : transaction p.5,
      Auxiliary : p.1 -> $p.2 -> transaction p.7,
      Render : (variant full -> string -> xbody) -> p.7 -> $p.2 -> xtable,
      FreshWidgets : p.5 -> transaction p.6,
      WidgetsFrom : p.5 -> $p.2 -> p.7 -> transaction p.6,
      RenderWidgets : option p.1 -> p.5 -> p.6 -> xbody,
      ReadWidgets : p.6 -> signal ($p.3 * p.7 * option string (* Error message, if something is amiss *)),
      KeyOf : $p.2 -> p.1,
      ForIndex : transaction (list (p.1 * xbody))}

type t (full :: {Type}) (tables :: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}) =
    $(map (t1 full) tables)

type base1 = unit
type base2 = unit
type base3 = unit

val none [full ::: {Type}] = {}

datatype index_style exp row =
         Default of exp
       | Custom of transaction (list row)

fun one [full ::: {Type}]
        [tname :: Name] [key :: Name] [keyT ::: Type] [rest ::: {Type}] [cstrs ::: {{Unit}}]
        [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
        [[key] ~ rest] [[tname] ~ old]
        (tab : sql_table ([key = keyT] ++ rest) cstrs) (title : string) (extra : transaction xbody)
        (isty : index_style (sql_exp [Tab = [key = keyT] ++ rest] [] [] bool) (keyT * xbody))
        (sh : show keyT) (eq : eq keyT) (ord : ord keyT) (inj : sql_injectable keyT) (injs : $(map sql_injectable rest))
        (fl : folder rest) (ofl : folder old) (old : t full old) =
    {tname = {Title = title,
              Extra = extra,
              Table = tab,
              Insert = fn r () => @@Sql.easy_insert [[key = _] ++ rest] [_] ({key = inj} ++ injs) (@Folder.cons [_] [_] ! fl) tab r,
              Update = fn k r () => @@Sql.easy_update' [[key = _]] [rest] [_] ! {key = inj} injs _ fl tab {key = k} r (WHERE TRUE),
              Delete = fn _ => return (),
              List = fn wher => List.mapQuery (SELECT tab.{key}
                                               FROM tab
                                               WHERE {wher}
                                               ORDER BY tab.{key})
                                              (fn {Tab = r} => r.key),
              KeyIs = fn [nm ::_] v => (WHERE {{nm}}.{key} = {[v]}),
              Show = sh,
              Eq = eq,
              Ord = ord,
              Config = return (),
              Auxiliary = fn _ _ => return (),
              Render = fn _ _ _ => <xml></xml>,
              FreshWidgets = fn () => return (),
              WidgetsFrom = fn () _ _ => return (),
              RenderWidgets = fn _ () () => <xml></xml>,
              ReadWidgets = fn () => return ((), (), None),
              KeyOf = fn r => r.key,
              ForIndex = case isty of
                             Default fltr => List.mapQuery (SELECT tab.{key}
                                                            FROM tab
                                                            WHERE {fltr}
                                                            ORDER BY tab.{key})
                                                           (fn {Tab = r} => (r.key, txt r.key))
                           | Custom xa => xa}} ++ old

fun two [full ::: {Type}]
        [tname :: Name] [key1 :: Name] [key2 :: Name] [keyT1 ::: Type] [keyT2 ::: Type]
        [rest ::: {Type}] [cstrs ::: {{Unit}}] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
        [[key1] ~ [key2]] [[key1, key2] ~ rest] [[tname] ~ old]
        (tab: sql_table ([key1 = keyT1, key2 = keyT2] ++ rest) cstrs) (title : string) (extra : transaction xbody)
        (isty : index_style (sql_exp [Tab = [key1 = keyT1, key2 = keyT2] ++ rest] [] [] bool) (keyT1 * keyT2 * xbody))
        (sh : show (keyT1 * keyT2)) (eq : eq (keyT1 * keyT2)) (ord : ord (keyT1 * keyT2)) (inj1 : sql_injectable keyT1) (inj2 : sql_injectable keyT2)
        (injs : $(map sql_injectable rest)) (fl : folder rest) (ofl : folder old)
        (old : t full old) =
    {tname = {Title = title,
              Extra = extra,
              Table = tab,
              Insert = fn r () => @@Sql.easy_insert [[key1 = _, key2 = _] ++ rest] [_] ({key1 = inj1, key2 = inj2} ++ injs) (@Folder.cons [_] [_] ! (@Folder.cons [_] [_] ! fl)) tab r,
              Update = fn (k1, k2) r () => @@Sql.easy_update' [[key1 = _, key2 = _]] [rest] [_] ! {key1 = inj1, key2 = inj2} injs _ fl tab {key1 = k1, key2 = k2} r (WHERE TRUE),
              Delete = fn _ => return (),
              List = fn wher => List.mapQuery (SELECT tab.{key1}, tab.{key2}
                                               FROM tab
                                               WHERE {wher}
                                               ORDER BY tab.{key1}, tab.{key2})
                                   (fn {Tab = r} => (r.key1, r.key2)),
              KeyIs = fn [nm ::_] (v1, v2) => (WHERE {{nm}}.{key1} = {[v1]}
                                                 AND {{nm}}.{key2} = {[v2]}),
              Show = sh,
              Eq = eq,
              Ord = ord,
              Config = return (),
              Auxiliary = fn _ _ => return (),
              Render = fn _ _ _ => <xml></xml>,
              FreshWidgets = fn () => return (),
              WidgetsFrom = fn () _ _ => return (),
              RenderWidgets = fn _ () () => <xml></xml>,
              ReadWidgets = fn () => return ((), (), None),
              KeyOf = fn r => (r.key1, r.key2),
              ForIndex = case isty of
                             Default fltr => List.mapQuery (SELECT tab.{key1}, tab.{key2}
                                                            FROM tab
                                                            WHERE {fltr}
                                                            ORDER BY tab.{key1}, tab.{key2})
                                                           (fn {Tab = r} => ((r.key1, r.key2), txt (r.key1, r.key2)))
                           | Custom xa =>
                             ls <- xa;
                             return (List.mp (fn (k1, k2, b) => ((k1, k2), b)) ls)}} ++ old

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
                            FreshWidgets = fn cfg =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r aux =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv, aux, err)}}

fun hyperref [full ::: {Type}]
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
                                          {case checkUrl (show r.col) of
                                               None => <xml></xml>
                                             | Some url => <xml><tr>
                                               <th>{[lab]}</th>
                                               <td><a href={url}><tt>{[r.col]}</tt></a></td>
                                             </tr></xml>}
                                        </xml>,
                            FreshWidgets = fn cfg =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r aux =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv, aux, err)}}

fun image [full ::: {Type}]
          [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
          [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
          [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
          [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
          (lab : string) (_ : show colT) (_ : read colT) (cls : css_class)
          (old : t full ([tname = (key, [col = colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
    old -- tname
        ++ {tname = old.tname
                        -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                        ++ {Render = fn entry aux r =>
                                        <xml>
                                          {old.tname.Render entry aux r}
                                          {case checkUrl (show r.col) of
                                               None => <xml></xml>
                                             | Some url => <xml><tr>
                                               <th>{[lab]}</th>
                                               <td><img src={url} class={cls}/></td>
                                             </tr></xml>}
                                        </xml>,
                            FreshWidgets = fn cfg =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r aux =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv, aux, err)}}

fun textOpt [full ::: {Type}]
            [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
            [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
            [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
            [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
            (lab : string) (_ : show colT) (_ : read colT)
            (old : t full ([tname = (key, [col = option colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
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
                            FreshWidgets = fn cfg =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r aux =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = case v of
                                                                "" => None
                                                              | _ => Some (readError v)} ++ wsv, aux, err)}}

type checkbox1 t = t
type checkbox2 t = source bool * t
type checkbox3 t = t

fun checkbox [full ::: {Type}]
         [tname :: Name] [key ::: Type] [col :: Name]
         [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
         [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
         [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
         (lab : string)
         (old : t full ([tname = (key, [col = bool] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
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
                            FreshWidgets = fn cfg =>
                               s <- source False;
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r aux =>
                               s <- source r.col;
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ccheckbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = v} ++ wsv, aux, err)}}

type htmlbox1 t = t
type htmlbox2 t = Widget.htmlbox * t
type htmlbox3 t = t

fun htmlbox [full ::: {Type}]
            [tname :: Name] [key ::: Type] [col :: Name]
            [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
            [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
            [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
            (lab : string)
            (old : t full ([tname = (key, [col = string] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
       old -- tname
           ++ {tname = old.tname
                           -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                           ++ {Render = fn entry aux r =>
                                           <xml>
                                             {old.tname.Render entry aux r}
                                             <tr>
                                               <th>{[lab]}</th>
                                               <td>{Widget.html r.col}</td>
                                             </tr>
                                           </xml>,
                               FreshWidgets = fn cfg =>
                                  s <- @Widget.create Widget.htmlbox ();
                                  ws <- old.tname.FreshWidgets cfg;
                                  return (s, ws),
                               WidgetsFrom = fn cfg r aux =>
                                  s <- @Widget.initialize Widget.htmlbox () r.col;
                                  ws <- old.tname.WidgetsFrom cfg r aux;
                                  return (s, ws),
                               RenderWidgets = fn k cfg (s, ws) =>
                                                  <xml>
                                                    {old.tname.RenderWidgets k cfg ws}
                                                    <div class="form-group">
                                                      <label class="control-label">{[lab]}</label>
                                                      {@Widget.asWidget Widget.htmlbox s None}
                                                    </div>
                                                  </xml>,
                               ReadWidgets = fn (s, ws) =>
                                                v <- @Widget.value Widget.htmlbox s;
                                                (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                                return ({col = v} ++ wsv, aux, err)}}

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
                            FreshWidgets = fn (_, cfg) =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn (_, cfg) r aux =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux;
                               return (s, ws),
                            RenderWidgets = fn k (cfg1, cfg2) (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg2 ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <cselect class="form-control" source={s}>
                                                     {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                   </cselect>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = readError v} ++ wsv, aux, err)},
           ftname = old.ftname
                        -- #Insert -- #Update -- #Auxiliary -- #Render -- #ReadWidgets -- #WidgetsFrom
                        ++ {Insert = fn r (_, aux) => old.ftname.Insert r aux,
                            Update = fn k r (_, aux) => old.ftname.Update k r aux,
                            Auxiliary = fn fkey row =>
                                           let
                                               val tab = old.tname.Table
                                           in
                                               keys <- old.tname.List (WHERE tab.{col} = {[fkey]});
                                               aux <- old.ftname.Auxiliary fkey row;
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
                                       end,
                            ReadWidgets = fn w =>
                                             (v, aux, err) <- old.ftname.ReadWidgets w;
                                             return (v, ([], aux), err),
                            WidgetsFrom = fn cfg r (_, aux) => old.ftname.WidgetsFrom cfg r aux}}

type manyToMany11 t key1 key2 others = $(map thd3 others) * list key2 * t
type manyToMany12 t key1 key2 others = source string * $(map snd3 others) * source (list (key2 * source ($(map fst3 others)) * source (option ($(map snd3 others))))) * source bool (* dropdown changed since last button push? *) * t
type manyToMany13 t key1 key2 others = list (key2 * $(map fst3 others)) * t
type manyToMany21 t key1 key2 others = $(map thd3 others) * list key1 * t
type manyToMany22 t key1 key2 others = source string * $(map snd3 others) * source (list (key1 * source ($(map fst3 others)) * source (option ($(map snd3 others))))) * source bool * t
type manyToMany23 t key1 key2 others = list (key1 * $(map fst3 others)) * t

fun manyToMany [full ::: {Type}] [tname1 :: Name] [key1 ::: Type] [col1 :: Name] [colR1 :: Name]
               [cols1 ::: {Type}] [colsDone1 ::: {Type}] [cstrs1 ::: {{Unit}}]
               [impl11 ::: Type] [impl12 ::: Type] [impl13 ::: Type]
               [tname2 :: Name] [key2 ::: Type] [col2 :: Name] [colR2 :: Name]
               [cols2 ::: {Type}] [colsDone2 ::: {Type}] [cstrs2 ::: {{Unit}}]
               [impl21 ::: Type] [impl22 ::: Type] [impl23 ::: Type]
               [cstrs ::: {{Unit}}]
               [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
               [others ::: {(Type * Type * Type)}]
               [[tname1] ~ [tname2]] [[tname1, tname2] ~ old] [[tname1, tname2] ~ full]
               [[col1] ~ cols1] [[col2] ~ cols2] [[col1] ~ [col2]] [[colR1] ~ [colR2]]
               [others ~ [colR1, colR2]]
               (rel : sql_table ([colR1 = key1, colR2 = key2] ++ map fst3 others) cstrs)
               (lab1 : string) (lab2 : string)
               (_ : eq key1) (_ : ord key1) (_ : show key1) (_ : read key1) (_ : sql_injectable key1)
               (_ : eq key2) (_ : ord key2) (_ : show key2) (_ : read key2) (_ : sql_injectable key2)
               (fl : folder others) (ws : $(map Widget.t' others)) (injs : $(map (fn p => sql_injectable p.1) others)) (labels : $(map (fn _ => string) others))
               (old : t ([tname1 = key1, tname2 = key2] ++ full)
                        ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                          tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)) =
    old -- tname1 -- tname2
        ++ {tname1 = old.tname1
                         -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                         ++ {Insert =
                             fn r (k2s, aux) =>
                                old.tname1.Insert r aux;
                                List.app (fn (k2, others) =>
                                             @Sql.easy_insert ({colR1 = _, colR2 = _} ++ injs)
                                              (@Folder.cons [colR1] [_] !
                                                (@Folder.cons [colR2] [_] !
                                                  (@Folder.mp fl)))
                                              rel ({colR1 = r.col1, colR2 = k2} ++ others)) k2s,
                             Update =
                             fn k r (k2s, aux) =>
                                old.tname1.Update k r aux;
                                dml (DELETE FROM rel
                                     WHERE t.{colR1} = {[r.col1]});
                                List.app (fn (k2, others) =>
                                             @Sql.easy_insert ({colR1 = _, colR2 = _} ++ injs)
                                              (@Folder.cons [colR1] [_] !
                                                (@Folder.cons [colR2] [_] !
                                                  (@Folder.mp fl)))
                                              rel ({colR1 = r.col1, colR2 = k2} ++ others)) k2s,
                             Delete = fn k1 => dml (DELETE FROM rel WHERE t.{colR1} = {[k1]}),
                             Config =
                             let
                                 val tab = old.tname2.Table
                             in
                                 keys <- List.mapQuery (SELECT DISTINCT tab.{col2}
                                                        FROM tab
                                                        ORDER BY tab.{col2})
                                                       (fn r => r.Tab.col2);
                                 wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                          (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                          fl ws;
                                 cfg <- old.tname1.Config;
                                 return (wcfg, keys, cfg)
                             end,
                             Auxiliary = fn k1 row =>
                                 keys <- List.mapQuery (SELECT rel.{colR2}, rel.{{map fst3 others}}
                                                        FROM rel
                                                        WHERE rel.{colR1} = {[k1]}
                                                        ORDER BY rel.{colR2})
                                                       (fn r => (r.Rel.colR2, r.Rel -- colR2));
                                 aux <- old.tname1.Auxiliary k1 row;
                                 return (keys, aux),
                             Render = fn entry (k2s, aux) r =>
                                         <xml>
                                           {old.tname1.Render entry aux r}
                                           <tr>
                                             <th>{[lab1]}</th>
                                             <td>{List.mapX (fn (k2, others) => <xml>{entry (make [tname2] k2) (show k2)}
                                               {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                 (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                     <xml> - {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                               <br/></xml>) k2s}</td>
                                           </tr>
                                         </xml>,
                             FreshWidgets = fn (cfg0, _, cfg) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                sl <- source [];
                                changed <- source False;
                                ws <- old.tname1.FreshWidgets cfg;
                                return (s, ws0, sl, changed, ws),
                             WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                keys <- List.mapM (fn (k, vs) => vs <- source vs; notEditing <- source None; return (k, vs, notEditing)) keys;
                                sl <- source keys;
                                changed <- source False;
                                ws <- old.tname1.WidgetsFrom cfg r aux;
                                return (s, ws0, sl, changed, ws),
                             RenderWidgets = fn k (cfgs, cfg1, cfg2) (s, ws0, sl, changed, wso) =>
                                                <xml>
                                                  {old.tname1.RenderWidgets k cfg2 wso}
                                                  <div class="form-group">
                                                    <label class="control-label">{[lab1]}</label>
                                                    <div class="input-group">
                                                      {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                            <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                        fl labels ws ws0}
                                                    </div>
                                                    <div class="input-group">
                                                      <span class="input-group-btn">
                                                        <button class="btn"
                                                                onclick={fn _ =>
                                                                            sv <- get s;
                                                                            if sv = "" then
                                                                                return ()
                                                                            else
                                                                                set changed False;
                                                                                sv <- return (readError sv);
                                                                                slv <- get sl;
                                                                                if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                    return ()
                                                                                else
                                                                                    vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                               current (@Widget.value w s))
                                                                                           fl ws ws0;
                                                                                    vs <- source vs;
                                                                                    notEditing <- source None;
                                                                                    set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                      </span>
                                                      <cselect class="form-control" source={s} onchange={set changed True}>
                                                        <coption/>
                                                        {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                      </cselect> 
                                                    </div>
                                                    <table>
                                                      <tr><td/>
                                                        {@mapX [fn _ => string] [tr]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                              <xml><th>{[label]}</th></xml>)
                                                          fl labels}
                                                      <td/></tr>
                                                      <dyn signal={slv <- signal sl;
                                                                   return (List.mapX (fn (k2, vs, ws0) => <xml>
                                                                     <dyn signal={vals <- signal vs;
                                                                                  wids <- signal ws0;
                                                                                  case wids of
                                                                                      None => return <xml>
                                                                                        <tr>
                                                                                          <td>{[k2]}</td>
                                                                                          {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                                <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                            fl ws vals}
                                                                                          {if @Row.isEmpty fl then
                                                                                               <xml></xml>
                                                                                           else <xml>
                                                                                             <td><button class="btn"
                                                                                                         onclick={fn _ =>
                                                                                                                     wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                              (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                                  @Widget.initialize w cfg v)
                                                                                                                              fl ws cfgs vals;
                                                                                                                     set ws0 (Some wids)}>
                                                                                               <span class="glyphicon glyphicon-pencil"/>
                                                                                             </button></td>
                                                                                             <td><button class="btn"
                                                                                                         onclick={fn _ => set sl (List.filter (fn (k2', _, _) => k2' <> k2) slv)}>
                                                                                               <span class="glyphicon glyphicon-remove"/>
                                                                                             </button></td>
                                                                                           </xml>}
                                                                                        </tr>
                                                                                      </xml>
                                                                                    | Some wids => return <xml>
                                                                                      <tr>
                                                                                        <td>{[k2]}</td>
                                                                                        {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                              <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                          fl ws wids}
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ =>
                                                                                                                vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                            current (@Widget.value w s))
                                                                                                                        fl ws wids;
                                                                                                                set vs vsv;
                                                                                                                set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-check"/>
                                                                                        </button></td>
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ => set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-remove"/>
                                                                                        </button></td>
                                                                                      </tr>
                                                                                    </xml>}/>
                                                                   </xml>) slv)}/>
                                                    </table>
                                                </div>
                                              </xml>,
                           ReadWidgets = fn (s, _, sl, changed, ws) =>
                                            slv <- signal sl;
                                            slv <- List.mapM (fn (k2, vs, _) => vs <- signal vs; return (k2, vs)) slv;
                                            chd <- signal changed;
                                            (wsv, aux, err) <- old.tname1.ReadWidgets ws;
                                            return (wsv, (slv, aux),
                                                    if chd then
                                                        let
                                                            val msg = "The dropdown for \"" ^ lab1 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                        in
                                                            case err of
                                                                None => Some msg
                                                              | Some err => Some (err ^ "\n" ^ msg)
                                                        end
                                                    else
                                                        err)},
          tname2 = old.tname2
                       -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                       ++ {Insert =
                             fn r (k1s, aux) =>
                                old.tname2.Insert r aux;
                                List.app (fn (k1, others) =>
                                             @Sql.easy_insert ({colR1 = _, colR2 = _} ++ injs)
                                              (@Folder.cons [colR1] [_] !
                                                (@Folder.cons [colR2] [_] !
                                                  (@Folder.mp fl)))
                                              rel ({colR1 = k1, colR2 = r.col2} ++ others)) k1s,
                             Update =
                             fn k r (k1s, aux) =>
                                old.tname2.Update k r aux;
                                dml (DELETE FROM rel
                                     WHERE t.{colR2} = {[r.col2]});
                                List.app (fn (k1, others) =>
                                             @Sql.easy_insert ({colR1 = _, colR2 = _} ++ injs)
                                              (@Folder.cons [colR1] [_] !
                                                (@Folder.cons [colR2] [_] !
                                                  (@Folder.mp fl)))
                                              rel ({colR1 = k1, colR2 = r.col2} ++ others)) k1s,
                             Delete = fn k2 => dml (DELETE FROM rel WHERE t.{colR2} = {[k2]}),
                             Config =
                             let
                                 val tab = old.tname1.Table
                             in
                                 keys <- List.mapQuery (SELECT DISTINCT tab.{col1}
                                                        FROM tab
                                                        ORDER BY tab.{col1})
                                                       (fn r => r.Tab.col1);
                                 wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                          (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                          fl ws;
                                 cfg <- old.tname2.Config;
                                 return (wcfg, keys, cfg)
                             end,
                             Auxiliary = fn k2 row =>
                                 keys <- List.mapQuery (SELECT rel.{colR1}, rel.{{map fst3 others}}
                                                        FROM rel
                                                        WHERE rel.{colR2} = {[k2]}
                                                        ORDER BY rel.{colR1})
                                                       (fn r => (r.Rel.colR1, r.Rel -- colR1));
                                 aux <- old.tname2.Auxiliary k2 row;
                                 return (keys, aux),
                             Render = fn entry (k1s, aux) r =>
                                         <xml>
                                           {old.tname2.Render entry aux r}
                                           <tr>
                                             <th>{[lab2]}</th>
                                             <td>{List.mapX (fn (k1, others) => <xml>{entry (make [tname1] k1) (show k1)}
                                               {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                 (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                     <xml>- {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                               <br/></xml>) k1s}</td>
                                           </tr>
                                         </xml>,
                             FreshWidgets = fn (cfg0, _, cfg) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                sl <- source [];
                                changed <- source False;
                                ws <- old.tname2.FreshWidgets cfg;
                                return (s, ws0, sl, changed, ws),
                             WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                keys <- List.mapM (fn (k, vs) => vs <- source vs; notEditing <- source None; return (k, vs, notEditing)) keys;
                                sl <- source keys;
                                changed <- source False;
                                ws <- old.tname2.WidgetsFrom cfg r aux;
                                return (s, ws0, sl, changed, ws),
                             RenderWidgets = fn k (cfgs, cfg1, cfg2) (s, ws0, sl, changed, wso) =>
                                                <xml>
                                                  {old.tname2.RenderWidgets k cfg2 wso}
                                                  <div class="form-group">
                                                    <label class="control-label">{[lab2]}</label>
                                                    <div class="input-group">
                                                      {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                            <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                        fl labels ws ws0}
                                                    </div>
                                                    <div class="input-group">
                                                      <span class="input-group-btn">
                                                        <button class="btn"
                                                                onclick={fn _ =>
                                                                            sv <- get s;
                                                                            if sv = "" then
                                                                                return ()
                                                                            else
                                                                                set changed False;
                                                                                sv <- return (readError sv);
                                                                                slv <- get sl;
                                                                                if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                    return ()
                                                                                else
                                                                                    vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                               current (@Widget.value w s))
                                                                                           fl ws ws0;
                                                                                    vs <- source vs;
                                                                                    notEditing <- source None;
                                                                                    set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                      </span>
                                                      <cselect class="form-control" source={s} onchange={set changed True}>
                                                        <coption/>
                                                        {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                      </cselect> 
                                                    </div>
                                                    <table>
                                                      <tr><td/>
                                                        {@mapX [fn _ => string] [tr]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                              <xml><th>{[label]}</th></xml>)
                                                          fl labels}
                                                      <td/></tr>
                                                      <dyn signal={slv <- signal sl;
                                                                   return (List.mapX (fn (k2, vs, ws0) => <xml>
                                                                     <dyn signal={vals <- signal vs;
                                                                                  wids <- signal ws0;
                                                                                  case wids of
                                                                                      None => return <xml>
                                                                                        <tr>
                                                                                          <td>{[k2]}</td>
                                                                                          {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                                <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                            fl ws vals}
                                                                                          {if @Row.isEmpty fl then
                                                                                               <xml></xml>
                                                                                           else <xml>
                                                                                             <td><button class="btn"
                                                                                                         onclick={fn _ =>
                                                                                                                     wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                              (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                                  @Widget.initialize w cfg v)
                                                                                                                              fl ws cfgs vals;
                                                                                                                     set ws0 (Some wids)}>
                                                                                               <span class="glyphicon glyphicon-pencil"/>
                                                                                             </button></td>
                                                                                             <td><button class="btn"
                                                                                                         onclick={fn _ => set sl (List.filter (fn (k2', _, _) => k2' <> k2) slv)}>
                                                                                               <span class="glyphicon glyphicon-remove"/>
                                                                                             </button></td>
                                                                                           </xml>}
                                                                                        </tr>
                                                                                      </xml>
                                                                                    | Some wids => return <xml>
                                                                                      <tr>
                                                                                        <td>{[k2]}</td>
                                                                                        {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                              <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                          fl ws wids}
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ =>
                                                                                                                vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                            current (@Widget.value w s))
                                                                                                                        fl ws wids;
                                                                                                                set vs vsv;
                                                                                                                set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-check"/>
                                                                                        </button></td>
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ => set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-remove"/>
                                                                                        </button></td>
                                                                                      </tr>
                                                                                    </xml>}/>
                                                                   </xml>) slv)}/>
                                                    </table>
                                                </div>
                                              </xml>,
                           ReadWidgets = fn (s, _, sl, changed, ws) =>
                                            slv <- signal sl;
                                            slv <- List.mapM (fn (k2, vs, _) => vs <- signal vs; return (k2, vs)) slv;
                                            chd <- signal changed;
                                            (wsv, aux, err) <- old.tname2.ReadWidgets ws;
                                            return (wsv, (slv, aux),
                                                    if chd then
                                                        let
                                                            val msg = "The dropdown for \"" ^ lab1 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                        in
                                                            case err of
                                                                None => Some msg
                                                              | Some err => Some (err ^ "\n" ^ msg)
                                                        end
                                                    else
                                                        err)}}

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
                           end) = struct
    open M

    type manyToManyWithFile11 = $(map thd3 others) * list key2 * impl11
    type manyToManyWithFile12 = source string * $(map snd3 others) * source (option AjaxUpload.handle) * source (list (key2 * source ($(map fst3 others) * option AjaxUpload.handle) * source (option ($(map snd3 others) * source (option AjaxUpload.handle))))) * source bool * impl12
    type manyToManyWithFile13 = list (key2 * $(map fst3 others) * option AjaxUpload.handle) * impl13
    type manyToManyWithFile21 = $(map thd3 others) * list key1 * impl21
    type manyToManyWithFile22 = source string * $(map snd3 others) * source (option AjaxUpload.handle) * source (list (key1 * source ($(map fst3 others) * option AjaxUpload.handle) * source (option ($(map snd3 others) * source (option AjaxUpload.handle))))) * source bool * impl22
    type manyToManyWithFile23 = list (key1 * $(map fst3 others) * option AjaxUpload.handle) * impl23

    fun download k1 k2 =
        ok <- authorize k1 k2;
        if not ok then
            error <xml>Access denied</xml>
        else
            r <- oneRow1 (SELECT rel.FileName, rel.FileType, rel.FileData
                          FROM rel
                          WHERE rel.{colR1} = {[k1]}
                            AND rel.{colR2} = {[k2]});
            setHeader (blessResponseHeader "Content-Disposition")
                      ("attachment; filename=" ^ r.FileName);
            returnBlob r.FileData (blessMime r.FileType)
                                
    val make (old : t ([tname1 = key1, tname2 = key2] ++ full)
                      ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                        tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)) =
        old -- tname1 -- tname2
            ++ {tname1 = old.tname1
                             -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                             ++ {Insert =
                                 fn r (k2s, aux) =>
                                    old.tname1.Insert r aux;
                                    List.app (fn (k2, others, uploadO) =>
                                                 case uploadO of
                                                     None => error <xml>Can't complete creation process: no file was associated with a connection between {[lab1]} and {[lab2]}.</xml>
                                                   | Some upload =>
                                                     res <- AjaxUpload.claim upload;
                                                     case res of
                                                         AjaxUpload.NotFound => error <xml>Timeout: it has been too long since you uploaded that file.</xml>
                                                       | AjaxUpload.Found fr =>
                                                         case fr.Filename of
                                                             None => error <xml>No filename associated with upload</xml>
                                                           | Some fname =>
                                                             @Sql.easy_insert ({colR1 = _, colR2 = _, FileName = _, FileType = _, FileData = _} ++ injs)
                                                              (@Folder.cons [colR1] [_] !
                                                                (@Folder.cons [colR2] [_] !
                                                                  (@Folder.cons [#FileName] [_] !
                                                                    (@Folder.cons [#FileType] [_] !
                                                                      (@Folder.cons [#FileData] [_] !
                                                                        (@Folder.mp fl))))))
                                                              rel ({colR1 = r.col1, colR2 = k2, FileName = fname, FileType = fr.MimeType, FileData = fr.Content} ++ others)) k2s,
                                 Update =
                                 fn k r (k2s, aux) =>
                                    old.tname1.Update k r aux;
                                    whichSurvive <- return (List.foldl (fn (k2, _, _) acc => (WHERE {acc} AND t.{colR2} <> {[k2]})) (WHERE TRUE) k2s);
                                    dml (DELETE FROM rel
                                         WHERE t.{colR1} = {[r.col1]}
                                           AND {whichSurvive});
                                    List.app (fn (k2, others, uploadO) =>
                                                 case uploadO of
                                                     None =>
                                                     @@Sql.easy_update''
                                                       [[colR1 = _, colR2 = _]]
                                                       [map fst3 others]
                                                       [_]
                                                       [[FileName = _, FileType = _, FileData = _]]
                                                       ! !
                                                       {colR1 = _, colR2 = _}
                                                       injs
                                                       (@Folder.cons [colR1] [_] !
                                                         (@Folder.cons [colR2] [_] !
                                                           Folder.nil))
                                                       (@Folder.mp fl)
                                                       rel {colR1 = r.col1, colR2 = k2} others
                                                   | Some upload =>
                                                     dml (DELETE FROM rel
                                                                 WHERE t.{colR1} = {[r.col1]}
                                                                   AND t.{colR2} = {[k2]});
                                                     res <- AjaxUpload.claim upload;
                                                     case res of
                                                         AjaxUpload.NotFound => error <xml>Timeout: it has been too long since you uploaded that file.</xml>
                                                       | AjaxUpload.Found fr =>
                                                         case fr.Filename of
                                                             None => error <xml>No filename associated with upload</xml>
                                                           | Some fname =>
                                                             @Sql.easy_insert ({colR1 = _, colR2 = _, FileName = _, FileType = _, FileData = _} ++ injs)
                                                              (@Folder.cons [colR1] [_] !
                                                                (@Folder.cons [colR2] [_] !
                                                                  (@Folder.cons [#FileName] [_] !
                                                                    (@Folder.cons [#FileType] [_] !
                                                                      (@Folder.cons [#FileData] [_] !
                                                                        (@Folder.mp fl))))))
                                                              rel ({colR1 = r.col1, colR2 = k2, FileName = fname, FileType = fr.MimeType, FileData = fr.Content} ++ others)) k2s,
                                 Delete = fn k1 => dml (DELETE FROM rel WHERE t.{colR1} = {[k1]}),
                                 Config =
                                 let
                                     val tab = old.tname2.Table
                                 in
                                     keys <- List.mapQuery (SELECT DISTINCT tab.{col2}
                                                            FROM tab
                                                            ORDER BY tab.{col2})
                                                           (fn r => r.Tab.col2);
                                     wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                              (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                              fl ws;
                                     cfg <- old.tname1.Config;
                                     return (wcfg, keys, cfg)
                                 end,
                                 Auxiliary = fn k1 row =>
                                     keys <- List.mapQuery (SELECT rel.{colR2}, rel.{{map fst3 others}}
                                                            FROM rel
                                                            WHERE rel.{colR1} = {[k1]}
                                                            ORDER BY rel.{colR2})
                                                           (fn r => (r.Rel.colR2, r.Rel -- colR2, None));
                                     aux <- old.tname1.Auxiliary k1 row;
                                     return (keys, aux),
                                 Render = fn entry (k2s, aux) r =>
                                             <xml>
                                               {old.tname1.Render entry aux r}
                                               <tr>
                                                 <th>{[lab1]}</th>
                                                 <td>
                                                   {List.mapX (fn (k2, others, uploadO) => <xml>{entry (make [tname2] k2) (show k2)}
                                                     {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                       (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                           <xml> - {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                                     - <a link={download r.col1 k2}>Download</a>
                                                     <br/></xml>) k2s}
                                                 </td>
                                               </tr>
                                             </xml>,
                                 FreshWidgets = fn (cfg0, _, cfg) =>
                                    s <- source "";
                                    ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                            fl ws cfg0;
                                    upl <- source None;
                                    sl <- source [];
                                    changed <- source False;
                                    ws <- old.tname1.FreshWidgets cfg;
                                    return (s, ws0, upl, sl, changed, ws),
                                 WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                    s <- source "";
                                    ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                            fl ws cfg0;
                                    upl <- source None;
                                    keys <- List.mapM (fn (k, vs, _) => vs <- source (vs, None); notEditing <- source None; return (k, vs, notEditing)) keys;
                                    sl <- source keys;
                                    changed <- source False;
                                    ws <- old.tname1.WidgetsFrom cfg r aux;
                                    return (s, ws0, upl, sl, changed, ws),
                                 RenderWidgets = fn k1 (cfgs, cfg1, cfg2) (s, ws0, upl, sl, changed, wso) =>
                                                    <xml>
                                                      {old.tname1.RenderWidgets k1 cfg2 wso}
                                                      <div class="form-group">
                                                        <label class="control-label">{[lab1]}</label>
                                                        <div class="input-group">
                                                          {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                                <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                            fl labels ws ws0}
                                                          File: <active code={AjaxUpload.render {SubmitLabel = None,
                                                                                                 OnBegin = return (),
                                                                                                 OnSuccess = fn upload => set upl (Some upload),
                                                                                                 OnError = error <xml>Error uploading file.  Maybe it has an unsupported type?</xml>}}/>
                                                        </div>
                                                        <div class="input-group">
                                                          <span class="input-group-btn">
                                                            <button class="btn"
                                                                    onclick={fn _ =>
                                                                                sv <- get s;
                                                                                if sv = "" then
                                                                                    return ()
                                                                                else
                                                                                    set changed False;
                                                                                    sv <- return (readError sv);
                                                                                    slv <- get sl;
                                                                                    if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                        return ()
                                                                                    else
                                                                                        uploadO <- get upl;
                                                                                        case uploadO of
                                                                                            None => error <xml>You must upload a file first.</xml>
                                                                                          | Some upload =>
                                                                                            set upl None;
                                                                                            vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                   (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                       current (@Widget.value w s))
                                                                                                   fl ws ws0;
                                                                                            vs <- source (vs, Some upload);
                                                                                            notEditing <- source None;
                                                                                            set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                          </span>
                                                          <cselect class="form-control" source={s} onchange={set changed True}>
                                                            <coption/>
                                                            {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                          </cselect> 
                                                        </div>
                                                        <table>
                                                          <tr><td/>
                                                            {@mapX [fn _ => string] [tr]
                                                              (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                                  <xml><th>{[label]}</th></xml>)
                                                              fl labels}
                                                            <th>File</th>
                                                          <td/></tr>
                                                          <dyn signal={slv <- signal sl;
                                                                       return (List.mapX (fn (k2, vs, ws0) => <xml>
                                                                         <dyn signal={(vals, _) <- signal vs;
                                                                                      wids <- signal ws0;
                                                                                      case wids of
                                                                                          None => return <xml>
                                                                                            <tr>
                                                                                              <td>{[k2]}</td>
                                                                                              {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                                (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                                    <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                                fl ws vals}
                                                                                              <td>{case k1 of
                                                                                                       None => <xml></xml>
                                                                                                     | Some k1 => <xml><a link={download k1 k2}>File</a></xml>}</td>
                                                                                              {if @Row.isEmpty fl then
                                                                                                   <xml></xml>
                                                                                               else <xml>
                                                                                                 <td><button class="btn"
                                                                                                             onclick={fn _ =>
                                                                                                                         wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                                  (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                                      @Widget.initialize w cfg v)
                                                                                                                                  fl ws cfgs vals;
                                                                                                                         upl <- source None;
                                                                                                                         set ws0 (Some (wids, upl))}>
                                                                                                   <span class="glyphicon glyphicon-pencil"/>
                                                                                                 </button></td>
                                                                                                 <td><button class="btn"
                                                                                                             onclick={fn _ => set sl (List.filter (fn (k2', _, _) => k2' <> k2) slv)}>
                                                                                                   <span class="glyphicon glyphicon-remove"/>
                                                                                                 </button></td>
                                                                                               </xml>}
                                                                                            </tr>
                                                                                          </xml>
                                                                                        | Some (wids, upl) => return <xml>
                                                                                          <tr>
                                                                                            <td>{[k2]}</td>
                                                                                            {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                              (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                                  <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                              fl ws wids}
                                                                                            <td>
                                                                                              <active code={AjaxUpload.render {SubmitLabel = None,
                                                                                                                               OnBegin = return (),
                                                                                                                               OnSuccess = fn upload => set upl (Some upload),
                                                                                                                               OnError = error <xml>Error uploading file.  Maybe it has an unsupported type?</xml>}}/>
                                                                                                                                                                                                                                                                                      </td>
                                                                                            <td><button class="btn"
                                                                                                        onclick={fn _ =>
                                                                                                                    vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                                current (@Widget.value w s))
                                                                                                                            fl ws wids;
                                                                                                                    uploadO <- get upl;
                                                                                                                    set vs (vsv, uploadO);
                                                                                                                    set ws0 None}>
                                                                                              <span class="glyphicon glyphicon-check"/>
                                                                                            </button></td>
                                                                                            <td><button class="btn"
                                                                                                        onclick={fn _ => set ws0 None}>
                                                                                              <span class="glyphicon glyphicon-remove"/>
                                                                                            </button></td>
                                                                                          </tr>
                                                                                        </xml>}/>
                                                                       </xml>) slv)}/>
                                                        </table>
                                                    </div>
                                                  </xml>,
                               ReadWidgets = fn (s, _, _, sl, changed, ws) =>
                                                slv <- signal sl;
                                                slv <- List.mapM (fn (k2, vs, _) => (vs, uploadO) <- signal vs; return (k2, vs, uploadO)) slv;
                                                chd <- signal changed;
                                                (wsv, aux, err) <- old.tname1.ReadWidgets ws;
                                                return (wsv, (slv, aux),
                                                        if chd then
                                                            let
                                                                val msg = "The dropdown for \"" ^ lab1 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                            in
                                                                case err of
                                                                    None => Some msg
                                                                  | Some err => Some (err ^ "\n" ^ msg)
                                                            end
                                                        else
                                                            err)},
              tname2 = old.tname2
                           -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                           ++ {Insert =
                                 fn r (k1s, aux) =>
                                    old.tname2.Insert r aux;
                                    List.app (fn (k1, others, uploadO) =>
                                                 case uploadO of
                                                     None => error <xml>Can't complete creation process: no file was associated with a connection between {[lab1]} and {[lab2]}.</xml>
                                                   | Some upload =>
                                                     res <- AjaxUpload.claim upload;
                                                     case res of
                                                         AjaxUpload.NotFound => error <xml>Timeout: it has been too long since you uploaded that file.</xml>
                                                       | AjaxUpload.Found fr =>
                                                         case fr.Filename of
                                                             None => error <xml>No filename associated with upload</xml>
                                                           | Some fname =>
                                                             @Sql.easy_insert ({colR1 = _, colR2 = _, FileName = _, FileType = _, FileData = _} ++ injs)
                                                              (@Folder.cons [colR1] [_] !
                                                                (@Folder.cons [colR2] [_] !
                                                                  (@Folder.cons [#FileName] [_] !
                                                                    (@Folder.cons [#FileType] [_] !
                                                                      (@Folder.cons [#FileData] [_] !
                                                                        (@Folder.mp fl))))))
                                                              rel ({colR1 = k1, colR2 = r.col2, FileName = fname, FileType = fr.MimeType, FileData = fr.Content} ++ others)) k1s,
                                 Update =
                                 fn k r (k1s, aux) =>
                                    old.tname2.Update k r aux;
                                    whichSurvive <- return (List.foldl (fn (k1, _, _) acc => (WHERE {acc} AND t.{colR1} <> {[k1]})) (WHERE TRUE) k1s);
                                    dml (DELETE FROM rel
                                         WHERE t.{colR2} = {[r.col2]}
                                           AND {whichSurvive});
                                    List.app (fn (k1, others, uploadO) =>
                                                 case uploadO of
                                                     None =>
                                                     @@Sql.easy_update''
                                                       [[colR1 = _, colR2 = _]]
                                                       [map fst3 others]
                                                       [_]
                                                       [[FileName = _, FileType = _, FileData = _]]
                                                       ! !
                                                       {colR1 = _, colR2 = _}
                                                       injs
                                                       (@Folder.cons [colR1] [_] !
                                                         (@Folder.cons [colR2] [_] !
                                                           Folder.nil))
                                                       (@Folder.mp fl)
                                                       rel {colR1 = k1, colR2 = r.col2} others
                                                   | Some upload =>
                                                     dml (DELETE FROM rel
                                                                 WHERE t.{colR2} = {[r.col2]}
                                                                   AND t.{colR1} = {[k1]});
                                                     res <- AjaxUpload.claim upload;
                                                     case res of
                                                         AjaxUpload.NotFound => error <xml>Timeout: it has been too long since you uploaded that file.</xml>
                                                       | AjaxUpload.Found fr =>
                                                         case fr.Filename of
                                                             None => error <xml>No filename associated with upload</xml>
                                                           | Some fname =>
                                                             @Sql.easy_insert ({colR1 = _, colR2 = _, FileName = _, FileType = _, FileData = _} ++ injs)
                                                              (@Folder.cons [colR1] [_] !
                                                                (@Folder.cons [colR2] [_] !
                                                                  (@Folder.cons [#FileName] [_] !
                                                                    (@Folder.cons [#FileType] [_] !
                                                                      (@Folder.cons [#FileData] [_] !
                                                                        (@Folder.mp fl))))))
                                                              rel ({colR1 = k1, colR2 = r.col2, FileName = fname, FileType = fr.MimeType, FileData = fr.Content} ++ others)) k1s,
                                 Delete = fn k2 => dml (DELETE FROM rel WHERE t.{colR2} = {[k2]}),
                                 Config =
                                 let
                                     val tab = old.tname1.Table
                                 in
                                     keys <- List.mapQuery (SELECT DISTINCT tab.{col1}
                                                            FROM tab
                                                            ORDER BY tab.{col1})
                                                           (fn r => r.Tab.col1);
                                     wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                              (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                              fl ws;
                                     cfg <- old.tname2.Config;
                                     return (wcfg, keys, cfg)
                                 end,
                                 Auxiliary = fn k2 row =>
                                     keys <- List.mapQuery (SELECT rel.{colR1}, rel.{{map fst3 others}}
                                                            FROM rel
                                                            WHERE rel.{colR2} = {[k2]}
                                                            ORDER BY rel.{colR1})
                                                           (fn r => (r.Rel.colR1, r.Rel -- colR1, None));
                                     aux <- old.tname2.Auxiliary k2 row;
                                     return (keys, aux),
                                 Render = fn entry (k1s, aux) r =>
                                             <xml>
                                               {old.tname2.Render entry aux r}
                                               <tr>
                                                 <th>{[lab2]}</th>
                                                 <td>
                                                   {List.mapX (fn (k1, others, uploadO) => <xml>{entry (make [tname1] k1) (show k1)}
                                                     {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                       (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                           <xml> - {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                                     - <a link={download k1 r.col2}>Download</a>
                                                     <br/></xml>) k1s}
                                                 </td>
                                               </tr>
                                             </xml>,
                                 FreshWidgets = fn (cfg0, _, cfg) =>
                                    s <- source "";
                                    ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                            fl ws cfg0;
                                    upl <- source None;
                                    sl <- source [];
                                    changed <- source False;
                                    ws <- old.tname2.FreshWidgets cfg;
                                    return (s, ws0, upl, sl, changed, ws),
                                 WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                    s <- source "";
                                    ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                            fl ws cfg0;
                                    upl <- source None;
                                    keys <- List.mapM (fn (k, vs, _) => vs <- source (vs, None); notEditing <- source None; return (k, vs, notEditing)) keys;
                                    sl <- source keys;
                                    changed <- source False;
                                    ws <- old.tname2.WidgetsFrom cfg r aux;
                                    return (s, ws0, upl, sl, changed, ws),
                                 RenderWidgets = fn k2 (cfgs, cfg1, cfg2) (s, ws0, upl, sl, changed, wso) =>
                                                    <xml>
                                                      {old.tname2.RenderWidgets k2 cfg2 wso}
                                                      <div class="form-group">
                                                        <label class="control-label">{[lab2]}</label>
                                                        <div class="input-group">
                                                          {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                                <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                            fl labels ws ws0}
                                                          File: <active code={AjaxUpload.render {SubmitLabel = None,
                                                                                                 OnBegin = return (),
                                                                                                 OnSuccess = fn upload => set upl (Some upload),
                                                                                                 OnError = error <xml>Error uploading file.  Maybe it has an unsupported type?</xml>}}/>
                                                        </div>
                                                        <div class="input-group">
                                                          <span class="input-group-btn">
                                                            <button class="btn"
                                                                    onclick={fn _ =>
                                                                                sv <- get s;
                                                                                if sv = "" then
                                                                                    return ()
                                                                                else
                                                                                    set changed False;
                                                                                    sv <- return (readError sv);
                                                                                    slv <- get sl;
                                                                                    if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                        return ()
                                                                                    else
                                                                                        uploadO <- get upl;
                                                                                        case uploadO of
                                                                                            None => error <xml>You must upload a file first.</xml>
                                                                                          | Some upload =>
                                                                                            set upl None;
                                                                                            vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                   (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                       current (@Widget.value w s))
                                                                                                   fl ws ws0;
                                                                                            vs <- source (vs, Some upload);
                                                                                            notEditing <- source None;
                                                                                            set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                          </span>
                                                          <cselect class="form-control" source={s} onchange={set changed True}>
                                                            <coption/>
                                                            {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                          </cselect> 
                                                        </div>
                                                        <table>
                                                          <tr><td/>
                                                            {@mapX [fn _ => string] [tr]
                                                              (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                                  <xml><th>{[label]}</th></xml>)
                                                              fl labels}
                                                            <th>File</th>
                                                          <td/></tr>
                                                          <dyn signal={slv <- signal sl;
                                                                       return (List.mapX (fn (k1, vs, ws0) => <xml>
                                                                         <dyn signal={(vals, _) <- signal vs;
                                                                                      wids <- signal ws0;
                                                                                      case wids of
                                                                                          None => return <xml>
                                                                                            <tr>
                                                                                              <td>{[k1]}</td>
                                                                                              {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                                (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                                    <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                                fl ws vals}
                                                                                              <td>{case k2 of
                                                                                                       None => <xml></xml>
                                                                                                     | Some k2 => <xml><a link={download k1 k2}>File</a></xml>}</td>
                                                                                              {if @Row.isEmpty fl then
                                                                                                   <xml></xml>
                                                                                               else <xml>
                                                                                                 <td><button class="btn"
                                                                                                             onclick={fn _ =>
                                                                                                                         wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                                  (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                                      @Widget.initialize w cfg v)
                                                                                                                                  fl ws cfgs vals;
                                                                                                                         upl <- source None;
                                                                                                                         set ws0 (Some (wids, upl))}>
                                                                                                   <span class="glyphicon glyphicon-pencil"/>
                                                                                                 </button></td>
                                                                                                 <td><button class="btn"
                                                                                                             onclick={fn _ => set sl (List.filter (fn (k1', _, _) => k1' <> k1) slv)}>
                                                                                                   <span class="glyphicon glyphicon-remove"/>
                                                                                                 </button></td>
                                                                                               </xml>}
                                                                                            </tr>
                                                                                          </xml>
                                                                                        | Some (wids, upl) => return <xml>
                                                                                          <tr>
                                                                                            <td>{[k1]}</td>
                                                                                            {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                              (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                                  <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                              fl ws wids}
                                                                                            <td>
                                                                                              <active code={AjaxUpload.render {SubmitLabel = None,
                                                                                                                               OnBegin = return (),
                                                                                                                               OnSuccess = fn upload => set upl (Some upload),
                                                                                                                               OnError = error <xml>Error uploading file.  Maybe it has an unsupported type?</xml>}}/>
                                                                                                                                                                                                                                                                                      </td>
                                                                                            <td><button class="btn"
                                                                                                        onclick={fn _ =>
                                                                                                                    vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                                current (@Widget.value w s))
                                                                                                                            fl ws wids;
                                                                                                                    uploadO <- get upl;
                                                                                                                    set vs (vsv, uploadO);
                                                                                                                    set ws0 None}>
                                                                                              <span class="glyphicon glyphicon-check"/>
                                                                                            </button></td>
                                                                                            <td><button class="btn"
                                                                                                        onclick={fn _ => set ws0 None}>
                                                                                              <span class="glyphicon glyphicon-remove"/>
                                                                                            </button></td>
                                                                                          </tr>
                                                                                        </xml>}/>
                                                                       </xml>) slv)}/>
                                                        </table>
                                                    </div>
                                                  </xml>,
                               ReadWidgets = fn (s, _, _, sl, changed, ws) =>
                                                slv <- signal sl;
                                                slv <- List.mapM (fn (k1, vs, _) => (vs, uploadO) <- signal vs; return (k1, vs, uploadO)) slv;
                                                chd <- signal changed;
                                                (wsv, aux, err) <- old.tname2.ReadWidgets ws;
                                                return (wsv, (slv, aux),
                                                        if chd then
                                                            let
                                                                val msg = "The dropdown for \"" ^ lab2 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                            in
                                                                case err of
                                                                    None => Some msg
                                                                  | Some err => Some (err ^ "\n" ^ msg)
                                                            end
                                                        else
                                                            err)}}
end
    
fun manyToManyOrdered [full ::: {Type}] [tname1 :: Name] [key1 ::: Type] [col1 :: Name] [colR1 :: Name]
                      [cols1 ::: {Type}] [colsDone1 ::: {Type}] [cstrs1 ::: {{Unit}}]
                      [impl11 ::: Type] [impl12 ::: Type] [impl13 ::: Type]
                      [tname2 :: Name] [key2 ::: Type] [col2 :: Name] [colR2 :: Name]
                      [cols2 ::: {Type}] [colsDone2 ::: {Type}] [cstrs2 ::: {{Unit}}]
                      [impl21 ::: Type] [impl22 ::: Type] [impl23 ::: Type]
                      [cstrs ::: {{Unit}}]
                      [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
                      [others ::: {(Type * Type * Type)}]
                      [[tname1] ~ [tname2]] [[tname1, tname2] ~ old] [[tname1, tname2] ~ full]
                      [[col1] ~ cols1] [[col2] ~ cols2] [[col1] ~ [col2]] [[col1, col2] ~ [SeqNum]]
                      [[colR1] ~ [colR2]] [[colR1, colR2] ~ [SeqNum]] [others ~ [colR1, colR2, SeqNum]]
                      (rel : sql_table ([colR1 = key1, colR2 = key2, SeqNum = int] ++ map fst3 others) cstrs)
                      (lab1 : string) (lab2 : string)
                      (_ : eq key1) (_ : ord key1) (_ : show key1) (_ : read key1) (_ : sql_injectable key1)
                      (_ : eq key2) (_ : ord key2) (_ : show key2) (_ : read key2) (_ : sql_injectable key2)
                      (fl : folder others) (ws : $(map Widget.t' others)) (injs : $(map (fn p => sql_injectable p.1) others)) (labels : $(map (fn _ => string) others))
                      (old : t ([tname1 = key1, tname2 = key2] ++ full)
                               ([tname1 = (key1, [col1 = key1] ++ cols1, colsDone1, cstrs1, impl11, impl12, impl13),
                                 tname2 = (key2, [col2 = key2] ++ cols2, colsDone2, cstrs2, impl21, impl22, impl23)] ++ old)) =
    old -- tname1 -- tname2
        ++ {tname1 = old.tname1
                         -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                         ++ {Insert =
                             fn r (k2s, aux) =>
                                old.tname1.Insert r aux;
                                List.appi (fn i (k2, others) =>
                                              @Sql.easy_insert ({colR1 = _, colR2 = _, SeqNum = _} ++ injs)
                                               (@Folder.cons [colR1] [_] !
                                                 (@Folder.cons [colR2] [_] !
                                                   (@Folder.cons [#SeqNum] [_] !
                                                     (@Folder.mp fl))))
                                               rel ({colR1 = r.col1, colR2 = k2, SeqNum = i} ++ others)) k2s,
                             Update =
                             fn k r (k2s, aux) =>
                                old.tname1.Update k r aux;
                                dml (DELETE FROM rel
                                     WHERE t.{colR1} = {[r.col1]});
                                List.appi (fn i (k2, others) =>
                                              @Sql.easy_insert ({colR1 = _, colR2 = _, SeqNum = _} ++ injs)
                                               (@Folder.cons [colR1] [_] !
                                                 (@Folder.cons [colR2] [_] !
                                                   (@Folder.cons [#SeqNum] [_] !
                                                     (@Folder.mp fl))))
                                               rel ({colR1 = r.col1, colR2 = k2, SeqNum = i} ++ others)) k2s,
                             Delete = fn k1 => dml (DELETE FROM rel WHERE t.{colR1} = {[k1]}),
                             Config =
                             let
                                 val tab = old.tname2.Table
                             in
                                 keys <- List.mapQuery (SELECT DISTINCT tab.{col2}
                                                        FROM tab
                                                        ORDER BY tab.{col2})
                                                       (fn r => r.Tab.col2);
                                 wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                          (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                          fl ws;
                                 cfg <- old.tname1.Config;
                                 return (wcfg, keys, cfg)
                             end,
                             Auxiliary = fn k1 row =>
                                 keys <- List.mapQuery (SELECT rel.{colR2}, rel.{{map fst3 others}}
                                                        FROM rel
                                                        WHERE rel.{colR1} = {[k1]}
                                                        ORDER BY rel.{colR2})
                                                       (fn r => (r.Rel.colR2, r.Rel -- colR2));
                                 aux <- old.tname1.Auxiliary k1 row;
                                 return (keys, aux),
                             Render = fn entry (k2s, aux) r =>
                                         <xml>
                                           {old.tname1.Render entry aux r}
                                           <tr>
                                             <th>{[lab1]}</th>
                                             <td>{List.mapX (fn (k2, others) => <xml>{entry (make [tname2] k2) (show k2)}
                                               {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                 (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                     <xml>- {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                               <br/></xml>) k2s}</td>
                                           </tr>
                                         </xml>,
                             FreshWidgets = fn (cfg0, _, cfg) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                sl <- source [];
                                changed <- source False;
                                ws <- old.tname1.FreshWidgets cfg;
                                return (s, ws0, sl, changed, ws),
                             WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                keys <- List.mapM (fn (k, vs) => vs <- source vs; notEditing <- source None; return (k, vs, notEditing)) keys;
                                sl <- source keys;
                                changed <- source False;
                                ws <- old.tname1.WidgetsFrom cfg r aux;
                                return (s, ws0, sl, changed, ws),
                             RenderWidgets = fn k (cfgs, cfg1, cfg2) (s, ws0, sl, changed, wso) =>
                                                <xml>
                                                  {old.tname1.RenderWidgets k cfg2 wso}
                                                  <div class="form-group">
                                                    <label class="control-label">{[lab1]}</label>
                                                    <div class="input-group">
                                                      {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                            <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                        fl labels ws ws0}
                                                    </div>
                                                    <div class="input-group">
                                                      <span class="input-group-btn">
                                                        <button class="btn"
                                                                onclick={fn _ =>
                                                                            sv <- get s;
                                                                            if sv = "" then
                                                                                return ()
                                                                            else
                                                                                set changed False;
                                                                                sv <- return (readError sv);
                                                                                slv <- get sl;
                                                                                if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                    return ()
                                                                                else
                                                                                    vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                               current (@Widget.value w s))
                                                                                           fl ws ws0;
                                                                                    vs <- source vs;
                                                                                    notEditing <- source None;
                                                                                    set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                      </span>
                                                      <cselect class="form-control" source={s} onchange={set changed True}>
                                                        <coption/>
                                                        {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                      </cselect> 
                                                    </div>
                                                    <table>
                                                      <tr><td/>
                                                        {@mapX [fn _ => string] [tr]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                              <xml><th>{[label]}</th></xml>)
                                                          fl labels}
                                                      <td/></tr>
                                                      <dyn signal={slv <- signal sl;
                                                                   len <- return (List.length slv);
                                                                   return (List.mapXi (fn i (k2, vs, ws0) => <xml>
                                                                     <dyn signal={vals <- signal vs;
                                                                                  wids <- signal ws0;
                                                                                  case wids of
                                                                                      None => return <xml><tr>
                                                                                        <td>{[k2]}</td>
                                                                                        {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                              <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                          fl ws vals}
                                                                                        <td>{if i = 0 then
                                                                                                 <xml></xml>
                                                                                             else
                                                                                                 <xml><button class="btn"
                                                                                                              onclick={fn _ =>
                                                                                                                          let
                                                                                                                              val (before, after) = List.splitAt (i-1) slv
                                                                                                                          in
                                                                                                                              case after of
                                                                                                                                  prev :: this :: after =>
                                                                                                                                  set sl (List.append before (this :: prev :: after))
                                                                                                                                | _ => error <xml>Explorer: impossible splitAt</xml>
                                                                                                                          end}>
                                                                                                   <span class="glyphicon glyphicon-arrow-up"/>
                                                                                                 </button></xml>}</td>
                                                                                        <td>{if i = len-1 then
                                                                                                 <xml></xml>
                                                                                             else
                                                                                                 <xml><button class="btn"
                                                                                                              onclick={fn _ =>
                                                                                                                          let
                                                                                                                              val (before, after) = List.splitAt i slv
                                                                                                                          in
                                                                                                                              case after of
                                                                                                                                  this :: next :: after =>
                                                                                                                                  set sl (List.append before (next :: this :: after))
                                                                                                                                | _ => error <xml>Explorer: impossible splitAt</xml>
                                                                                                                          end}>
                                                                                                   <span class="glyphicon glyphicon-arrow-down"/>
                                                                                        </button></xml>}</td>
                                                                                        {if @Row.isEmpty fl then
                                                                                             <xml></xml>
                                                                                         else <xml>
                                                                                           <td><button class="btn"
                                                                                                       onclick={fn _ =>
                                                                                                                   wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                                @Widget.initialize w cfg v)
                                                                                                                            fl ws cfgs vals;
                                                                                                                   set ws0 (Some wids)}>
                                                                                                         <span class="glyphicon glyphicon-pencil"/>
                                                                                           </button></td>
                                                                                           <td><button class="btn"
                                                                                                       onclick={fn _ => set sl (List.filter (fn (k2', _, _) => k2' <> k2) slv)}>
                                                                                             <span class="glyphicon glyphicon-remove"/>
                                                                                           </button></td>
                                                                                         </xml>}
                                                                                      </tr></xml>
                                                                                    | Some wids => return <xml>
                                                                                      <tr>
                                                                                        <td>{[k2]}</td>
                                                                                        {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                              <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                          fl ws wids}
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ =>
                                                                                                                vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                            current (@Widget.value w s))
                                                                                                                        fl ws wids;
                                                                                                                set vs vsv;
                                                                                                                set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-check"/>
                                                                                        </button></td>
                                                                                        <td><button class="btn"
                                                                                                    onclick={fn _ => set ws0 None}>
                                                                                          <span class="glyphicon glyphicon-remove"/>
                                                                                        </button></td>
                                                                                      </tr>
                                                                                    </xml>}/>
                                                                   </xml>) slv)}/>
                                                    </table>
                                                </div>
                                              </xml>,
                           ReadWidgets = fn (s, _, sl, changed, ws) =>
                                            slv <- signal sl;
                                            slv <- List.mapM (fn (k2, vs, _) => vs <- signal vs; return (k2, vs)) slv;
                                            chd <- signal changed;
                                            (wsv, aux, err) <- old.tname1.ReadWidgets ws;
                                            return (wsv, (slv, aux),
                                                    if chd then
                                                        let
                                                            val msg = "The dropdown for \"" ^ lab1 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                        in
                                                            case err of
                                                                None => Some msg
                                                              | Some err => Some (err ^ "\n" ^ msg)
                                                        end
                                                    else
                                                        err)},
          tname2 = old.tname2
                       -- #Insert -- #Update -- #Delete -- #Config -- #Auxiliary -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                       ++ {Insert =
                             fn r (k1s, aux) =>
                                old.tname2.Insert r aux;
                                List.appi (fn i (k1, others) =>
                                              @Sql.easy_insert ({colR1 = _, colR2 = _, SeqNum = _} ++ injs)
                                               (@Folder.cons [colR1] [_] !
                                                 (@Folder.cons [colR2] [_] !
                                                   (@Folder.cons [#SeqNum] [_] !
                                                     (@Folder.mp fl))))
                                               rel ({colR1 = k1, colR2 = r.col2, SeqNum = i} ++ others)) k1s,
                             Update =
                             fn k r (k1s, aux) =>
                                old.tname2.Update k r aux;
                                dml (DELETE FROM rel
                                     WHERE t.{colR2} = {[r.col2]});
                                List.appi (fn i (k1, others) =>
                                              @Sql.easy_insert ({colR1 = _, colR2 = _, SeqNum = _} ++ injs)
                                               (@Folder.cons [colR1] [_] !
                                                 (@Folder.cons [colR2] [_] !
                                                   (@Folder.cons [#SeqNum] [_] !
                                                     (@Folder.mp fl))))
                                               rel ({colR1 = k1, colR2 = r.col2, SeqNum = i} ++ others)) k1s,
                             Delete = fn k2 => dml (DELETE FROM rel WHERE t.{colR2} = {[k2]}),
                             Config =
                             let
                                 val tab = old.tname1.Table
                             in
                                 keys <- List.mapQuery (SELECT DISTINCT tab.{col1}
                                                        FROM tab
                                                        ORDER BY tab.{col1})
                                                       (fn r => r.Tab.col1);
                                 wcfg <- @Monad.mapR _ [Widget.t'] [thd3]
                                          (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w)
                                          fl ws;
                                 cfg <- old.tname2.Config;
                                 return (wcfg, keys, cfg)
                             end,
                             Auxiliary = fn k2 row =>
                                 keys <- List.mapQuery (SELECT rel.{colR1}, rel.{{map fst3 others}}
                                                        FROM rel
                                                        WHERE rel.{colR2} = {[k2]}
                                                        ORDER BY rel.SeqNum)
                                                       (fn r => (r.Rel.colR1, r.Rel -- colR1));
                                 aux <- old.tname2.Auxiliary k2 row;
                                 return (keys, aux),
                             Render = fn entry (k1s, aux) r =>
                                         <xml>
                                           {old.tname2.Render entry aux r}
                                           <tr>
                                             <th>{[lab2]}</th>
                                             <td>{List.mapX (fn (k1, others) => <xml>{entry (make [tname1] k1) (show k1)}
                                               {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                                 (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (v : p.1) =>
                                                     <xml>- {[label]}: {[@Widget.asValue w v]}</xml>) fl labels ws others}
                                               <br/></xml>) k1s}</td>
                                           </tr>
                                         </xml>,
                             FreshWidgets = fn (cfg0, _, cfg) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                sl <- source [];
                                changed <- source False;
                                ws <- old.tname2.FreshWidgets cfg;
                                return (s, ws0, sl, changed, ws),
                             WidgetsFrom = fn (cfg0, _, cfg) r (keys, aux) =>
                                s <- source "";
                                ws0 <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) => @Widget.create w cfg)
                                        fl ws cfg0;
                                keys <- List.mapM (fn (k, vs) => vs <- source vs; notEditing <- source None; return (k, vs, notEditing)) keys;
                                sl <- source keys;
                                changed <- source False;
                                ws <- old.tname2.WidgetsFrom cfg r aux;
                                return (s, ws0, sl, changed, ws),
                             RenderWidgets = fn k (cfgs, cfg1, cfg2) (s, ws0, sl, changed, wso) =>
                                                <xml>
                                                  {old.tname2.RenderWidgets k cfg2 wso}
                                                  <div class="form-group">
                                                    <label class="control-label">{[lab2]}</label>
                                                    <div class="input-group">
                                                      {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) (w : Widget.t' p) (s : p.2) =>
                                                            <xml>{[label]}: {@Widget.asWidget w s None}</xml>)
                                                        fl labels ws ws0}
                                                    </div>
                                                    <div class="input-group">
                                                      <span class="input-group-btn">
                                                        <button class="btn"
                                                                onclick={fn _ =>
                                                                            sv <- get s;
                                                                            if sv = "" then
                                                                                return ()
                                                                            else
                                                                                set changed False;
                                                                                sv <- return (readError sv);
                                                                                slv <- get sl;
                                                                                if List.exists (fn (sv', _, _) => sv' = sv) slv then
                                                                                    return ()
                                                                                else
                                                                                    vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                               current (@Widget.value w s))
                                                                                           fl ws ws0;
                                                                                    vs <- source vs;
                                                                                    notEditing <- source None;
                                                                                    set sl (List.sort (fn x y => x.1 > y.1) ((sv, vs, notEditing) :: slv))}>Select:</button>
                                                      </span>
                                                      <cselect class="form-control" source={s} onchange={set changed True}>
                                                        <coption/>
                                                        {List.mapX (fn s => <xml><coption>{[s]}</coption></xml>) cfg1}
                                                      </cselect> 
                                                    </div>
                                                    <table>
                                                      <tr><td/>
                                                        {@mapX [fn _ => string] [tr]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (label : string) =>
                                                              <xml><th>{[label]}</th></xml>)
                                                          fl labels}
                                                      <td/></tr>
                                                      <dyn signal={slv <- signal sl;
                                                                   return (List.mapX (fn (k2, vs, ws0) => <xml>
                                                                     <dyn signal={vals <- signal vs;
                                                                                  wids <- signal ws0;
                                                                                  case wids of
                                                                                      None => return <xml>
                                                                                        <tr>
                                                                                          <td>{[k2]}</td>
                                                                                          {@mapX2 [Widget.t'] [fst3] [tr]
                                                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : p.1) =>
                                                                                                <xml><td>{[@Widget.asValue w v]}</td></xml>)
                                                                                            fl ws vals}
                                                                                          <td><button class="btn"
                                                                                                      onclick={fn _ =>
                                                                                                                  wids <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (v : p.1) =>
                                                                                                                               @Widget.initialize w cfg v)
                                                                                                                           fl ws cfgs vals;
                                                                                                                  set ws0 (Some wids)}>
                                                                                            <span class="glyphicon glyphicon-pencil"/>
                                                                                          </button></td>
                                                                                          <td><button class="btn"
                                                                                                      onclick={fn _ => set sl (List.filter (fn (k2', _, _) => k2' <> k2) slv)}>
                                                                                            <span class="glyphicon glyphicon-remove"/>
                                                                                          </button></td>
                                                                                        </tr>
                                                                                      </xml>
                                                                                    | Some wids => return <xml>
                                                                                      <tr>
                                                                                        <td>{[k2]}</td>
                                                                                        {@mapX2 [Widget.t'] [snd3] [tr]
                                                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (s : p.2) =>
                                                                                              <xml><td>{@Widget.asWidget w s None}</td></xml>)
                                                                                          fl ws wids}
                                                                                        {if @Row.isEmpty fl then
                                                                                             <xml></xml>
                                                                                         else <xml>
                                                                                           <td><button class="btn"
                                                                                                       onclick={fn _ =>
                                                                                                                   vsv <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                                                                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (s : p.2) =>
                                                                                                                               current (@Widget.value w s))
                                                                                                                           fl ws wids;
                                                                                                                   set vs vsv;
                                                                                                                   set ws0 None}>
                                                                                             <span class="glyphicon glyphicon-check"/>
                                                                                           </button></td>
                                                                                           <td><button class="btn"
                                                                                                       onclick={fn _ => set ws0 None}>
                                                                                             <span class="glyphicon glyphicon-remove"/>
                                                                                           </button></td>
                                                                                         </xml>}
                                                                                      </tr>
                                                                                    </xml>}/>
                                                                   </xml>) slv)}/>
                                                    </table>
                                                </div>
                                              </xml>,
                           ReadWidgets = fn (s, _, sl, changed, ws) =>
                                            slv <- signal sl;
                                            slv <- List.mapM (fn (k2, vs, _) => vs <- signal vs; return (k2, vs)) slv;
                                            chd <- signal changed;
                                            (wsv, aux, err) <- old.tname2.ReadWidgets ws;
                                            return (wsv, (slv, aux),
                                                    if chd then
                                                        let
                                                            val msg = "The dropdown for \"" ^ lab2 ^ "\" has changed, but the new value hasn't been selected by pushing the adjacent button."
                                                        in
                                                            case err of
                                                                None => Some msg
                                                              | Some err => Some (err ^ "\n" ^ msg)
                                                        end
                                                    else
                                                        err)}}

type custom1 stash t = t
type custom2 stash t = source string * t
type custom3 stash t = option stash * t

fun custom [full ::: {Type}]
           [tname :: Name] [key ::: Type] [col :: Name] [colT ::: Type]
           [cols ::: {Type}] [colsDone ::: {Type}] [cstrs ::: {{Unit}}]
           [stash ::: Type]
           [impl1 ::: Type] [impl2 ::: Type] [impl3 ::: Type] [old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}]
           [[col] ~ cols] [[col] ~ colsDone] [[tname] ~ old]
           (lab : string) (_ : show colT) (_ : read colT)
           (content : colT -> transaction (option stash))
           (render : stash -> xtable)
           (dbchange : colT -> transaction unit)
           (old : t full ([tname = (key, [col = option colT] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)) =
    old -- tname
        ++ {tname = old.tname
                        -- #Auxiliary -- #Insert -- #Update -- #Render -- #FreshWidgets -- #WidgetsFrom -- #RenderWidgets -- #ReadWidgets
                        ++ {Auxiliary = fn k row =>
                                           aux1 <- (case row.col of
                                                        None => return None
                                                      | Some v => content v);
                                           aux2 <- old.tname.Auxiliary k row;
                                           return (aux1, aux2),
                            Insert = fn r (sto, aux) => old.tname.Insert r aux; Option.app dbchange r.col,
                            Update = fn k r (sto, aux) => old.tname.Update k r aux; Option.app dbchange r.col,
                            Render = fn entry (aux1, aux2) r => <xml>
                              {old.tname.Render entry aux2 r}
                              {case aux1 of
                                   None => <xml></xml>
                                 | Some aux1 => render aux1}
                            </xml>,
                            FreshWidgets = fn cfg =>
                               s <- source "";
                               ws <- old.tname.FreshWidgets cfg;
                               return (s, ws),
                            WidgetsFrom = fn cfg r (_, aux2) =>
                               s <- source (show r.col);
                               ws <- old.tname.WidgetsFrom cfg r aux2;
                               return (s, ws),
                            RenderWidgets = fn k cfg (s, ws) =>
                                               <xml>
                                                 {old.tname.RenderWidgets k cfg ws}
                                                 <div class="form-group">
                                                   <label class="control-label">{[lab]}</label>
                                                   <ctextbox class="form-control" source={s}/>
                                                 </div>
                                               </xml>,
                            ReadWidgets = fn (s, ws) =>
                                             v <- signal s;
                                             (wsv, aux, err) <- old.tname.ReadWidgets ws;
                                             return ({col = case v of
                                                                "" => None
                                                              | _ => Some (readError v)} ++ wsv, (None, aux), err)}}

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

                 con preTabs :: {Unit}
                 con postTabs :: {Unit}
                 con hiddenTabs :: {Unit}
                 constraint preTabs ~ postTabs
                 constraint (preTabs ++ postTabs) ~ hiddenTabs
                 val preTabs : $(mapU (string * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) preTabs)
                 val preFl : folder preTabs
                 val postTabs : $(mapU (string * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) postTabs)
                 val postFl : folder postTabs
                 val hiddenTabs : $(mapU (string * ((variant (mapU unit (preTabs ++ postTabs ++ hiddenTabs)) -> url) -> transaction xbody)) hiddenTabs)
                 val hiddenFl : folder hiddenTabs
                 constraint (preTabs ++ postTabs ++ hiddenTabs) ~ tables
             end) = struct
    open M
    open Ui.Make(Theme)

    type tag = variant (map (fn _ => unit) tables)
    con tabs' = map (fn _ => unit) tables ++ mapU unit (preTabs ++ postTabs)
    con tabs = tabs' ++ mapU unit hiddenTabs
    con tabsU' = map (fn _ => ()) tables ++ preTabs ++ postTabs
    con tabsU = tabsU' ++ hiddenTabs
    type tagPlus = variant tabs
    val eq_tag : eq tag = @Variant.eqU (@@Folder.mp [fn _ => ()] [_] fl)
    val eq_tagPlus : eq (variant tabs') = @Variant.eqU (@Folder.concat ! preFl (@Folder.concat ! (@@Folder.mp [fn _ => ()] [_] fl) postFl))

    con dupF (p :: (Type * {Type} * {{Unit}} * Type * Type * Type)) = (p.1, p.2, p.2, p.3, p.4, p.5, p.6)
    con dup = map dupF tables

    con tables' = map (fn p => p.1) tables

    val titleOf : variant tabs' -> string =
        @@Variant.proj [string] [tabsU']
          (@Folder.concat ! preFl
            (@Folder.concat ! (@Folder.mp fl) postFl))
          (@mp [fn _ => string * _] [fn _ => string]
            (fn [u] (titl, _) => titl) preFl preTabs
            ++ @mp [fn _ => string * _] [fn _ => string]
            (fn [u] (titl, _) => titl) postFl postTabs
            ++ @mp [t1 tables'] [fn _ => string]
            (fn [p] (r : t1 tables' p) => r.Title) (@@Folder.mp [dupF] [_] fl) t)

    val tabsFl = @Folder.concat ! postFl (@Folder.concat ! (@Folder.mp fl) preFl)

    fun tabbed (f : tagPlus -> transaction page)
               (which : option (variant tabs'))
        : (Ui.context -> transaction xbody) -> transaction page =
        @@tabbedStatic [tabsU'] tabsFl
          title
          (@@Variant.mp [tabsU'] [_] tabsFl
             (fn v => (titleOf v,
                       case which of
                           None => False
                         | Some which => @eq eq_tagPlus v which,
                       url (f (@Variant.weaken ! (@Folder.mp tabsFl) v)))))

    datatype editingState row widgets aux =
             NotEditing of row * aux
           | Editing of row * widgets
           | Deleted

    fun auth act =
        b <- authorize act;
        if b then
            return ()
        else
            error <xml>Access denied</xml>

    val weakener = @Variant.weaken ! (@Folder.mp fl)

    type anyKey = variant (map (fn p => p.1) tables)
    type anyRow = variant (map (fn p => $p.2 * p.6) tables)

    datatype indexDelta t =
             IndexAdd of t
           | IndexRename of t * t
           | IndexDelete of t
    type indexDeltaV = variant (map (fn p => indexDelta p.1) tables)
    table indexListeners : { Table : serialized tag, Channel : channel indexDeltaV }

    datatype entryDelta t =
             EntryUpdate of t
           | EntryDelete
           | EntryLock
           | EntryUnlock
    type entryDeltaV = variant (map (fn p => entryDelta ($p.2 * p.6)) tables)
    table entryListeners : { Key : serialized anyKey, Channel : channel entryDeltaV }
    table entryLocks : { Key : serialized anyKey, Owner : client }

    task clientLeaves = fn cl =>
                           queryI1 (SELECT entryListeners.*
                                    FROM entryListeners
                                      JOIN entryLocks ON entryLocks.Key = entryListeners.Key
                                    WHERE entryLocks.Owner = {[cl]})
                           (fn {Key = k, Channel = ch : channel entryDeltaV} =>
                               @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction unit] [tables]
                               (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables))
                                           _ (k : p.1) (r : t1 tables' (dupF p)) =>
                                   send ch (maker [fn p => entryDelta ($p.2 * p.6)] EntryUnlock))
                               fl (deserialize k) t)
                       
    fun page (which : tagPlus) =
        @match which
        (@@Variant.mp [map (fn _ => ()) tables] [_] (@Folder.mp fl) (fn v () => index v)
          ++ @Variant.mp preFl (fn v () => tabbed page (Some (@Variant.weaken ! (@Folder.mp preFl) v)) (fn _ => (@Variant.proj preFl preTabs v).2 (fn v' => url (page (@Variant.weaken ! (@Folder.mp (@Folder.concat ! hiddenFl (@Folder.concat ! preFl postFl))) v')))))
          ++ @Variant.mp postFl (fn v () => tabbed page (Some (@Variant.weaken ! (@Folder.mp postFl) v)) (fn _ => (@Variant.proj postFl postTabs v).2 (fn v' => url (page (@Variant.weaken ! (@Folder.mp (@Folder.concat ! hiddenFl (@Folder.concat ! preFl postFl))) v')))))
          ++ @Variant.mp hiddenFl (fn v () => tabbed page None (fn _ => (@Variant.proj hiddenFl hiddenTabs v).2 (fn v' => url (page (@Variant.weaken ! (@Folder.mp (@Folder.concat ! hiddenFl (@Folder.concat ! preFl postFl))) v'))))))

    and index (which : tag) =
        auth (Read which);

        ch <- channel;
        dml (INSERT INTO indexListeners(Table, Channel)
             VALUES ({[serialize which]}, {[ch]}));
        
        mayAdd <- authorize (Create which);
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
          (fn [p ::_]
              (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables))
              (dester : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> variant (map tf tables) -> option (tf p))
              () r =>
              extra <- r.Extra;
              rows <- r.ForIndex;
              rows <- source rows;
              return <xml>
                <active code={let
                                  fun loop () =
                                      msg <- recv ch;
                                      case dester [fn p => indexDelta p.1] msg of
                                          None => error <xml>Wrong type of key arrived at index listener.</xml>
                                        | Some (IndexAdd newKey) =>
                                          rs <- get rows;
                                          set rows (@List.assocAddSorted r.Eq r.Ord newKey (@txt r.Show newKey) rs);
                                          loop ()
                                        | Some (IndexRename (oldKey, newKey)) =>
                                          rs <- get rows;
                                          rs <- return (List.filter (fn (k, _) => not (@eq r.Eq k oldKey)) rs);
                                          set rows (@List.assocAddSorted r.Eq r.Ord newKey (@txt r.Show newKey) rs);
                                          loop ()
                                        | Some (IndexDelete key) =>
                                          rs <- get rows;
                                          set rows (List.filter (fn (k, _) => not (@eq r.Eq k key)) rs);
                                          loop ()
                              in
                                  spawn (loop ());
                                  return <xml></xml>
                               end}/>

                {extra}

                <table class="bs-table table-striped">
                  <dyn signal={rows <- signal rows;
                               return (List.mapX (fn (k, bod) => <xml><tr><td><a link={entry (maker [fn p => p.1] k)}>{bod}</a></td></tr></xml>) rows)}/>
                </table>

                {if mayAdd then
                     <xml><a class="btn btn-primary" link={create which}>New Entry</a></xml>
                 else
                     <xml></xml>}
              </xml>)
          fl which t;
        tabbed page (Some (weakener which)) (fn _ => return bod)

    and create (which : tag) =
        auth (Create which);
        bod <- @@Variant.destrR' [fn _ => unit] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) _ () r =>
              cfg <- r.Config;
              ws <- r.FreshWidgets cfg;
              return <xml>
                <h1>Create {[r.Title]}</h1>

                {r.RenderWidgets None cfg ws}

                <button value="Create" class="btn btn-primary"
                        onclick={fn _ =>
                                    (p1, p2, err) <- current (r.ReadWidgets ws);
                                    proceed <- (case err of
                                                  None => return True
                                                | Some msg => confirm ("Are you sure you want to proceed with creating that entry?  The following issues were noted:\n\n" ^ msg));
                                    if proceed then
                                        rpc (doCreate (maker [fn p => $p.2 * p.6] (p1, p2)));
                                        redirect (url (index which))
                                    else
                                        return ()}/>
              </xml>)
          fl which t;
        tabbed page (Some (weakener which)) (fn _ => return bod)

    and doCreate (which : variant (map (fn p => $p.2 * p.7) dup)) =
        auth (Create (@Variant.erase (@Folder.mp fl) which));
        @@Variant.destrR' [fn p => $p.2 * p.7] [t1 tables'] [transaction unit] [dup]
          (fn [p ::_] (maker : tf :: ((Type * {Type} * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf dup))
                      _ (vs : $p.2, aux : p.7) r =>
              r.Insert vs aux;
              queryI1 (SELECT indexListeners.Channel
                       FROM indexListeners
                       WHERE indexListeners.Table = {[serialize (maker [fn p => unit] ())]})
              (fn {Channel = ch} => send ch (maker [fn p => indexDelta p.1] (IndexAdd (r.KeyOf vs)))))
          (@Folder.mp fl) which t

    and entry (which : anyKey) =
        auth (Read (@Variant.erase (@Folder.mp fl) which));

        ch <- channel;
        dml (INSERT INTO entryListeners(Key, Channel)
             VALUES ({[serialize which]}, {[ch]}));
        
        mayUpdate <- authorize (Update which);
        mayDelete <- authorize (Delete which);
        (ctx : source (option Ui.context)) <- source None;
        bod <- @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction xbody] [tables]
               (fn [p ::_] (maker : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables))
                           (dester : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> variant (map tf tables) -> option (tf p))
                           (k : p.1) (r : t1 tables' (dupF p)) =>
              let
                  val tab = r.Table
              in
                  curKey <- source k;
                  cfg <- r.Config;
                  row <- oneRow1 (SELECT *
                                  FROM tab
                                  WHERE {r.KeyIs [#Tab] k});
                  aux <- r.Auxiliary k row;

                  locks <- oneRowE1 (SELECT COUNT( * )
                                     FROM entryLocks
                                     WHERE entryLocks.Key = {[serialize (maker [fn p => p.1] k)]});
                  locks <- source locks;

                  est <- source (NotEditing (row, aux));
                  return <xml>
                    <active code={let
                                      fun loop () =
                                          upd <- recv ch;
                                          case dester [fn p => entryDelta ($p.2 * p.6)] upd of
                                              None => error <xml>Entry update is for the wrong table.</xml>
                                            | Some (EntryUpdate (row, aux)) =>
                                              set curKey (r.KeyOf row);
                                              set est (NotEditing (row, aux));
                                              n <- get locks;
                                              set locks (n - 1);
                                              loop ()
                                            | Some EntryDelete =>
                                              set est Deleted
                                            | Some EntryLock =>
                                              n <- get locks;
                                              set locks (n + 1);
                                              loop ()
                                            | Some EntryUnlock =>
                                              n <- get locks;
                                              set locks (n - 1);
                                              loop ()
                                  in
                                      spawn (loop ());
                                      return <xml></xml>
                                  end}/>

                    <dyn signal={esta <- signal est;
                                 case esta of
                                     NotEditing (row, aux) =>
                                     ctx <- signal ctx;
                                     (case ctx of
                                         None => return <xml></xml>
                                       | Some ctx => return <xml>
                                         <p>
                                           {if mayUpdate then
                                                <xml>
                                                  <button dynClass={n <- signal locks;
                                                                    return (if n = 0 then
                                                                                CLASS "btn btn-primary"
                                                                            else
                                                                                CLASS "btn btn-danger")}
                                                          onclick={fn _ =>
                                                                      approved <- rpc (edit (maker [fn p => p.1] (r.KeyOf row)) False);
                                                                      if approved then
                                                                          ws <- r.WidgetsFrom cfg row aux;
                                                                          set est (Editing (row, ws))
                                                                      else
                                                                          proceed <- confirm "Warning: that entry is already open for editing elsewhere!  Do you want to edit it anyway?";
                                                                          if not proceed then
                                                                              return ()
                                                                          else
                                                                              approved <- rpc (edit (maker [fn p => p.1] (r.KeyOf row)) True);
                                                                              if not approved then
                                                                                  error <xml>Override was rejected!</xml>
                                                                              else
                                                                                  ws <- r.WidgetsFrom cfg row aux;
                                                                                  set est (Editing (row, ws))}>Edit</button>
                                                </xml>
                                            else
                                                <xml/>}
                                           {if mayDelete then
                                                <xml>
                                                  <dyn signal={n <- signal locks;
                                                               return (Ui.modalButton ctx (if n = 0 then CLASS "btn" else CLASS "btn btn-danger")
                                                                                      <xml>Delete</xml>
                                                                                      (return (Ui.modal (ck <- get curKey;
                                                                                                         worked <- rpc (delete (maker [fn p => p.1] ck) False);
                                                                                                         worked <- (if worked then
                                                                                                                        return True
                                                                                                                    else
                                                                                                                        proceed <- confirm "Warning: that entry is open for editing elsewhere!  Do you want to delete it anyway?";
                                                                                                                        if proceed then
                                                                                                                            rpc (delete (maker [fn p => p.1] ck) True)
                                                                                                                        else
                                                                                                                            return False);
                                                                                                         if worked then
                                                                                                             redirect (url (index (maker [fn _ => unit] ())))
                                                                                                         else
                                                                                                             return ())
                                                                                                        <xml>Are you sure you want to delete this entry?</xml>
                                                                                                        <xml></xml>
                                                                                                        <xml>Yes, delete it.</xml>)))}/>
                                                </xml>
                                            else
                                                <xml/>}
                                           </p>

                                         <table class="bs-table table-striped">
                                           {r.Render (fn key text => <xml><a link={entry key}>{[text]}</a></xml>) aux row}
                                         </table>
                                      </xml>)
                                   | Editing (row, ws) => return <xml>
                                     <p>
                                       <button class="btn btn-primary"
                                               onclick={fn _ =>
                                                           (row1, row2, err) <- current (r.ReadWidgets ws);
                                                           proceed <- (case err of
                                                                           None => return True
                                                                         | Some msg => confirm ("Are you sure you want to proceed with saving that entry?  The following issues were noted:\n\n" ^ msg));
                                                           if proceed then
                                                               ck <- get curKey;
                                                               rpc (save (maker [fn p => p.1 * $p.2 * p.6] (ck, row1, row2)));
                                                               set est (NotEditing (row1, row2))
                                                           else
                                                               return ()}>Save</button>
                                       <button class="btn"
                                               onclick={fn _ => rpc (unedit (maker [fn p => p.1] (r.KeyOf row)));
                                                           set est (NotEditing (row, aux))}>Cancel</button>
                                     </p>

                                     {r.RenderWidgets (Some k) cfg ws}
                                   </xml>
                                   | Deleted => return <xml>
                                     That entry has been deleted.
                                     Perhaps you would like to return to <a link={index (maker [fn _ => unit] ())}>the {[r.Title]} index</a>?
                                   </xml>
                                }/>
                  </xml>
              end)
          fl which t;

        tabbed page (Some (weakener (@Variant.erase (@Folder.mp fl) which))) (fn ctxv => return <xml>
          <active code={set ctx (Some ctxv); return <xml></xml>}/>
          {bod}
        </xml>)

    and edit (which : anyKey) (override : bool) =
        auth (Update which);
        conflict <- (if override then
                         return False
                     else
                         oneRowE1 (SELECT COUNT( * ) > 0
                                   FROM entryLocks
                                   WHERE entryLocks.Key = {[serialize which]}));
        if conflict then
            return False
        else
            cl <- self;

            alreadyLocked <- oneRowE1 (SELECT COUNT( * ) > 0
                                       FROM entryLocks
                                       WHERE entryLocks.Key = {[serialize which]}
                                         AND entryLocks.Owner = {[cl]});
            if alreadyLocked then
                return False
            else
                dml (INSERT INTO entryLocks(Key, Owner)
                     VALUES ({[serialize which]}, {[cl]}));
                @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction unit] [tables]
                  (fn [p ::_] (mk : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) _ (k : p.1) r =>
                      queryI1 (SELECT entryListeners.Channel
                               FROM entryListeners
                               WHERE entryListeners.Key = {[serialize which]})
                              (fn {Channel = ch} => send ch (mk [fn p => entryDelta ($p.2 * p.6)] EntryLock)))
                  fl which t;
                return True

    and unedit (which : anyKey) =
        auth (Update which);
        cl <- self;
        conflict <- oneRowE1 (SELECT COUNT( * ) > 0
                              FROM entryLocks
                              WHERE entryLocks.Key = {[serialize which]}
                                AND entryLocks.Owner = {[cl]});
        if not conflict then
            return ()
        else
            dml (DELETE FROM entryLocks
                 WHERE Owner = {[cl]});
            @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction unit] [tables]
            (fn [p ::_] (mk : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) _ (k : p.1) r =>
                queryI1 (SELECT entryListeners.Channel
                         FROM entryListeners
                         WHERE entryListeners.Key = {[serialize which]})
                        (fn {Channel = ch} => send ch (mk [fn p => entryDelta ($p.2 * p.6)] EntryUnlock)))
            fl which t

    and save (which : variant (map (fn p => p.1 * $p.2 * p.6) tables)) =
        @@Variant.destrR' [fn p => p.1 * $p.2 * p.6] [fn p => t1 tables' (dupF p)] [transaction unit] [tables]
          (fn [p ::_] (mk : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables)) _ (k : p.1, vs : $p.2, aux : p.6) r =>
              auth (Update (mk [fn p => p.1] k));

              queryI1 (SELECT entryListeners.Channel
                       FROM entryListeners
                       WHERE entryListeners.Key = {[serialize (mk [fn p => p.1] k)]})
              (fn {Channel = ch} => send ch (mk [fn p => entryDelta ($p.2 * p.6)] (EntryUpdate (vs, aux))));
              
              dml (DELETE FROM entryLocks
                   WHERE Key = {[serialize (mk [fn p => p.1] k)]});
              
              r.Update k vs aux;

              if @eq r.Eq k (r.KeyOf vs) then
                  return ()
              else
                  queryI1 (SELECT indexListeners.Channel
                           FROM indexListeners
                           WHERE indexListeners.Table = {[serialize (mk [fn _ => unit] ())]})
                          (fn {Channel = ch} => send ch (mk [fn p => indexDelta p.1] (IndexRename (k, r.KeyOf vs))));
                  
                  dml (UPDATE entryListeners
                       SET Key = {[serialize (mk [fn p => p.1] (r.KeyOf vs))]}
                       WHERE Key = {[serialize (mk [fn p => p.1] k)]}))
          fl which t

    and delete (which : anyKey) (override : bool) =
        auth (Delete which);
        conflict <- (if override then
                         return False
                     else
                         oneRowE1 (SELECT COUNT( * ) > 0
                                   FROM entryLocks
                                   WHERE entryLocks.Key = {[serialize which]}));
        if conflict then
            return False
        else
            @@Variant.destrR' [fn p => p.1] [fn p => t1 tables' (dupF p)] [transaction unit] [tables]
              (fn [p ::_] (mk : tf :: ((Type * {Type} * {{Unit}} * Type * Type * Type) -> Type) -> tf p -> variant (map tf tables))
                          _ (k : p.1) (r : t1 tables' (dupF p)) =>
                  let
                      val tab = r.Table
                  in
                      r.Delete k;

                      queryI1 (SELECT indexListeners.Channel
                               FROM indexListeners
                               WHERE indexListeners.Table = {[serialize (mk [fn _ => unit] ())]})
                              (fn {Channel = ch} => send ch (mk [fn p => indexDelta p.1] (IndexDelete k)));

                      queryI1 (SELECT entryListeners.Channel
                               FROM entryListeners
                               WHERE entryListeners.Key = {[serialize which]})
                              (fn {Channel = ch} => send ch (mk [fn p => entryDelta ($p.2 * p.6)] EntryDelete));

                      dml (DELETE FROM entryListeners
                           WHERE Key = {[serialize which]});

                      dml (DELETE FROM tab
                           WHERE {r.KeyIs [#T] k})
                  end)
              fl which t;
            return True

    val tableNames = @mp [fn p => t1 tables' (dupF p)] [fn _ => string]
                      (fn [p :::_] (t : t1 tables' (dupF p)) => t.Title)
                      fl t

end
