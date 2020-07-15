open Bootstrap4

(* One of these is all about generating content table headers and rows *)
type t (inp :: Type) (r :: {Type}) (cfg :: Type) (st :: Type) = {
     (* Run on server: *)
     Configure : transaction cfg,
     Generate : cfg -> $r -> transaction st,
     Filter : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     FilterLinks : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     SortBy : sql_order_by [Tab = r] [] -> sql_order_by [Tab = r] [],
     ModifyBeforeCreate : cfg -> inp -> $r -> $r,
     OnCreate : cfg -> inp -> $r -> transaction unit,

     (* Run on client: *)
     OnLoad : {ReloadState : transaction unit} -> cfg -> transaction unit,
     GenerateLocal : cfg -> inp -> transaction st,
     WidgetForCreate : cfg -> st -> xbody,
     OnCreateLocal : $r -> st -> transaction unit,
     Header : cfg -> xtr,
     Row : cfg -> Ui.context -> st -> xtr,
     Todos : cfg -> st -> signal int
}

type inputIs_cfg = unit
type inputIs_st = unit
fun inputIs [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ inp r => r -- col ++ {col = inp},
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type inputIsOpt_cfg = unit
type inputIsOpt_st = unit
fun inputIsOpt [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable_prim inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[Some inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ inp r => r -- col ++ {col = Some inp},
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type inputConnected_cfg = unit
type inputConnected_st = unit
fun inputConnected [inp] [key :: Name] [keyT ::: Type] [r ::: {Type}] [ckey :: Name]
    [inpCol :: Name] [cothers ::: {Type}] [ckeys ::: {{Unit}}]
    [[key] ~ r] [[ckey] ~ [inpCol]] [[ckey, inpCol] ~ cothers]
    (_ : sql_injectable inp)
    (ctr : sql_table ([ckey = keyT, inpCol = inp] ++ cothers) ckeys) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE NOT ((SELECT TRUE
                                            FROM ctr
                                            WHERE ctr.{ckey} = tab.{key}
                                              AND ctr.{inpCol} = {[inp]}) IS NULL)),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type inputConnects_cfg = unit
type inputConnects_st = unit
fun inputConnects [inp] [key :: Name] [keyT ::: Type] [r ::: {Type}]
                  [ckey :: Name] [inpCol :: Name] [ckeys ::: {{Unit}}]
                  [[key] ~ r] [[ckey] ~ [inpCol]]
                  (_ : sql_injectable keyT) (_ : sql_injectable inp)
                  (ctr : sql_table [ckey = keyT, inpCol = inp] ckeys) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () _ => None,
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn () inp r =>
                  dml (DELETE FROM ctr
                       WHERE T.{ckey} = {[r.key]}
                         AND T.{inpCol} = {[inp]});
                  dml (INSERT INTO ctr({ckey}, {inpCol})
                                   VALUES ({[r.key]}, {[inp]})),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

val empty [inp] [r] = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () _ => None,
    FilterLinks = fn () _ => None,
    SortBy = fn sb => sb,
    ModifyBeforeCreate = fn _ _ r => r,
    OnLoad = fn _ () => return (),
    OnCreate = fn () _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn () () => <xml></xml>,
    OnCreateLocal = fn _ () => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ () => <xml></xml>,
    Todos = fn () () => return 0
}

fun compose [inp] [r] [cfgb] [cfga] [stb] [sta] (b : t inp r cfgb stb) (a : t inp r cfga sta) = {
    Configure = cfga <- a.Configure;
                cfgb <- b.Configure;
                return (cfgb, cfga),
    Generate = fn (cfgb, cfga) r =>
                  sta <- a.Generate cfga r;
                  stb <- b.Generate cfgb r;
                  return (stb, sta),
    Filter = fn (cfgb, cfga) inp => case (a.Filter cfga inp, b.Filter cfgb inp) of
                                        (None, x) => x
                                      | (x, None) => x
                                      | (Some x, Some y) => Some (WHERE {x} AND {y}),
    FilterLinks = fn (cfgb, cfga) inp => case (a.FilterLinks cfga inp, b.FilterLinks cfgb inp) of
                                             (None, x) => x
                                           | (x, None) => x
                                           | (Some x, Some y) => Some (WHERE {x} OR {y}),
    SortBy = fn sb => b.SortBy (a.SortBy sb),
    ModifyBeforeCreate = fn (cfgb, cfga) inp r => a.ModifyBeforeCreate cfga inp (b.ModifyBeforeCreate cfgb inp r),
    OnLoad = fn fs (cfgb, cfga) => b.OnLoad fs cfgb; a.OnLoad fs cfga,
    OnCreate = fn (cfgb, cfga) inp r => b.OnCreate cfgb inp r; a.OnCreate cfga inp r,
    GenerateLocal = fn (cfgb, cfga) inp => b <- b.GenerateLocal cfgb inp; a <- a.GenerateLocal cfga inp; return (b, a),
    WidgetForCreate = fn (cfgb, cfga) (y, x) => <xml>{b.WidgetForCreate cfgb y}{a.WidgetForCreate cfga x}</xml>,
    OnCreateLocal = fn r (y, x) => b.OnCreateLocal r y; a.OnCreateLocal r x,
    Header = fn (cfgb, cfga) => <xml>{b.Header cfgb}{a.Header cfga}</xml>,
    Row = fn (cfgb, cfga) ctx (y, x) => <xml>{b.Row cfgb ctx y}{a.Row cfga ctx x}</xml>,
    Todos = fn (cfgb, cfga) (y, x) => nb <- b.Todos cfgb y; na <- a.Todos cfga x; return (nb + na)
}

type column_cfg (t :: Type) = unit
type column_st (t :: Type) = option t
fun column [inp ::: Type] [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
           (_ : show colT) (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return (Some r.col),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return None,
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () _ v => <xml><td>{[v]}</td></xml>,
    Todos = fn _ _ => return 0
}

type html_cfg = unit
type html_st = xbody
fun html [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return (Widget.html r.col),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return <xml></xml>,
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () _ v => <xml><td>{v}</td></xml>,
    Todos = fn _ _ => return 0
}

type checkmarks_cfg = unit
type checkmarks_st = bool
fun checkmarks [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return (Option.get False r.col),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL COALESCE(tab.{col}, FALSE)) sql_asc,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return False,
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () _ v => <xml><td>{if v then
                                     <xml><i class="glyphicon glyphicon-check"/></xml>
                                 else
                                     <xml></xml>}</td></xml>,
    Todos = fn _ _ => return 0
}

type iconButton_cfg (cols :: {Type}) = option string * time
type iconButton_st (cols :: {Type}) = option $cols
fun iconButton [inp ::: Type] [cols ::: {Type}] [r ::: {Type}] [cols ~ r]
    (whoami : transaction (option string))
    (render : option string -> time -> $cols -> option (css_class * url))
    (lbl : string) = {
    Configure = u <- whoami; tm <- now; return (u, tm),
    Generate = fn _ r => return (Some (r --- r)),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn _ _ => return None,
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml><th>{[lbl]}</th></xml>,
    Row = fn (u, tm) _ cols =>
             case cols of
                 None => error <xml>Missing iconButton columns</xml>
               | Some cols =>
                 case render u tm cols of
                     None => <xml><td/></xml>
                   | Some (cl, ur) => <xml><td>
                     <a href={ur}
                     class={classes cl (CLASS "btn btn-primary btn-lg glyphicon")}/>
                   </td></xml>,
    Todos = fn _ _ => return 0
}

fun links [a] (_ : show a) (ls : list a) : xbody = <xml>
  {List.mapX (fn x => <xml> <span class="badge badge-pill badge-info">{[x]}</span></xml>) ls}
</xml>

type linked_cfg (t :: Type) = unit
type linked_st (t :: Type) = list t
fun linked [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr ::: {Type}] [ks ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [fthat]] [[fthis, fthat] ~ fr]
    ( _ : show thatT) (_ : sql_injectable thisT)
    (tab : sql_table ([fthis = thisT, fthat = thatT] ++ fr) ks)
    (l : string) = {
    Configure = return (),
    Generate = fn () r => List.mapQuery (SELECT tab.{fthat}
                                         FROM tab
                                         WHERE tab.{fthis} = {[r.this]}
                                         ORDER BY tab.{fthat})
                                        (fn r => r.Tab.fthat),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{links ls}</td></xml>,
    Todos = fn _ _ => return 0
}

fun weightedLinks [a] (_ : show a) (ls : list (a * int)) : xbody = <xml>
  {List.mapX (fn (x, n) => <xml> <span class="badge badge-pill badge-info">{[x]}{[case n of
                                                                                      1 => ""
                                                                                    | _ => " x" ^ show n]}</span></xml>) ls}
</xml>

type doubleLinked_cfg (t :: Type) = unit
type doubleLinked_st (t :: Type) = list (t * int)
fun doubleLinked [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [finterm1 :: Name] [finterm2 :: Name] [intermT ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr1 ::: {Type}] [fr2 ::: {Type}]
    [ks1 ::: {{Unit}}] [ks2 ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [finterm1]] [[fthis, finterm1] ~ fr1]
    [[finterm2] ~ [fthat]] [[finterm2, fthat] ~ fr2]
    (_ : show thatT) (_ : sql_injectable thisT)
    (tab1 : sql_table ([fthis = thisT, finterm1 = intermT] ++ fr1) ks1)
    (tab2 : sql_table ([finterm2 = intermT, fthat = thatT] ++ fr2) ks2)
    (l : string) = {
    Configure = return (),
    Generate = fn () r => List.mapQuery (SELECT tab2.{fthat}, COUNT( * ) AS Count
                                         FROM tab1 JOIN tab2
                                           ON tab1.{finterm1} = tab2.{finterm2}
                                         WHERE tab1.{fthis} = {[r.this]}
                                         GROUP BY tab2.{fthat}
                                         ORDER BY Count DESC, tab2.{fthat})
                                        (fn r => (r.Tab2.fthat, r.Count)),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{weightedLinks ls}</td></xml>,
    Todos = fn _ _ => return 0
}

type tripleLinked_cfg (t :: Type) = unit
type tripleLinked_st (t :: Type) = list (t * int)
fun tripleLinked [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [finterm1 :: Name] [finterm2 :: Name] [finterm3 :: Name] [finterm4 :: Name]
    [intermT1 ::: Type] [intermT2 ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr1 ::: {Type}] [fr2 ::: {Type}] [fr3 ::: {Type}]
    [ks1 ::: {{Unit}}] [ks2 ::: {{Unit}}] [ks3 ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [finterm1]] [[fthis, finterm1] ~ fr1]
    [[finterm2] ~ [finterm3]] [[finterm2, finterm3] ~ fr2]
    [[finterm4] ~ [fthat]] [[finterm4, fthat] ~ fr3]
    (_ : show thatT) (_ : sql_injectable thisT)
    (tab1 : sql_table ([fthis = thisT, finterm1 = intermT1] ++ fr1) ks1)
    (tab2 : sql_table ([finterm2 = intermT1, finterm3 = intermT2] ++ fr2) ks2)
    (tab3 : sql_table ([finterm4 = intermT2, fthat = thatT] ++ fr3) ks3)
    (l : string) = {
    Configure = return (),
    Generate = fn () r => List.mapQuery (SELECT tab3.{fthat}, COUNT( * ) AS Count
                                         FROM tab1 JOIN tab2
                                           ON tab1.{finterm1} = tab2.{finterm2}
                                           JOIN tab3
                                           ON tab2.{finterm3} = tab3.{finterm4}
                                         WHERE tab1.{fthis} = {[r.this]}
                                         GROUP BY tab3.{fthat}
                                         ORDER BY Count DESC, tab3.{fthat})
                                        (fn r => (r.Tab3.fthat, r.Count)),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{weightedLinks ls}</td></xml>,
    Todos = fn _ _ => return 0
}

type orderedLinked_cfg (t :: Type) = unit
type orderedLinked_st (t :: Type) = list t
fun orderedLinked [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr ::: {Type}] [ks ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [fthat]] [[fthis, fthat] ~ [SeqNum]] [[fthis, fthat, SeqNum] ~ fr]
    ( _ : show thatT) (_ : sql_injectable thisT)
    (tab : sql_table ([fthis = thisT, fthat = thatT, SeqNum = int] ++ fr) ks)
    (l : string) = {
    Configure = return (),
    Generate = fn () r => List.mapQuery (SELECT tab.{fthat}
                                         FROM tab
                                         WHERE tab.{fthis} = {[r.this]}
                                         ORDER BY tab.SeqNum)
                                        (fn r => r.Tab.fthat),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{links ls}</td></xml>,
    Todos = fn _ _ => return 0
}

fun linksWithUrls [a] (_ : show a) (ls : list (a * option url)) : xbody = <xml>
  {List.mapX (fn (x, uo) =>
                 case uo of
                     None => <xml> <span class="badge badge-pill badge-info">{[x]}</span></xml>
                   | Some u => <xml> <a class="badge badge-pill badge-info" href={u}>{[x]}</a></xml>) ls}
</xml>

type linkedWithUrl_cfg (t :: Type) = ChangeWatcher.client_part
type linkedWithUrl_st (t :: Type) = list (t * option url)
fun linkedWithUrl [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr ::: {Type}] [ks ::: {{Unit}}]
    [lthat :: Name] [lurl :: Name] [lr ::: {Type}] [lks ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [fthat]] [[fthis, fthat] ~ fr]
    [[lthat] ~ [lurl]] [[lthat, lurl] ~ lr]
    ( _ : show thatT) (_ : sql_injectable thisT)
    (tab : sql_table ([fthis = thisT, fthat = thatT] ++ fr) ks)
    (ltab : sql_table ([lthat = thatT, lurl = string] ++ lr) lks)
    (title : string) (l : string) = {
    Configure = ChangeWatcher.listen title,
    Generate = fn _ r => List.mapQuery (SELECT tab.{fthat}, ltab.{lurl}
                                         FROM tab JOIN ltab
                                           ON tab.{fthat} = ltab.{lthat}
                                         WHERE tab.{fthis} = {[r.this]}
                                         ORDER BY tab.{fthat})
                                        (fn r => (r.Tab.fthat, checkUrl r.Ltab.lurl)),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn fs ch => ChangeWatcher.onChange ch fs.ReloadState,
    GenerateLocal = fn _ _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml><th>{[l]}</th></xml>,
    Row = fn _ _ ls => <xml><td>{linksWithUrls ls}</td></xml>,
    Todos = fn _ _ => return 0
}

type orderedLinkedWithUrl_cfg (t :: Type) = unit
type orderedLinkedWithUrl_st (t :: Type) = list (t * option url)
fun orderedLinkedWithUrl [inp ::: Type] [this :: Name] [fthis :: Name] [thisT ::: Type]
    [fthat :: Name] [thatT ::: Type]
    [r ::: {Type}] [fr ::: {Type}] [ks ::: {{Unit}}]
    [lthat :: Name] [lurl :: Name] [lr ::: {Type}] [lks ::: {{Unit}}]
    [[this] ~ r] [[fthis] ~ [fthat]] [[fthis, fthat] ~ [SeqNum]] [[fthis, fthat, SeqNum] ~ fr]
    [[lthat] ~ [lurl]] [[lthat, lurl] ~ lr]
    ( _ : show thatT) (_ : sql_injectable thisT)
    (tab : sql_table ([fthis = thisT, fthat = thatT, SeqNum = int] ++ fr) ks)
    (ltab : sql_table ([lthat = thatT, lurl = string] ++ lr) lks)
    (l : string) = {
    Configure = return (),
    Generate = fn () r => List.mapQuery (SELECT tab.{fthat}, ltab.{lurl}
                                         FROM tab JOIN ltab
                                           ON tab.{fthat} = ltab.{lthat}
                                         WHERE tab.{fthis} = {[r.this]}
                                         ORDER BY tab.SeqNum)
                                        (fn r => (r.Tab.fthat, checkUrl r.Ltab.lurl)),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return [],
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{linksWithUrls ls}</td></xml>,
    Todos = fn _ _ => return 0
}

functor LinkedWithEdit(M : sig
                           type inp
                           con this :: Name
                           con fthis :: Name
                           con thisT :: Type
                           con fthat :: Name
                           con thatT :: Type
                           con r :: {Type}
                           constraint [this] ~ r
                           constraint [fthis] ~ [fthat]
                           val show_that : show thatT
                           val read_that : read thatT
                           val eq_that : eq thatT
                           val inj_this : sql_injectable thisT
                           val inj_that : sql_injectable thatT
                           table link : {fthis : thisT, fthat : thatT}
                           val title : string

                           con tkey :: Name
                           con tr :: {Type}
                           constraint [tkey] ~ tr
                           table that : ([tkey = thatT] ++ tr)
                           val thatTitle : string

                           val label : string
                           val authorized : transaction bool
                       end) = struct
    open M

    type cfg = option (list thatT * ChangeWatcher.client_part) (* if present, allowed to add *)
    type internal = option thisT * source (list thatT)

    val changed = ChangeWatcher.changed title

    fun add k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]});
            dml (INSERT INTO link({fthis}, {fthat})
                 VALUES ({[k]}, {[v]}));
            changed

    fun addAll k vs =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            List.app (fn v =>
                         dml (DELETE FROM link
                              WHERE T.{fthis} = {[k]}
                                AND T.{fthat} = {[v]});
                         dml (INSERT INTO link({fthis}, {fthat})
                              VALUES ({[k]}, {[v]}))) vs;
            changed

    fun remove k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]});
            changed

    val savedLabel = label
    val label = Basis.label

    val t = {
        Configure = authed <- authorized;
          if not authed then
              return None
          else
              ts <- List.mapQuery (SELECT that.{tkey}
                                   FROM that
                                   ORDER BY that.{tkey})
                                  (fn r => r.That.tkey);
              ch <- ChangeWatcher.listen thatTitle;
              return (Some (ts, ch)),
        Generate = fn _ r =>
                      ls <- List.mapQuery (SELECT link.{fthat}
                                            FROM link
                                            WHERE link.{fthis} = {[r.this]})
                                          (fn r => r.Link.fthat);
                      ls <- source ls;
                      return (Some r.this, ls),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs authed =>
                    case authed of
                        None => return ()
                      | Some (_, ch) => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ _ => ls <- source []; return (None, ls),
        WidgetForCreate = fn ts (_, ls) =>
          case ts of
              None => <xml></xml>
            | Some (ts, _) =>
              <xml>
                <div class="form-group">
                  <label class="control-label">{[savedLabel]}</label>
                  <active code={lsV <- get ls;
                                s2 <- Select2.create (List.mapX (fn t =>
                                                                    if List.mem t lsV then
                                                                        <xml><coption selected={True}>{[t]}</coption></xml>
                                                                    else
                                                                        <xml><coption>{[t]}</coption></xml>) ts);
                                return <xml>
                                  {Select2.render s2}
                                  <dyn signal={seled <- Select2.selected s2;
                                               return <xml>
                                                 <active code={set ls (List.mp readError seled);
                                                               return <xml></xml>}/>
                                               </xml>}/>
                                </xml>}/>
                </div>
              </xml>,
        OnCreateLocal = fn r (_, ls) =>
                           ls <- get ls;
                           case ls of
                               [] => return ()
                             | _ => rpc (addAll r.this ls),
        Header = fn _ => <xml><th>{[savedLabel]}</th></xml>,
        Row = fn authed ctx (k, ls) => let
                     fun one v = <xml>
                       <span class="badge badge-pill badge-info p-2">
                         {[v]}
                         {case authed of
                              None => <xml></xml>
                            | Some _ => <xml>
                              <span class="text-white" style="cursor: pointer"
                                    onclick={fn _ =>
                                                case k of
                                                    None => error <xml>Missing self for buttons</xml>
                                                  | Some k =>
                                                    rpc (remove k v);
                                                    lsV <- get ls;
                                                    set ls (List.filter (fn v' => v' <> v) lsV)}>
                                &times;
                              </span>
                            </xml>}
                       </span>
                     </xml>
                 in
                     <xml><td>
                       <dyn signal={ls <- signal ls; return (List.mapX one ls)}/>
                       {case authed of
                            None => <xml></xml>
                          | Some (ts, _) =>
                            case k of
                                None => error <xml>Missing self for buttons</xml>
                              | Some k =>
                                Ui.modalIcon ctx
                                             (CLASS "glyphicon glyphicon-pencil-alt")
                                             (lsV <- get ls;
                                              s2 <- Select2.create (List.mapX (fn t =>
                                                                        if List.mem t lsV then
                                                                            <xml><coption selected={True}>{[t]}</coption></xml>
                                                                        else
                                                                            <xml><coption>{[t]}</coption></xml>) ts);
                                              return (Ui.modal (seled <- current (Select2.selected s2);
                                                                seled <- return (List.mp readError seled);
                                                                List.app (fn selectedNow =>
                                                                             if List.mem selectedNow lsV then
                                                                                 return ()
                                                                             else
                                                                                 rpc (add k selectedNow)) seled;
                                                                List.app (fn selectedBefore =>
                                                                             if List.mem selectedBefore seled then
                                                                                 return ()
                                                                             else
                                                                                 rpc (remove k selectedBefore)) lsV;
                                                                set ls seled)
                                                               <xml>Change selection</xml>
                                                               (Select2.render s2)
                                                               <xml>Save</xml>))}
                     </td></xml>
                 end,
        Todos = fn _ _ => return 0
    }
end

functor LinkedWithEditForOwner(M : sig
                                   type inp
                                   con this :: Name
                                   con owner :: Name
                                   con fthis :: Name
                                   con thisT :: Type
                                   con fthat :: Name
                                   con thatT :: Type
                                   con r :: {Type}
                                   constraint [this] ~ [owner]
                                   constraint [this, owner] ~ r
                                   constraint [fthis] ~ [fthat]
                                   table tab : ([this = thisT, owner = option string] ++ r)
                                   val show_that : show thatT
                                   val read_that : read thatT
                                   val eq_that : eq thatT
                                   val inj_this : sql_injectable thisT
                                   val inj_that : sql_injectable thatT
                                   table link : {fthis : thisT, fthat : thatT}
                                   val title : string

                                   con tkey :: Name
                                   con tr :: {Type}
                                   constraint [tkey] ~ tr
                                   table that : ([tkey = thatT] ++ tr)
                                   val thatTitle : string

                                   val label : string
                                   val whoami : transaction (option string)
                       end) = struct
    open M

    type cfg = option (string * list thatT * ChangeWatcher.client_part) (* if present, allowed to add *)
    type internal = option thisT * source (list thatT) * bool (* owner? *)

    val changed = ChangeWatcher.changed title

    fun enforce k =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            owner <- oneRowE1 (SELECT COUNT( * ) > 0
                               FROM tab
                               WHERE tab.{this} = {[k]}
                                 AND tab.{owner} = {[Some u]});
            if not owner then
                error <xml>Access denied</xml>
            else
                return ()

    fun add k v =
        enforce k;
        dml (DELETE FROM link
             WHERE T.{fthis} = {[k]}
               AND T.{fthat} = {[v]});
        dml (INSERT INTO link({fthis}, {fthat})
             VALUES ({[k]}, {[v]}));
        changed

    fun addAll k vs =
        enforce k;
        List.app (fn v =>
                     dml (DELETE FROM link
                          WHERE T.{fthis} = {[k]}
                            AND T.{fthat} = {[v]});
                     dml (INSERT INTO link({fthis}, {fthat})
                          VALUES ({[k]}, {[v]}))) vs;
        changed

    fun remove k v =
        enforce k;
        dml (DELETE FROM link
             WHERE T.{fthis} = {[k]}
               AND T.{fthat} = {[v]});
        changed

    val savedLabel = label
    val label = Basis.label

    val t = {
        Configure = uo <- whoami;
          case uo of
              None => return None
            | Some u =>
              ts <- List.mapQuery (SELECT that.{tkey}
                                   FROM that
                                   ORDER BY that.{tkey})
                                  (fn r => r.That.tkey);
              ch <- ChangeWatcher.listen thatTitle;
              return (Some (u, ts, ch)),
        Generate = fn cfg r =>
                      owner <- (case cfg of
                                    None => return False
                                  | Some (u, _, _) =>
                                    oneRowE1 (SELECT COUNT( * ) > 0
                                              FROM tab
                                              WHERE tab.{this} = {[r.this]}
                                                AND tab.{owner} = {[Some u]}));
                      ls <- List.mapQuery (SELECT link.{fthat}
                                            FROM link
                                            WHERE link.{fthis} = {[r.this]})
                                          (fn r => r.Link.fthat);
                      ls <- source ls;
                      return (Some r.this, ls, owner),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs authed =>
                    case authed of
                        None => return ()
                      | Some (_, _, ch) => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ _ => ls <- source []; return (None, ls, True),
        WidgetForCreate = fn ts (_, ls, _) =>
          case ts of
              None => <xml></xml>
            | Some (_, ts, _) =>
              <xml>
                <div class="form-group">
                  <label class="control-label">{[savedLabel]}</label>
                  <active code={lsV <- get ls;
                                s2 <- Select2.create (List.mapX (fn t =>
                                                                    if List.mem t lsV then
                                                                        <xml><coption selected={True}>{[t]}</coption></xml>
                                                                    else
                                                                        <xml><coption>{[t]}</coption></xml>) ts);
                                return <xml>
                                  {Select2.render s2}
                                  <dyn signal={seled <- Select2.selected s2;
                                               return <xml>
                                                 <active code={set ls (List.mp readError seled);
                                                               return <xml></xml>}/>
                                               </xml>}/>
                                </xml>}/>
                </div>
              </xml>,
        OnCreateLocal = fn r (_, ls, _) =>
                           ls <- get ls;
                           case ls of
                               [] => return ()
                             | _ => rpc (addAll r.this ls),
        Header = fn _ => <xml><th>{[savedLabel]}</th></xml>,
        Row = fn cfg ctx (k, ls, owner) => let
                     fun one v = <xml>
                       <span class="badge badge-pill badge-info p-2">
                         {[v]}
                         {if not owner then
                              <xml></xml>
                          else
                              <xml>
                                <span class="text-white" style="cursor: pointer"
                                      onclick={fn _ =>
                                                  case k of
                                                      None => error <xml>Missing self for buttons</xml>
                                                    | Some k =>
                                                      rpc (remove k v);
                                                      lsV <- get ls;
                                                      set ls (List.filter (fn v' => v' <> v) lsV)}>
                                  &times;
                                </span>
                              </xml>}
                       </span>
                     </xml>
                 in
                     <xml><td>
                       <dyn signal={ls <- signal ls; return (List.mapX one ls)}/>
                       {if not owner then
                            <xml></xml>
                        else
                            case cfg of
                                None => <xml></xml>
                              | Some (_, ts, _) =>
                                case k of
                                    None => error <xml>Missing self for buttons</xml>
                                  | Some k =>
                                    Ui.modalIcon ctx
                                                 (CLASS "glyphicon glyphicon-pencil-alt")
                                                 (lsV <- get ls;
                                                  s2 <- Select2.create (List.mapX (fn t =>
                                                                            if List.mem t lsV then
                                                                                <xml><coption selected={True}>{[t]}</coption></xml>
                                                                            else
                                                                                <xml><coption>{[t]}</coption></xml>) ts);
                                                  return (Ui.modal (seled <- current (Select2.selected s2);
                                                                    seled <- return (List.mp readError seled);
                                                                    List.app (fn selectedNow =>
                                                                                 if List.mem selectedNow lsV then
                                                                                     return ()
                                                                                 else
                                                                                     rpc (add k selectedNow)) seled;
                                                                    List.app (fn selectedBefore =>
                                                                                 if List.mem selectedBefore seled then
                                                                                     return ()
                                                                                 else
                                                                                     rpc (remove k selectedBefore)) lsV;
                                                                    set ls seled)
                                                                   <xml>Change selection</xml>
                                                                   (Select2.render s2)
                                                                   <xml>Save</xml>))}
                     </td></xml>
                 end,
        Todos = fn _ _ => return 0
    }
end

functor LinkedWithEditAndDefault(M : sig
                                     con this :: Name
                                     con fthis :: Name
                                     con thisT :: Type
                                     con fthat :: Name
                                     con thatT :: Type
                                     con r :: {Type}
                                     constraint [this] ~ r
                                     constraint [fthis] ~ [fthat]
                                     val show_that : show thatT
                                     val read_that : read thatT
                                     val eq_that : eq thatT
                                     val inj_this : sql_injectable thisT
                                     val inj_that : sql_injectable thatT
                                     table link : {fthis : thisT, fthat : thatT}
                                     val title : string

                                     con tkey :: Name
                                     con tr :: {Type}
                                     constraint [tkey] ~ tr
                                     table that : ([tkey = thatT] ++ tr)
                                     val thatTitle : string

                                     val label : string
                                     val authorized : transaction bool
                                 end) = struct
    open M

    type cfg = option (list thatT * ChangeWatcher.client_part) (* if present, allowed to add *)
    type internal = option thisT * source (list thatT)

    val changed = ChangeWatcher.changed title

    fun add k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]});
            dml (INSERT INTO link({fthis}, {fthat})
                 VALUES ({[k]}, {[v]}));
            changed

    fun addAll k vs =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            List.app (fn v =>
                         dml (DELETE FROM link
                              WHERE T.{fthis} = {[k]}
                                AND T.{fthat} = {[v]});
                         dml (INSERT INTO link({fthis}, {fthat})
                              VALUES ({[k]}, {[v]}))) vs;
            changed

    fun remove k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]});
            changed

    val savedLabel = label
    val label = Basis.label

    val t = {
        Configure = authed <- authorized;
          if not authed then
              return None
          else
              ts <- List.mapQuery (SELECT that.{tkey}
                                   FROM that
                                   ORDER BY that.{tkey})
                                  (fn r => r.That.tkey);
              ch <- ChangeWatcher.listen thatTitle;
              return (Some (ts, ch)),
        Generate = fn _ r =>
                      ls <- List.mapQuery (SELECT link.{fthat}
                                            FROM link
                                            WHERE link.{fthis} = {[r.this]})
                                          (fn r => r.Link.fthat);
                      ls <- source ls;
                      return (Some r.this, ls),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs authed =>
                    case authed of
                        None => return ()
                      | Some (_, ch) => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ inp => ls <- source (inp :: []); return (None, ls),
        WidgetForCreate = fn ts (_, ls) =>
          case ts of
              None => <xml></xml>
            | Some (ts, _) =>
              <xml>
                <div class="form-group">
                  <label class="control-label">{[savedLabel]}</label>
                  <active code={lsV <- get ls;
                                s2 <- Select2.create (List.mapX (fn t =>
                                                                    if List.mem t lsV then
                                                                        <xml><coption selected={True}>{[t]}</coption></xml>
                                                                    else
                                                                        <xml><coption>{[t]}</coption></xml>) ts);
                                return <xml>
                                  {Select2.render s2}
                                  <dyn signal={seled <- Select2.selected s2;
                                               return <xml>
                                                 <active code={set ls (List.mp readError seled);
                                                               return <xml></xml>}/>
                                               </xml>}/>
                                </xml>}/>
                </div>
              </xml>,
        OnCreateLocal = fn r (_, ls) =>
                           ls <- get ls;
                           case ls of
                               [] => return ()
                             | _ => rpc (addAll r.this ls),
        Header = fn _ => <xml><th>{[savedLabel]}</th></xml>,
        Row = fn authed ctx (k, ls) => let
                     fun one v = <xml>
                       <span class="badge badge-pill badge-info p-2">
                         {[v]}
                         {case authed of
                              None => <xml></xml>
                            | Some _ => <xml>
                              <span class="text-white" style="cursor: pointer"
                                    onclick={fn _ =>
                                                case k of
                                                    None => error <xml>Missing self for buttons</xml>
                                                  | Some k =>
                                                    rpc (remove k v);
                                                    lsV <- get ls;
                                                    set ls (List.filter (fn v' => v' <> v) lsV)}>
                                &times;
                              </span>
                            </xml>}
                       </span>
                     </xml>
                 in
                     <xml><td>
                       <dyn signal={ls <- signal ls; return (List.mapX one ls)}/>
                       {case authed of
                            None => <xml></xml>
                          | Some (ts, _) =>
                            case k of
                                None => error <xml>Missing self for buttons</xml>
                              | Some k =>
                                Ui.modalIcon ctx
                                             (CLASS "glyphicon glyphicon-pencil-alt")
                                             (lsV <- get ls;
                                              s2 <- Select2.create (List.mapX (fn t =>
                                                                        if List.mem t lsV then
                                                                            <xml><coption selected={True}>{[t]}</coption></xml>
                                                                        else
                                                                            <xml><coption>{[t]}</coption></xml>) ts);
                                              return (Ui.modal (seled <- current (Select2.selected s2);
                                                                seled <- return (List.mp readError seled);
                                                                List.app (fn selectedNow =>
                                                                             if List.mem selectedNow lsV then
                                                                                 return ()
                                                                             else
                                                                                 rpc (add k selectedNow)) seled;
                                                                List.app (fn selectedBefore =>
                                                                             if List.mem selectedBefore seled then
                                                                                 return ()
                                                                             else
                                                                                 rpc (remove k selectedBefore)) lsV;
                                                                set ls seled)
                                                               <xml>Change selection</xml>
                                                               (Select2.render s2)
                                                               <xml>Save</xml>))}
                     </td></xml>
                 end,
        Todos = fn _ _ => return 0
    }
end

functor LinkedWithFollow(M : sig
                             type inp
                             con this :: Name
                             con fthis :: Name
                             con thisT :: Type
                             con fthat :: Name
                             con thatT :: Type
                             con r :: {Type}
                             constraint [this] ~ r
                             constraint [fthis] ~ [fthat]
                             val show_that : show thatT
                             val inj_this : sql_injectable thisT
                             val inj_that : sql_injectable thatT
                             table from : {fthis : thisT, fthat : thatT}

                             con user :: Name
                             con cthat :: Name
                             constraint [user] ~ [cthat]
                             table to : {user : string, cthat : thatT}

                             val label : string
                             val whoami : transaction (option string)
                         end) = struct
    open M

    type cfg = option string
    type internal = list (thatT * source bool)

    fun follow v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM to
                 WHERE T.{user} = {[u]}
                   AND T.{cthat} = {[v]});
            dml (INSERT INTO to({user}, {cthat})
                 VALUES ({[u]}, {[v]}))

    fun unfollow v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM to
                 WHERE T.{user} = {[u]}
                   AND T.{cthat} = {[v]})

    val t = {
        Configure = whoami,
        Generate = fn uo r =>
                      List.mapQueryM (SELECT from.{fthat}, (SELECT COUNT( * ) > 0
                                                            FROM to
                                                            WHERE to.{cthat} = from.{fthat}
                                                              AND {sql_nullable (SQL to.{user})}
                                                                  = {[uo]}) AS Count
                                      FROM from
                                      WHERE from.{fthis} = {[r.this]}
                                      ORDER BY from.{fthat})
                                     (fn r =>
                                         s <- source (r.Count = Some True);
                                         return (r.From.fthat, s)),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn _ _ => return [],
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo _ ls => let
                     fun one (v, followed) = <xml>
                       <span class="badge badge-pill badge-info">{[v]}</span>
                       {case uo of
                            None => <xml></xml>
                          | Some _ => <xml>
                            <dyn signal={f <- signal followed;
                                         return (if f then
                                                     <xml>
                                                       <button class="btn btn-sm btn-secondary"
                                                               onclick={fn _ => rpc (unfollow v); set followed False}>
                                                         Unfollow
                                                       </button>
                                                     </xml>
                                                 else
                                                     <xml>
                                                       <button class="btn btn-sm btn-primary"
                                                               onclick={fn _ => rpc (follow v); set followed True}>
                                                         Follow
                                                       </button>
                                                     </xml>)}/>
                       </xml>}
                     </xml>
                 in
                     <xml><td>{List.mapX one ls}</td></xml>
                 end,
        Todos = fn _ _ => return 0
    }
end

functor Like(M : sig
                 type inp
                 con this :: Name
                 con fthis :: Name
                 con thisT :: Type
                 con user :: Name
                 con r :: {Type}
                 constraint [this] ~ r
                 constraint [fthis] ~ [user]
                 val inj_this : sql_injectable thisT
                 table like : {fthis : thisT, user : string}

                 val label : string
                 val whoami : transaction (option string)
             end) = struct
    open M

    type cfg = option string
    type internal = option thisT * source bool

    fun unlike v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM like
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]})

    fun yeslike v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM like
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO like({fthis}, {user})
                 VALUES ({[v]}, {[u]}))

    val t = {
        Configure = whoami,
        Generate = fn uo r =>
                      case uo of
                          None =>
                          s <- source False;
                          return (Some r.this, s)
                        | Some u =>
                          b <- oneRowE1 (SELECT COUNT( * ) > 0
                                         FROM like
                                         WHERE like.{fthis} = {[r.this]}
                                           AND like.{user} = {[u]});
                          s <- source b;
                          return (Some r.this, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn _ _ => s <- source False; return (None, s),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo _ (v, s) => <xml><td>
          <button class="btn" onclick={fn _ => case v of None => error <xml>Missing like reference</xml> | Some v => rpc (yeslike v); set s True}>
            <span dynClass={b <- signal s;
                            return (if b then
                                        CLASS "glyphicon-2x fas glyphicon-smile"
                                    else
                                        CLASS "glyphicon-2x far glyphicon-smile")}/>
          </button>
          <button class="btn" onclick={fn _ => case v of None => error <xml>Missing like reference</xml> | Some v => rpc (unlike v); set s False}>
            <span dynClass={b <- signal s;
                            return (if not b then
                                        CLASS "glyphicon-2x fas glyphicon-frown"
                                    else
                                        CLASS "glyphicon-2x far glyphicon-frown")}/>
          </button>
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor Bid(M : sig
                type inp
                con this :: Name
                con fthis :: Name
                con thisT :: Type
                con user :: Name
                con preferred :: Name
                con r :: {Type}
                constraint [this] ~ r
                constraint [fthis] ~ [user]
                constraint [fthis, user] ~ [preferred]
                val inj_this : sql_injectable thisT
                table bid : {fthis : thisT, user : string, preferred : bool}
                val title : string

                val label : string
                val whoami : transaction (option string)
                val response : option string
            end) = struct
    open M

    type cfg = option string
    datatype pref = Unavailable | Available | Preferred
    type internal = option thisT * source pref

    val changed = ChangeWatcher.changed title

    fun unavailable v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            changed

    fun available v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO bid({fthis}, {user}, {preferred})
                 VALUES ({[v]}, {[u]}, FALSE));
            changed

    fun preferred v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO bid({fthis}, {user}, {preferred})
                 VALUES ({[v]}, {[u]}, TRUE));
            changed

    val maybeAlert =
        case response of
            None => return ()
          | Some msg => alert msg

    val t = {
        Configure = whoami,
        Generate = fn uo r =>
                      case uo of
                          None =>
                          s <- source Unavailable;
                          return (Some r.this, s)
                        | Some u =>
                          po <- oneOrNoRowsE1 (SELECT (bid.{preferred})
                                               FROM bid
                                               WHERE bid.{fthis} = {[r.this]}
                                                 AND bid.{user} = {[u]});
                          s <- source (case po of
                                           None => Unavailable
                                         | Some False => Available
                                         | Some True => Preferred);
                          return (Some r.this, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn _ _ => s <- source Unavailable; return (None, s),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo _ (v, s) => <xml><td>
          <active code={idp <- fresh;
                        ida <- fresh;
                        idu <- fresh;
                        return <xml>
                          <button id={idp} class="btn"
                                  data-toggle="tooltip" data-placement="bottom" title="Preferred"
                                  onclick={fn _ => case v of None => error <xml>Missing self for Bid</xml> | Some v => rpc (preferred v); set s Preferred; maybeAlert}>
                            <span dynClass={s <- signal s;
                                            return (case s of
                                                        Preferred => CLASS "glyphicon-2x fas glyphicon-smile"
                                                      | _ => CLASS "glyphicon-2x far glyphicon-smile")}/>
                          </button>
                          <button id={ida} class="btn"
                                  data-toggle="tooltip" data-placement="bottom" title="Available"
                                  onclick={fn _ => case v of None => error <xml>Missing self for Bid</xml> | Some v => rpc (available v); set s Available; maybeAlert}>
                            <span dynClass={s <- signal s;
                                            return (case s of
                                                        Available => CLASS "glyphicon-2x fas glyphicon-meh"
                                                      | _ => CLASS "glyphicon-2x far glyphicon-meh")}/>
                          </button>
                          <button id={idu} class="btn"
                                  data-toggle="tooltip" data-placement="bottom" title="Unavailable"
                                  onclick={fn _ => case v of None => error <xml>Missing self for Bid</xml> | Some v => rpc (unavailable v); set s Unavailable}>
                            <span dynClass={s <- signal s;
                                            return (case s of
                                                        Unavailable => CLASS "glyphicon-2x fas glyphicon-frown"
                                                      | _ => CLASS "glyphicon-2x far glyphicon-frown")}/>
                          </button>
                          <active code={Ui.tooltip idp;
                                        Ui.tooltip ida;
                                        Ui.tooltip idu;
                                        return <xml></xml>}/>
                        </xml>}/>
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor AssignFromBids(M : sig
                           type inp
                           con this :: Name
                           con assignee :: Name
                           con fthis :: Name
                           con thisT :: Type
                           con user :: Name
                           con preferred :: Name
                           con r :: {Type}
                           constraint [this] ~ [assignee]
                           constraint [this, assignee] ~ r
                           constraint [fthis] ~ [user]
                           constraint [fthis, user] ~ [preferred]
                           val inj_this : sql_injectable thisT
                           table bid : {fthis : thisT, user : string, preferred : bool}
                           val bidTitle : string

                           val label : string
                           val whoami : transaction (option string)

                           table tab : ([this = thisT, assignee = option string] ++ r)
                           val tabTitle : string
                       end) = struct
    open M

    type cfg = ChangeWatcher.client_part
    type internal = option thisT * list (string * bool (* preferred? *)) * source (option string)

    fun assign ch v u =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some _ =>
            dml (UPDATE tab
                 SET {assignee} = {[u]}
                 WHERE T.{this} = {[v]});
            ChangeWatcher.changedBy ch tabTitle

    val t = {
        Configure = ChangeWatcher.listen bidTitle,
        Generate = fn _ r =>
                      s <- source r.assignee;
                      choices <- List.mapQuery (SELECT bid.{user}, bid.{preferred}
                                                FROM bid
                                                WHERE bid.{fthis} = {[r.this]}
                                                ORDER BY bid.{preferred} DESC, bid.{fthis})
                                               (fn {Bid = r} => (r.user, r.preferred));
                      return (Some r.this, choices, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs ch => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ _ => s <- source None; return (None, [], s),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn ch ctx (v, cs, s) => <xml><td>
          <dyn signal={sv <- signal s;
                       let
                           val reassign = ss <- source "";
                               return (Ui.modal (ssv <- get ss;
                                                 case read ssv of
                                                     None => error <xml>Invalid assignee!</xml>
                                                   | Some ssv =>
                                                     case v of
                                                         None => error <xml>Missing bid reference</xml>
                                                       | Some v =>
                                                         rpc (assign (ChangeWatcher.server ch) v ssv);
                                                         set s ssv)
                                                <xml>Who do you want to assign?</xml>
                                                <xml><cselect source={ss} class="form-control">
                                                  <coption/>
                                                  {List.mapX (fn (u, p) => <xml><coption value={show u}>{[u]}{if p then <xml> [preferred]</xml> else <xml></xml>}</coption></xml>) cs}
                                                </cselect></xml>
                                                <xml>Assign</xml>)
                       in
                           return (case sv of
                                       None =>
                                       (case cs of
                                            [] => <xml><span class="small text-muted">Waiting for response</span></xml>
                                          | _ => <xml><h5>
                                            {let
                                                 val len = List.length cs
                                             in
                                                 Ui.modalAnchor ctx
                                                                (CLASS "badge badge-warning")
                                                                <xml>{[len]} response{[case len of 1 => "" | _ => "s"]}</xml>
                                                                reassign
                                             end}
                                          </h5></xml>)
                                     | Some u => <xml>
                                       <span class="badge badge-pill badge-info">{[u]}</span>
                                       {Ui.modalIcon ctx
                                                     (CLASS "glyphicon glyphicon-pencil-alt")
                                                     reassign}
                                     </xml>)
                       end}/>
        </td></xml>,
        Todos = fn _ (_, cs, s) => sv <- signal s; return (case sv of Some _ => 0 | None => case cs of [] => 0 | _ => 1)
    }
end

functor AssignFromBids2(M : sig
                            type inp
                            con fthat :: Name
                            con thatT :: Type
                            con user :: Name
                            con preferred :: Name
                            constraint [fthat] ~ [user]
                            constraint [fthat, user] ~ [preferred]
                            table bid : {fthat : thatT, user : string, preferred : bool}

                            con this :: Name
                            con thisT :: Type
                            con that :: Name
                            con assignees :: {Unit}
                            con r :: {Type}
                            constraint [this] ~ [that]
                            constraint [this, that] ~ assignees
                            constraint [this, that] ~ r
                            constraint assignees ~ r
                            table tab : ([this = thisT, that = option thatT] ++ mapU (option string) assignees ++ r)

                            val fl : folder assignees
                            val show_that : show thatT
                            val read_that : read thatT
                            val eq_that : eq thatT
                            val inj_that : sql_injectable_prim thatT
                            val inj_this : sql_injectable thisT

                            val label : string
                            val whoami : transaction (option string)
                        end) = struct
    open M

    type cfg = unit
    type internal = option thisT * list (thatT * int (* #preferred? *)) * source (option thatT)

    fun assign v t =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some _ =>
            dml (UPDATE tab
                 SET {that} = {[t]}
                 WHERE T.{this} = {[v]})

    fun extendPrefs (uo : option string) (pso : option (list (thatT * int))) =
        case uo of
            None => return None
          | Some u =>
            case pso of
                None => return None
              | Some ps =>
                ps <- query1 (SELECT bid.{fthat}, bid.{preferred}
                              FROM bid
                              WHERE bid.{user} = {[u]})
                             (fn r ps =>
                                 return (if List.exists (fn (t, _) => t = r.fthat) ps then
                                             List.mp (fn (t, n) => (t, if t = r.fthat && r.preferred then
                                                                           n + 1
                                                                       else
                                                                           n)) ps
                                         else
                                             (r.fthat, if r.preferred then 1 else 0) :: ps))
                             ps;
                return (Some ps)

    fun buildPrefs (uos : $(mapU (option string) assignees)) =
      pso <- @Monad.foldR _ [fn _ => option string] [fn _ => option (list (thatT * int))]
              (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] => extendPrefs)
              (Some []) fl uos;
      return (case pso of
                  None => []
                | Some ps => List.sort (fn (_, n1) (_, n2) => n1 < n2) ps)

    fun stars n =
        if n <= 0 then
            ""
        else if n = 1 then
            " [preferred]"
        else
            " [preferred X" ^ show n ^ "]"

    val t = {
        Configure = return (),
        Generate = fn () r =>
                      s <- source r.that;
                      choices <- buildPrefs (r --- _);
                      return (Some r.this, choices, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn () _ => s <- source None; return (None, [], s),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo ctx (v, cs, s) => <xml><td>
          <dyn signal={sv <- signal s;
                       let
                           val reassign = ss <- source (show sv);
                               return (Ui.modal (ssv <- get ss;
                                                 case read ssv of
                                                     None => error <xml>Invalid assignment!</xml>
                                                   | Some ssv =>
                                                     case v of
                                                         None => error <xml>Missing assignment reference</xml>
                                                       | Some v =>
                                                         rpc (assign v ssv);
                                                         set s ssv)
                                                <xml>Which value do you want to assign?</xml>
                                                <xml><cselect source={ss} class="form-control">
                                                  <coption/>
                                                  {List.mapX (fn (t, n) => <xml><coption value={show t}>{[t]}{[stars n]}</coption></xml>) cs}
                                                </cselect></xml>
                                                <xml>Assign</xml>)
                       in
                           return (case sv of
                                       None =>
                                       (case cs of
                                            [] => <xml><span class="small text-muted">Waiting for response</span></xml>
                                          | _ => <xml><h5>
                                            {let
                                                 val len = List.length cs
                                             in
                                                 Ui.modalAnchor ctx
                                                                (CLASS "badge badge-warning")
                                                                <xml>{[len]} response{[case len of 1 => "" | _ => "s"]}</xml>
                                                                reassign
                                             end}
                                          </h5></xml>)
                                     | Some u => <xml>
                                       <span class="badge badge-pill badge-info">{[u]}</span>
                                       {Ui.modalIcon ctx
                                                     (CLASS "glyphicon glyphicon-pencil-alt")
                                                     reassign}
                                     </xml>)
                       end}/>
        </td></xml>,
        Todos = fn _ (_, cs, s) => sv <- signal s; return (case sv of Some _ => 0 | None => case cs of [] => 0 | _ => 1)
    }
end

type nonnull_cfg = unit
type nonnull_st = unit
val nonnull [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE NOT (tab.{col} IS NULL)),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type isnull_cfg = unit
type isnull_st = unit
val isnull [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} IS NULL),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type past_cfg = unit
type past_st = unit
val past [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}
val pastOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type future_cfg = unit
type future_st = unit
val future [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}
val futureOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type taggedWithUser_cfg = option string
type taggedWithUser_st = unit
fun taggedWithUser [inp ::: Type] [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn uo _ => case uo of
                                 None => Some (WHERE FALSE)
                               | Some u => Some (WHERE tab.{user} = {[u]}),
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn _ _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type taggedWithUserOpt_cfg = option string
type taggedWithUserOpt_st = unit
fun taggedWithUserOpt [inp ::: Type] [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn uo _ => case uo of
                                 None => Some (WHERE FALSE)
                               | Some u => Some (WHERE tab.{user} = {[Some u]}),
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn _ _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type linkedToUser_cfg = option string * ChangeWatcher.client_part
type linkedToUser_st = unit
fun linkedToUser [inp ::: Type] [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
    [ckey :: Name] [user :: Name] [cr ::: {Type}] [ks ::: {{Unit}}] [[ckey] ~ [user]] [[ckey, user] ~ cr]
    (link : sql_table ([ckey = keyT, user = string] ++ cr) ks)
    (whoami : transaction (option string)) title = {
    Configure = uo <- whoami; ch <- ChangeWatcher.listen title; return (uo, ch),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn (uo, _) _ => case uo of
                                      None => Some (WHERE FALSE)
                                    | Some u => Some (WHERE (SELECT COUNT( * ) > 0
                                                             FROM link
                                                             WHERE link.{user} = {[u]}
                                                               AND link.{ckey} = tab.{key}) = {[Some True]}),
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn r (_, ch) => ChangeWatcher.onChange ch r.ReloadState,
    GenerateLocal = fn _ _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type doubleLinkedToUser_cfg = option string
type doubleLinkedToUser_st = unit
fun doubleLinkedToUser [inp ::: Type] [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
                       [ckey :: Name] [ikey :: Name] [ikeyT ::: Type] [cr1 ::: {Type}] [ks1 ::: {{Unit}}]
                       [[ckey] ~ [ikey]] [[ckey, ikey] ~ cr1]
                       [ikey2 :: Name] [user :: Name] [cr2 ::: {Type}] [ks2 ::: {{Unit}}]
                       [[ikey2] ~ [user]] [[ikey2, user] ~ cr2]
                       (link1 : sql_table ([ckey = keyT, ikey = ikeyT] ++ cr1) ks1)
                       (link2 : sql_table ([ikey2 = ikeyT, user = string] ++ cr2) ks2)
                       (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn uo _ => case uo of
                                 None => Some (WHERE FALSE)
                               | Some u => Some (WHERE (SELECT COUNT( * ) > 0
                                                        FROM link1, link2
                                                        WHERE link1.{ckey} = tab.{key}
                                                          AND link2.{ikey2} = link1.{ikey}
                                                          AND link2.{user} = {[u]}) = {[Some True]}),
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn _ _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type owner_cfg = option string
type owner_st = option string
fun owner [inp ::: Type] [owner :: Name] [r ::: {Type}] [[owner] ~ r]
    (whoami : transaction (option string)) (lbl : string) = {
    Configure = whoami,
    Generate = fn _ r => return r.owner,
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn uo _ r => r -- owner ++ {owner = uo},
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn uo _ => return uo,
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml><th>{[lbl]}</th></xml>,
    Row = fn _ _ v => <xml><td>{[v]}</td></xml>,
    Todos = fn _ _ => return 0
}

type sortby_cfg = unit
type sortby_st = unit
val sortby [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_asc,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}
val sortbyDesc [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_desc,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type isTrue_cfg = unit
type isTrue_st = unit
val isTrue [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type isTrueOpt_cfg = unit
type isTrueOpt_st = unit
val isTrueOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE COALESCE(tab.{col}, FALSE)),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

type isNotTrueOpt_cfg = unit
type isNotTrueOpt_st = unit
val isNotTrueOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE NOT COALESCE(tab.{col}, FALSE)),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    ModifyBeforeCreate = fn _ _ r => r,
    OnCreate = fn _ _ _ => return (),
    OnLoad = fn _ _ => return (),
    GenerateLocal = fn () _ => return (),
    WidgetForCreate = fn _ _ => <xml></xml>,
    OnCreateLocal = fn _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>,
    Todos = fn _ _ => return 0
}

functor Moderate(M : sig
                     type inp
                     con key :: Name
                     type keyT
                     con hide :: Name
                     con r :: {Type}
                     constraint [key] ~ [hide]
                     constraint [key, hide] ~ r
                     val show_key : show keyT
                     val inj_key : sql_injectable keyT

                     table tab : ([key = keyT, hide = option bool] ++ r)
                     val title : string

                     val authorized : transaction bool

                     val label : string
                 end) = struct
    open M

    type cfg = unit
    type internal = option keyT * bool

    val changed = ChangeWatcher.changed title

    fun hide k =
        auth <- authorized;
        if not auth then
            error <xml>Access denied</xml>
        else
            dml (UPDATE tab
                 SET {hide} = {[Some True]}
                 WHERE T.{key} = {[k]});
            changed

    fun unhide k =
        auth <- authorized;
        if not auth then
            error <xml>Access denied</xml>
        else
            dml (UPDATE tab
                 SET {hide} = NULL
                 WHERE T.{key} = {[k]});
            changed

    val t  = {
        Configure = return (),
        Generate = fn () r => return (Some r.key, Option.get False r.hide),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = sql_order_by_Cons (SQL tab.{hide}) sql_asc,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn () _ => return (None, False),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn () ctx (k, hidden) => <xml><td>
          {if hidden then
               <xml><b>Hidden</b></xml>
           else
               <xml>Shown</xml>}
          {Ui.modalIcon ctx (CLASS "glyphicon glyphicon-pencil-alt")
                        (return (Ui.modal (case k of
                                               None => error <xml>No key set</xml>
                                             | Some k =>
                                               if hidden then
                                                   rpc (unhide k)
                                               else
                                                   rpc (hide k))
                                          <xml>Are you sure?</xml>
                                          <xml>You would <b>{if hidden then
                                                                 <xml>unhide</xml>
                                                             else
                                                                 <xml>hide</xml>}</b> this entry: <b>{[k]}</b>.</xml>
                                          (if hidden then
                                               <xml>Unhide</xml>
                                           else
                                               <xml>Hide</xml>)))}
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor ExternalAction(M : sig
                           type inp
                           con key :: Name
                           type keyT
                           con performed :: Name
                           con r :: {Type}
                           constraint [key] ~ [performed]
                           constraint [key, performed] ~ r
                           val show_key : show keyT
                           val inj_key : sql_injectable keyT

                           table tab : ([key = keyT, performed = option bool] ++ r)
                           val title : string

                           val label : string
                           val authorized : transaction bool
                           val perform : $([key = keyT, performed = option bool] ++ r) -> transaction unit
                       end) = struct
    open M

    type cfg = unit
    type internal = option keyT * bool

    val changed = ChangeWatcher.changed title

    fun activate k =
        auth <- authorized;
        if not auth then
            error <xml>Access denied</xml>
        else
            r <- oneRow1 (SELECT * FROM tab WHERE tab.{key} = {[k]});
            perform r;
            dml (UPDATE tab
                 SET {performed} = {[Some True]}
                 WHERE T.{key} = {[k]});
            changed

    fun rollback k =
        auth <- authorized;
        if not auth then
            error <xml>Access denied</xml>
        else
            dml (UPDATE tab
                 SET {performed} = NULL
                 WHERE T.{key} = {[k]});
            changed

    val t  = {
        Configure = return (),
        Generate = fn () r => return (Some r.key, Option.get False r.performed),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = sql_order_by_Cons (SQL COALESCE(tab.{performed}, FALSE)) sql_asc,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn _ _ => return (),
        GenerateLocal = fn () _ => return (None, False),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn () ctx (k, performed) => <xml><td>
          {if performed then
               <xml><i class="glyphicon glyphicon-check"/></xml>
           else
               <xml></xml>}
          {Ui.modalIcon ctx (CLASS "glyphicon glyphicon-pencil-alt")
                        (return (Ui.modal (case k of
                                               None => error <xml>No key set</xml>
                                             | Some k =>
                                               if performed then
                                                   rpc (rollback k)
                                               else
                                                   rpc (activate k))
                                          <xml>Are you sure?</xml>
                                          <xml>You would {if performed then
                                                              <xml>revert</xml>
                                                          else
                                                              <xml>perform</xml>} action <b>{[label]}</b> for this entry: <b>{[k]}</b>.</xml>
                                          (if performed then
                                               <xml>Revert</xml>
                                           else
                                               <xml>Perform</xml>)))}
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor Upvote(M : sig
                type inp
                con this :: Name
                con fthis :: Name
                con thisT :: Type
                con user :: Name
                con r :: {Type}
                constraint [this] ~ r
                constraint [fthis] ~ [user]
                val inj_this : sql_injectable thisT
                table vote : {fthis : thisT, user : string}
                val title : string

                val label : string
                val whoami : transaction (option string)
            end) = struct
    open M

    type cfg = option string * ChangeWatcher.client_part
    type internal = {Total : int,
                     OwnVote : option (thisT * bool)}

    val changed = ChangeWatcher.changed title

    fun unvote v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM vote
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            changed

    fun upvote v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM vote
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO vote({fthis}, {user})
                 VALUES ({[v]}, {[u]}));
            changed

    val t = {
        Configure = uo <- whoami; ch <- ChangeWatcher.listen title; return (uo, ch),
        Generate = fn (uo, _) r =>
                      count <- oneRowE1 (SELECT COUNT( * )
                                         FROM vote
                                         WHERE vote.{fthis} = {[r.this]});
                      self <- (case uo of
                                   None => return None
                                 | Some u =>
                                   n <- oneRowE1 (SELECT COUNT( * ) > 0
                                                  FROM vote
                                                  WHERE vote.{fthis} = {[r.this]}
                                                    AND vote.{user} = {[u]});
                                   return (Some (r.this, n)));
                      return {Total = count, OwnVote = self},
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = sql_order_by_Cons (SQL (SELECT COUNT( * )
                                         FROM vote
                                         WHERE vote.{fthis} = tab.{this})) sql_desc,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs (_, ch) => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ _ => return {Total = 0, OwnVote = None},
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn (uo, _) _ r => <xml><td>
          {[r.Total]}
          {case r.OwnVote of
               None => <xml></xml>
             | Some (k, True) => <xml>
                 <button class="btn btn-secondary"
                         onclick={fn _ => rpc (unvote k)}>
                   <span class="glyphicon glyphicon-minus"/>
                 </button>
               </xml>
             | Some (k, False) => <xml>
                 <button class="btn btn-secondary"
                         onclick={fn _ => rpc (upvote k)}>
                   <span class="glyphicon glyphicon-plus"/>
                 </button>
               </xml>}
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor Upload(M : sig
                   type inp
                   con this :: Name
                   type thisT
                   con r :: {Type}
                   constraint [this] ~ r
                   val inj_this : sql_injectable thisT

                   con fthis :: Name
                   con user :: Name
                   con when :: Name
                   constraint [fthis] ~ [user]
                   constraint [fthis, user] ~ [when]
                   constraint [fthis, user, when] ~ [FileName, FileType, FileData]
                   table upload : {fthis : thisT, user : string, when : time, FileName : string, FileType : string, FileData : blob}
                   val title : string

                   val label : string
                   val whoami : transaction (option string)
               end) = struct
    open M

    type cfg = option string * ChangeWatcher.client_part
    type internal = option thisT * option {user : string, when : time, FileName : string}

    val changed = ChangeWatcher.changed title

    fun save k h =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u =>
            r <- AjaxUpload.claim h;
            case r of
                AjaxUpload.NotFound => error <xml>Upload not found</xml>
              | AjaxUpload.Found r =>
                case r.Filename of
                    None => error <xml>No filename indicated with upload</xml>
                  | Some fname =>
                    dml (DELETE FROM upload
                         WHERE T.{fthis} = {[k]});
                    dml (INSERT INTO upload({fthis}, {user}, {when},
                             FileName, FileType, FileData)
                         VALUES ({[k]}, {[u]}, CURRENT_TIMESTAMP,
                             {[fname]}, {[r.MimeType]}, {[r.Content]}));
                    changed

    fun download k =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            r <- oneRow1 (SELECT upload.FileName, upload.FileType, upload.FileData
                          FROM upload
                          WHERE upload.{fthis} = {[k]}
                          ORDER BY upload.{when} DESC
                          LIMIT 1);
            setHeader (blessResponseHeader "Content-Disposition")
                      ("attachment; filename=" ^ r.FileName);
            returnBlob r.FileData (blessMime r.FileType)

    datatype upload_state = Initial | Uploading | Failed | Succeeded of AjaxUpload.handle

    val t = {
        Configure = uo <- whoami; ch <- ChangeWatcher.listen title; return (uo, ch),
        Generate = fn (uo, _) r =>
                      f <- oneOrNoRows1 (SELECT upload.{user}, upload.{when}, upload.FileName
                                         FROM upload
                                         WHERE upload.{fthis} = {[r.this]}
                                         ORDER BY upload.{when} DESC
                                         LIMIT 1);
                      return (Some r.this, f),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        ModifyBeforeCreate = fn _ _ r => r,
        OnCreate = fn _ _ _ => return (),
        OnLoad = fn fs (_, ch) => ChangeWatcher.onChange ch fs.ReloadState,
        GenerateLocal = fn _ _ => return (None, None),
        WidgetForCreate = fn _ _ => <xml></xml>,
        OnCreateLocal = fn _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn (uo, _) ctx (k, r) => <xml><td>
          {case (k, r) of
               (Some k, Some r) => <xml>
                 <a class="badge badge-pill" link={download k}><tt>{[r.FileName]}</tt> by {[r.user]} at {[r.when]}</a>
               </xml>
               | _ => <xml></xml>}
          {Ui.modalIcon ctx (CLASS "glyphicon glyphicon-pencil-alt")
                        (h <- source Initial;
                         return (Ui.modal
                                     (h <- get h;
                                      case h of
                                          Succeeded h =>
                                          (case k of
                                               None => error <xml>Missing SmartTable key</xml>
                                             | Some k => rpc (save k h))
                                        | _ => return ())
                                     <xml>Upload {[title]}</xml>
                                 <xml>
                                   <active code={AjaxUpload.render {SubmitLabel = None,
                                                                    OnBegin = set h Uploading,
                                                                    OnError = set h Failed,
                                                                    OnSuccess = fn hv => set h (Succeeded hv)}}/>
                                   <hr/>
                                   <dyn signal={h <- signal h;
                                                return (case h of
                                                            Initial => <xml></xml>
                                                          | Uploading => <xml><h4 class="text-info">Uploading....</h4></xml>
                                                          | Failed => <xml><h4 class="text-warning">Upload failed!</h4></xml>
                                                          | Succeeded _ => <xml><h4 class="text-success">Upload succeeded!</h4></xml>)}/>
                                 </xml>
                                 <xml>Save</xml>))}
        </td></xml>,
        Todos = fn _ _ => return 0
    }
end

functor Make(M : sig
                 con key :: Name
                 type keyT
                 type key2
                 type key3
                 con r :: {(Type * Type * Type)}
                 constraint [key] ~ r
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val tab : sql_table ([key = keyT] ++ map fst3 r) ([keyName = [key]] ++ otherKeys)
                 val title : string

                 type cfg
                 type st
                 val t : t unit ([key = keyT] ++ map fst3 r) cfg st
                 val widgets : $(map Widget.t' ([key = (keyT, key2, key3)] ++ r))
                 val fl : folder ([key = (keyT, key2, key3)] ++ r)
                 val labels : $(map (fn _ => string) ([key = (keyT, key2, key3)] ++ r))
                 val injs : $(map (fn p => sql_injectable p.1) ([key = (keyT, key2, key3)] ++ r))

                 val authorized : transaction bool
                 val allowCreate : bool

                 con buttons :: {Unit}
                 val buttonsFl : folder buttons
             end) = struct
    open M

    type input = $(mapU (unit -> {key : keyT} -> string * url) buttons)
    type a = {Config : source cfg,
              Configs : source $([key = key3] ++ map thd3 r),
              Rows : source (list (keyT * st)),
              Changes : ChangeWatcher.client_part,
              Buttons : source $(mapU (unit -> {key : keyT} -> string * url) buttons)}

    val rows =
        cfg <- t.Configure;
        cfgs <- @Monad.mapR _ [Widget.t'] [thd3]
                 (fn [nm ::_] [p ::_] => @Widget.configure) fl widgets;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg ())}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg ())}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => st <- t.Generate cfg r; return (r.key, st));
        return (cfg, cfgs, rs)

    fun create btns =
        (cfg, cfgs, rs) <- rows;
        cfg <- source cfg;
        cfgs <- source cfgs;
        rs <- source rs;
        ch <- ChangeWatcher.listen title;
        btns <- source btns;
        return {Config = cfg, Configs = cfgs, Rows = rs, Changes = ch, Buttons = btns}

    val redo =
        ok <- authorized;
        if not ok then
            error <xml>Access denied</xml>
        else
            rows

    fun onload self =
        let
            val onChange =
                (cfg, cfgs, rs) <- rpc redo;
                set self.Config cfg;
                set self.Configs cfgs;
                set self.Rows rs
        in
            cfg <- get self.Config;
            t.OnLoad {ReloadState = onChange} cfg;
            ChangeWatcher.onChange self.Changes onChange
        end

    fun add ch r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                cfg <- t.Configure;
                @@Sql.easy_insert [[key = keyT] ++ map fst3 r] [_] injs (@@Folder.mp [fst3] [_] fl) tab (t.ModifyBeforeCreate cfg () r);
                t.OnCreate cfg () r

    val notify =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                ChangeWatcher.changed title

    fun generate r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                cfg <- t.Configure;
                t.Generate cfg r

    fun render ctx self = <xml>
      {if not allowCreate then
           <xml></xml>
       else
           Ui.modalButton ctx
                          (CLASS "btn btn-primary")
                          <xml>Create New {[title]}</xml>
                          (cfgs <- get self.Configs;
                           ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                  (fn [nm ::_] [p ::_] => @Widget.create)
                                  fl widgets cfgs;
                           cfg <- get self.Config;
                           stl <- t.GenerateLocal cfg ();
                           return (Ui.modal
                                   (vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.2) => current (@Widget.value w v))
                                           fl widgets ws;
                                    rpc (add (ChangeWatcher.server self.Changes) vs);
                                    vs <- return (t.ModifyBeforeCreate cfg () vs);
                                    t.OnCreateLocal vs stl;
                                    rpc notify)
                                   <xml>Add</xml>
                                   <xml>
                                     {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                       (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                           (lab : string) (w : Widget.t' p) x =>
                                           if @Widget.optional w then
                                               <xml></xml>
                                           else <xml>
                                             <div class="form-group">
                                               <label class="control-label">{[lab]}</label>
                                               {@Widget.asWidget w x None}
                                             </div>
                                           </xml>)
                                       fl labels widgets ws}
                                     {t.WidgetForCreate cfg stl}
                                   </xml>
                                   <xml>Add</xml>))}

      <dyn signal={btns <- signal self.Buttons;
                   cfg <- signal self.Config;
                   return <xml>
                     <table class="bs-table">
                       <thead>
                         <tr>{if @Row.isEmpty buttonsFl then <xml></xml> else <xml><th/></xml>}{t.Header cfg}</tr>
                       </thead>
                       <tbody>
                         <dyn signal={rs <- signal self.Rows;
                                      return (List.mapX (fn (k, r) => <xml><tr>
                                        {if @Row.isEmpty buttonsFl then
                                             <xml></xml>
                                         else <xml><td>
                                           {@mapUX [unit -> {key : keyT} -> string * url] [body]
                                             (fn [nm ::_] [r ::_] [[nm] ~ r] make =>
                                                 let
                                                     val (label, url) = make () {key = k}
                                                 in
                                                     <xml><a class="btn btn-sm btn-primary" href={url}>{[label]}</a></xml>
                                                 end)
                                             buttonsFl btns}
                                         </td></xml>}
                                        {t.Row cfg ctx r}
                                      </tr></xml>) rs)}/>
                       </tbody>
                     </table>
                   </xml>}/>
    </xml>

    fun notification _ self = <xml>
      <dyn signal={cfg <- signal self.Config;
                   st <- signal self.Rows;
                   n <- List.foldlM (fn (_, r) n =>
                                        m <- t.Todos cfg r;
                                        return (m + n)) 0 st;
                   return (if n = 0 then
                               <xml></xml>
                           else
                               <xml><span class="badge badge-pill badge-warning">{[n]}</span></xml>)}/>
    </xml>

    fun ui btns = {Create = create btns,
                   Onload = onload,
                   Render = render,
                   Notification = notification}
end

functor Make1(M : sig
                  type inp
                  con key :: Name
                  type keyT
                  type key2
                  type key3
                  con r :: {(Type * Type * Type)}
                  constraint [key] ~ r
                  con keyName :: Name
                  con otherKeys :: {{Unit}}
                  constraint [keyName] ~ otherKeys
                  val tab : sql_table ([key = keyT] ++ map fst3 r) ([keyName = [key]] ++ otherKeys)
                  val title : string

                  type cfg
                  type st
                  val t : t inp ([key = keyT] ++ map fst3 r) cfg st
                  val widgets : $(map Widget.t' ([key = (keyT, key2, key3)] ++ r))
                  val fl : folder ([key = (keyT, key2, key3)] ++ r)
                  val labels : $(map (fn _ => string) ([key = (keyT, key2, key3)] ++ r))
                  val injs : $(map (fn p => sql_injectable p.1) ([key = (keyT, key2, key3)] ++ r))

                  val authorized : transaction bool
                  val allowCreate : bool
                  val notifyWhenEmpty : bool
                  val notifyWhenNonempty : bool

                  con buttons :: {Unit}
                  val buttonsFl : folder buttons
              end) = struct
    open M

    type input = inp * $(mapU (unit -> {key : keyT} -> string * url) buttons)
    type a = {Config : source cfg,
              Input : inp,
              Configs : source $([key = key3] ++ map thd3 r),
              Rows : source (list (keyT * st)),
              Changes : ChangeWatcher.client_part,
              Buttons : source $(mapU (unit -> {key : keyT} -> string * url) buttons)}

    fun rows inp =
        cfg <- t.Configure;
        cfgs <- @Monad.mapR _ [Widget.t'] [thd3]
                 (fn [nm ::_] [p ::_] => @Widget.configure) fl widgets;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg inp)}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg inp)}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => st <- t.Generate cfg r; return (r.key, st));
        return (cfg, cfgs, rs)

    fun create (inp, btns) =
        (cfg, cfgs, rs) <- rows inp;
        cfg <- source cfg;
        cfgs <- source cfgs;
        rs <- source rs;
        ch <- ChangeWatcher.listen title;
        btns <- source btns;
        return {Config = cfg, Input = inp, Configs = cfgs, Rows = rs,
                Changes = ch, Buttons = btns}

    fun redo inp =
        ok <- authorized;
        if not ok then
            error <xml>Access denied</xml>
        else
            rows inp

    fun onload self =
        let
            val onChange =
                (cfg, cfgs, rs) <- rpc (redo self.Input);
                set self.Config cfg;
                set self.Configs cfgs;
                set self.Rows rs
        in
            cfg <- get self.Config;
            t.OnLoad {ReloadState = onChange} cfg;
            ChangeWatcher.onChange self.Changes onChange
        end

    fun add inp ch r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                cfg <- t.Configure;
                @@Sql.easy_insert [[key = keyT] ++ map fst3 r] [_] injs (@@Folder.mp [fst3] [_] fl) tab (t.ModifyBeforeCreate cfg inp r);
                t.OnCreate cfg inp r

    val notify =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                ChangeWatcher.changed title

    fun generate r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                cfg <- t.Configure;
                t.Generate cfg r

    fun render ctx self = <xml>
      {if not allowCreate then
           <xml></xml>
       else
           Ui.modalButton ctx
                          (CLASS "btn btn-primary")
                          <xml>Create New {[title]}</xml>
                          (cfgs <- get self.Configs;
                           ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                  (fn [nm ::_] [p ::_] => @Widget.create)
                                  fl widgets cfgs;
                           cfg <- get self.Config;
                           stl <- t.GenerateLocal cfg self.Input;
                           return (Ui.modal
                                   (vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.2) => current (@Widget.value w v))
                                           fl widgets ws;
                                    rpc (add self.Input (ChangeWatcher.server self.Changes) vs);
                                    vs <- return (t.ModifyBeforeCreate cfg self.Input vs);
                                    t.OnCreateLocal vs stl;
                                    rpc notify)
                                   <xml>Add</xml>
                                   <xml>
                                     {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                       (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                                    (lab : string) (w : Widget.t' p) x =>
                                           if @Widget.optional w then
                                               <xml></xml>
                                           else <xml>
                                             <div class="form-group">
                                               <label class="control-label">{[lab]}</label>
                                               {@Widget.asWidget w x None}
                                             </div>
                                           </xml>)
                                       fl labels widgets ws}
                                     {t.WidgetForCreate cfg stl}
                                   </xml>
                                   <xml>Add</xml>))}

      <dyn signal={btns <- signal self.Buttons;
                   cfg <- signal self.Config;
                   return <xml>
                     <table class="bs-table">
                       <thead>
                         <tr>{if @Row.isEmpty buttonsFl then <xml></xml> else <xml><th/></xml>}{t.Header cfg}</tr>
                       </thead>
                       <tbody>
                         <dyn signal={rs <- signal self.Rows;
                                      return (List.mapX (fn (k, r) => <xml><tr>
                                        {if @Row.isEmpty buttonsFl then
                                             <xml></xml>
                                         else <xml><td>
                                           {@mapUX [unit -> {key : keyT} -> string * url] [body]
                                             (fn [nm ::_] [r ::_] [[nm] ~ r] make =>
                                                 let
                                                     val (label, url) = make () {key = k}
                                                 in
                                                     <xml><a class="btn btn-sm btn-primary" href={url}>{[label]}</a></xml>
                                                 end)
                                             buttonsFl btns}
                                         </td></xml>}
                                        {t.Row cfg ctx r}
                                      </tr></xml>) rs)}/>
                       </tbody>
                     </table>
                   </xml>}/>
    </xml>

    fun notification _ self = <xml>
      {if not notifyWhenEmpty then
           <xml></xml>
       else <xml>
         <dyn signal={st <- signal self.Rows;
                      return (case st of
                                  [] => <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                                | _ => <xml></xml>)}/>
       </xml>}
      {if not notifyWhenNonempty then
           <xml></xml>
       else <xml>
         <dyn signal={st <- signal self.Rows;
                      return (case st of
                                  _ :: _ => <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                                | _ => <xml></xml>)}/>
       </xml>}
      <dyn signal={cfg <- signal self.Config;
                   st <- signal self.Rows;
                   n <- List.foldlM (fn (_, r) n =>
                                        m <- t.Todos cfg r;
                                        return (m + n)) 0 st;
                   return (if n = 0 then
                               <xml></xml>
                           else
                               <xml><span class="badge badge-pill badge-warning">{[n]}</span></xml>)}/>
    </xml>

   fun ui inp = {Create = create inp,
                 Onload = onload,
                 Render = render,
                 Notification = notification}
end
