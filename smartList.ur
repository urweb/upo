open Bootstrap4

(* One of these is all about generating content for the two main parts of a Bootstrap card. *)
type t (r :: {Type}) (cfg :: Type) (st :: Type) = {
     (* Run on server: *)
     Configure : transaction cfg,
     Generate : cfg -> $r -> transaction st,
     Filter : cfg -> sql_exp [Tab = r] [] [] bool,
     SortBy : sql_order_by [Tab = r] [] -> sql_order_by [Tab = r] [],
     
     (* Run on client: *)
     Header : cfg -> st -> xbody,
     Body : cfg -> st -> xbody
}

fun compose [r] [cfga] [cfgb] [sta] [stb] (a : t r cfga sta) (b : t r cfgb stb) = {
    Configure = cfga <- a.Configure;
                cfgb <- b.Configure;
                return (cfga, cfgb),
    Generate = fn (cfga, cfgb) r =>
                  sta <- a.Generate cfga r;
                  stb <- b.Generate cfgb r;
                  return (sta, stb),
    Filter = fn (cfga, cfgb) => (WHERE {a.Filter cfga} AND {b.Filter cfgb}),
    SortBy = fn sb => b.SortBy (a.SortBy sb),
    Header = fn (cfga, cfgb) (x, y) => <xml>{b.Header cfgb y}{a.Header cfga x}</xml>,
    Body = fn (cfga, cfgb) (x, y) => <xml>{b.Body cfgb y}{a.Body cfga x}</xml>
}

type columnInHeader_cfg (t :: Type) = unit
type columnInHeader_st (t :: Type) = t
fun columnInHeader [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
                   (_ : show colT) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ => (WHERE TRUE),
    SortBy = fn x => x,
    Header = fn () v => txt v,
    Body = fn () _ => <xml></xml>
}

type columnInBody_cfg (t :: Type) = unit
type columnInBody_st (t :: Type) = t
fun columnInBody [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
                 (_ : show colT) (l : string) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ => (WHERE TRUE),
    SortBy = fn x => x,
    Header = fn () _ => <xml></xml>,
    Body = fn () v => <xml><p><i>{[l]}:</i> {[v]}</p></xml>
}

type iconButtonInHeader_cfg (cols :: {Type}) = option string * time
type iconButtonInHeader_st (cols :: {Type}) = $cols
fun iconButtonInHeader [cols ::: {Type}] [r ::: {Type}] [cols ~ r]
    (whoami : transaction (option string))
    (render : option string -> time -> $cols -> option (css_class * url)) = {
    Configure = u <- whoami; tm <- now; return (u, tm),
    Generate = fn _ r => return (r --- r),
    Filter = fn _ => (WHERE TRUE),
    SortBy = fn x => x,
    Header = fn (u, tm) cols =>
                case render u tm cols of
                    None => <xml></xml>
                  | Some (cl, ur) => <xml>
                      <a href={ur}
                         class={classes cl (CLASS "btn btn-primary btn-lg glyphicon")}/>
                    </xml>,
    Body = fn _ _ => <xml></xml>
}

type linked_cfg (t :: Type) = unit
type linked_st (t :: Type) = list t
fun linked [this :: Name] [fthis :: Name] [thisT ::: Type]
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
    Filter = fn _ => (WHERE TRUE),
    SortBy = fn x => x,
    Header = fn () _ => <xml></xml>,
    Body = fn () ls => case ls of
                           [] => <xml></xml>
                         | x :: ls' => <xml><p><i>{[l]}:</i> {[x]}{List.mapX (fn v => <xml>, {[v]}</xml>) ls'}</p></xml>
}

type orderedLinked_cfg (t :: Type) = unit
type orderedLinked_st (t :: Type) = list t
fun orderedLinked [this :: Name] [fthis :: Name] [thisT ::: Type]
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
    Filter = fn _ => (WHERE TRUE),
    SortBy = fn x => x,
    Header = fn () _ => <xml></xml>,
    Body = fn () ls => case ls of
                           [] => <xml></xml>
                         | x :: ls' => <xml><p><i>{[l]}:</i> {[x]}{List.mapX (fn v => <xml>, {[v]}</xml>) ls'}</p></xml>
}

type nonnull_cfg = unit
type nonnull_st = unit
val nonnull [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ => (WHERE NOT (tab.{col} IS NULL)),
    SortBy = fn x => x,
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}
    
type taggedWithUser_cfg = option string
type taggedWithUser_st = unit
fun taggedWithUser [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn uo => case uo of
                          None => (WHERE FALSE)
                        | Some u => (WHERE tab.{user} = {[u]}),
    SortBy = fn x => x,
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type sortby_cfg = unit
type sortby_st = unit
val sortby [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ => (WHERE TRUE),
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_asc,
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

functor Make(M : sig
                 con r :: {Type}
                 table tab : r

                 type cfg
                 type st
                 val t : t r cfg st
             end) = struct
    open M

    type a = {Config : cfg,
              Rows : list st}

    val create =
        cfg <- t.Configure;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {t.Filter cfg}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        return {Config = cfg, Rows = rs}

    fun onload _ = return ()

    fun render _ self = <xml>
      {List.mapX (fn r => <xml>
        <div class="card">
          <div class="card-header"><h3>
            {t.Header self.Config r}
          </h3></div>
          <div class="card-body">
            {t.Body self.Config r}
          </div>
        </div>
      </xml>) self.Rows}
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
