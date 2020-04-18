open Bootstrap4

(* One of these is all about generating content table headers and rows *)
type t (r :: {Type}) (cfg :: Type) (st :: Type) = {
     (* Run on server: *)
     Configure : transaction cfg,
     Generate : cfg -> $r -> transaction st,
     Filter : cfg -> option (sql_exp [Tab = r] [] [] bool),
     FilterLinks : cfg -> option (sql_exp [Tab = r] [] [] bool),
     SortBy : sql_order_by [Tab = r] [] -> sql_order_by [Tab = r] [],
     
     (* Run on client: *)
     Header : cfg -> xtr,
     Row : cfg -> st -> xtr
}

fun compose [r] [cfgb] [cfga] [stb] [sta] (b : t r cfgb stb) (a : t r cfga sta) = {
    Configure = cfga <- a.Configure;
                cfgb <- b.Configure;
                return (cfgb, cfga),
    Generate = fn (cfgb, cfga) r =>
                  sta <- a.Generate cfga r;
                  stb <- b.Generate cfgb r;
                  return (stb, sta),
    Filter = fn (cfgb, cfga) => case (a.Filter cfga, b.Filter cfgb) of
                                    (None, x) => x
                                  | (x, None) => x
                                  | (Some x, Some y) => Some (WHERE {x} AND {y}),
    FilterLinks = fn (cfgb, cfga) => case (a.FilterLinks cfga, b.FilterLinks cfgb) of
                                         (None, x) => x
                                       | (x, None) => x
                                       | (Some x, Some y) => Some (WHERE {x} OR {y}),
    SortBy = fn sb => b.SortBy (a.SortBy sb),
    Header = fn (cfgb, cfga) => <xml>{b.Header cfgb}{a.Header cfga}</xml>,
    Row = fn (cfgb, cfga) (y, x) => <xml>{b.Row cfgb y}{a.Row cfga x}</xml>
}

type column_cfg (t :: Type) = unit
type column_st (t :: Type) = t
fun column [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
           (_ : show colT) (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () v => <xml><td>{[v]}</td></xml>
}

type html_cfg = unit
type html_st = xbody
fun html [col :: Name] [r ::: {Type}] [[col] ~ r] (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return (Widget.html r.col),
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () v => <xml><td>{v}</td></xml>
}

type iconButton_cfg (cols :: {Type}) = option string * time
type iconButton_st (cols :: {Type}) = $cols
fun iconButton [cols ::: {Type}] [r ::: {Type}] [cols ~ r]
    (whoami : transaction (option string))
    (render : option string -> time -> $cols -> option (css_class * url))
    (lbl : string) = {
    Configure = u <- whoami; tm <- now; return (u, tm),
    Generate = fn _ r => return (r --- r),
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn _ => <xml><th>{[lbl]}</th></xml>,
    Row = fn (u, tm) cols =>
             case render u tm cols of
                 None => <xml><td/></xml>
               | Some (cl, ur) => <xml><td>
                 <a href={ur}
                 class={classes cl (CLASS "btn btn-primary btn-lg glyphicon")}/>
               </td></xml>
}

fun links [a] (_ : show a) (ls : list a) : xbody = <xml>
  {List.mapX (fn x => <xml> <span class="badge badge-pill badge-info">{[x]}</span></xml>) ls}
</xml>

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
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () ls => <xml><td>{links ls}</td></xml>
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
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () ls => <xml><td>{links ls}</td></xml>
}

functor LinkedWithFollow(M : sig
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
        Filter = fn _ => None,
        FilterLinks = fn _ => None,
        SortBy = fn x => x,
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo ls => let
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
                 end
    }
end

functor Bid(M : sig
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

                val label : string
                val whoami : transaction (option string)
            end) = struct
    open M

    type cfg = option string
    datatype pref = Unavailable | Available | Preferred
    type internal = thisT * source pref

    fun unavailable v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]})

    fun available v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO bid({fthis}, {user}, {preferred})
                 VALUES ({[v]}, {[u]}, FALSE))

    fun preferred v =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some u =>
            dml (DELETE FROM bid
                 WHERE T.{user} = {[u]}
                   AND T.{fthis} = {[v]});
            dml (INSERT INTO bid({fthis}, {user}, {preferred})
                 VALUES ({[v]}, {[u]}, TRUE))

    val t = {
        Configure = whoami,
        Generate = fn uo r =>
                      case uo of
                          None =>
                          s <- source Unavailable;
                          return (r.this, s)
                        | Some u =>
                          po <- oneOrNoRowsE1 (SELECT (bid.{preferred})
                                               FROM bid
                                               WHERE bid.{fthis} = {[r.this]}
                                                 AND bid.{user} = {[u]});
                          s <- source (case po of
                                           None => Unavailable
                                         | Some False => Available
                                         | Some True => Preferred);
                          return (r.this, s),
        Filter = fn _ => None,
        FilterLinks = fn _ => None,
        SortBy = fn x => x,
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo (v, s) => <xml><td>
          <button class="btn" onclick={fn _ => rpc (unavailable v); set s Unavailable}>
            <span dynClass={s <- signal s;
                            return (case s of
                                        Unavailable => CLASS "glyphicon-2x glyphicon glyphicon-times-circle text-danger"
                                      | _ => CLASS "glyphicon glyphicon-times-circle text-danger")}/>
          </button>
          <button class="btn" onclick={fn _ => rpc (available v); set s Available}>
            <span dynClass={s <- signal s;
                            return (case s of
                                        Available => CLASS "glyphicon-2x glyphicon glyphicon-check-circle text-success"
                                      | _ => CLASS "glyphicon glyphicon-check-circle text-success")}/>
          </button>
          <button class="btn" onclick={fn _ => rpc (preferred v); set s Preferred}>
            <span dynClass={s <- signal s;
                            return (case s of
                                        Preferred => CLASS "glyphicon-2x glyphicon glyphicon-exclamation-circle text-success"
                                      | _ => CLASS "glyphicon glyphicon-exclamation-circle text-success")}/>
          </button>
        </td></xml>
    }
end

type nonnull_cfg = unit
type nonnull_st = unit
val nonnull [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ => Some (WHERE NOT (tab.{col} IS NULL)),
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn _ => <xml></xml>,
    Row = fn _ _ => <xml></xml>
}
    
type taggedWithUser_cfg = option string
type taggedWithUser_st = unit
fun taggedWithUser [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn uo => case uo of
                          None => Some (WHERE FALSE)
                        | Some u => Some (WHERE tab.{user} = {[u]}),
    FilterLinks = fn _ => None,
    SortBy = fn x => x,
    Header = fn _ => <xml></xml>,
    Row = fn _ _ => <xml></xml>
}

type linkedToUser_cfg = option string
type linkedToUser_st = unit
fun linkedToUser [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
    [ckey :: Name] [user :: Name] [cr ::: {Type}] [ks ::: {{Unit}}] [[ckey] ~ [user]] [[ckey, user] ~ cr]
    (link : sql_table ([ckey = keyT, user = string] ++ cr) ks)
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ => None,
    FilterLinks = fn uo => case uo of
                          None => Some (WHERE FALSE)
                        | Some u => Some (WHERE (SELECT COUNT( * ) > 0
                                                 FROM link
                                                 WHERE link.{user} = {[u]}
                                                   AND link.{ckey} = tab.{key}) = {[Some True]}),
    SortBy = fn x => x,
    Header = fn _ => <xml></xml>,
    Row = fn _ _ => <xml></xml>
}

type doubleLinkedToUser_cfg = option string
type doubleLinkedToUser_st = unit
fun doubleLinkedToUser [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
                       [ckey :: Name] [ikey :: Name] [ikeyT ::: Type] [cr1 ::: {Type}] [ks1 ::: {{Unit}}]
                       [[ckey] ~ [ikey]] [[ckey, ikey] ~ cr1]
                       [ikey2 :: Name] [user :: Name] [cr2 ::: {Type}] [ks2 ::: {{Unit}}]
                       [[ikey2] ~ [user]] [[ikey2, user] ~ cr2]
                       (link1 : sql_table ([ckey = keyT, ikey = ikeyT] ++ cr1) ks1)
                       (link2 : sql_table ([ikey2 = ikeyT, user = string] ++ cr2) ks2)
                       (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ => None,
    FilterLinks = fn uo => case uo of
                               None => Some (WHERE FALSE)
                             | Some u => Some (WHERE (SELECT COUNT( * ) > 0
                                                      FROM link1, link2
                                                      WHERE link1.{ckey} = tab.{key}
                                                        AND link2.{ikey2} = link1.{ikey}
                                                        AND link2.{user} = {[u]}) = {[Some True]}),
    SortBy = fn x => x,
    Header = fn _ => <xml></xml>,
    Row = fn _ _ => <xml></xml>
}

type sortby_cfg = unit
type sortby_st = unit
val sortby [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ => None,
    FilterLinks = fn _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_asc,
    Header = fn _ => <xml></xml>,
    Row = fn _ _ => <xml></xml>
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
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg)}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg)}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        return {Config = cfg, Rows = rs}

    fun onload _ = return ()

    fun render _ self = <xml>
      <table class="bs-table">
        <thead>
          <tr>{t.Header self.Config}</tr>
        </thead>
        <tbody>
          {List.mapX (fn r => <xml><tr>{t.Row self.Config r}</tr></xml>) self.Rows}
        </tbody>
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
