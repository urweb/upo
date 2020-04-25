open Bootstrap4

(* One of these is all about generating content table headers and rows *)
type t (inp :: Type) (r :: {Type}) (cfg :: Type) (st :: Type) = {
     (* Run on server: *)
     Configure : transaction cfg,
     Generate : cfg -> $r -> transaction st,
     Filter : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     FilterLinks : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     SortBy : sql_order_by [Tab = r] [] -> sql_order_by [Tab = r] [],
     OnCreate : cfg -> inp -> $r -> transaction unit,

     (* Run on client: *)
     Header : cfg -> xtr,
     Row : cfg -> Ui.context -> st -> xtr
}

type inputIs_cfg = unit
type inputIs_st = unit
fun inputIs [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>
}

type inputIsOpt_cfg = unit
type inputIsOpt_st = unit
fun inputIsOpt [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable_prim inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[Some inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>
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
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>
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
    OnCreate = fn () inp r => dml (INSERT INTO ctr({ckey}, {inpCol})
                                   VALUES ({[r.key]}, {[inp]})),
    Header = fn () => <xml></xml>,
    Row = fn () _ _ => <xml></xml>
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
    OnCreate = fn (cfgb, cfga) inp r => b.OnCreate cfgb inp r; a.OnCreate cfga inp r,
    Header = fn (cfgb, cfga) => <xml>{b.Header cfgb}{a.Header cfga}</xml>,
    Row = fn (cfgb, cfga) ctx (y, x) => <xml>{b.Row cfgb ctx y}{a.Row cfga ctx x}</xml>
}

type column_cfg (t :: Type) = unit
type column_st (t :: Type) = t
fun column [inp ::: Type] [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
           (_ : show colT) (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () _ v => <xml><td>{[v]}</td></xml>
}

type html_cfg = unit
type html_st = xbody
fun html [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (lbl : string) = {
    Configure = return (),
    Generate = fn () r => return (Widget.html r.col),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml><th>{[lbl]}</th></xml>,
    Row = fn () _ v => <xml><td>{v}</td></xml>
}

type iconButton_cfg (cols :: {Type}) = option string * time
type iconButton_st (cols :: {Type}) = $cols
fun iconButton [inp ::: Type] [cols ::: {Type}] [r ::: {Type}] [cols ~ r]
    (whoami : transaction (option string))
    (render : option string -> time -> $cols -> option (css_class * url))
    (lbl : string) = {
    Configure = u <- whoami; tm <- now; return (u, tm),
    Generate = fn _ r => return (r --- r),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml><th>{[lbl]}</th></xml>,
    Row = fn (u, tm) _ cols =>
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
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{links ls}</td></xml>
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
    OnCreate = fn _ _ _ => return (),
    Header = fn () => <xml><th>{[l]}</th></xml>,
    Row = fn () _ ls => <xml><td>{links ls}</td></xml>
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

                           con tkey :: Name
                           con tr :: {Type}
                           constraint [tkey] ~ tr
                           table that : ([tkey = thatT] ++ tr)

                           val label : string
                           val authorized : transaction bool
                       end) = struct
    open M

    type cfg = option (list thatT) (* if present, allowed to add *)
    type internal = thisT * source (list thatT)

    fun add k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]});
            dml (INSERT INTO link({fthis}, {fthat})
                 VALUES ({[k]}, {[v]}))

    fun remove k v =
        authed <- authorized;
        if not authed then
            error <xml>Must be logged in</xml>
        else
            dml (DELETE FROM link
                 WHERE T.{fthis} = {[k]}
                   AND T.{fthat} = {[v]})

    val t = {
        Configure = authed <- authorized;
          if not authed then
              return None
          else
              ts <- List.mapQuery (SELECT that.{tkey}
                                   FROM that
                                   ORDER BY that.{tkey})
                                  (fn r => r.That.tkey);
              return (Some ts),
        Generate = fn _ r =>
                      ls <- List.mapQuery (SELECT link.{fthat}
                                            FROM link
                                            WHERE link.{fthis} = {[r.this]})
                                          (fn r => r.Link.fthat);
                      ls <- source ls;
                      return (r.this, ls),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        OnCreate = fn _ _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn authed ctx (k, ls) => let
                     fun one v = <xml>
                       <span class="badge badge-pill badge-info">
                         {[v]}
                         {case authed of
                              None => <xml></xml>
                            | Some _ => <xml>
                              <button class="btn btn-sm"
                                      onclick={fn _ => rpc (remove k v);
                                                  lsV <- get ls;
                                                  set ls (List.filter (fn v' => v' <> v) lsV)}>
                                &times;
                              </button>
                            </xml>}
                       </span>
                     </xml>
                 in
                     <xml><td>
                       <dyn signal={ls <- signal ls; return (List.mapX one ls)}/>
                       {case authed of
                            None => <xml></xml>
                          | Some ts =>
                            Ui.modalButton ctx
                                           (CLASS "btn btn-sm btn-secondary")
                                           <xml>Add</xml>
                                           (s <- source "";
                                            lsV <- get ls;
                                            return (Ui.modal (v <- get s;
                                                              case read v of
                                                                  None => error <xml>Bad selection</xml>
                                                                | Some v =>
                                                                  rpc (add k v);
                                                                  lsV <- get ls;
                                                                  set ls (List.append lsV (v :: [])))
                                                             <xml>Add</xml>
                                                             <xml>
                                                               <cselect source={s} class="form-control">
                                                                 {List.mapX (fn t =>
                                                                                if List.mem t lsV then
                                                                                    <xml></xml>
                                                                                else
                                                                                    <xml><coption>{[t]}</coption></xml>) ts}
                                                               </cselect>
                                                             </xml>
                                                             <xml>Add</xml>))}
                     </td></xml>
                 end
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
        OnCreate = fn _ _ _ => return (),
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
                 end
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
    type internal = thisT * source bool

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
                          return (r.this, s)
                        | Some u =>
                          b <- oneRowE1 (SELECT COUNT( * ) > 0
                                         FROM like
                                         WHERE like.{fthis} = {[r.this]}
                                           AND like.{user} = {[u]});
                          s <- source b;
                          return (r.this, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        OnCreate = fn _ _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo _ (v, s) => <xml><td>
          <button class="btn" onclick={fn _ => rpc (unlike v); set s False}>
            <span dynClass={b <- signal s;
                            return (if not b then
                                        CLASS "glyphicon-2x glyphicon glyphicon-times-circle text-danger"
                                    else
                                        CLASS "glyphicon glyphicon-times-circle text-danger")}/>
          </button>
          <button class="btn" onclick={fn _ => rpc (yeslike v); set s True}>
            <span dynClass={b <- signal s;
                            return (if b then
                                        CLASS "glyphicon-2x glyphicon glyphicon-check-circle text-success"
                                    else
                                        CLASS "glyphicon glyphicon-check-circle text-success")}/>
          </button>
        </td></xml>
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
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        OnCreate = fn _ _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo _ (v, s) => <xml><td>
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

                           val label : string
                           val whoami : transaction (option string)

                           table tab : ([this = thisT, assignee = option string] ++ r)
                       end) = struct
    open M

    type cfg = unit
    type internal = thisT * list (string * bool (* preferred? *)) * source (option string)

    fun assign v u =
        uo <- whoami;
        case uo of
            None => error <xml>Must be logged in</xml>
          | Some _ =>
            dml (UPDATE tab
                 SET {assignee} = {[u]}
                 WHERE T.{this} = {[v]})

    val t = {
        Configure = return (),
        Generate = fn () r =>
                      s <- source r.assignee;
                      choices <- List.mapQuery (SELECT bid.{user}, bid.{preferred}
                                                FROM bid
                                                WHERE bid.{fthis} = {[r.this]}
                                                ORDER BY bid.{preferred} DESC, bid.{fthis})
                                 (fn {Bid = r} => (r.user, r.preferred));
                      return (r.this, choices, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        OnCreate = fn _ _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo ctx (v, cs, s) => <xml><td>
          <dyn signal={sv <- signal s;
                       let
                           val reassign = ss <- source "";
                               return (Ui.modal (ssv <- get ss;
                                                 case read ssv of
                                                     None => error <xml>Invalid assignee!</xml>
                                                   | Some ssv =>
                                                     rpc (assign v ssv);
                                                     set s ssv)
                                                <xml>Who do you want to assign?</xml>
                                                <xml><cselect source={ss} class="form-control">
                                                  <coption/>
                                                  {List.mapX (fn (u, p) => <xml><coption value={show u}>{[u]}{if p then <xml>*</xml> else <xml></xml>}</coption></xml>) cs}
                                                </cselect></xml>
                                                <xml>Assign</xml>)
                       in
                           return (case sv of
                                       None => Ui.modalButton ctx
                                                              (CLASS "btn btn-sm text-muted")
                                                              <xml><span class="glyphicon glyphicon-plus-circle"/> Unassigned</xml>
                                                              reassign
                                     | Some u => <xml>
                                       <span class="badge badge-pill badge-info">{[u]}</span>
                                       {Ui.modalButton ctx
                                                       (CLASS "btn btn-sm btn-secondary")
                                                       <xml>reassign</xml>
                                                       reassign}
                                     </xml>)
                       end}/>
        </td></xml>
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
    type internal = thisT * list (thatT * int (* #preferred? *)) * source (option thatT)

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
        else
            "*" ^ stars (n - 1)

    val t = {
        Configure = return (),
        Generate = fn () r =>
                      s <- source r.that;
                      choices <- buildPrefs (r --- _);
                      return (r.this, choices, s),
        Filter = fn _ _ => None,
        FilterLinks = fn _ _ => None,
        SortBy = fn x => x,
        OnCreate = fn _ _ _ => return (),
        Header = fn _ => <xml><th>{[label]}</th></xml>,
        Row = fn uo ctx (v, cs, s) => <xml><td>
          <dyn signal={sv <- signal s;
                       let
                           val reassign = ss <- source (show sv);
                               return (Ui.modal (ssv <- get ss;
                                                 case read ssv of
                                                     None => error <xml>Invalid assignment!</xml>
                                                   | Some ssv =>
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
                                       None => Ui.modalButton ctx
                                                              (CLASS "btn btn-sm text-muted")
                                                              <xml><span class="glyphicon glyphicon-plus-circle"/> Unassigned</xml>
                                                              reassign
                                     | Some u => <xml>
                                       <span class="badge badge-pill badge-info">{[u]}</span>
                                       {Ui.modalButton ctx
                                                       (CLASS "btn btn-sm btn-secondary")
                                                       <xml>reassign</xml>
                                                       reassign}
                                     </xml>)
                       end}/>
        </td></xml>
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
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type isnull_cfg = unit
type isnull_st = unit
val isnull [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} IS NULL),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type past_cfg = unit
type past_st = unit
val past [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}
val pastOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type future_cfg = unit
type future_st = unit
val future [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}
val futureOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type taggedWithUser_cfg = option string
type taggedWithUser_st = unit
fun taggedWithUser [inp ::: Type] [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn uo _ => case uo of
                            None => Some (WHERE FALSE)
                          | Some u => Some (WHERE tab.{user} = {[u]}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type linkedToUser_cfg = option string
type linkedToUser_st = unit
fun linkedToUser [inp ::: Type] [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
    [ckey :: Name] [user :: Name] [cr ::: {Type}] [ks ::: {{Unit}}] [[ckey] ~ [user]] [[ckey, user] ~ cr]
    (link : sql_table ([ckey = keyT, user = string] ++ cr) ks)
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn uo _ => case uo of
                                 None => Some (WHERE FALSE)
                               | Some u => Some (WHERE (SELECT COUNT( * ) > 0
                                                        FROM link
                                                        WHERE link.{user} = {[u]}
                                                          AND link.{ckey} = tab.{key}) = {[Some True]}),
    SortBy = fn x => x,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
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
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

type sortby_cfg = unit
type sortby_st = unit
val sortby [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_asc,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}
val sortbyDesc [inp ::: Type] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_desc,
    OnCreate = fn _ _ _ => return (),
    Header = fn _ => <xml></xml>,
    Row = fn _ _ _ => <xml></xml>
}

functor Make(M : sig
                 con r :: {(Type * Type * Type)}
                 table tab : (map fst3 r)

                 type cfg
                 type st
                 val t : t unit (map fst3 r) cfg st
                 val widgets : $(map Widget.t' r)
                 val fl : folder r
                 val labels : $(map (fn _ => string) r)
                 val injs : $(map (fn p => sql_injectable p.1) r)

                 val authorized : transaction bool
                 val allowCreate : bool
             end) = struct
    open M

    type a = {Config : cfg,
              Configs : $(map thd3 r),
              Rows : source (list st)}

    val create =
        cfg <- t.Configure;
        cfgs <- @Monad.mapR _ [Widget.t'] [thd3]
                 (fn [nm ::_] [p ::_] => @Widget.configure) fl widgets;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg ())}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg ())}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        rs <- source rs;
        return {Config = cfg, Configs = cfgs, Rows = rs}

    fun onload _ = return ()

    fun add r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                @@Sql.easy_insert [map fst3 r] [_] injs (@@Folder.mp [fst3] [_] fl) tab r;
                cfg <- t.Configure;
                t.OnCreate cfg () r;
                t.Generate cfg r

    fun render ctx self = <xml>
      {if not allowCreate then
           <xml></xml>
       else
           Ui.modalButton ctx
                          (CLASS "btn btn-primary")
                          <xml>Add <span class="glyphicon glyphicon-plus"/></xml>
                          (ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                  (fn [nm ::_] [p ::_] => @Widget.create)
                                  fl widgets self.Configs;
                           return (Ui.modal
                                   (vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.2) => current (@Widget.value w v))
                                           fl widgets ws;
                                    st <- rpc (add vs);
                                    rs <- get self.Rows;
                                    set self.Rows (List.append rs (st :: [])))
                                   <xml>Add</xml>
                                   (@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                     (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                         (lab : string) (w : Widget.t' p) x => <xml>
                                           <div class="form-group">
                                             <label class="control-label">{[lab]}</label>
                                             {@Widget.asWidget w x None}
                                           </div>
                                         </xml>)
                                     fl labels widgets ws)
                                   <xml>Add</xml>))}

      <table class="bs-table">
        <thead>
          <tr>{t.Header self.Config}</tr>
        </thead>
        <tbody>
          <dyn signal={rs <- signal self.Rows;
                       return (List.mapX (fn r => <xml><tr>{t.Row self.Config ctx r}</tr></xml>) rs)}/>
        </tbody>
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end

functor Make1(M : sig
                  type inp
                  con r :: {(Type * Type * Type)}
                  table tab : (map fst3 r)

                  type cfg
                  type st
                  val t : t inp (map fst3 r) cfg st
                  val widgets : $(map Widget.t' r)
                  val fl : folder r
                  val labels : $(map (fn _ => string) r)
                  val injs : $(map (fn p => sql_injectable p.1) r)

                  val authorized : transaction bool
                  val allowCreate : bool
              end) = struct
    open M

    type input = inp
    type a = {Config : cfg,
              Input : inp,
              Configs : $(map thd3 r),
              Rows : source (list st)}

    fun create inp =
        cfg <- t.Configure;
        cfgs <- @Monad.mapR _ [Widget.t'] [thd3]
                 (fn [nm ::_] [p ::_] => @Widget.configure) fl widgets;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg inp)}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg inp)}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        rs <- source rs;
        return {Config = cfg, Input = inp, Configs = cfgs, Rows = rs}

    fun onload _ = return ()

    fun add inp r =
        if not allowCreate then
            error <xml>Access denied</xml>
        else
            authed <- authorized;
            if not authed then
                error <xml>Access denied</xml>
            else
                @@Sql.easy_insert [map fst3 r] [_] injs (@@Folder.mp [fst3] [_] fl) tab r;
                cfg <- t.Configure;
                t.OnCreate cfg inp r;
                t.Generate cfg r

    fun render ctx self = <xml>
      {if not allowCreate then
           <xml></xml>
       else
           Ui.modalButton ctx
                          (CLASS "btn btn-primary")
                          <xml>Add <span class="glyphicon glyphicon-plus"/></xml>
                          (ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                  (fn [nm ::_] [p ::_] => @Widget.create)
                                  fl widgets self.Configs;
                           return (Ui.modal
                                   (vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                           (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.2) => current (@Widget.value w v))
                                           fl widgets ws;
                                    st <- rpc (add self.Input vs);
                                    rs <- get self.Rows;
                                    set self.Rows (List.append rs (st :: [])))
                                   <xml>Add</xml>
                                   (@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
                                     (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                         (lab : string) (w : Widget.t' p) x => <xml>
                                           <div class="form-group">
                                             <label class="control-label">{[lab]}</label>
                                             {@Widget.asWidget w x None}
                                           </div>
                                         </xml>)
                                     fl labels widgets ws)
                                   <xml>Add</xml>))}

      <table class="bs-table">
        <thead>
          <tr>{t.Header self.Config}</tr>
        </thead>
        <tbody>
          <dyn signal={rs <- signal self.Rows;
                       return (List.mapX (fn r => <xml><tr>{t.Row self.Config ctx r}</tr></xml>) rs)}/>
        </tbody>
      </table>
    </xml>

    fun ui inp = {Create = create inp,
                  Onload = onload,
                  Render = render}
end
