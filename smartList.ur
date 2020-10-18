open Bootstrap4

(* One of these is all about generating content for the two main parts of a Bootstrap card. *)
type t (inp :: Type) (r :: {Type}) (cfg :: Type) (st :: Type) = {
     (* Run on server: *)
     Configure : transaction cfg,
     Generate : cfg -> $r -> transaction st,
     Filter : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     FilterLinks : cfg -> inp -> option (sql_exp [Tab = r] [] [] bool),
     SortBy : sql_order_by [Tab = r] [] -> sql_order_by [Tab = r] [],

     (* Run on client: *)
     OnLoad : {ReloadState : transaction unit} -> cfg -> transaction unit,
     Header : cfg -> st -> xbody,
     Body : cfg -> st -> xbody
}

val empty [inp] [r] = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () _ => None,
    FilterLinks = fn () _ => None,
    SortBy = fn sb => sb,
    OnLoad = fn _ () => return (),
    Header = fn () () => <xml></xml>,
    Body = fn () () => <xml></xml>
}

fun compose [inp] [r] [cfga] [cfgb] [sta] [stb] (a : t inp r cfga sta) (b : t inp r cfgb stb) = {
    Configure = cfga <- a.Configure;
                cfgb <- b.Configure;
                return (cfga, cfgb),
    Generate = fn (cfga, cfgb) r =>
                  sta <- a.Generate cfga r;
                  stb <- b.Generate cfgb r;
                  return (sta, stb),
    Filter = fn (cfga, cfgb) inp => case (a.Filter cfga inp, b.Filter cfgb inp) of
                                        (None, x) => x
                                      | (x, None) => x
                                      | (Some x, Some y) => Some (WHERE {x} AND {y}),
    FilterLinks = fn (cfga, cfgb) inp => case (a.FilterLinks cfga inp, b.FilterLinks cfgb inp) of
                                             (None, x) => x
                                           | (x, None) => x
                                           | (Some x, Some y) => Some (WHERE {x} OR {y}),
    SortBy = fn sb => b.SortBy (a.SortBy sb),
    OnLoad = fn fs (cfga, cfgb) => a.OnLoad fs cfga; b.OnLoad fs cfgb,
    Header = fn (cfga, cfgb) (x, y) => <xml>{a.Header cfga x}{b.Header cfgb y}</xml>,
    Body = fn (cfga, cfgb) (x, y) => <xml>{a.Body cfga x}{b.Body cfgb y}</xml>
}

type inputIs_cfg = unit
type inputIs_st = unit
fun inputIs [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () _ => <xml></xml>
}

type inputIsOpt_cfg = unit
type inputIsOpt_st = unit
fun inputIsOpt [inp ::: Type] [col :: Name] [r ::: {Type}] [[col] ~ r] (_ : sql_injectable_prim inp) = {
    Configure = return (),
    Generate = fn () _ => return (),
    Filter = fn () inp => Some (WHERE tab.{col} = {[Some inp]}),
    FilterLinks = fn () _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () _ => <xml></xml>
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
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () _ => <xml></xml>
}

type columnInHeader_cfg (t :: Type) = unit
type columnInHeader_st (t :: Type) = t
fun columnInHeader [inp] [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
                   (_ : show colT) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn () v => txt v,
    Body = fn () _ => <xml></xml>
}

type columnInBody_cfg (t :: Type) = unit
type columnInBody_st (t :: Type) = t
fun columnInBody [inp] [col :: Name] [colT ::: Type] [r ::: {Type}] [[col] ~ r]
                 (_ : show colT) (l : string) = {
    Configure = return (),
    Generate = fn () r => return r.col,
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () v => <xml><p><i>{[l]}:</i> {[v]}</p></xml>
}

type htmlInBody_cfg = unit
type htmlInBody_st = xbody
val htmlInBody [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn () r => return (Widget.html r.col),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () v => v
}

type iconButtonInHeader_cfg (cols :: {Type}) = option string * time
type iconButtonInHeader_st (cols :: {Type}) = $cols
fun iconButtonInHeader [inp] [cols ::: {Type}] [r ::: {Type}] [cols ~ r]
    (whoami : transaction (option string))
    (render : option string -> time -> $cols -> option (css_class * url)) = {
    Configure = u <- whoami; tm <- now; return (u, tm),
    Generate = fn _ r => return (r --- r),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
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
fun linked [inp] [this :: Name] [fthis :: Name] [thisT ::: Type]
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
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () ls => case ls of
                           [] => <xml></xml>
                         | x :: ls' => <xml><p><i>{[l]}:</i> {[x]}{List.mapX (fn v => <xml>, {[v]}</xml>) ls'}</p></xml>
}

type orderedLinked_cfg (t :: Type) = unit
type orderedLinked_st (t :: Type) = list t
fun orderedLinked [inp] [this :: Name] [fthis :: Name] [thisT ::: Type]
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
    OnLoad = fn _ _ => return (),
    Header = fn () _ => <xml></xml>,
    Body = fn () ls => case ls of
                           [] => <xml></xml>
                         | x :: ls' => <xml><p><i>{[l]}:</i> {[x]}{List.mapX (fn v => <xml>, {[v]}</xml>) ls'}</p></xml>
}

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
        OnLoad = fn _ _ => return (),
        Header = fn _ _ => <xml></xml>,
        Body = fn uo ls => case ls of
                               [] => <xml></xml>
                             | x :: ls' =>
                               let
                                   fun one (v, followed) = <xml>{[v]}{case uo of
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
                                                                        </xml>}</xml>
                               in
                                   <xml><p><i>{[label]}:</i> {one x}{List.mapX (fn v => <xml>, {one v}</xml>) ls'}</p></xml>
                               end
    }
end

type nonnull_cfg = unit
type nonnull_st = unit
val nonnull [inp] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE NOT (tab.{col} IS NULL)),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type isnull_cfg = unit
type isnull_st = unit
val isnull [inp] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} IS NULL),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type isTrue_cfg = unit
type isTrue_st = unit
val isTrue [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type past_cfg = unit
type past_st = unit
val past [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}
val pastOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} < {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type future_cfg = unit
type future_st = unit
val future [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > CURRENT_TIMESTAMP),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}
val futureOpt [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => Some (WHERE tab.{col} > {sql_nullable (SQL CURRENT_TIMESTAMP)}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type interval_cfg = time * time
type interval_st = unit
fun interval [inp] [col :: Name] [r ::: {Type}] [[col] ~ r] (bef : string) (aft : string) = {
    Configure = tm <- now; return (addSeconds tm (-(FullCalendar.durationToSeconds bef)),
                                   addSeconds tm (FullCalendar.durationToSeconds aft)),
    Generate = fn _ _ => return (),
    Filter = fn (bef, aft) _ => Some (WHERE tab.{col} > {[bef]}
                                        AND tab.{col} < {[aft]}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type taggedWithUser_cfg = option string
type taggedWithUser_st = unit
fun taggedWithUser [inp] [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn uo _ => case uo of
                            None => Some (WHERE FALSE)
                          | Some u => Some (WHERE tab.{user} = {[u]}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}
fun taggedWithUserOpt [inp] [user :: Name] [r ::: {Type}] [[user] ~ r]
    (whoami : transaction (option string)) = {
    Configure = whoami,
    Generate = fn _ _ => return (),
    Filter = fn uo _ => case uo of
                            None => Some (WHERE FALSE)
                          | Some u => Some (WHERE tab.{user} = {[Some u]}),
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type linkedToUser_cfg = option string
type linkedToUser_st = unit
fun linkedToUser [inp] [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
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
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type doubleLinkedToUser_cfg = option string
type doubleLinkedToUser_st = unit
fun doubleLinkedToUser [inp] [key :: Name] [keyT ::: Type] [r ::: {Type}] [[key] ~ r]
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
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type sortby_cfg = unit
type sortby_st = unit
val sortby [inp] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_asc,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}
val sortbyDesc [inp] [col :: Name] [ct ::: Type] [r ::: {Type}] [[col] ~ r] = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = sql_order_by_Cons (SQL tab.{col}) sql_desc,
    OnLoad = fn _ _ => return (),
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

type periodicRefresh_cfg = unit
type periodicRefresh_st = unit
fun periodicRefresh [inp ::: Type] [r ::: {Type}] (dur : string) = {
    Configure = return (),
    Generate = fn _ _ => return (),
    Filter = fn _ _ => None,
    FilterLinks = fn _ _ => None,
    SortBy = fn x => x,
    OnLoad = fn fs _ =>
                let
                    fun loop () =
                        sleep (1000 * FullCalendar.durationToSeconds dur);
                        fs.ReloadState;
                        loop ()
                in
                    spawn (loop ())
                end,
    Header = fn _ _ => <xml></xml>,
    Body = fn _ _ => <xml></xml>
}

functor Make(M : sig
                 con r :: {Type}
                 table tab : r
                 val title : string

                 type cfg
                 type st
                 val t : t unit r cfg st
                 val notifyOnNonempty : bool
                 val authorized : transaction bool
             end) = struct
    open M

    type a = {Config : source cfg,
              Rows : source (list st),
              Changes : ChangeWatcher.client_part}

    val rows =
        cfg <- t.Configure;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg ())}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg ())}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        return (cfg, rs)

    val create =
        (cfg, rs) <- rows;
        cfg <- source cfg;
        rs <- source rs;
        ch <- ChangeWatcher.listen title;
        return {Config = cfg, Rows = rs, Changes = ch}

    val relist =
        b <- authorized;
        if not b then
            error <xml>Access denied</xml>
        else
            rows

    fun onload self =
        let
            val onChange =
                (cfg, rs) <- rpc relist;
                set self.Config cfg;
                set self.Rows rs
        in
            cfg <- get self.Config;
            t.OnLoad {ReloadState = onChange} cfg;
            ChangeWatcher.onChange self.Changes onChange
        end

    fun render _ self = <xml>
      <dyn signal={cfg <- signal self.Config;
                   rs <- signal self.Rows;
                   return (List.mapX (fn r => <xml>
                     <div class="card">
                       <div class="card-header"><h3>
                         {t.Header cfg r}
                       </h3></div>
                       <div class="card-body">
                         {t.Body cfg r}
                       </div>
                     </div>
                   </xml>) rs)}/>
    </xml>

    fun notification _ self =
        if not notifyOnNonempty then
            <xml></xml>
        else <xml>
          <dyn signal={rs <- signal self.Rows;
                       return (case rs of
                                   _ :: _ => <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                                 | _ => <xml></xml>)}/>
        </xml>

    fun buttons _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification,
              Buttons = buttons}
end

functor Make1(M : sig
                  type inp
                  con r :: {Type}
                  table tab : r
                  val title : string

                  type cfg
                  type st
                  val t : t inp r cfg st
                  val notifyOnNonempty : bool
                  val authorized : transaction bool
             end) = struct
    open M

    type input = inp
    type a = {Input : inp,
              Config : source cfg,
              Rows : source (list st),
              Changes : ChangeWatcher.client_part}

    fun rows inp =
        cfg <- t.Configure;
        rs <- List.mapQueryM (SELECT *
                              FROM tab
                              WHERE {Option.get (WHERE TRUE) (t.Filter cfg inp)}
                                AND {Option.get (WHERE TRUE) (t.FilterLinks cfg inp)}
                              ORDER BY {{{t.SortBy (sql_order_by_Nil [[]])}}})
                             (fn {Tab = r} => t.Generate cfg r);
        return (cfg, rs)

    fun create inp =
        (cfg, rs) <- rows inp;
        cfg <- source cfg;
        rs <- source rs;
        ch <- ChangeWatcher.listen title;
        return {Input = inp, Config = cfg, Rows = rs, Changes = ch}

    fun relist inp =
        b <- authorized;
        if not b then
            error <xml>Access denied</xml>
        else
            rows inp

    fun onload self = ChangeWatcher.onChange self.Changes
                                             ((cfg, rs) <- rpc (relist self.Input);
                                              set self.Config cfg;
                                              set self.Rows rs)

    fun render _ self = <xml>
      <dyn signal={cfg <- signal self.Config;
                   rs <- signal self.Rows;
                   return (List.mapX (fn r => <xml>
                     <div class="card">
                       <div class="card-header"><h3>
                         {t.Header cfg r}
                       </h3></div>
                       <div class="card-body">
                         {t.Body cfg r}
                       </div>
                     </div>
                   </xml>) rs)}/>
    </xml>

    fun notification _ self =
        if not notifyOnNonempty then
            <xml></xml>
        else <xml>
          <dyn signal={rs <- signal self.Rows;
                       return (case rs of
                                   _ :: _ => <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                                 | _ => <xml></xml>)}/>
        </xml>

    fun buttons _ _ = <xml></xml>

    fun ui inp = {Create = create inp,
                  Onload = onload,
                  Render = render,
                  Notification = notification,
                  Buttons = buttons}
end
