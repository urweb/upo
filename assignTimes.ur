open Bootstrap4

val eventLengthInSeconds = 60 * 90

functor Make(M : sig
                 con key :: Name
                 con others :: {Type}
                 constraint [key] ~ others
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val times : sql_table ([key = time] ++ others) ([keyName = [key]] ++ otherKeys)
                 val others : time -> $others
                 val inj_others : $(map sql_injectable others)
                 val ofl : folder others

                 con btime :: Name
                 con user :: Name
                 con preferred :: Name
                 constraint [btime] ~ [user]
                 constraint [btime, user] ~ [preferred]
                 table bid : {btime : time, user : string, preferred : bool}
                 val bidTitle : string

                 con this :: Name
                 con thisT :: Type
                 con ttime :: Name
                 con assignees :: {Unit}
                 con r :: {Type}
                 constraint [this] ~ [ttime]
                 constraint [this, ttime] ~ assignees
                 constraint [this, ttime] ~ r
                 constraint assignees ~ r
                 constraint [T, Times, Preferred, SubPreferred, SubUnpreferred] ~ assignees
                 table t : ([this = thisT, ttime = option time] ++ mapU (option string) assignees ++ r)
                 val tTitle : string

                 val fl : folder assignees
                 val show_this : show thisT
                 val read_this : read thisT
                 val eq_this : eq thisT
                 val inj_this : sql_injectable thisT
                 val assignees : $(mapU string assignees)

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([this = thisT, ttime = option time] ++ mapU (option string) assignees ++ r)
                 val schedAddon : SchedulingAddons.t thisT
             end) = struct

    open M

    type prefs = {
         Preferred : int,
         SubPreferred : int,
         SubUnpreferred : int
    }

    type a = source {Times : list (time * list (thisT * $(mapU string assignees) * prefs * source (option time))),
                     Events : list (thisT * source (option time)),
                     Calendar : FullCalendar.t,
                     Context : source (option Ui.context),
                     Changes : ChangeWatcher.client_part,
                     ChangesSource : source (option ChangeWatcher.client_part),
                     BidChanges : ChangeWatcher.client_part,
                     Empty : source bool}

    fun stars r =
        let
            val numAssignees = @fold [fn _ => int] (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] n => n + 1) 0 fl
            val unpreferred = numAssignees - r.Preferred
            fun one cls n =
                if n <= 0 then
                    <xml></xml>
                else
                    <xml> <i class={classes glyphicon cls}/> {[n]}</xml>
        in
            <xml>{one (CLASS "glyphicon-lg glyphicon-smile") r.Preferred}{one (CLASS "glyphicon-lg glyphicon-meh") unpreferred}{one glyphicon_smile r.SubPreferred}{one glyphicon_meh r.SubUnpreferred}</xml>
        end

    fun setTime ch k tmo =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            dml (UPDATE t
                 SET {ttime} = {[tmo]}
                 WHERE T.{this} = {[k]});
            ChangeWatcher.changedBy ch tTitle

    fun idoIsSpecial ido =
        case ido of
            Some id => Option.isSome (String.index id #"_")
          | None => False

    fun add tm =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            dml (DELETE FROM times
                        WHERE T.{key} = {[tm]});
            @@Sql.easy_insert [[key = time] ++ others] [_]
              ({key = _} ++ inj_others)
              (@Folder.cons [key] [_] ! ofl) times ({key = tm} ++ others tm)

    fun del tm =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            dml (DELETE FROM bid
                 WHERE T.{btime} = {[tm]});
            dml (DELETE FROM times
                 WHERE T.{key} = {[tm]})

    fun addEvent cal tm =
        Monad.ignore (FullCalendar.addEvent cal
                                            {Id = None,
                                             AllDay = False,
                                             Start = tm,
                                             End = Some (addSeconds tm eventLengthInSeconds),
                                             Title = "",
                                             Rendering = FullCalendar.Normal,
                                             TextColor = None,
                                             BackgroundColor = None})

    val create =
        let
            fun collectTimes r (acc : list (time * list (thisT * $(mapU string assignees) * prefs))) =
                let
                    val k = r.T.this
                    val tm = r.Times.key
                    val pref = {Preferred = r.Preferred,
                                SubPreferred = r.SubPreferred,
                                SubUnpreferred = r.SubUnpreferred}
                    val assignees = @foldUR [option string] [fn r => option $(mapU string r)]
                                    (fn [nm ::_] [r ::_] [[nm] ~ r] o acc =>
                                        case o of
                                            None => None
                                          | Some v =>
                                            case acc of
                                                None => None
                                              | Some acc => Some ({nm = v} ++ acc))
                                    (Some {}) fl (r.T -- this)
                    fun maybeCons items = if r.Complete then
                                              case assignees of
                                                  None => error <xml>AssignTimes: some role isn't assigned after all</xml>
                                                | Some assignees => (k, assignees, pref) :: items
                                          else
                                              items
                in
                    case acc of
                        [] => (tm, maybeCons []) :: []
                      | (tm', items) :: acc' =>
                        if tm' = tm then
                            (tm, maybeCons items) :: acc'
                        else
                            (tm, maybeCons []) :: acc
                end
        in
            anyInTabs <- oneRowE1 (SELECT COUNT( * ) > 0
                                   FROM t);
            (evs, tms) <- (if anyInTabs then
                              tms <- query ({{{sql_query1 [assignees]
                                                          {Distinct = False,
                                                           From = @fold [fn assignees => others :: {Unit} -> [others ~ assignees] => [[T, Times] ~ assignees]
                                                                            => [[this = thisT, ttime = option time] ++ r ~ others ++ assignees]
                                                                            => sql_table ([this = thisT, ttime = option time] ++ mapU (option string) (others ++ assignees) ++ r) t_hidden_constraints
                                                                            -> sql_from_items [] ([T = [this = thisT, ttime = option time] ++ mapU (option string) (others ++ assignees) ++ r,
                                                                                                   Times = [key = time] ++ M.others]
                                                                                                      ++ mapU [btime = option time, user = option string, preferred = option bool] assignees)]
                                                                   (fn [nm ::_] [u ::_] [assignees ::_] [[nm] ~ assignees]
                                                                                (acc : others :: {Unit} -> [others ~ assignees] => [[T, Times] ~ assignees]
                                                                                 => [[this = thisT, ttime = option time] ++ r ~ others ++ assignees]
                                                                                 => sql_table ([this = thisT, ttime = option time] ++ mapU (option string) (others ++ assignees) ++ r) t_hidden_constraints
                                                                                 -> sql_from_items [] ([T = [this = thisT, ttime = option time] ++ mapU (option string) (others ++ assignees) ++ r,
                                                                                                        Times = [key = time] ++ M.others]
                                                                                                           ++ mapU [btime = option time, user = option string, preferred = option bool] assignees))
                                                                                [others ::_] [others ~ [nm] ++ assignees] [[T, Times] ~ [nm] ++ assignees]
                                                                                [[this = thisT, ttime = option time] ++ r ~ others ++ [nm] ++ assignees]
                                                                                (tab : sql_table ([this = thisT, ttime = option time] ++ mapU (option string) (others ++ [nm] ++ assignees) ++ r) t_hidden_constraints) =>
                                                                       (FROM {{acc [[nm] ++ others] tab}}
                                                                          LEFT JOIN bid AS {nm}
                                                                          ON {{nm}}.{btime} = times.{key}
                                                                            AND {sql_nullable (SQL {{nm}}.{user})} = t.{nm}))
                                                                   (fn [others ::_] [others ~ []] [[T, Times] ~ []] [_ ~ others] t =>
                                                                       (FROM t JOIN times ON TRUE)) fl
                                                                   [[]] ! ! ! t,
                                                           Where = (WHERE TRUE),
                                                           GroupBy = sql_subset_all [_],
                                                           Having = (WHERE TRUE),
                                                           SelectFields = sql_subset [[T = ([this = _] ++ mapU (option string) assignees, _), Times = ([key = _], _)] ++ mapU ([], _) assignees],
                                                           SelectExps = {Preferred = sql_window
                                                                                         (@fold [fn assignees => irrel :: {{Type}} -> others :: {Unit} -> [others ~ assignees] => [irrel ~ others ++ assignees]
                                                                                                    => sql_exp (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                               (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                               [] int]
                                                                                           (fn [nm ::_] [u ::_] [assignees ::_] [[nm] ~ assignees]
                                                                                                        (acc : irrel :: {{Type}} -> others :: {Unit} -> [others ~ assignees] => [irrel ~ others ++ assignees]
                                                                                                         => sql_exp (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                                    (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                                    [] int)
                                                                                                        [irrel ::_] [others ::_] [others ~ [nm] ++ assignees] [irrel ~ others ++ [nm] ++ assignees] =>
                                                                                               (SQL {acc [irrel] [[nm] ++ others]}
                                                                                                  + (IF COALESCE({{nm}}.{preferred}, FALSE) THEN 1 ELSE 0)))
                                                                                           (fn [irrel ::_] [others ::_] [others ~ []] [irrel ~ others] => (SQL 0))
                                                                                           fl [_] [[]] ! !),
                                                                         SubPreferred = sql_window (sql_exp_weaken (SchedulingAddons.preferred [#SubPreferred] [#SubUnpreferred] schedAddon (SQL t.{this}) (SQL times.{key}))),
                                                                         SubUnpreferred = sql_window (sql_exp_weaken (SchedulingAddons.unpreferred [#SubPreferred] [#SubUnpreferred] schedAddon (SQL t.{this}) (SQL times.{key}))),
                                                                         Complete = sql_window
                                                                                         (@fold [fn assignees => irrel :: {{Type}} -> others :: {Unit} -> [others ~ assignees] => [irrel ~ others ++ assignees]
                                                                                                    => sql_exp (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                               (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                               [] bool]
                                                                                           (fn [nm ::_] [u ::_] [assignees ::_] [[nm] ~ assignees]
                                                                                                        (acc : irrel :: {{Type}} -> others :: {Unit} -> [others ~ assignees] => [irrel ~ others ++ assignees]
                                                                                                         => sql_exp (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                                    (mapU [btime = option time, user = option string, preferred = option bool] (others ++ assignees) ++ irrel)
                                                                                                                    [] bool)
                                                                                                        [irrel ::_] [others ::_] [others ~ [nm] ++ assignees] [irrel ~ others ++ [nm] ++ assignees] =>
                                                                                               (SQL {acc [irrel] [[nm] ++ others]}
                                                                                                  AND NOT ({{nm}}.{preferred} IS NULL)))
                                                                                           (fn [irrel ::_] [others ::_] [others ~ []] [irrel ~ others] => (SQL TRUE))
                                                                                           fl [_] [[]] ! !)}}}}}
                                            ORDER BY times.{key} DESC, Preferred, Complete, SubPreferred, SubUnpreferred, t.{this} DESC)
                                           (fn r acc => return (collectTimes r acc)) [];
                              evs <- List.mapQueryM (SELECT t.{this}, t.{ttime}
                                                     FROM t)
                                                    (fn r => tm <- source r.T.ttime; return (r.T.this, tm));
                              tms <- List.mapM (fn (tm, choices) =>
                                                   choices <- List.mapM (fn (k, assignees, n) =>
                                                                            s <- (case List.assoc k evs of
                                                                                      None => source None
                                                                                    | Some s => return s);
                                                                            return (k, assignees, n, s)) choices;
                                                   return (tm, choices)) tms;
                              return (evs, tms)
                          else
                              tms <- List.mapQuery (SELECT * FROM times)
                                                   (fn r => (r.Times.key, []));
                              return ([], tms));

            ctx <- source None;
            chs <- source None;
            empty <- source (case tms of
                                 [] => True
                               | _ => False);
            delbutton <- return (fn cal ev => <xml>
              <i class="glyphicon glyphicon-trash float-right" style="cursor: pointer"
                 onclick={fn _ =>
                             start <- FullCalendar.eventStart ev;
                             rpc (del start);
                             FullCalendar.removeEvent ev;
                             evs <- FullCalendar.events cal;
                             set empty (case evs of
                                            [] => True
                                          | _ => False)}/>
            </xml>);
            cal <- FullCalendar.create {DefaultDate = case tms of
                                                          [] => None
                                                        | (tm, _) :: _ => Some tm,
                                        AllDaySlot = False,
                                        SlotDuration = None,
                                        SnapDuration = None,
                                        Content = Some (fn cal ev => {Header = <xml>
              <dyn signal={ctx <- signal ctx;
                           case ctx of
                               None => return <xml></xml>
                             | Some ctx => return <xml>
                               <active code={ido <- FullCalendar.eventId ev;
                                             if idoIsSpecial ido then
                                                 return <xml></xml>
                                             else
                                                 tm <- FullCalendar.eventStart ev;
                                                 case List.assoc tm tms of
                                                     None => return (delbutton cal ev)
                                                   | Some [] => return (delbutton cal ev)
                                                   | Some choices => return <xml>
                                                     <dyn signal={used <- List.existsM (fn (_, _, _, tmS) =>
                                                                                           tm' <- signal tmS;
                                                                                           return (tm' = Some tm)) choices;
                                                                  return (if used then
                                                                              <xml></xml>
                                                                          else
                                                                              delbutton cal ev)}/>
                                                     {Ui.modalIcon ctx
                                                                   (CLASS "float-right glyphicon glyphicon-pencil-alt")
                                                                   (ch <- get chs;
                                                                    ch <- return (case ch of None => error <xml>ChangeWatcher not set yet</xml>
                                                                                           | Some ch => ch);
                                                                    choices <- List.mapM (fn (k, assignees, np, tmS) =>
                                                                                             tm' <- get tmS;
                                                                                             s <- source (tm' = Some tm);
                                                                                             return (k, assignees, np, tmS, s)) choices;
                                                                    return (Ui.modal (List.app (fn (k, _, _, tmS, s) =>
                                                                                                   tm' <- get tmS;
                                                                                                   b <- get s;
                                                                                                   if b then
                                                                                                       if tm' = Some tm then
                                                                                                           return ()
                                                                                                       else
                                                                                                           rpc (setTime (ChangeWatcher.server ch) k (Some tm));
                                                                                                           set tmS (Some tm)
                                                                                                   else
                                                                                                       if tm' = Some tm then
                                                                                                           rpc (setTime (ChangeWatcher.server ch) k None);
                                                                                                           set tmS None
                                                                                                       else
                                                                                                           return ()) choices)
                                                                                     <xml>What should we schedule here?</xml>
                                                                                     <xml><ul class="list-group">
                                                                                       {List.mapX (fn (k, assignees, np, _, s) => <xml><li class="list-group-item">
                                                                                         <ccheckbox source={s}/>
                                                                                         {[k]}
                                                                                         {@mapUX [string] [body]
                                                                                           (fn [nm ::_] [r ::_] [[nm] ~ r] u => <xml> [{[u]}]</xml>)
                                                                                           fl assignees}
                                                                                         {stars np}
                                                                                         </li></xml>) choices}
                                                                                     </ul></xml>
                                                                                     <xml>Save</xml>))}
                                                     <dyn signal={count <- List.foldlM (fn (_, _, _, tmS) acc =>
                                                                                           tm' <- signal tmS;
                                                                                           return (case tm' of
                                                                                                       None => 1 + acc
                                                                                                     | Some _ => acc)) 0 choices;
                                                                  return (if count = 0 then
                                                                              <xml></xml>
                                                                          else
                                                                              <xml><span class="badge badge-pill badge-warning float-right">{[count]}</span></xml>)}/>
                                                   </xml>}/>

                                               {(CalendarAddons.withinEvent addon ctx cal ev).Header}
                             </xml>}/>
            </xml>,
            Body = <xml>
              <active code={ido <- FullCalendar.eventId ev;
                            if idoIsSpecial ido then
                                return <xml></xml>
                            else
                                tm <- FullCalendar.eventStart ev;
                                case List.assoc tm tms of
                                    None => return <xml></xml>
                                  | Some choices =>
                                    let
                                        fun renderEvent (k, prefs, assignees) = <xml>
                                          {[k]}{@mapUX [string] [body]
                                                 (fn [nm ::_] [r ::_] [[nm] ~ r] u => <xml> [{[u]}]</xml>)
                                                 fl assignees}{stars prefs}
                                        </xml>
                                    in
                                        return <xml>
                                          <dyn signal={evs <- List.mapPartialM (fn (k, assignees, prefs, tmS) =>
                                                                                   tm' <- signal tmS;
                                                                                   return (if tm' = Some tm then
                                                                                               Some (k, prefs, assignees)
                                                                                           else
                                                                                               None)) choices;
                                                       return (case evs of
                                                                   [] => <xml></xml>
                                                                 | ev1 :: [] => <xml>{renderEvent ev1}</xml>
                                                                 | ev1 :: evs' => <xml>{renderEvent ev1}{List.mapX (fn ev => <xml>, {renderEvent ev}</xml>) evs'}</xml>)}/>
                                        </xml>
                                    end}/>
              <dyn signal={ctx <- signal ctx;
                           case ctx of
                               None => return <xml></xml>
                             | Some ctx => return (CalendarAddons.withinEvent addon ctx cal ev).Body}/>
            </xml>}),
            OnSelect = Some (fn cal tm _ =>
                                evs <- FullCalendar.events cal;
                                alreadyUsed <- List.existsM (fn ev => tm' <- FullCalendar.eventStart ev;
                                                                if tm' = tm then
                                                                    ido <- FullCalendar.eventId ev;
                                                                    return (not (idoIsSpecial ido))
                                                                else
                                                                    return False) evs;
                                if alreadyUsed then
                                    FullCalendar.unselect cal
                                else
                                    rpc (add tm);
                                    addEvent cal tm;
                                    set empty False),
            OnDrop = None};
            ch <- ChangeWatcher.listen tTitle;
            bch <- ChangeWatcher.listen bidTitle;
            source {Times = tms,
                    Events = List.filter (fn (k, _) =>
                                             List.exists (fn (_, choices) =>
                                                             List.exists (fn (k', _, _, _) => k' = k) choices) tms) evs,
                    Calendar = cal,
                    Context = ctx,
                    ChangesSource = chs,
                    Changes = ch,
                    BidChanges = bch,
                    Empty = empty}
        end

    fun initCalendar self =
        List.app (fn (tm, _) => addEvent self.Calendar tm) self.Times

    val allEvents =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ => queryL1 (SELECT t.{this}, t.{ttime} FROM t)

    fun rcreate ch bch =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            ChangeWatcher.retire ch;
            ChangeWatcher.retire bch;
            create

    fun onload selfS =
        self <- get selfS;
        set self.ChangesSource (Some self.Changes);
        initCalendar self;
        ChangeWatcher.onChange self.Changes
        (evs <- rpc allEvents;
         changed <- List.existsM (fn r =>
                                     List.allM (fn (k, tmS) =>
                                                   tm <- get tmS;
                                                   return (r.this <> k || r.ttime <> tm)) self.Events) evs;
         if not changed then
             return ()
         else
             self <- rpc (rcreate (ChangeWatcher.server self.Changes) (ChangeWatcher.server self.BidChanges));
             self <- get self;
             set selfS self;
             onload selfS);
        ChangeWatcher.onChange self.BidChanges
                               (self <- rpc (rcreate (ChangeWatcher.server self.Changes) (ChangeWatcher.server self.BidChanges));
                                self <- get self;
                                set selfS self;
                                onload selfS)

    fun render ctx self = <xml>
      <dyn signal={self <- signal self;
                   unscheduled <- List.mapPartialM (fn (k, tmS) =>
                                                       tm <- signal tmS;
                                                       return (case tm of
                                                                   None => Some k
                                                                 | Some _ => None)) self.Events;
                   return (case unscheduled of
                               [] => <xml></xml>
                             | k1 :: ks => <xml><div><b>Unscheduled:</b> {[k1]}{List.mapX (fn k => <xml>, {[k]}</xml>) ks}</div></xml>)}/>

      <dyn signal={self <- signal self;
                   return (CalendarAddons.aboveCalendar addon ctx self.Calendar)}/>

      <dyn signal={self <- signal self;
                   return <xml>
                     <active code={set self.Context (Some ctx);
                                   return (FullCalendar.render self.Calendar)}/>
                   </xml>}/>
    </xml>

    fun notification _ self = <xml>
      <dyn signal={self <- signal self;
                   empty <- signal self.Empty;
                   return (if empty then
                               <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                           else
                               <xml></xml>)}/>
      <dyn signal={self <- signal self;
                   unscheduled <- List.foldlM (fn (_, tmS) n =>
                                                  tm <- signal tmS;
                                                  return (case tm of
                                                              None => n + 1
                                                            | Some _ => n)) 0 self.Events;
                   return (if unscheduled = 0 then
                               <xml></xml>
                           else
                               <xml><span class="badge badge-pill badge-warning">{[unscheduled]}</span></xml>)}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification}
end
