open Bootstrap4

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

                 con this :: Name
                 con thisT :: Type
                 con ttime :: Name
                 con r :: {Type}
                 constraint [this] ~ [ttime]
                 constraint [this, ttime] ~ r
                 table t : ([this = thisT, ttime = option time] ++ r)
                 val tTitle : string

                 val show_this : show thisT
                 val read_this : read thisT
                 val eq_this : eq thisT
                 val inj_this : sql_injectable thisT

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([this = thisT, ttime = option time] ++ r)
                 val schedAddon : SchedulingAddons.t thisT
                 val slotDuration : option string
             end) = struct
    open M

    val halvedDuration = Option.mp FullCalendar.halveDuration slotDuration

    val eventLengthInSeconds = case slotDuration of
                                   None => 60 * 60
                                 | Some d => FullCalendar.durationToSeconds d

    type prefs = {
         SubPreferred : int,
         SubUnpreferred : int
    }

    type a = source {Times : list (time * list (thisT * prefs * source (option time))),
                     Events : list (thisT * source (option time)),
                     Calendar : FullCalendar.t,
                     Context : source (option Ui.context),
                     Changes : ChangeWatcher.client_part,
                     ChangesSource : source (option ChangeWatcher.client_part),
                     Empty : source bool}

    fun stars r =
        let
            fun one cls n =
                if n <= 0 then
                    <xml></xml>
                else
                    <xml> <i class={classes glyphicon cls}/> {[n]}</xml>
        in
            <xml>{one glyphicon_smile r.SubPreferred}{one glyphicon_meh r.SubUnpreferred}</xml>
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
            fun collectTimes r (acc : list (time * list (thisT * prefs))) =
                let
                    val k = r.T.this
                    val tm = r.Times.key
                    val pref = {SubPreferred = r.SubPreferred,
                                SubUnpreferred = r.SubUnpreferred}
                    fun doCons items = (k, pref) :: items
                in
                    case acc of
                        [] => (tm, doCons []) :: []
                      | (tm', items) :: acc' =>
                        if tm' = tm then
                            (tm, doCons items) :: acc'
                        else
                            (tm, doCons []) :: acc
                end
        in
            anyInTabs <- oneRowE1 (SELECT COUNT( * ) > 0
                                   FROM t);
            (evs, tms) <- (if anyInTabs then
                               tms <- query (SELECT t.{this}, times.{key},
                                               {sql_exp_weaken (SchedulingAddons.preferred [#SubPreferred] [#SubUnpreferred] schedAddon (SQL t.{this}) (SQL times.{key}))} AS SubPreferred,
                                               {sql_exp_weaken (SchedulingAddons.unpreferred [#SubPreferred] [#SubUnpreferred] schedAddon (SQL t.{this}) (SQL times.{key}))} AS SubUnpreferred
                                             FROM t, times
                                            ORDER BY times.{key} DESC, SubPreferred, SubUnpreferred, t.{this} DESC)
                                           (fn r acc => return (collectTimes r acc)) [];
                              evs <- List.mapQueryM (SELECT t.{this}, t.{ttime}
                                                     FROM t)
                                                    (fn r => tm <- source r.T.ttime; return (r.T.this, tm));
                              tms <- List.mapM (fn (tm, choices) =>
                                                   choices <- List.mapM (fn (k, n) =>
                                                                            s <- (case List.assoc k evs of
                                                                                      None => source None
                                                                                    | Some s => return s);
                                                                            return (k, n, s)) choices;
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
                                        SlotDuration = halvedDuration,
                                        SnapDuration = slotDuration,
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
                                                     <dyn signal={used <- List.existsM (fn (_, _, tmS) =>
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
                                                                    choices <- List.mapM (fn (k, np, tmS) =>
                                                                                             tm' <- get tmS;
                                                                                             s <- source (tm' = Some tm);
                                                                                             return (k, np, tmS, s)) choices;
                                                                    showUnpreferred <- source False;
                                                                    return (Ui.modal (List.app (fn (k, _, tmS, s) =>
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
                                                                                     <xml><dyn signal={su <- signal showUnpreferred;
                                                                                                       return <xml>
                                                                                                         <ul class="list-group">
                                                                                                           {List.mapX (fn (k, np, _, s) =>
                                                                                                                          if su || np.SubPreferred + np.SubUnpreferred > 0 then <xml>
                                                                                                                            <li class="list-group-item">
                                                                                                                              <ccheckbox source={s}/>
                                                                                                                              {[k]}
                                                                                                                              {stars np}
                                                                                                                            </li></xml>
                                                                                                                          else <xml></xml>) choices}
                                                                                                         </ul>
                                                                                                         <button class="btn btn-primary"
                                                                                                                 onclick={fn _ => set showUnpreferred (not su)}>
                                                                                                           {if su then
                                                                                                                <xml>Hide</xml>
                                                                                                            else
                                                                                                                <xml>Show</xml>} choices where no one likes this time
                                                                                                         </button>
                                                                                                       </xml>}/></xml>
                                                                                     <xml>Save</xml>))}
                                                     <dyn signal={count <- List.foldlM (fn (_, np, tmS) acc =>
                                                                                           tm' <- signal tmS;
                                                                                           return (case tm' of
                                                                                                       Some _ => acc
                                                                                                     | None =>
                                                                                                       if np.SubPreferred + np.SubUnpreferred > 0 then
                                                                                                           1 + acc
                                                                                                       else
                                                                                                           acc)) 0 choices;
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
                                        fun renderEvent (k, prefs) = <xml>
                                          {[k]}{stars prefs}
                                        </xml>
                                    in
                                        return <xml>
                                          <dyn signal={evs <- List.mapPartialM (fn (k, prefs, tmS) =>
                                                                                   tm' <- signal tmS;
                                                                                   return (if tm' = Some tm then
                                                                                               Some (k, prefs)
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
            source {Times = tms,
                    Events = List.filter (fn (k, _) =>
                                             List.exists (fn (_, choices) =>
                                                             List.exists (fn (k', _, _) => k' = k) choices) tms) evs,
                    Calendar = cal,
                    Context = ctx,
                    ChangesSource = chs,
                    Changes = ch,
                    Empty = empty}
        end

    fun initCalendar self =
        List.app (fn (tm, _) => addEvent self.Calendar tm) self.Times

    val allEvents =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ => queryL1 (SELECT t.{this}, t.{ttime} FROM t)

    fun rcreate ch =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            ChangeWatcher.retire ch;
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
             self <- rpc (rcreate (ChangeWatcher.server self.Changes));
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
