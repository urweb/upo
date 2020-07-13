open Bootstrap4

functor Make(M : sig
                 con key :: Name
                 type keyT
                 con tm :: Name
                 con others :: {Type}
                 constraint [key] ~ [tm]
                 constraint [key, tm] ~ others
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val t : sql_table ([key = keyT, tm = option time] ++ others) ([keyName = [key]] ++ otherKeys)

                 val eq_key : eq keyT
                 val read_key : read keyT
                 val show_key : show keyT
                 val inj_key : sql_injectable keyT

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t ([key = keyT, tm = option time] ++ others)
                 val schedAddon : SchedulingAddons.t keyT
             end) = struct
    open M

    type pref = {
         Preferred : int,
         Unpreferred : int
    }

    type a = {Calendar : FullCalendar.t,
              ExtraEvents : list CalendarAddons.event_data,
              Settable : list (keyT * (list (time * pref) * source (option time))),
              InitialTimes : list (keyT * time),
              Context : source (option Ui.context)}

    fun addEvent cal settable tm =
        Monad.ignore (FullCalendar.addEvent cal
                                            {Id = None,
                                             AllDay = False,
                                             Start = tm,
                                             End = None,
                                             Title = Option.get "" settable,
                                             Rendering = if Option.isNone settable then
                                                             FullCalendar.Normal
                                                         else
                                                             FullCalendar.Background,
                                             TextColor = if Option.isNone settable then
                                                             None
                                                         else
                                                             Some (return (Some "black")),
                                             BackgroundColor = if Option.isNone settable then
                                                                   None
                                                               else
                                                                   Some (return (Some "gray"))})

    fun setTime k tmo =
        uo <- whoami;
        allowed <- oneRowE1 (SELECT COUNT( * ) > 0
                             FROM t
                             WHERE t.{key} = {[k]}
                               AND {Option.get (WHERE TRUE) (CalendarAddons.mayEdit addon [#T] [#U] uo)});
        if not allowed then
            error <xml>Access denied</xml>
        else
            dml (UPDATE t
                 SET {tm} = {[tmo]}
                 WHERE T.{key} = {[k]})

    fun stars r =
        let
            fun one cls n =
                if n <= 0 then
                    <xml></xml>
                else
                    <xml> <i class={classes glyphicon cls}/> {[n]}</xml>
        in
            <xml>{one glyphicon_smile r.Preferred}{one glyphicon_meh r.Unpreferred}</xml>
        end

    val create =
        uo <- whoami;
        firstFuture <- oneRowE1 (SELECT MIN(t.{tm})
                                 FROM t
                                 WHERE t.{tm} > {sql_nullable (SQL CURRENT_TIMESTAMP)});
        (inits, settable) <- (case SchedulingAddons.times schedAddon of
                                  None =>
                                  ts <- queryL (SELECT t.{key}, t.{tm}, {sql_exp_weaken (Option.get (WHERE TRUE) (CalendarAddons.mayEdit addon [#T] [#U] uo))} AS MayEdit
                                                FROM t
                                                WHERE {Option.get (WHERE TRUE) (CalendarAddons.maySee addon [#T] [#U] uo)}
                                                ORDER BY t.{key});
                                  inits <- return (List.mapPartial (fn r => Option.mp (fn tm => (r.T.key, tm)) r.T.tm) ts);
                                  settable <- List.mapPartialM (fn r =>
                                               if r.MayEdit then
                                                   s <- source r.T.tm;
                                                   return (Some (r.T.key, ([], s)))
                                               else
                                                   return None) ts;
                                  return (inits, settable)
                                | Some times =>
                                  let
                                      fun build r (acc : list (keyT * bool * list (time * pref) * option time)) =
                                          case acc of
                                              [] => (r.T.key, r.MayEdit, (r.Times.Time, {Preferred = r.Preferred, Unpreferred = r.Unpreferred}) :: [], r.T.tm) :: []
                                            | (k, _, tms, tm) :: acc' =>
                                              if k = r.T.key then
                                                  (k, r.MayEdit, (r.Times.Time, {Preferred = r.Preferred, Unpreferred = r.Unpreferred}) :: tms, tm) :: acc'
                                              else
                                                  (r.T.key, r.MayEdit, (r.Times.Time, {Preferred = r.Preferred, Unpreferred = r.Unpreferred}) :: [], r.T.tm) :: acc
                                  in
                                      ts <- query (SELECT t.{key}, t.{tm}, {sql_exp_weaken (Option.get (WHERE TRUE) (CalendarAddons.mayEdit addon [#T] [#U] uo))} AS MayEdit,
                                                     Times.Time,
                                                     {sql_exp_weaken (SchedulingAddons.preferred [#T1] [#T2] schedAddon (SQL t.{key}) (SQL Times.Time))} AS Preferred,
                                                     {sql_exp_weaken (SchedulingAddons.unpreferred [#T1] [#T2] schedAddon (SQL t.{key}) (SQL Times.Time))} AS Unpreferred
                                                   FROM t, ({{times}}) AS Times
                                                   WHERE {sql_exp_weaken (Option.get (WHERE TRUE) (CalendarAddons.maySee addon [#T] [#U] uo))}
                                                   ORDER BY t.{key} DESC)
                                                     (fn r acc => return (build r acc)) [];
                                      inits <- return (List.mapPartial (fn (k, _, _, tm) => Option.mp (fn tm => (k, tm)) tm) ts);
                                      settable <- List.mapPartialM (fn (k, me, tms, tm) =>
                                                                       if me then
                                                                           s <- source tm;
                                                                           return (Some (k, (tms, s)))
                                                                       else
                                                                           return None) ts;
                                      return (inits, settable)
                                  end);
        removeEmpty <- return (fn cal =>
                                  evs <- FullCalendar.events cal;
                                  List.app (fn ev =>
                                               rendering <- FullCalendar.eventRendering ev;
                                               case rendering of
                                                   FullCalendar.Background => return ()
                                                 | _ =>
                                                   tm' <- FullCalendar.eventStart ev;
                                                   used <- List.existsM (fn (_, (_, tms)) =>
                                                                            tmo <- get tms;
                                                                            return (tmo = Some tm')) settable;
                                                   if used then
                                                       return ()
                                                   else
                                                       FullCalendar.removeEvent ev) evs);

        ctx <- source None;
        cal <- FullCalendar.create {DefaultDate = firstFuture,
                                    AllDaySlot = False,
                                    SlotDuration = None,
                                    SnapDuration = None,
                                    Content = Some (fn cal ev =>
                                                       {Header = <xml>
                                                         <active code={ctx <- get ctx;
                                                                       case ctx of
                                                                           None => error <xml>Context not initialized</xml>
                                                                         | Some ctx =>
                                                                           tm <- FullCalendar.eventStart ev;
                                                                           rendering <- FullCalendar.eventRendering ev;
                                                                           return (case rendering of
                                                                                       FullCalendar.Background => <xml></xml>
                                                                                     | _ => Ui.modalIcon ctx (CLASS "glyphicon glyphicon-pencil-alt float-right")
                                                                                                         (choices <- List.mapM (fn (k, (prefs, tms)) =>
                                                                                                                                   tmo <- get tms;
                                                                                                                                   s <- source (tmo = Some tm);
                                                                                                                                   return (k, prefs, tms, s)) settable;
                                                                                                          return (Ui.modal
                                                                                                                      (List.app (fn (k, _, tms, s) =>
                                                                                                                                    b <- get s;
                                                                                                                                    if b then
                                                                                                                                        rpc (setTime k (Some tm));
                                                                                                                                        set tms (Some tm)
                                                                                                                                    else
                                                                                                                                        tmo <- get tms;
                                                                                                                                        if tmo = Some tm then
                                                                                                                                            rpc (setTime k None);
                                                                                                                                            set tms None
                                                                                                                                        else
                                                                                                                                            return ()) choices;
                                                                                                                       removeEmpty cal)
                                                                                                                      <xml>What should we schedule at this time?</xml>
                                                                                                                      <xml><ul class="list-group">
                                                                                                                        {List.mapX (fn (k, prefs, _, s) => <xml><li class="list-group-item">
                                                                                                                          <ccheckbox source={s}/> {[k]}{case List.assoc tm prefs of
                                                                                                                                                            None => <xml></xml>
                                                                                                                                                          | Some prefs => stars prefs}
                                                                                                                         </li></xml>) choices}
                                                                                                                      </ul></xml>
                                                                                                                      <xml>Save</xml>)))}/>
                                                        </xml>,
                                                        Body = <xml>
                                                          <active code={tm <- FullCalendar.eventStart ev;
                                                                        return <xml>
                                                                          <dyn signal={settables <- List.mapPartialM (fn (k, (tms, s)) =>
                                                                                                                         tmo <- signal s;
                                                                                                                         return (if tmo <> Some tm then
                                                                                                                                     None
                                                                                                                                 else
                                                                                                                                     Some <xml><b>{[k]}{case List.assoc tm tms of
                                                                                                                                                            None => <xml></xml>
                                                                                                                                                          | Some prefs => stars prefs}</b></xml>)) settable;
                                                                                       unsettables <- return (List.mapPartial (fn (k, tm') =>
                                                                                                                                  if tm' <> tm then
                                                                                                                                      None
                                                                                                                                  else
                                                                                                                                      case List.assoc k settable of
                                                                                                                                          Some _ => None
                                                                                                                                        | None => Some <xml>{[k]}</xml>) inits);
                                                                                       combined <- return (List.append settables unsettables);
                                                                                       return (case combined of
                                                                                                   [] => <xml></xml>
                                                                                                 | x :: combined' => <xml>{x}{List.mapX (fn y => <xml>, {y}</xml>) combined'}</xml>)}/>
                                                                        </xml>}/>
                                                        </xml>}),
                                    OnSelect = Some (fn cal tm _ =>
                                                        ctx <- get ctx;
                                                        case ctx of
                                                            None => error <xml>Calendar not ready yet</xml>
                                                          | Some ctx =>
                                                            choices <- List.mapM (fn (k, (prefs, tms)) =>
                                                                                     tmo <- get tms;
                                                                                     s <- source (tmo = Some tm);
                                                                                     return (k, prefs, tms, s)) settable;
                                                            (case choices of
                                                                 [] => return ()
                                                               | (k, _, tms, _) :: [] =>
                                                                 rpc (setTime k (Some tm)); set tms (Some tm); removeEmpty cal; addEvent cal None tm
                                                               | _ :: _ :: _ =>
                                                                 Ui.activateModal ctx
                                                                                  (Ui.modal
                                                                                       (addEvent cal None tm;
                                                                                        List.app (fn (k, _, tms, s) =>
                                                                                                     b <- get s;
                                                                                                     if b then
                                                                                                         rpc (setTime k (Some tm));
                                                                                                         set tms (Some tm)
                                                                                                     else
                                                                                                         tmo <- get tms;
                                                                                                         if tmo = Some tm then
                                                                                                             rpc (setTime k None);
                                                                                                             set tms None
                                                                                                         else
                                                                                                             return ()) choices;
                                                                                        removeEmpty cal)
                                                                                       <xml>What should we schedule at this time?</xml>
                                                                                       <xml><ul class="list-group">
                                                                                         {List.mapX (fn (k, prefs, _, s) => <xml><li class="list-group-item">
                                                                                           <ccheckbox source={s}/> {[k]}{case List.assoc tm prefs of
                                                                                                                             None => <xml></xml>
                                                                                                                           | Some prefs => stars prefs}
                                                                                         </li></xml>) choices}
                                                                                       </ul></xml>
                                                                                       <xml>Save</xml>))),
                                    OnDrop = Some (fn cal from to =>
                                                      old <- FullCalendar.eventStart from;
                                                      new <- FullCalendar.eventStart to;
                                                      List.app (fn (k, (_, tms)) =>
                                                                   tmo <- get tms;
                                                                   if tmo = Some old then
                                                                       set tms (Some new);
                                                                       rpc (setTime k (Some new))
                                                                   else
                                                                       return ()) settable;
                                                      removeEmpty cal)};
        extras <- CalendarAddons.extraEvents addon uo;
        return {Calendar = cal, ExtraEvents = extras, Settable = settable, InitialTimes = inits, Context = ctx}

    fun onload self =
        let
            val events = List.foldl (fn (k, tm) events =>
                                        case List.assoc tm events of
                                            None => (tm, Option.isSome (List.assoc k self.Settable)) :: events
                                          | Some _ => List.mp (fn (tm', settable) =>
                                                                  (tm', if tm' = tm then
                                                                            settable || Option.isSome (List.assoc k self.Settable)
                                                                        else
                                                                            settable)) events) [] self.InitialTimes
        in
            List.app (fn ev =>
                         Monad.ignore (FullCalendar.addEvent self.Calendar
                                                             {Id = ev.Id,
                                                              AllDay = False,
                                                              Start = ev.Start,
                                                              End = None,
                                                              Title = ev.Title,
                                                              Rendering = if ev.Background then
                                                                              FullCalendar.Background
                                                                          else
                                                                              FullCalendar.Normal,
                                                              TextColor = Option.mp (fn c => return (Some c)) ev.TextColor,
                                                              BackgroundColor = Option.mp (fn c => return (Some c)) ev.BackgroundColor})) self.ExtraEvents;
            List.app (fn (tm, settable) =>
                         matching <- return (List.mapPartial (fn (k, tm') =>
                                                                 case List.assoc k self.Settable of
                                                                     Some _ => None
                                                                   | None =>
                                                                     if tm' = tm then
                                                                         Some k
                                                                     else
                                                                         None) self.InitialTimes);
                         (case matching of
                              [] => return ()
                            | k :: ks => addEvent self.Calendar (Some (List.foldl (fn k s => s ^ ", " ^ show k) (show k) ks)) tm);
                         if settable then
                             addEvent self.Calendar None tm
                         else
                             return ()) events
        end

    fun render ctx self = <xml>
      <dyn signal={unsched <- List.mapPartialM (fn (k, (_, tms)) =>
                                                   tmo <- signal tms;
                                                   return (case tmo of
                                                               None => Some k
                                                             | Some _ => None)) self.Settable;
                   return (case unsched of
                               [] => <xml></xml>
                             | k1 :: ks => <xml><div><b>Unscheduled:</b> {[k1]}{List.mapX (fn k => <xml>, {[k]}</xml>) ks}</div></xml>)}/>
      <active code={set self.Context (Some ctx);
                    return (FullCalendar.render self.Calendar)}/>
    </xml>

    fun notification _ self = <xml>
      <dyn signal={unplaced <- List.existsM (fn (_, (_, tms)) => tmo <- signal tms; return (Option.isNone tmo)) self.Settable;
                   return (if unplaced then
                               <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>
                           else
                               <xml></xml>)}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification}
end

