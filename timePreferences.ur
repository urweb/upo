open Bootstrap4

val eventLengthInSeconds = 60 * 90

functor Make(M : sig
                 con key :: Name
                 con kother :: {Type}
                 constraint [key] ~ kother
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val times : sql_table ([key = time] ++ kother) ([keyName = [key]] ++ otherKeys)

                 con user :: Name
                 con tcol :: Name
                 con preferred :: Name
                 constraint [user] ~ [tcol]
                 constraint [user, tcol] ~ [preferred]
                 table prefs : {user : string, tcol : time, preferred : bool}
                 val title : string

                 val whoami : transaction (option string)

                 val addon : CalendarAddons.t [key = time]
             end) = struct
    open M

    datatype level = Unavailable | Available | Preferred

    type input = string
    type a = {Calendar : FullCalendar.t,
              Choices : list (time * source level),
              Context : source (option Ui.context)}

    val changed = ChangeWatcher.changed title

    fun unavail tm =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u =>
            dml (DELETE FROM prefs
                 WHERE T.{tcol} = {[tm]}
                   AND T.{user} = {[u]});
            changed

    fun avail tm =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u =>
            dml (DELETE FROM prefs
                 WHERE T.{tcol} = {[tm]}
                   AND T.{user} = {[u]});
            dml (INSERT INTO prefs({tcol}, {user}, {preferred})
                 VALUES ({[tm]}, {[u]}, FALSE));
            changed

    fun prefed tm =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u =>
            dml (DELETE FROM prefs
                 WHERE T.{tcol} = {[tm]}
                   AND T.{user} = {[u]});
            dml (INSERT INTO prefs({tcol}, {user}, {preferred})
                 VALUES ({[tm]}, {[u]}, TRUE));
            changed

    fun create u =
        firstFuture <- oneRowE1 (SELECT MIN(times.{key})
                                 FROM times
                                 WHERE times.{key} > CURRENT_TIMESTAMP);
        cs <- List.mapQueryM (SELECT times.{key}, prefs.{preferred}
                              FROM times LEFT JOIN prefs
                                ON prefs.{tcol} = times.{key}
                                  AND prefs.{user} = {[u]}
                              ORDER BY times.{key})
              (fn r =>
                  s <- source (case r.Prefs.preferred of
                                   None => Unavailable
                                 | Some False => Available
                                 | Some True => Preferred);
                  return (r.Times.key, s));
        ctx <- source None;
        cal <- FullCalendar.create {DefaultDate = firstFuture,
                                    AllDaySlot = False,
                                    Content = Some (fn cal ev =>
                                                       {Header = <xml>
                                                         <active code={start <- FullCalendar.eventStart ev;
                                                                       case List.assoc start cs of
                                                                           None => return <xml></xml>
                                                                         | Some s =>
                                                                           idu <- fresh;
                                                                           ida <- fresh;
                                                                           idp <- fresh;
                                                                           return <xml>
                                                                             <i dynClass={s <- signal s;
                                                                                          return (case s of
                                                                                                      Unavailable => CLASS "float-right glyphicon-lg fas glyphicon-frown"
                                                                                                    | _ => CLASS "float-right glyphicon-lg far glyphicon-frown")}
                                                                                id={idu} style="cursor: pointer"
                                                                                data-toggle="tooltip" data-placement="bottom" title="Unavailable"
                                                                                onclick={fn _ => rpc (unavail start); set s Unavailable}/>
                                                                             <i dynClass={s <- signal s;
                                                                                          return (case s of
                                                                                                      Available => CLASS "float-right glyphicon-lg fas glyphicon-meh"
                                                                                                    | _ => CLASS "float-right glyphicon-lg far glyphicon-meh")}
                                                                                id={ida} style="cursor: pointer"
                                                                                data-toggle="tooltip" data-placement="bottom" title="Available"
                                                                                onclick={fn _ => rpc (avail start); set s Available}/>
                                                                             <i dynClass={s <- signal s;
                                                                                          return (case s of
                                                                                                      Preferred => CLASS "float-right glyphicon-lg fas glyphicon-smile"
                                                                                                    | _ => CLASS "float-right glyphicon-lg far glyphicon-smile")}
                                                                                id={idp} style="cursor: pointer"
                                                                                data-toggle="tooltip" data-placement="bottom" title="Preferred"
                                                                                onclick={fn _ => rpc (prefed start); set s Preferred}/>
                                                                             <active code={Ui.tooltip idu;
                                                                                           Ui.tooltip ida;
                                                                                           Ui.tooltip idp;
                                                                                           return <xml></xml>}/>
                                                                           </xml>}/>

                                                         <dyn signal={ctx <- signal ctx;
                                                                      return (case ctx of
                                                                                  None => <xml></xml>
                                                                                | Some ctx => (CalendarAddons.withinEvent addon ctx cal ev).Header)}/>
                                                        </xml>,
                                                        Body = <xml>
                                                          <dyn signal={ctx <- signal ctx;
                                                                       return (case ctx of
                                                                                   None => <xml></xml>
                                                                                 | Some ctx => (CalendarAddons.withinEvent addon ctx cal ev).Body)}/>
                                                        </xml>}),
                                    OnSelect = None,
                                    OnDrop = None};
        return {Calendar = cal, Choices = cs, Context = ctx}

    fun addEvent cal tm s =
        Monad.ignore (FullCalendar.addEvent cal
                                            {Id = None,
                                             AllDay = False,
                                             Start = tm,
                                             End = Some (addSeconds tm eventLengthInSeconds),
                                             Title = "Are you available?",
                                             Rendering = FullCalendar.Normal,
                                             TextColor = Some (l <- signal s;
                                                                     return (case l of
                                                                                 Unavailable => Some "#cc1515"
                                                                               | _ => None)),
                                             BackgroundColor = Some (l <- signal s;
                                                                     return (case l of
                                                                                 Unavailable => Some "#ff0c0c8c"
                                                                               | _ => None))})
    fun onload self =
        List.app (fn (tm, s) => addEvent self.Calendar tm s) self.Choices

    fun render ctx self = <xml>
      <active code={set self.Context (Some ctx);
                    return (CalendarAddons.aboveCalendar addon ctx self.Calendar)}/>

      {FullCalendar.render self.Calendar}
    </xml>

    fun notification _ self = <xml>
      <dyn signal={anyChosen <- List.existsM (fn (_, sl) =>
                                                 l <- signal sl;
                                                 return (case l of
                                                             Unavailable => False
                                                           | _ => True)) self.Choices;
                   return (if anyChosen then
                               <xml></xml>
                           else
                               <xml><i class="glyphicon glyphicon-lg glyphicon-exclamation-circle"/></xml>)}/>
    </xml>

    fun ui uo = {Create = create uo,
                 Onload = onload,
                 Render = render,
                 Notification = notification}
end

