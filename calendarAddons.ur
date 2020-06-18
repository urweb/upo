open Bootstrap4

type event_data = {
     Id : option string,
     Start : time,
     Title : string,
     Background : bool,
     TextColor : option string,
     BackgroundColor : option string
}

type t (r :: {Type}) = {
     ExtraEvents : option string -> transaction (list event_data),
     AboveCalendar : Ui.context -> FullCalendar.t -> xbody,
     WithinEvent : Ui.context -> FullCalendar.t -> FullCalendar.event -> {Header : xbody, Body : xbody},
     MaySee : tab :: Name -> tab' :: Name -> [[tab] ~ [tab']] => option string -> option (sql_exp [tab = r] [] [] bool),
     MayEdit : tab :: Name -> tab' :: Name -> [[tab] ~ [tab']] => option string -> option (sql_exp [tab = r] [] [] bool)
}

fun extraEvents [r] t = t.ExtraEvents
fun aboveCalendar [r] t = t.AboveCalendar
fun withinEvent [r] t = t.WithinEvent
fun maySee [r] (t : t r) [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =
    case t.MaySee [tab] [tab'] ! uo of
        None => t.MayEdit [tab] [tab'] ! uo
      | Some e1 =>
        Some (case t.MayEdit [tab] [tab'] ! uo of
                  None => e1
                | Some e2 => (WHERE {e1} OR {e2}))
fun mayEdit [r] t = t.MayEdit

val empty [r] = {
    ExtraEvents = fn _ => return [],
    AboveCalendar = fn _ _ => <xml></xml>,
    WithinEvent = fn _ _ _ => {Header = <xml></xml>, Body = <xml></xml>},
    MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None,
    MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None
}
fun compose [r] (x : t r) (y : t r) = {
    ExtraEvents = fn uo =>
                     evs1 <- x.ExtraEvents uo;
                     evs2 <- y.ExtraEvents uo;
                     return (List.append evs1 evs2),
    AboveCalendar = fn ctx cal => <xml>{x.AboveCalendar ctx cal}{y.AboveCalendar ctx cal}</xml>,
    WithinEvent = fn ctx cal ev => let
                         val x = x.WithinEvent ctx cal ev
                         val y = y.WithinEvent ctx cal ev
                     in
                         {Header = <xml>{x.Header}{y.Header}</xml>,
                          Body = <xml>{x.Body}{y.Body}</xml>}
                     end,
    MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
                                      case x.MaySee [tab] [tab'] ! uo of
                                          None => y.MaySee [tab] [tab'] ! uo
                                        | Some e1 =>
                                          case y.MaySee [tab] [tab'] ! uo of
                                              None => Some e1
                                            | Some e2 => Some (WHERE {e1} OR {e2}),
    MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
                                       case x.MayEdit [tab] [tab'] ! uo of
                                           None => y.MayEdit [tab] [tab'] ! uo
                                         | Some e1 =>
                                           case y.MayEdit [tab] [tab'] ! uo of
                                               None => Some e1
                                             | Some e2 => Some (WHERE {e1} OR {e2})
}

functor EventSource(M : sig
                        con r :: {Type}
                        con key :: Name
                        type keyT
                        con tm :: Name
                        con others :: {Type}
                        constraint [key] ~ [tm]
                        constraint [key, tm] ~ others
                        con keyName :: Name
                        con otherKeys :: {{Unit}}
                        constraint [keyName] ~ otherKeys
                        val source : sql_table ([key = keyT, tm = option time] ++ others)
                                               ([keyName = [key]] ++ otherKeys)
                        val title : string
                        val show_key : show keyT
                        val prefix : string
                        val background : bool
                        val textColor : option string
                        val backgroundColor : option string
                        val addons : t ([key = keyT, tm = option time] ++ others)
                    end) = struct
    open M

    val t = {
        ExtraEvents = fn uo =>
                         ls <- queryL1 (SELECT source.{key}, source.{tm}
                                        FROM source
                                        WHERE NOT (source.{tm} IS NULL)
                                          AND {Option.get (SQL TRUE) (maySee addons [#Source] [#Other] uo)});
                         return (List.mapi (fn i r =>
                                               {Id = Some (title ^ "_" ^ show i),
                                                Start = Option.unsafeGet r.tm,
                                                Title = prefix ^ show r.key,
                                                Background = background,
                                                TextColor = textColor,
                                                BackgroundColor = backgroundColor}) ls),
        AboveCalendar = fn _ _ => <xml></xml>,
        WithinEvent = fn _ _ _ => {Header = <xml></xml>, Body = <xml></xml>},
        MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None,
        MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None
    }
end

functor OnlyOwnerMayEdit(M : sig
                             con owner :: Name
                             con r :: {Type}
                             constraint [owner] ~ r
                        end) = struct
    open M

    val t = {
        ExtraEvents = fn _ => return [],
        AboveCalendar = fn _ _ => <xml></xml>,
        WithinEvent = fn _ _ _ => {Header = <xml></xml>, Body = <xml></xml>},
        MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo => None,
        MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
                     Some (case uo of
                               None => (WHERE FALSE)
                             | Some _ => (WHERE {{tab}}.{owner} = {[uo]}))
    }
end

functor OnlyOwnerMaySee(M : sig
                            con owner :: Name
                            con r :: {Type}
                            constraint [owner] ~ r
                        end) = struct
    open M

    val t = {
        ExtraEvents = fn _ => return [],
        AboveCalendar = fn _ _ => <xml></xml>,
        WithinEvent = fn _ _ _ => {Header = <xml></xml>, Body = <xml></xml>},
        MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
          Some (case uo of
                    None => (WHERE FALSE)
                  | Some _ => (WHERE {{tab}}.{owner} = {[uo]})),
        MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
          Some (case uo of
                    None => (WHERE FALSE)
                  | Some _ => (WHERE {{tab}}.{owner} = {[uo]}))
    }
end

functor ConnectedMaySee(M : sig
                            con key :: Name
                            type keyT
                            con r :: {Type}
                            constraint [key] ~ r
                            con keyLabels = [key]

                            con cev :: Name
                            con cuser :: Name
                            con cother :: {Type}
                            constraint [cev] ~ [cuser]
                            constraint [cev, cuser] ~ cother
                            table ctr : ([cev = keyT, cuser = string] ++ cother)
                        end) = struct
    open M

    val t = {
        ExtraEvents = fn _ => return [],
        AboveCalendar = fn _ _ => <xml></xml>,
        WithinEvent = fn _ _ _ => {Header = <xml></xml>, Body = <xml></xml>},
        MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] uo =>
          Some (case uo of
                    None => (WHERE FALSE)
                  | Some u => (WHERE COALESCE((SELECT COUNT( * ) > 0
                                               FROM ctr AS {tab'}
                                               WHERE {{tab'}}.{cev} = {{tab}}.{key}
                                                 AND {{tab'}}.{cuser} = {[u]}),
                                              FALSE))),
        MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None
    }
end

functor ShowExternalCalendar(M : sig
                                 con r :: {Type}

                                 type calendar_id
                                 val calendars : transaction (list (calendar_id * string (* description *)))
                                 (* Retrieve user's own calendars. *)
                                 val events : calendar_id -> {Min : option time, Max : option time}
                                              -> transaction (list {Summary : option string, Start : option time, End : option time})
                                 (* Get all events (in some time wondow) from a calendar. *)
                             end) = struct
    val calendars = M.calendars

    fun events cals =
        tm <- now;
        List.mapConcatM (fn cal =>
                            M.events cal
                                     {Min = Some tm,
                                      Max = Some (addSeconds tm (30 * 24 * 60 * 60))}) cals

    val t = {
        ExtraEvents = fn _ => return [],
        AboveCalendar = fn ctx cal => Ui.modalButton ctx
                                                     (CLASS "btn btn-primary")
                                                     <xml>Show events from your Google Calendar(s)</xml>
                                      (cals <- rpc calendars;
                                       cals <- List.mapM (fn (id, sum) =>
                                                             chosen <- source False;
                                                             return (id, sum, chosen)) cals;
                                       return (Ui.modal
                                               (cals <- List.mapPartialM (fn (id, _, chosen) =>
                                                                             ch <- get chosen;
                                                                             return (if ch then
                                                                                         Some id
                                                                                     else
                                                                                         None)) cals;
                                                evs <- rpc (events cals);
                                                oldEvs <- FullCalendar.events cal;
                                                List.app (fn ev =>
                                                             id <- FullCalendar.eventId ev;
                                                             case id of
                                                                 None => return ()
                                                               | Some id =>
                                                                 if String.isPrefix {Full = id, Prefix = "google_"} then
                                                                     FullCalendar.removeEvent ev
                                                                 else
                                                                     return ()) oldEvs;
                                                evs <- return (List.filter (fn ev =>
                                                                               case ev.Start of
                                                                                   None => False
                                                                                 | _ => True) evs);
                                                FullCalendar.addEvents cal (List.mapi (fn i ev =>
                                                                                          case ev.Start of
                                                                                              None => error <xml>Impossible</xml>
                                                                                            | Some start => {Id = Some ("google_" ^ show i),
                                                                                                             AllDay = False,
                                                                                                             Start = start,
                                                                                                             End = ev.End,
                                                                                                             Rendering = FullCalendar.Background,
                                                                                                             Title = Option.get "Busy" ev.Summary,
                                                                                                             TextColor = Some (return (Some "black")),
                                                                                                             BackgroundColor = Some (return (Some "gray"))}) evs))
                                               <xml>Which calendar(s) would you like to show?</xml>
                                               <xml><ul class="list-group">
                                                 {List.mapX (fn (_, sum, chosen) => <xml>
                                                   <li class="list-group-item"><ccheckbox source={chosen}/> {[sum]}</li>
                                                 </xml>) cals}
                                               </ul></xml>
                                               <xml>Show</xml>)),
        WithinEvent = fn _ cal ev => {Header = <xml></xml>,
                                      Body = <xml></xml>},
        MaySee = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None,
        MayEdit = fn [tab ::_] [tab' ::_] [[tab] ~ [tab']] _ => None
    }
end
