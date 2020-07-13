open FullCalendarFfi

type event = source (option event)

type t = {Show : xbody,
          Calendar : source (option FullCalendarFfi.t),
          QueuedEvents : source (list (event * event_data))}

type settings = {
     DefaultDate : option time,
     AllDaySlot : bool,
     SlotDuration : option string,
     SnapDuration : option string,
     Content : option (t -> event -> {Header : xbody, Body : xbody}),
     OnSelect : option (t -> time -> time -> transaction unit),
     OnDrop : option (t -> event -> event -> transaction unit)
}

fun create settings =
    id <- fresh;
    cal <- source None;
    evs <- source [];
    t' <- return {Show = <xml></xml>,
                  Calendar = cal,
                  QueuedEvents = evs};
    return {Calendar = cal,
            QueuedEvents = evs,
            Show = <xml>
              <div id={id}/>
              <active code={calV <- FullCalendarFfi.replace id
                                                            (settings --- [Content = _, OnSelect = _, OnDrop = _]
                                                                      ++ {Content = Option.mp (fn f ev =>
                                                                                                  ev <- source (Some ev);
                                                                                                  return (f t' ev)) settings.Content,
                                                                          OnSelect = Option.mp (fn f => f t') settings.OnSelect,
                                                                          OnDrop = Option.mp (fn f ev1 ev2 =>
                                                                                                 ev1 <- source (Some ev1);
                                                                                                 ev2 <- source (Some ev2);
                                                                                                 f t' ev1 ev2) settings.OnDrop});
                            evsV <- get evs;
                            List.app (fn (s, ev) =>
                                         ev <- FullCalendarFfi.addEvent calV ev;
                                         set s (Some ev)) evsV;
                            set evs [];
                            set cal (Some calV);
                            FullCalendarFfi.refresh calV;
                            return <xml></xml>}/>
            </xml>}

fun render self = self.Show

fun events self =
    cal <- get self.Calendar;
    case cal of
        None =>
        evs <- get self.QueuedEvents;
        return (List.mp (fn (ev, _) => ev) evs)
      | Some cal =>
        evs <- FullCalendarFfi.events cal;
        List.mapM (fn ev => source (Some ev)) evs

fun addEvent self ev =
    cal <- get self.Calendar;
    case cal of
        None =>
        evs <- get self.QueuedEvents;
        s <- source None;
        set self.QueuedEvents ((s, ev) :: evs);
        return s
      | Some cal =>
        ev <- FullCalendarFfi.addEvent cal ev;
        FullCalendarFfi.refresh cal;
        source (Some ev)

fun addEvents self evl =
    cal <- get self.Calendar;
    case cal of
        None =>
        evs <- get self.QueuedEvents;
        evl <- List.mapM (fn ev => s <- source None; return (s, ev)) evl;
        set self.QueuedEvents (List.append evl evs)
      | Some cal =>
        FullCalendarFfi.addEvents cal evl;
        FullCalendarFfi.refresh cal

fun clear cal =
    calv <- get cal.Calendar;
    case calv of
        None => set cal.QueuedEvents []
      | Some cal =>
        evs <- FullCalendarFfi.events cal;
        List.app FullCalendarFfi.removeEvent evs

fun unselect cal =
    calv <- get cal.Calendar;
    case calv of
        None => return ()
      | Some cal => FullCalendarFfi.unselect cal

fun removeEvent ev =
    ev <- get ev;
    case ev of
        None => error <xml>Trying to remove event that wasn't created yet</xml>
      | Some ev => FullCalendarFfi.removeEvent ev

fun eventStart ev =
    ev <- get ev;
    case ev of
        None => error <xml>Trying to examine event that wasn't created yet</xml>
      | Some ev => FullCalendarFfi.eventStart ev

fun eventTitle ev =
    ev <- get ev;
    case ev of
        None => error <xml>Trying to examine event that wasn't created yet</xml>
      | Some ev => FullCalendarFfi.eventTitle ev

fun eventRendering ev =
    ev <- get ev;
    case ev of
        None => error <xml>Trying to examine event that wasn't created yet</xml>
      | Some ev => FullCalendarFfi.eventRendering ev

fun eventId ev =
    ev <- get ev;
    case ev of
        None => error <xml>Trying to examine event that wasn't created yet</xml>
      | Some ev => FullCalendarFfi.eventId ev

fun getEventById cal id =
    calv <- get cal.Calendar;
    case calv of
        None =>
        evs <- get cal.QueuedEvents;
        return (List.search (fn (ev, evd) => if evd.Id = Some id then Some ev else None) evs)
      | Some cal =>
        evo <- FullCalendarFfi.getEventById cal id;
        case evo of
            None => return None
          | Some ev =>
            ev <- source (Some ev);
            return (Some ev)

fun durationToSeconds s =
    case String.split s #":" of
        None => error <xml>Bad duration format: {[s]}</xml>
      | Some (h, m) =>
        case (read h, read m) of
            (Some h, Some m) => 60 * (60 * h + m)
          | _ => error <xml>Bad duration format: {[s]}</xml>

fun pad n s =
    if String.length s >= n then
        s
    else
        "0" ^ pad (n - 1) s

fun secondsToDuration n =
    let
        val n = n / 60
    in
        pad 2 (show (n / 60)) ^ ":" ^ pad 2 (show (n % 60))
    end

fun halveDuration s =
    let
        val n = durationToSeconds s

        fun divider i =
            if n / i % 60 = 0 then
                n / i
            else
                divider (i + 1)
    in
        secondsToDuration (divider 2)
    end
