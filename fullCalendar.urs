type t
type event
type settings = {
     DefaultDate : option time,
     AllDaySlot : bool,
     SlotDuration : option string,
     SnapDuration : option string,
     Content : option (t -> event -> {Header : xbody, Body : xbody}),
     OnSelect : option (t -> time -> time -> transaction unit),
     OnDrop : option (t -> (* before *) event -> (* after *) event -> transaction unit)
}

val create : settings -> transaction t
val render : t -> xbody
(* Warning: will break if the same calendar is rendered in multiple places! *)

datatype rendering =
         Normal
       | Background
       | InverseBackground

type event_data = {
     Id : option string,
     AllDay : bool,
     Start : time,
     End : option time,
     Title : string,
     Rendering : rendering,
     TextColor : option (signal (option string)),
     BackgroundColor : option (signal (option string))
}

val events : t -> transaction (list event)
val addEvent : t -> event_data -> transaction event
val addEvents : t -> list event_data -> transaction unit
(* ^-- much faster than iterated [addEvent], when adding many events! *)
val clear : t -> transaction unit
val removeEvent : event -> transaction unit
val eventStart : event -> transaction time
val eventTitle : event -> transaction string
val eventRendering : event -> transaction rendering
val eventId : event -> transaction (option string)
val getEventById : t -> string -> transaction (option event)
val unselect : t -> transaction unit

val durationToSeconds : string -> int
val secondsToDuration : int -> string
val halveDuration : string -> string
(* actually returns the largest smaller even divisor *)
