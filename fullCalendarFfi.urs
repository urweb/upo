type t
type event
type settings = {
     DefaultDate : option time,
     AllDaySlot : bool,
     Content : option (event -> transaction {Header : xbody, Body : xbody}),
     OnSelect : option (time -> time -> transaction unit),
     OnDrop : option ((* before *) event -> (* after *) event -> transaction unit)
}
val replace : id
              -> settings
              -> transaction t
val refresh : t -> transaction unit

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
val removeEvent : event -> transaction unit
val eventStart : event -> transaction time
val eventTitle : event -> transaction string
val eventRendering : event -> transaction rendering
val eventId : event -> transaction (option string)
val getEventById : t -> string -> transaction (option event)
val unselect : t -> transaction unit
