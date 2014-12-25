table h : { Title : string, Bogosity : int, Office : string }
  PRIMARY KEY Title

val show_h : show {Title : string} = mkShow (fn r => r.Title)
val read_h : read {Title : string} = mkRead' (fn s => Some {Title = s}) "h"
val eq_h : eq {Title : string} = Record.equal

table a : { Company : string, EmployeeId : int, Awesome : bool }
  PRIMARY KEY (Company, EmployeeId)

val show_a : show {Company : string, EmployeeId : int} =
    mkShow (fn r => r.Company ^ " #" ^ show r.EmployeeId)
val eq_a : eq {Company : string, EmployeeId : int} = Record.equal

table event : { Event : string, Description : string }
  PRIMARY KEY Event

val show_event : show {Event : string} = mkShow (fn r => show r.Event)
val show_event' : show {Description : string} = mkShow (fn r => show r.Description)
val read_a : read {Company : string, EmployeeId : int} =
    mkRead' (fn s =>
                case String.split s #"#" of
                    None => None
                  | Some (s1, s2) => case read s2 of
                                         None => None
                                       | Some n =>
                                         Some {Company = String.substring s1 {Start = 0, Len = String.length s1 - 1},
                                               EmployeeId = n})
    "a"
val eq_event : eq {Event : string} = Record.equal

cookie homeC : { Title : string }
cookie awayC : { Company : string, EmployeeId : int }

structure S = Rsvp2.Make(struct
                             con homeRest = []
                             con awayRest = []
                             con eventRest = []

                             val home = h
                             val away = a
                             val event = event

                             val homeDataLabels = {Bogosity = "Bogosity",
                                                   Office = "Office"}
                             val awayDataLabels = {Awesome = "Awesome"}

                             val homeLabel = "Home"
                             val awayLabel = "Away"

                             val amHome = getCookie homeC
                             val amAway = getCookie awayC
                         end)

fun home s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        setCookie homeC {Value = ho,
                         Secure = False,
                         Expires = None};
        oa <- S.Home.create ho;
        Theme.page
            (S.Home.onload oa)
            ("Your Options (" ^ s ^ ")")
            <xml>
              {S.Home.render oa}
            </xml>

fun away s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        setCookie awayC {Value = aw,
                         Secure = False,
                         Expires = None};
        oa <- S.Away.create aw;
        Theme.page
            (return ())
            ("Your Options (" ^ s ^ ")")
            <xml>
              {S.Away.render oa}
            </xml>


task initialize = fn () =>
     doNothing <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM h);
     if doNothing then
         return ()
     else
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('A', 1, '123'));
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('B', 2, '456'));
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('C', 3, 'B07'));

         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Weyland-Yutani', 1, TRUE));
         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Weyland-Yutani', 2, FALSE));
         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Massive Dynamic', 1, FALSE));

         dml (INSERT INTO event (Event, Description) VALUES ('Forest Party', 'meet under the big tree'));
         dml (INSERT INTO event (Event, Description) VALUES ('Tundra Adventure', 'bring a coat'))
