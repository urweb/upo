table h : { Title : string, Bogosity : int }
  PRIMARY KEY Title

val show_h : show {Title : string} = mkShow (fn r => r.Title)
val read_h : read {Title : string} = mkRead' (fn s => Some {Title = s}) "h"
val eq_h : eq {Title : string} = Record.equal

table a : { Company : string, EmployeeId : int, Awesome : bool }
  PRIMARY KEY (Company, EmployeeId)

val show_a : show {Company : string, EmployeeId : int} =
    mkShow (fn r => r.Company ^ " #" ^ show r.EmployeeId)
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
val eq_a : eq {Company : string, EmployeeId : int} = Record.equal

table time : { When : time, Description : string }
  PRIMARY KEY When

val show_time : show {When : time} = mkShow (fn r => timef "%H:%M" r.When)
val eq_time : eq {When : time} = Record.equal

structure S = MeetingGrid.Make(struct
                                   val home = h
                                   val away = a
                                   val time = time
                               end)

val main =
    fg <- S.FullGrid.create;
    Theme.page
        (S.FullGrid.onload fg)
        "MG"
        <xml>
          {S.FullGrid.render fg}
        </xml>

fun away s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        oa <- S.OneAway.create aw;
        Theme.page
            (S.OneAway.onload oa)
            ("Your Schedule (" ^ s ^ ")")
            <xml>
              {S.OneAway.render oa}
            </xml>

fun homepref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        hp <- S.HomePrefs.create ho;
        Theme.page
            (return ())
            ("Your Preferences (" ^ s ^ ")")
            <xml>
              {S.HomePrefs.render hp}
            </xml>

fun awaypref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        ap <- S.AwayPrefs.create aw;
        Theme.page
            (return ())
            ("Your Preferences (" ^ s ^ ")")
            <xml>
              {S.AwayPrefs.render ap}
            </xml>

task initialize = fn () =>
     doNothing <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM h);
     if doNothing then
         return ()
     else
         dml (INSERT INTO h (Title, Bogosity) VALUES ('A', 1));
         dml (INSERT INTO h (Title, Bogosity) VALUES ('B', 2));
         dml (INSERT INTO h (Title, Bogosity) VALUES ('C', 3));

         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Weyland-Yutani', 1, TRUE));
         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Weyland-Yutani', 2, FALSE));
         dml (INSERT INTO a (Company, EmployeeId, Awesome) VALUES ('Massive Dynamic', 1, FALSE));

         dml (INSERT INTO time (When, Description) VALUES ({[readError '2014-12-25 11:00:00']}, 'eleven'));
         dml (INSERT INTO time (When, Description) VALUES ({[readError '2014-12-25 11:30:00']}, 'eleven-thirty'));
         dml (INSERT INTO time (When, Description) VALUES ({[readError '2014-12-25 12:00:00']}, 'noon'));

         S.addMeeting {Title = 'A', Company = 'Weyland-Yutani', EmployeeId = 1, When = readError '2014-12-25 11:00:00'}
