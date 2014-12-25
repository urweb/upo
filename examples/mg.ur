table h : { Title : string, Bogosity : int, Office : string }
  PRIMARY KEY Title

val show_h : show {Title : string} = mkShow (fn r => r.Title)
val read_h : read {Title : string} = mkRead' (fn s => Some {Title = s}) "h"
val eq_h : eq {Title : string} = Record.equal

val show_office : show {Office : string} = mkShow (fn r => " (" ^ r.Office ^ ")")
val eq_office : eq {Office : string} = Record.equal

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

table time : { Hour : int, Minute : int, Description : string }
  PRIMARY KEY (Hour, Minute)

fun pad s =
    let
        val s' = show s
    in
        if String.length s' = 1 then
            "0" ^ s'
        else
            s'
    end

val show_time : show {Hour : int, Minute : int} = mkShow (fn r => show r.Hour ^ ":" ^ pad r.Minute)
val eq_time : eq {Hour : int, Minute : int} = Record.equal
val read_time : read {Hour : int, Minute : int} = mkRead' (fn s => case String.split s #":" of
                                                                       None => None
                                                                     | Some (hr, mn) =>
                                                                       case (read hr, read mn) of
                                                                           (Some hr, Some mn) =>
                                                                           Some {Hour = hr, Minute = mn}
                                                                         | _ => None) "time"

cookie homeC : { Title : string }
cookie awayC : { Company : string, EmployeeId : int }

structure S = MeetingGrid.Make(struct
                                   con homeOffice = [Office = _]

                                   val amHome = getCookie homeC
                                   val amAway = getCookie awayC

                                   val home = h
                                   val away = a
                                   val time = time
                               end)

val main =
    fg <- S.Home.FullGrid.create;
    Theme.page
        (S.Home.FullGrid.onload fg)
        "MG"
        <xml>
          {S.Home.FullGrid.render fg}

          <hr/>

          <button value="Schedule" onclick={fn _ => rpc S.scheduleSome}/>
        </xml>

val mainRev =
    fg <- S.Away.FullGrid.create;
    Theme.page
        (S.Away.FullGrid.onload fg)
        "MG"
        <xml>
          {S.Away.FullGrid.render fg}
        </xml>

fun away s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        setCookie awayC {Value = aw,
                         Secure = False,
                         Expires = None};
        oa <- S.Away.One.create aw;
        Theme.page
            (S.Away.One.onload oa)
            ("Your Schedule (" ^ s ^ ")")
            <xml>
              {S.Away.One.render oa}
            </xml>

fun home s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        setCookie homeC {Value = aw,
                         Secure = False,
                         Expires = None};
        oa <- S.Home.One.create aw;
        Theme.page
            (S.Home.One.onload oa)
            ("Your Schedule (" ^ s ^ ")")
            <xml>
              {S.Home.One.render oa}
            </xml>

fun homepref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        hp <- S.Home.Prefs.create ho;
        Theme.page
            (return ())
            ("Your Preferences (" ^ s ^ ")")
            <xml>
              {S.Home.Prefs.render hp}
            </xml>

fun awaypref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        ap <- S.Away.Prefs.create aw;
        Theme.page
            (return ())
            ("Your Preferences (" ^ s ^ ")")
            <xml>
              {S.Away.Prefs.render ap}
            </xml>

fun homeavail s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        hp <- S.Home.Unavail.create ho;
        Theme.page
            (return ())
            ("Your Time Conflicts (" ^ s ^ ")")
            <xml>
              {S.Home.Unavail.render hp}
            </xml>

fun awayavail s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        ap <- S.Away.Unavail.create aw;
        Theme.page
            (return ())
            ("Your Time Conflicts (" ^ s ^ ")")
            <xml>
              {S.Away.Unavail.render ap}
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

         dml (INSERT INTO time (Hour, Minute, Description) VALUES (11, 00, 'eleven'));
         dml (INSERT INTO time (Hour, Minute, Description) VALUES (11, 30, 'eleven-thirty'));
         dml (INSERT INTO time (Hour, Minute, Description) VALUES (12, 00, 'noon'))
