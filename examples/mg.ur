structure Theme = Ui.Make(Default)

table h : { Title : string, Bogosity : int, Office : string }
  PRIMARY KEY Title

val show_h : show {Title : string} = mkShow (fn r => r.Title)
val read_h : read {Title : string} = mkRead' (fn s => Some {Title = s}) "h"

val show_office : show {Office : string} = mkShow (fn r => " (" ^ r.Office ^ ")")

table a : { Company : string, EmployeeId : int, Something : string, SomethingElse : string }
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
val read_time : read {Hour : int, Minute : int} = mkRead' (fn s => case String.split s #":" of
                                                                       None => None
                                                                     | Some (hr, mn) =>
                                                                       case (read hr, read mn) of
                                                                           (Some hr, Some mn) =>
                                                                           Some {Hour = hr, Minute = mn}
                                                                         | _ => None) "time"
val ord_time : ord {Hour : int, Minute : int} = Record.ord

cookie homeC : { Title : string }
cookie awayC : { Company : string, EmployeeId : int }

structure S = MeetingGrid.Make(struct
                                   con homeOffice = [Office = _]

                                   val amHome = getCookie homeC
                                   val amAway = getCookie awayC

                                   val home = h
                                   val away = a
                                   val time = time

                                   val const = {}
                                   val homeSoftConst = {}
                                   val homeHardConst = {}
                                   val awayConst = {}

                                   val fixed = return []
                               end)

val main = Theme.simple "MG"
                     (Ui.seq
                          (S.Home.FullGrid.ui,
                           Ui.const <xml>
                             <hr/>

                             <button value="Schedule" onclick={fn _ => rpc S.scheduleSome}/>
                           </xml>))

val mainRev = Theme.simple "MG"
                           (S.Away.FullGrid.ui (fn {Company = _, EmployeeId = _, Something = _, SomethingElse = _} =>
                                                   return True))

fun away s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        setCookie awayC {Value = aw,
                         Secure = False,
                         Expires = None};
        Theme.simple ("Your Schedule (" ^ s ^ ")")
                  (S.Away.One.ui aw)

fun home s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        setCookie homeC {Value = aw,
                         Secure = False,
                         Expires = None};
        Theme.simple ("Your Schedule (" ^ s ^ ")")
                  (S.Home.One.ui aw)

fun homepref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        Theme.simple ("Your Preferences (" ^ s ^ ")")
                  (S.Home.Prefs.ui ho)

fun awaypref s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        Theme.simple ("Your Preferences (" ^ s ^ ")")
                  (S.Away.Prefs.ui aw)

fun homeavail s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some ho =>
        Theme.simple ("Your Time Conflicts (" ^ s ^ ")")
                  (S.Home.Unavail.ui ho)

fun awayavail s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        Theme.simple ("Your Time Conflicts (" ^ s ^ ")")
                  (S.Away.Unavail.ui aw)

structure AP = InputStrings.Make(struct
                                     val const = {}
                                     val tab = a
                                     val chosenLabels = {Something = "Something",
                                                         SomethingElse = "Something Else"}
                                     val textLabel = "Your Data"
                                     val amGiven = getCookie awayC
                                end)

fun awayprof s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        Theme.simple ("Your Profile (" ^ s ^ ")")
                  (AP.ui aw)

structure AG = EditGrid.Make(struct
                                 con key = [Company = _, EmployeeId = _]
                                 val tab = a
                                 val labels = {Company = "Company",
                                               EmployeeId = "Employee ID",
                                               Something = "Something",
                                               SomethingElse = "Something Else"}
                                 val authorized = return True
                             end)

val awaygrid =
    Theme.simple "All Aways"
              AG.ui


task initialize = fn () =>
     doNothing <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM h);
     if doNothing then
         return ()
     else
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('A', 1, '123'));
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('B', 2, '456'));
         dml (INSERT INTO h (Title, Bogosity, Office) VALUES ('C', 3, 'B07'));

         dml (INSERT INTO a (Company, EmployeeId, Something, SomethingElse)
              VALUES ('Weyland-Yutani', 1, 'qq', 'zz'));
         dml (INSERT INTO a (Company, EmployeeId, Something, SomethingElse)
              VALUES ('Weyland-Yutani', 2, 'abo', 'oba'));
         dml (INSERT INTO a (Company, EmployeeId, Something, SomethingElse)
              VALUES ('Massive Dynamic', 1, 'xxx', 'yyy'));

         dml (INSERT INTO time (Hour, Minute, Description) VALUES (11, 00, 'eleven'));
         dml (INSERT INTO time (Hour, Minute, Description) VALUES (11, 30, 'eleven-thirty'));
         dml (INSERT INTO time (Hour, Minute, Description) VALUES (12, 00, 'noon'))


val importHomes = Csv.importTable h 0

open Bootstrap3

val admin =
    input <- source "";
    Theme.simple "Admin"
              (Ui.const <xml>
                <ctextarea class="form-control" source={input}/>
                <button class="btn btn-primary"
                value="Import Homes"
                onclick={fn _ =>
                            input <- get input;
                            rpc (importHomes input)}/>
               </xml>)
