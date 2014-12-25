table h : { Title : string, Bogosity : int, Office : string }
  PRIMARY KEY Title

val show_h : show {Title : string} = mkShow (fn r => r.Title)
val read_h : read {Title : string} = mkRead' (fn s => Some {Title = s}) "h"
val eq_h : eq {Title : string} = Record.equal

val show_office : show {Office : string} = mkShow (fn r => " (" ^ r.Office ^ ")")
val eq_office : eq {Office : string} = Record.equal

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

fun page f = Theme.page f "MG" <xml/>

val main =
    fg <- S.Home.FullGrid.create;
    page
        (S.Home.FullGrid.onload fg)
        "MG"
        <xml>
          {S.Home.FullGrid.render fg}

          <hr/>

          <button value="Schedule" onclick={fn _ => rpc S.scheduleSome}/>
        </xml>

val mainRev =
    fg <- S.Away.FullGrid.create;
    page
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
        page
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
        page
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
        page
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
        page
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
        page
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
        page
            (return ())
            ("Your Time Conflicts (" ^ s ^ ")")
            <xml>
              {S.Away.Unavail.render ap}
            </xml>

structure AP = InputStrings.Make(struct
                                     val const = {}
                                     val tab = a
                                     val chosenLabels = {Something = "Something",
                                                         SomethingElse = "Something Else"}
                                     val givenEq = Record.equal
                                     val textLabel = "Your Data"
                                     val amGiven = getCookie awayC
                                end)

fun awayprof s =
    case read s of
        None => error <xml>Bad self-description</xml>
      | Some aw =>
        ap <- AP.create aw;
        page
            (return ())
            ("Your Profile (" ^ s ^ ")")
            <xml>
              {AP.render ap}
            </xml>

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
    ap <- AG.create;
    page
        (return ())
        ("All Aways")
        (AG.render ap)


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


val importHomes = Csv.importTable h

open Bootstrap3

val admin =
    input <- source "";
    page (return ())
               "Admin"
               <xml>
                 <ctextarea class="form-control" source={input}/>
                 <button class="btn btn-primary"
                         value="Import Homes"
                         onclick={fn _ =>
                                     input <- get input;
                                     rpc (importHomes input)}/>
                 </xml>
