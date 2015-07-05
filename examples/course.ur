(* A simple course *)

open Bootstrap3

table user : { User : string, IsStudent : bool, IsInstructor : bool, IsStaff : bool }
  PRIMARY KEY User

val userShow : show {User : string} = mkShow (fn r => r.User)
val userRead : read {User : string} = mkRead' (fn s => Some {User = s}) "user"

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(User, IsStudent, IsInstructor, IsStaff)
           VALUES ('prof', FALSE, TRUE, TRUE))

cookie userC : string

val auth =
    lo <- getCookie userC;
    case lo of
        None => error <xml>You haven't set the cookie with your name.</xml>
      | Some r => return r

val requireAuth = Monad.ignore auth

val amInstructor =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.User = {[u]}
                AND user.IsInstructor)

val requireInstructor =
    isInstructor <- amInstructor;
    if isInstructor then
        return ()
    else
        error <xml>Access denied</xml>

val amUser = user <- auth; return (Some {User = user})

val instructorPermission =
    ai <- amInstructor;
    return {Add = ai,
            Delete = ai,
            Modify = ai}

val profOnly =
    b <- amInstructor;
    return (if b then
                Calendar.Write
            else
                Calendar.Read)

val profPrivate =
    b <- amInstructor;
    return (if b then
                Calendar.Write
            else
                Calendar.Forbidden)

table pset : { PsetNum : int, Released : time, Due : time }
  PRIMARY KEY PsetNum

val psetShow = mkShow (fn {PsetNum = n : int} => "Pset " ^ show n)
val psetRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {PsetNum = n}) "pset"

structure PsetCal = Calendar.FromTable(struct
                                           con tag = #Pset
                                           con key = [PsetNum = _]
                                           con times = [Released, Due]
                                           val tab = pset
                                           val title = "Pset"
                                           val labels = {PsetNum = "Pset#",
                                                         Released = "Released",
                                                         Due = "Due"}
                                           val kinds = {Released = "released",
                                                        Due = "due"}

                                           fun display r = return <xml>Pset #{[r.PsetNum]}</xml>
                                           val auth = profOnly
                                       end)

table exam : { ExamNum : int, When : time }
  PRIMARY KEY ExamNum

val examShow = mkShow (fn {ExamNum = n : int} => "Exam " ^ show n)
val examRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {ExamNum = n}) "exam"

structure ExamCal = Calendar.FromTable(struct
                                           con tag = #Exam
                                           con key = [ExamNum = _]
                                           con times = [When]
                                           val tab = exam
                                           val title = "Exam"
                                           val labels = {ExamNum = "Exam#",
                                                         When = "When"}
                                           val kinds = {When = ""}

                                           fun display r = return <xml>Exam #{[r.ExamNum]}</xml>
                                           val auth = profOnly
                                       end)

table staffMeeting : { MeetingNum : int, When : time }
  PRIMARY KEY MeetingNum

val meetingShow = mkShow (fn {MeetingNum = n : int} => "Meeting " ^ show n)
val meetingRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {MeetingNum = n}) "meeting"

structure MeetingCal = Calendar.FromTable(struct
                                           con tag = #Meeting
                                           con key = [MeetingNum = _]
                                           con times = [When]
                                           val tab = staffMeeting
                                           val title = "Staff Meeting"
                                           val labels = {MeetingNum = "Meeting#",
                                                         When = "When"}
                                           val kinds = {When = ""}

                                           fun display r = return <xml>Staff Meeting #{[r.MeetingNum]}</xml>
                                           val auth = profPrivate
                                       end)

val cal = MeetingCal.cal
              |> Calendar.compose ExamCal.cal
              |> Calendar.compose PsetCal.cal

structure EditUsers = EditableTable.Make(struct
                                             val tab = user
                                             val labels = {User = "Username",
                                                           IsInstructor = "Instructor?",
                                                           IsStudent = "Student?",
                                                           IsStaff = "Staff?"}
                                             val permission = instructorPermission

                                             fun onAdd _ = return ()
                                             fun onDelete _ = return ()
                                             fun onModify _ = return ()
                                         end)

structure Cal = Calendar.Make(struct
                                  val t = cal
                              end)

val admin =
    requireInstructor;

    Ui.tabbed "Instructor Dashboard"
              ((Some "Users",
                EditUsers.ui),
               (Some "Calendar",
                Cal.ui {FromDay = readError "06/01/15 00:00:00",
                        ToDay = readError "09/01/15 00:00:00"}))

val main =
    Ui.tabbed "Course Home Page"
              {1 = (Some "Calendar",
                    Cal.ui {FromDay = readError "06/01/15 00:00:00",
                            ToDay = readError "09/01/15 00:00:00"})}

fun setIt v =
    setCookie userC {Value = v,
                     Expires = None,
                     Secure = False}

val cookieSetup =
    sc <- source "";

    Ui.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={sc}/>
        <button value="Set" onclick={fn _ => v <- get sc; rpc (setIt v)}/>
        </xml>)}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={admin}>Instructor</a></li>
  <li><a link={main}>Student</a></li>
</body></xml>
