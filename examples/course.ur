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

table pset : { PsetNum : int, Due : time }
  PRIMARY KEY PsetNum

val psetShow = mkShow (fn {PsetNum = n : int} => "Pset " ^ show n)
val psetRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {PsetNum = n}) "pset"

structure PsetCal = Calendar.FromTable(struct
                                           con tag = #Pset
                                           con key = [PsetNum = _]
                                           con when = #Due
                                           val tab = pset
                                           val labels = {PsetNum = "Pset#",
                                                         Due = "Due"}
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
                                           con when = #When
                                           val tab = exam
                                           val labels = {ExamNum = "Exam#",
                                                         When = "When"}
                                       end)

val cal = Calendar.compose PsetCal.cal ExamCal.cal

cookie userC : string

val auth =
    lo <- getCookie userC;
    case lo of
        None => error <xml>You haven't set the cookie with your name.</xml>
      | Some r => return r

val requireAuth = Monad.ignore auth

(* Fail if not authenticated as an admin. *)
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

structure EditPsets = EditableTable.Make(struct
                                             val tab = pset
                                             val labels = {PsetNum = "#",
                                                           Due = "Due"}
                                             val permission = instructorPermission

                                             fun onAdd _ = return ()
                                             fun onDelete _ = return ()
                                             fun onModify _ = return ()
                                         end)

structure EditExams = EditableTable.Make(struct
                                             val tab = exam
                                             val labels = {ExamNum = "#",
                                                           When = "When"}
                                             val permission = instructorPermission

                                             fun onAdd _ = return ()
                                             fun onDelete _ = return ()
                                             fun onModify _ = return ()
                                         end)

val cshow_pset = mkShow (fn {PsetNum = n : int, When = _ : time} =>
                            "Pset #" ^ show n)
val cshow_exam = mkShow (fn {ExamNum = n : int, When = _ : time} =>
                            "Exam #" ^ show n)

structure Cal = Calendar.Make(struct
                                  val t = cal
                                  val labels = {Pset = "Pset",
                                                Exam = "Exam"}
                              end)

val admin =
    requireInstructor;

    stuff <- Calendar.items cal;

    Ui.tabbed "Instructor Dashboard"
              ((Some "Users",
                EditUsers.ui),
               (Some "Psets",
                EditPsets.ui),
               (Some "Exams",
                EditExams.ui),
               (Some "Raw Calendar",
                Ui.const (List.mapX (fn v =>
                                        match v
                                              {Pset = fn r => <xml>
                                                <li>Pset #{[r.PsetNum]} @ {[r.When]}</li>
                                              </xml>,
                                               Exam = fn r => <xml>
                                                 <li>Exam #{[r.ExamNum]} @ {[r.When]}</li>
                                               </xml>}) stuff)),
               (Some "Fancy Calendar",
                Cal.ui {FromDay = readError "06/01/15 00:00:00",
                        ToDay = readError "09/01/15 00:00:00"}))

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
</body></xml>
