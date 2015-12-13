(* A simple course *)

open Bootstrap3
structure Theme = Ui.Make(Default)

table section : { Section : string }
  PRIMARY KEY Section

table user : { User : string, IsStudent : bool, IsInstructor : bool, IsStaff : bool, Section : option string }
  PRIMARY KEY User,
  CONSTRAINT Section FOREIGN KEY Section REFERENCES section(Section) ON UPDATE CASCADE

val userShow : show {User : string} = mkShow (fn r => r.User)
val userRead : read {User : string} = mkRead' (fn s => Some {User = s}) "user"

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(User, IsStudent, IsInstructor, IsStaff, Section)
           VALUES ('prof', FALSE, TRUE, TRUE, NULL))

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

val amStaff =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.User = {[u]}
                AND (user.IsStaff OR user.IsInstructor))

val amStudent =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.User = {[u]}
                AND user.IsStudent)

val requireInstructor =
    isInstructor <- amInstructor;
    if isInstructor then
        return ()
    else
        error <xml>Access denied</xml>

val requireStaff =
    isStaff <- amStaff;
    if isStaff then
        return ()
    else
        error <xml>Access denied</xml>

val getStaff =
    isStaff <- amStaff;
    if isStaff then
        u <- getCookie userC;
        case u of
            None => error <xml>!</xml>
          | Some u => return u
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

structure Sm = LinearStateMachine.Make(struct
                                           con steps = [BeforeSemester, FirstWeekOfClass, SemesterOver]

                                           val mayChange = amInstructor
                                       end)

structure Smu = Sm.MakeUi(struct
                              val steps =
                                  {BeforeSemester = {Label = "Before semester starts",
                                                     WhenEntered = fn _ => debug "Back to square 1"},
                                   FirstWeekOfClass = {Label = "First week of class",
                                                       WhenEntered = fn aa =>
                                                                        case aa of
                                                                            LinearStateMachine.NextStep => debug "Next step"
                                                                          | LinearStateMachine.FastForward => debug "Fast forward"
                                                                          | LinearStateMachine.Rewind => debug "Rewind"},
                                   SemesterOver = {Label = "Semester ends",
                                                   WhenEntered = fn _ => debug "All done!"}}
                          end)

table pset : { PsetNum : int, Released : time, Due : time, GradesDue : time, Instructions : string, Czar : option string }
  PRIMARY KEY PsetNum,
  CONSTRAINT Czar FOREIGN KEY Czar REFERENCES user(User)

val psetShow = mkShow (fn {PsetNum = n : int} => "Pset " ^ show n)
val psetRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {PsetNum = n}) "pset"

structure PsetSub = Submission.Make(struct
                                        val tab = pset
                                        val user = user
                                        val whoami = getCookie userC
                                        con fs = [Confidence = (string, _, _),
                                                  Aggravation = (int, _, _)]
                                        val labels = {Confidence = "Confidence",
                                                      Aggravation = "Aggravation"}

                                        fun makeFilename k u = "ps" ^ show k.PsetNum ^ "_" ^ u ^ ".pdf"
                                        val mayInspect = amStaff
                                    end)

fun getPset id =
    oneRow1 (SELECT pset.Instructions
             FROM pset
             WHERE pset.PsetNum = {[id]})

table psetGrade : { PsetNum : int, PsetStudent : string, Grader : string, When : time, Grade : int, Comment : string }
  PRIMARY KEY (PsetNum, PsetStudent, Grader, When),
  CONSTRAINT PsetNum FOREIGN KEY PsetNum REFERENCES pset(PsetNum) ON UPDATE CASCADE,
  CONSTRAINT Student FOREIGN KEY PsetStudent REFERENCES user(User) ON UPDATE CASCADE,
  CONSTRAINT Grader FOREIGN KEY Grader REFERENCES user(User) ON UPDATE CASCADE

val psetGradeShow : show {PsetNum : int, PsetStudent : string}
  = mkShow (fn r => "#" ^ show r.PsetNum ^ ", " ^ r.PsetStudent)

structure PsetGrade = Review.Make(struct
                                      con reviewer = #Grader
                                      con reviewed = [PsetNum = _, PsetStudent = _]
                                      val tab = psetGrade
                                      val labels = {Grade = "Grade",
                                                    Comment = "Comment"}
                                      fun summarize r = txt r.Grade
                                      val whoami = getCookie userC
                                  end)

fun psetGrades n u =
    requireStaff;
    Theme.simple ("Grading Pset #" ^ show n ^ ", " ^ u)
    (Ui.seq
         (PsetSub.AllFiles.ui {Key = {PsetNum = n}, User = u},
          PsetGrade.One.ui {PsetNum = n, PsetStudent = u}))

table psetAssignedGrader : { PsetNum : int, PsetStudent : string, Grader : string }
  PRIMARY KEY (PsetNum, PsetStudent, Grader),
  CONSTRAINT PsetNum FOREIGN KEY PsetNum REFERENCES pset(PsetNum) ON UPDATE CASCADE,
  CONSTRAINT Student FOREIGN KEY PsetStudent REFERENCES user(User) ON UPDATE CASCADE,
  CONSTRAINT Grader FOREIGN KEY Grader REFERENCES user(User) ON UPDATE CASCADE

structure PsetGraders = AssignTasks.Make(struct
                                             con assignable = [PsetNum = int, PsetStudent = string]
                                             con assigned = #Grader
                                             val show_assignable = mkShow (fn r => "#" ^ show r.PsetNum ^ "/" ^ r.PsetStudent)
                                             val assignments = psetAssignedGrader
                                             val eligibleAssignees =
                                                 let
                                                     val cat = fn e =>
                                                         List.mapQuery (SELECT user.User
                                                                        FROM user
                                                                        WHERE {e}
                                                                        ORDER BY user.User)
                                                                       (fn r => r.User.User)
                                                 in
                                                     everybody <- cat (WHERE user.IsStaff);
                                                     tas <- cat (WHERE user.IsStaff AND NOT user.IsInstructor);
                                                     profs <- cat (WHERE user.IsInstructor);
                                                     return (("Everybody", everybody)
                                                                 :: ("TAs", tas)
                                                                 :: ("Profs", profs)
                                                                 :: [])
                                                 end

                                             type filter = int
                                             val allFilters = List.mapQuery (SELECT pset.PsetNum
                                                                             FROM pset
                                                                             ORDER BY pset.PsetNum)
                                                                            (fn r => r.Pset.PsetNum)
                                             fun filter n = (SELECT {[n]} AS PsetNum, user.User AS PsetStudent
                                                             FROM user
                                                             WHERE user.IsStudent
                                                             ORDER BY user.User)
                                         end)

structure PsetCal = Calendar.FromTable(struct
                                           con tag = #Pset
                                           con key = [PsetNum = _]
                                           con times = [Released, Due, GradesDue]
                                           val tab = pset
                                           val title = "Pset"
                                           val labels = {PsetNum = "Pset#",
                                                         Released = "Released",
                                                         Due = "Due",
                                                         GradesDue = "Grades due",
                                                         Czar = "Czar",
                                                         Instructions = "Instructions"}
                                           val kinds = {Released = "released",
                                                        Due = "due",
                                                        GradesDue = "grades due"}
                                           val ws = {Instructions = Widget.htmlbox,
                                                     Czar = Widget.foreignbox (SELECT (user.User)
                                                                               FROM user
                                                                               WHERE user.IsInstructor OR user.IsStaff
                                                                               ORDER BY user.User)} ++ _

                                           fun display ctx r =
                                               b <- rpc amStudent;
                                               content <- source <xml/>;
                                               (if b then
                                                   ps <- rpc (getPset r.PsetNum);
                                                   set content (Ui.simpleModal
                                                                    <xml>
                                                                      <h2>Pset #{[r.PsetNum]}</h2>
                                                                      
                                                                      <button class="btn btn-primary"
                                                                              onclick={fn _ =>
                                                                                          xm <- PsetSub.newUpload r;
                                                                                          set content xm}>
                                                                        New Submission
                                                                              </button>

                                                                      <hr/>

                                                                      <h2>Instructions</h2>
                                                                      
                                                                      {Widget.html ps.Instructions}
                                                                    </xml>
                                                                    <xml>Close</xml>)
                                                else
                                                    xm <- PsetSub.latests (fn u => <xml><a link={psetGrades r.PsetNum u}>[grade it]</a></xml>) r;
                                                    set content (Ui.simpleModal
                                                                     <xml>
                                                                       <h2>Pset #{[r.PsetNum]}</h2>
                                                                       
                                                                       {xm}
                                                                     </xml>
                                                                     <xml>Close</xml>));
                                               return <xml>
                                                 <dyn signal={signal content}/>
                                               </xml>

                                           val auth = profOnly
                                       end)

structure PsetTodo = Todo.WithDueDate(struct
                                          con tag = #Pset
                                          con due = #Due
                                          con key = [PsetNum = int]
                                          val items = pset
                                          val done = PsetSub.submission
                                          val users = user
                                          val title = "Pset"
                                          val ucond = (WHERE Users.IsStudent)

                                          fun render r u = <xml><a link={psetGrades r.PsetNum u}>{[r]}</a></xml>
                                      end)

structure PsetForum = TableDiscussion.Make(struct
                                               con thread = #PsetThread

                                               val parent = pset
                                               val text = Widget.htmlbox

                                               fun access _ =
                                                   u <- getCookie userC;
                                                   case u of
                                                       None => return Discussion.Forbidden
                                                     | Some u =>
                                                       flags <- oneOrNoRows1 (SELECT user.IsInstructor, user.IsStaff, user.IsStudent
                                                                              FROM user
                                                                              WHERE user.User = {[u]});
                                                       return (case flags of
                                                                   None => Discussion.Forbidden
                                                                 | Some r =>
                                                                   if r.IsInstructor || r.IsStaff then
                                                                       Discussion.Admin {User = u}
                                                                   else if r.IsStudent then
                                                                       Discussion.Post {User = u, MayEdit = True, MayDelete = True, MayMarkClosed = False}
                                                                   else
                                                                       Discussion.Read)

                                               val showOpenVsClosed = True
                                               val allowPrivate = True
                                               fun onNewMessage r = debug ("New message in " ^ r.Subject ^ ": " ^ r.Text)
                                           end)

fun psetInfo n =
    ps <- getPset n;
    
    Theme.simple ("Pset #" ^ show n)
              (Ui.seq
                   (Ui.constM (fn ctx => <xml>
                                           <active code={content <- source <xml/>;
                                                    set content <xml>
                                                      <h2>Pset #{[n]}</h2>

                                                      {Ui.modalButton ctx
                                                                      (CLASS "btn btn-primary")
                                                                      <xml>New Submission</xml>
                                                                      (PsetSub.newUpload {PsetNum = n})}

                                                      <hr/>

                                                      <h2>Instructions</h2>

                                                      {Widget.html ps.Instructions}
                                                    </xml>;
                                                    return <xml>
                                                      <dyn signal={signal content}/>
                                                    </xml>}/>
                                     </xml>),
                    Ui.const <xml><hr/> <h2>Forum for this Pset</h2></xml>,
                    PsetForum.ui {PsetNum = n}))

structure PsetForumTodo = PsetForum.Todo(struct
                                             con tag = #PsetForum
                                             con user = #Czar
                                             con thread = #PsetThread

                                             val assignments = pset

                                             val title = "Pset Forum"
                                             fun render r _ = <xml><a link={psetInfo r.PsetNum}>Pset #{[r.PsetNum]}, thread {[r.PsetThread]}</a></xml>

                                             val inj = _
                                         end)

table exam : { ExamNum : int, When : time, GradesDue : time }
  PRIMARY KEY ExamNum

val examShow = mkShow (fn {ExamNum = n : int} => "Exam " ^ show n)
val examRead = mkRead' (fn s => case read s : option int of
                                    None => None
                                  | Some n => Some {ExamNum = n}) "exam"

table examGrade : { ExamNum : int, ExamStudent : string, Grader : string, When : time, Grade : int, Comment : string }
  PRIMARY KEY (ExamNum, ExamStudent, Grader, When),
  CONSTRAINT ExamNum FOREIGN KEY ExamNum REFERENCES exam(ExamNum) ON UPDATE CASCADE,
  CONSTRAINT Student FOREIGN KEY ExamStudent REFERENCES user(User) ON UPDATE CASCADE,
  CONSTRAINT Grader FOREIGN KEY Grader REFERENCES user(User) ON UPDATE CASCADE

val examGradeShow : show {ExamNum : int, ExamStudent : string}
  = mkShow (fn r => "#" ^ show r.ExamNum ^ ", " ^ r.ExamStudent)

structure ExamGrade = Review.Make(struct
                                      con reviewer = #Grader
                                      con reviewed = [ExamNum = _, ExamStudent = _]
                                      val tab = examGrade
                                      val labels = {Grade = "Grade",
                                                    Comment = "Comment"}
                                      fun summarize r = txt r.Grade
                                      val whoami = getCookie userC
                                  end)

structure ExamCal = Calendar.FromTable(struct
                                           con tag = #Exam
                                           con key = [ExamNum = _]
                                           con times = [When]
                                           val tab = exam
                                           val title = "Exam"
                                           val labels = {ExamNum = "Exam#",
                                                         When = "When",
                                                         GradesDue = "Grades due"}
                                           val kinds = {When = ""}

                                           fun display _ r = return (Ui.simpleModal
                                                                         <xml>Exam #{[r.ExamNum]}</xml>
                                                                         <xml>Close</xml>)
                                           val auth = profOnly
                                       end)

structure ExamTodo = Todo.Happenings(struct
                                         con tag = #Exam
                                         con key = [ExamNum = _]
                                         con when = #When
                                         val items = exam
                                         val users = user
                                         val ucond = (WHERE Users.IsStudent)
                                         val title = "Exam"
                                         fun render r = <xml>{[r]}</xml>
                                     end)

fun examGrades n u =
    requireStaff;
    Theme.simple ("Grading Exam #" ^ show n ^ ", " ^ u)
    (ExamGrade.One.ui {ExamNum = n, ExamStudent = u})

table examAssignedGrader : { ExamNum : int, ExamStudent : string, Grader : string }
  PRIMARY KEY (ExamNum, ExamStudent, Grader),
  CONSTRAINT ExamNum FOREIGN KEY ExamNum REFERENCES exam(ExamNum) ON UPDATE CASCADE,
  CONSTRAINT Student FOREIGN KEY ExamStudent REFERENCES user(User) ON UPDATE CASCADE,
  CONSTRAINT Grader FOREIGN KEY Grader REFERENCES user(User) ON UPDATE CASCADE

structure ExamGraders = AssignTasks.Make(struct
                                             con assignable = [ExamNum = int, ExamStudent = string]
                                             con assigned = #Grader
                                             val show_assignable = mkShow (fn r => "#" ^ show r.ExamNum ^ "/" ^ r.ExamStudent)
                                             val assignments = examAssignedGrader
                                             val eligibleAssignees =
                                                 let
                                                     val cat = fn e =>
                                                         List.mapQuery (SELECT user.User
                                                                        FROM user
                                                                        WHERE {e}
                                                                        ORDER BY user.User)
                                                                       (fn r => r.User.User)
                                                 in
                                                     everybody <- cat (WHERE user.IsStaff);
                                                     tas <- cat (WHERE user.IsStaff AND NOT user.IsInstructor);
                                                     profs <- cat (WHERE user.IsInstructor);
                                                     return (("Everybody", everybody)
                                                                 :: ("TAs", tas)
                                                                 :: ("Profs", profs)
                                                                 :: [])
                                                 end

                                             type filter = int
                                             val allFilters = List.mapQuery (SELECT exam.ExamNum
                                                                             FROM exam
                                                                             ORDER BY exam.ExamNum)
                                                                            (fn r => r.Exam.ExamNum)
                                             fun filter n = (SELECT {[n]} AS ExamNum, user.User AS ExamStudent
                                                             FROM user
                                                             WHERE user.IsStudent
                                                             ORDER BY user.User)
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

                                           fun display _ r = return (Ui.simpleModal
                                                                         <xml>Staff Meeting #{[r.MeetingNum]}</xml>
                                                                         <xml>Close</xml>)
                                           val auth = profPrivate
                                       end)

structure EditSections = EditableTable.Make(struct
                                                val tab = section
                                                val labels = {Section = "Section"}
                                                val permission = instructorPermission

                                                fun onAdd _ = return ()
                                                fun onDelete _ = return ()
                                                fun onModify _ = return ()
                                            end)

structure EditUsers = EditableTable.Make(struct
                                             val tab = user
                                             val labels = {User = "Username",
                                                           IsInstructor = "Instructor?",
                                                           IsStudent = "Student?",
                                                           IsStaff = "Staff?",
                                                           Section = "Section"}
                                             val permission = instructorPermission

                                             val widgets = {Section = Widget.foreignbox
                                                                          (SELECT (section.Section)
                                                                           FROM section
                                                                           ORDER BY section.Section)} ++ _

                                             fun onAdd _ = return ()
                                             fun onDelete _ = return ()
                                             fun onModify _ = return ()
                                         end)

structure Cal = Calendar.Make(struct
                                  val t = MeetingCal.cal
                                              |> Calendar.compose ExamCal.cal
                                              |> Calendar.compose PsetCal.cal
                              end)

structure PsetGradingTodo = Todo.WithForeignDueDate(struct
                                                        con tag = #GradePset
                                                        con key = [PsetNum = _]
                                                        con subkey = [PsetStudent = _]
                                                        con due = #GradesDue
                                                        con user = #Grader
                                                        con ukey = #User

                                                        val items = psetAssignedGrader
                                                        val parent = pset
                                                        val done = psetGrade
                                                        val users = user
                                                        val ucond = (WHERE TRUE)

                                                        val title = "Pset Grading"
                                                        fun render r _ = <xml><a link={psetGrades r.PsetNum r.PsetStudent}>Grade Pset #{[r.PsetNum]}, {[r.PsetStudent]}</a></xml>
                                                    end)

structure ExamGradingTodo = Todo.WithForeignDueDate(struct
                                                        con tag = #GradeExam
                                                        con key = [ExamNum = _]
                                                        con subkey = [ExamStudent = _]
                                                        con due = #GradesDue
                                                        con user = #Grader
                                                        con ukey = #User

                                                        val items = examAssignedGrader
                                                        val parent = exam
                                                        val done = examGrade
                                                        val users = user
                                                        val ucond = (WHERE TRUE)

                                                        val title = "Exam Grading"
                                                        fun render r _ = <xml><a link={examGrades r.ExamNum r.ExamStudent}>Grade Exam #{[r.ExamNum]}, {[r.ExamStudent]}</a></xml>
                                                    end)

structure ProfTod = Todo.Make(struct
                                  val t = PsetGradingTodo.todo
                                              |> Todo.compose ExamGradingTodo.todo
                              end)

val admin =
    requireInstructor;

    Theme.tabbed "Instructor Dashboard"
              ((Some "Timeline",
                Smu.ui),
               (Some "Users",
                EditUsers.ui),
               (Some "Sections",
                EditSections.ui),
               (Some "Calendar",
                Cal.ui {FromDay = readError "06/01/15 00:00:00",
                        ToDay = readError "09/01/15 00:00:00"}),
               (Some "Global TODO",
                ProfTod.AllUsers.ui),
               (Some "Assign Pset Grading",
                PsetGraders.MakeAssignments.ui),
               (Some "Assign Exam Grading",
                ExamGraders.MakeAssignments.ui))

structure StaffMeetingTodo = Todo.Happenings(struct
                                                 con tag = #StaffMeeting
                                                 con key = [MeetingNum = _]
                                                 con when = #When
                                                 val items = staffMeeting
                                                 val users = user
                                                 val ucond = (WHERE Users.IsStaff)
                                                 val title = "Staff Meeting"
                                                 fun render r = <xml>{[r]}</xml>
                                             end)

structure ForumTodo = Todo.Make(struct
                                    val t = PsetForumTodo.todo
                                end)

structure StaffTod = Todo.Make(struct
                                   val t = PsetGradingTodo.todo
                                               |> Todo.compose ExamGradingTodo.todo
                                               |> Todo.compose StaffMeetingTodo.todo
                               end)

val gradeTree =
    Grades.combine
        "Overall"
        ((60,
          Grades.assignments
              [[PsetNum = _]]
              [#PsetStudent]
              [#When]
              [#Grade]
              [#User]
              "Psets"
              pset
              user
              psetGrade),
         (40,
          Grades.assignments
              [[ExamNum = _]]
              [#ExamStudent]
              [#When]
              [#Grade]
              [#User]
              "Exams"
              exam
              user
              examGrade))

structure GradeTree = struct
    val t = gradeTree
end

structure AllGrades = Grades.AllStudents(GradeTree)

val grades = {Aplus = "A+",
              A = "A",
              Aminus = "A-",
              Bplus = "B+",
              B = "B",
              Bminus = "B-"}

structure Final = FinalGrades.Make(struct
                                       val tab = user
                                       val filter = (WHERE tab.IsStudent)

                                       type summaries = list (string * int)
                                       type summary = int
                                       fun summary sms u = Option.get 0 (List.assoc u.User sms)

                                       val grades = grades

                                       val keyLabel = "Student"
                                       val summaryLabel = "Average"
                                       val gradeLabel = "Grade"

                                       val access =
                                           b <- amStaff;
                                           return (if b then
                                                       FinalGrades.Write
                                                   else
                                                       FinalGrades.Forbidden)
                                   end)

val getGrades =
    b <- amStaff;
    if not b then
        error <xml>Access denied</xml>
    else
        all_grades <- Grades.allStudents gradeTree;
        Final.grades (Grades.averagesOf all_grades)

val staff =
    u <- getStaff;
    all_grades <- Grades.allStudents gradeTree;
    grs <- source [];

    Theme.tabbed "Staff Dashboard"
              ((Some "TODO",
                Ui.seq (ForumTodo.OneUser.ui u,
                        StaffTod.OneUser.ui u)),
               (Some "Calendar",
                Cal.ui {FromDay = readError "06/01/15 00:00:00",
                        ToDay = readError "09/01/15 00:00:00"}),
               (Some "Grades",
                AllGrades.ui),
               (Some "Final Grades",
                Ui.seq (Final.ui (Grades.averagesOf all_grades),
                        Ui.const <xml>
                          <button class="btn btn-primary"
                                  value="Export"
                                  onclick={fn _ =>
                                              grsv <- rpc getGrades;
                                              set grs grsv}/>
                          <ul>
                            <dyn signal={grsv <- signal grs;
                                         return (List.mapX (fn (key, g) => <xml>
                                           <li>{[key]}: {[Record.select [fn _ :: Unit => string] [fn _ :: Unit => unit]
                                                                        (fn [t] (lab : string) () => lab)
                                                                        grades g]}</li>
                                         </xml>) grsv)}/>
                          </ul>
                        </xml>)))

structure PsetTodoStudent = Todo.WithDueDate(struct
                                                 con tag = #Pset
                                                 con due = #Due
                                                 con key = [PsetNum = int]
                                                 val items = pset
                                                 val done = PsetSub.submission
                                                 val users = user
                                                 val title = "Pset"
                                                 val ucond = (WHERE Users.IsStudent)

                                                 fun render r _ = <xml><a link={psetInfo r.PsetNum}>{[r]}</a></xml>
                                             end)

structure StudentTod = Todo.Make(struct
                                     val t = PsetTodoStudent.todo
                                                 |> Todo.compose ExamTodo.todo
                                 end)

structure StudentGrades = Grades.OneStudent(GradeTree)

val main =
    c <- getCookie userC;
    u <- return (case c of
                     None => ""
                   | Some u => u);
    st <- Sm.current;
    Theme.tabbed "Course Home Page"
              ((Some "TODO",
                StudentTod.OneUser.ui u),
               (Some "Calendar",
                Cal.ui {FromDay = readError "06/01/15 00:00:00",
                        ToDay = readError "09/01/15 00:00:00"}),
               (Some "Grades",
                StudentGrades.ui u),
               (Ui.when (st = make [#FirstWeekOfClass] ()) "First week of class!",
                Ui.const <xml>WOOOOOOO!</xml>),
               (Ui.when (st >= make [#FirstWeekOfClass] ()) "Semester started",
                Ui.const <xml>YEEEEEEAAAAHHHH!</xml>))

fun setIt v =
    setCookie userC {Value = v,
                     Expires = None,
                     Secure = False}

val cookieSetup =
    sc <- source "";

    Theme.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={sc}/>
        <button value="Set" onclick={fn _ => v <- get sc; rpc (setIt v)}/>
        </xml>)}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={admin}>Instructor</a></li>
  <li><a link={staff}>Staff</a></li>
  <li><a link={main}>Student</a></li>
</body></xml>
