open Bootstrap4

datatype single_student =
         Atomic of string * option int
       | Category of string * int * int * list single_student

datatype all_students =
         AAtomic of string * list (string * int)
       | ACategory of string * list (string * int) * list all_students

type t =
     {ByStudent : string -> transaction single_student,
      AllStudents : transaction all_students}

fun oneStudent (t : t) (r : string) = t.ByStudent r
fun allStudents (t : t) = t.AllStudents

functor OneStudent(M : sig
                       val t : t
                   end) = struct
    open M

    type input = string

    (* Private datatype of widget to display a tree *)
    datatype a =
             WAtomic of string * option int
           | WCategory of source bool (* expanded? *) * string * int * int * list a

    fun toWidget t =
        case t of
            Atomic v => return (WAtomic v)
          | Category (lab, min, max, ts) =>
            exp <- source False;
            ts' <- List.mapM toWidget ts;
            return (WCategory (exp, lab, min, max, ts'))

    fun render' w =
        case w of
            WAtomic (lab, no) => <xml>
              <li> {[lab]}{case no of
                               None => <xml/>
                             | Some n => <xml>: {[n]}</xml>}</li>
            </xml>
          | WCategory (exp, lab, min, max, ws) => <xml>
            <dyn signal={expV <- signal exp;
                         return (if expV then
                                     <xml><li>
                                       <button onclick={fn _ => set exp False}>
                                         <span class="glyphicon glyphicon-caret-up"/>
                                       </button>
                                       {[lab]} ({[min]}{if min = max then
                                                            <xml/>
                                                        else
                                                            <xml>-{[max]}</xml>})
                                       <ul>
                                         {List.mapX render' ws}
                                       </ul>
                                     </li></xml>
                                 else
                                     <xml><li>
                                       <button onclick={fn _ => set exp True}>
                                         <span class="glyphicon glyphicon-caret-down"/>
                                       </button>
                                       {[lab]} ({[min]}{if min = max then
                                                            <xml/>
                                                        else
                                                            <xml>-{[max]}</xml>})
                                     </li></xml>)}/>
            </xml>

    fun render w = <xml><ul>{render' w}</ul></xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui stid = {Create = tr <- oneStudent t stid; toWidget tr,
                   Onload = fn _ => return (),
                   Render = fn _ => render,
                   Notification = notification,
                   Buttons = buttons}
end

functor AllStudents(M : sig
                        val t : t
                    end) = struct
    open M

    (* Private datatype of widget to display a tree *)
    datatype a =
         WAtomic of source bool (* show grades? *)
                    * string * list (string * int)
       | WCategory of source bool (* show averages? *)
                      * source bool (* show children? *)
                      * string * list (string * int) * list a

    fun toWidget t =
        case t of
            AAtomic (lab, gs) =>
            exp <- source False;
            return (WAtomic (exp, lab, gs))
          | ACategory (lab, avg, ts) =>
            expA <- source False;
            expC <- source False;
            ts' <- List.mapM toWidget ts;
            return (WCategory (expA, expC, lab, avg, ts'))

    fun render w =
        case w of
            WAtomic (exp, lab, gs) => <xml>
              <li> {[lab]}
                <dyn signal={expV <- signal exp;
                             return (if expV then <xml>
                               <button class="btn btn-secondary"
                                       onclick={fn _ => set exp False}
                                       value="Hide Grades"/>
                               <table class="bs-table">
                                 <thead><tr>
                                   <th>Student</th>
                                   <th>Grade</th>
                                 </tr></thead>

                                 <tbody>
                                   {List.mapX (fn (student, grade) =>
                                                  <xml><tr> <td>{[student]}</td> <td>{[grade]}</td> </tr></xml>) gs}
                                 </tbody>
                               </table>
                             </xml> else <xml>
                               <button class="btn btn-secondary"
                                       onclick={fn _ => set exp True}
                                       value="Show Grades"/>
                             </xml>)}/>
              </li>
            </xml>
          | WCategory (expA, expC, lab, avg, ws) => <xml><li>
            <dyn signal={expV <- signal expC;
                                   return (if expV then <xml>
                                     <button onclick={fn _ => set expC False}>
                                       <span class="glyphicon glyphicon-caret-up"/>
                                     </button>
                                   </xml> else <xml>
                                     <button onclick={fn _ => set expC True}>
                                       <span class="glyphicon glyphicon-caret-down"/>
                                     </button>
                                   </xml>)}/>{[lab]}
            <dyn signal={expV <- signal expA;
                         return (if expV then <xml>
                           <button class="btn btn-secondary"
                                   onclick={fn _ => set expA False}
                                   value="Hide Averages"/>
                           <table class="bs-table">
                             <tr>
                               <th>Student</th>
                               <th>Grade</th>
                             </tr>

                             {List.mapX (fn (student, grade) =>
                                            <xml><tr> <td>{[student]}</td> <td>{[grade]}</td> </tr></xml>) avg}
                           </table>
                         </xml> else <xml>
                           <button class="btn btn-secondary"
                                   onclick={fn _ => set expA True}
                                   value="Show Averages"/>
                         </xml>)}/>
            <dyn signal={expV <- signal expC;
                         return (if expV then
                                   <xml>
                                     <ul>
                                       {List.mapX render ws}
                                     </ul>
                                   </xml>
                               else
                                   <xml/>)}/>
          </li></xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    val ui = {Create = tr <- allStudents t; toWidget tr,
              Onload = fn _ => return (),
              Render = fn _ => render,
              Notification = notification,
              Buttons = buttons}
end

fun minOf t =
    case t of
        Atomic (_, None) => 0
      | Atomic (_, Some n) => n
      | Category (_, n, _, _) => n

fun maxOf t =
    case t of
        Atomic (_, None) => 100
      | Atomic (_, Some n) => n
      | Category (_, _, n, _) => n

fun average lab ts =
    let
        val (min, max) = List.foldl (fn a (min, max) => (min + minOf a, max + maxOf a))
                                    (0, 0) ts
        val len = List.length ts
    in
        Category (lab,
                  if len = 0 then 0 else min / len,
                  if len = 0 then 0 else max / len,
                  ts)
    end

fun addGrade (students : list (string * int)) (student : string) (grade : int)
    : list (string * int) =
    let
        fun addGrade' students acc =
            case students of
                [] => List.revAppend acc ((student, grade) :: [])
              | student' :: students' =>
                if student = student'.1 then
                    List.revAppend acc ((student, grade + student'.2) :: students')
                else if student < student'.1 then
                    List.revAppend acc ((student, grade) :: students)
                else
                    addGrade' students' (student' :: acc)
    in
        addGrade' students []
    end

fun mergeGrades (weight : int) (students1 : list (string * int)) (students2 : list (string * int)) : list (string * int) =
    List.foldl (fn (student, grade) students => addGrade students student (weight * grade)) students2 students1

fun averagesOf x =
    case x of
        AAtomic (_, ls) => ls
      | ACategory (_, ls, _) => ls

fun assignments [aks ::: {{Unit}}] [sks ::: {{Unit}}] [gks ::: {{Unit}}]
    [assignment :: {Type}] [student :: Name] [when :: Name] [grade :: Name]
    [other ::: {Type}] [aother ::: {Type}] [skey :: Name] [sother ::: {Type}]
    [assignment ~ [student]] [other ~ assignment ++ [student = string]] [[grade] ~ [when]]
    [[grade, when] ~ assignment ++ [student = string] ++ other]
    [assignment ~ aother] [[skey] ~ sother]
    (_ : show $assignment) (eqs : $(map eq assignment)) (afl : folder assignment)
    (lab : string)
    (asn : sql_table (assignment ++ aother) aks)
    (stu : sql_table ([skey = string] ++ sother) sks)
    (gra : sql_table (assignment ++ [student = string, grade = int, when = time] ++ other) gks)
    : t =
      {ByStudent = fn stid =>
                      asns <- List.mapQuery (SELECT asn.{{assignment}},
                                               (SELECT (gra.{grade})
                                                FROM gra
                                                WHERE {@Sql.easy_join [#Asn] [#Gra]
                                                  ! ! ! ! afl}
                                                  AND Gra.{student} = {[stid]}
                                                  AND {sql_nullable (SQL gra.{when})}
                                                    = (SELECT MAX(gra2.{when})
                                                       FROM gra AS Gra2
                                                       WHERE {@Sql.easy_join [#Asn] [#Gra2]
                                                         ! ! ! ! afl}
                                                         AND Gra2.{student} = {[stid]}))
                                               AS Grade
                                             FROM asn
                                             ORDER BY {{{@Sql.order_by afl
                                                          (@Sql.some_fields [#Asn] [assignment] ! ! afl)
                                                          sql_asc}}})
                              (fn r => Atomic (show r.Asn, r.Grade));
                      return (average lab asns),
       AllStudents =
                     let
                         val fl = @Folder.concat ! afl
                                   (@Folder.cons [student] [_] !
                                     (@Folder.cons [grade] [_] ! Folder.nil))

                         fun group (ls : list $(assignment ++ [student = string, grade = int]))
                             (cur : option ($assignment * list (string * int)))
                             (acc : list all_students) =
                             case ls of
                                 [] =>
                                 (case cur of
                                      None => acc
                                    | Some (asn, sts) => AAtomic (show asn, sts) :: acc)
                               | r :: ls' =>
                                 case cur of
                                     None => group ls' (Some (r -- student -- grade,
                                                              (r.student, r.grade) :: [])) acc
                                   | Some (asn, sts) =>
                                     if @eq (@Record.eq eqs afl) asn (r -- student -- grade) then
                                         group ls' (Some (asn, (r.student, r.grade) :: sts)) acc
                                     else
                                         group ls' (Some (r -- student -- grade, (r.student, r.grade) :: []))
                                               (AAtomic (show asn, sts) :: acc)
                     in
                         ls <- queryL1 (SELECT gra.{{assignment}}, gra.{student}, gra.{grade}
                                        FROM gra
                                        ORDER BY {{{@Sql.order_by fl
                                          (@Sql.some_fields [#Gra] [_] ! ! fl)
                                          sql_desc}}});
                         ats <- return (group ls None []);
                         len <- return (List.length ats);
                         return (ACategory (lab,
                                            List.mp (fn (student, n) => (student, n / len))
                                                    (List.foldl (fn at avg => mergeGrades 1 (averagesOf at) avg) [] ats),
                                            ats))
                     end}

fun combine [cats ::: {Unit}] (fl : folder cats) (lab : string)
            (cats : $(mapU (int * t) cats)) : t =
    let
        val totalWeight = @foldUR [int * t] [fn _ => int]
                          (fn [nm ::_] [r ::_] [[nm] ~ r] (n, _) m => n + m)
                          0 fl cats
    in
        {ByStudent = fn stid =>
                        (min, max, ls) <-
                        @Monad.foldR _ [fn _ => int * t] [fn _ => int * int * list single_student]
                         (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (w, t) (min, max, ls) =>
                             res <- t.ByStudent stid;
                             return (min + minOf res * w,
                                     max + maxOf res * w,
                                     res :: ls))
                         (0, 0, []) fl cats;
                        if totalWeight = 0 then
                            error <xml>Grades.combine: total weight is 0</xml>
                        else
                            return (Category (lab,
                                              min / totalWeight,
                                              max / totalWeight,
                                              ls)),
         AllStudents =
                        (avg, ls) <- @Monad.foldR _ [fn _ => int * t] [fn _ => list (string * int) * list all_students]
                                      (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (w, t) (avg, ls) =>
                                          res <- t.AllStudents;
                                          return (mergeGrades w (averagesOf res) avg,
                                                  res :: ls))
                                      ([], []) fl cats;
                        return (ACategory (lab,
                                           if totalWeight = 0 then
                                               error <xml>Grades.combine: total weight is 0</xml>
                                           else
                                               List.mp (fn (student, grade) => (student, grade / totalWeight)) avg,
                                           ls))}
    end
