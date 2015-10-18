open Bootstrap3

datatype tree =
         Atomic of string * option int
       | Category of string * int * int * list tree

type t =
     {ByStudent : string -> transaction tree}

fun oneStudent (t : t) (r : string) = t.ByStudent r

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

    fun render w =
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
                                       <button class="glyphicon glyphicon-collapse-up"
                                               onclick={fn _ => set exp False}/>
                                       {[lab]} ({[min]}{if min = max then
                                                            <xml/>
                                                        else
                                                            <xml>-{[max]}</xml>})
                                       <ul>
                                         {List.mapX render ws}
                                       </ul>
                                     </li></xml>
                                 else
                                     <xml><li>
                                       <button class="glyphicon glyphicon-collapse-down"
                                               onclick={fn _ => set exp True}/>
                                       {[lab]} ({[min]}{if min = max then
                                                            <xml/>
                                                        else
                                                            <xml>-{[max]}</xml>})
                                     </li></xml>)}/>
            </xml>

    fun ui stid = {Create = tr <- oneStudent t stid; toWidget tr,
                   Onload = fn _ => return (),
                   Render = fn _ => render}
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

fun assignments [aks ::: {{Unit}}] [sks ::: {{Unit}}] [gks ::: {{Unit}}]
    [assignment :: {Type}] [student :: Name] [when :: Name] [grade :: Name]
    [other ::: {Type}] [aother ::: {Type}] [skey :: Name] [sother ::: {Type}]
    [assignment ~ [student]] [other ~ assignment ++ [student = string]] [[grade] ~ [when]]
    [[grade, when] ~ assignment ++ [student = string] ++ other]
    [assignment ~ aother] [[skey] ~ sother]
    (_ : show $assignment) (afl : folder assignment)
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
                                                  AND {eqNullable
                                                  (sql_nullable (SQL gra.{when}))
                                                  (SQL (SELECT MAX(gra2.{when})
                                                        FROM gra AS Gra2
                                                        WHERE {@Sql.easy_join [#Asn] [#Gra2]
                                                          ! ! ! ! afl}
                                                          AND Gra2.{student} = {[stid]}))})
                                               AS Grade
                                             FROM asn
                                             ORDER BY {{{@Sql.order_by afl
                                                          (@Sql.some_fields [#Asn] [assignment] ! ! afl)
                                                          sql_asc}}})
                              (fn r => Atomic (show r.Asn, r.Grade));
                      return (average lab asns)}

fun combine [cats ::: {Unit}] (fl : folder cats) (lab : string)
            (cats : $(mapU (int * t) cats)) : t =
    let
        val totalWeight = @foldUR [int * t] [fn _ => int]
                          (fn [nm ::_] [r ::_] [[nm] ~ r] (n, _) m => n + m)
                          0 fl cats
    in
        {ByStudent = fn stid =>
                        (min, max, ls) <-
                        @Monad.foldR _ [fn _ => int * t] [fn _ => int * int * list tree]
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
                                              ls))}
    end
