(* Extensible calculations of grades or other numeric evaluations assigned to users *)

(* Grade calculation produces trees of categories, bottoming out in atomic assignments. *)
datatype single_student =
         Atomic of string                  (* textual description *)
                   * option int            (* numeric grade, if set *)
       | Category of string                (* textual description *)
                     * int                 (* lowest possible grade, depending on outcomes of pending assignments *)
                     * int                 (* highest possible grade *)
                     * list single_student (* constituent grades *)

(* This other perspective gives grades for _all_ students who have completed assignments. *)
datatype all_students =
         AAtomic of string                  (* textual description *)
                    * list (string * int)   (* (sorted) association list mapping student IDs to grades *)
       | ACategory of string                (* textual description *)
                      * list (string * int) (* average grades in this category for student IDs *)
                      * list all_students   (* constituent grades *)

(* Generators of grade entries *)
type t

(* Compute the two views of grades. *)
val oneStudent : t -> string -> transaction single_student
val allStudents : t -> transaction all_students

(* And now the functor versions, with UIs *)
functor OneStudent(M : sig
                       val t : t
                   end) : Ui.S where type input = string
(* And now the functor versions, with UIs *)
functor AllStudents(M : sig
                        val t : t
                    end) : Ui.S0

(* The primitive grades construct: read grades out of a table.
 * This variant accepts, for each student-assignment pair, the grade with the latest timestamp.
 * We do even weighting of each assignment from the assignments table.
 * We also assume that each assignment is graded on a 0-100 scale. *)
val assignments : aks ::: {{Unit}}
                  -> sks ::: {{Unit}}
                  -> gks ::: {{Unit}}
                  -> assignment :: {Type}
                  -> student :: Name
                  -> when :: Name
                  -> grade :: Name
                  -> other ::: {Type}
                  -> aother ::: {Type}
                  -> skey :: Name
                  -> sother ::: {Type}
                  -> [assignment ~ [student]]
                  => [other ~ assignment ++ [student = string]]
                  => [[grade] ~ [when]]
                  => [[when, grade] ~ assignment ++ [student = string] ++ other]
                  => [assignment ~ aother]
                  => [[skey] ~ sother]
                  => show $assignment
                  -> $(map eq assignment)
                  -> folder assignment
                  -> string                                    (* category label *)
                  -> sql_table (assignment ++ aother) aks      (* assignments *)
                  -> sql_table ([skey = string] ++ sother) sks (* students *)
                  -> sql_table (assignment ++ [student = string, grade = int, when = time] ++ other) gks
                  -> t

(* Now we need a way to combine categories, giving a weight for each. *)
val combine : cats ::: {Unit}
              -> folder cats
              -> string
              -> $(mapU (int * t) cats)
              -> t
