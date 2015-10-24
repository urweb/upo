(* General functionality for building pure functions to enumerated types from keys of tables.
 * To make that a lot more concrete: a stereotypical example is assignment of a final letter grade to every student in a class.
 * Each key (e.g., student) should be associated with a summary value (e.g., numeric grade).
 * Users have a shared view of all keys sorted by summary value.
 * It is possible to assign values both with one-off rules for particular keys (e.g., "Bob gets an A")
 * and with range rules (e.g., "B+ for averages between 87 and 89"). *)

datatype access = Forbidden | Read | Write

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 con rest :: {Type}
                 constraint key ~ rest
                 constraint key ~ [Grade, Min, Max]
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val tab : sql_table (key ++ rest) ([keyName = map (fn _ => ()) key] ++ otherConstraints)
                 val filter : sql_exp [Tab = key ++ rest] [] [] bool (* which rows are eligible? *)
                 val inj : $(map sql_injectable_prim key)
                 val fl : folder key
                 val keyShow : show $key
                 val keyEq : $(map eq key)

                 (* Summarizing grades of all keys *)
                 type summaries
                 type summary
                 val summary : summaries -> $key -> summary
                 val ord_summary : ord summary
                 val show_summary : show summary
                 val eq_summary : eq summary
                 val inj_summary : sql_injectable_prim summary

                 (* Keys of a variant type used to represent grades (e.g., A+, B-) *)
                 con grades :: {Unit}
                 val grades : $(mapU string grades)
                 val gfl : folder grades

                 (* Textual labels: what do we call these concepts in human-readable text? *)
                 val keyLabel : string
                 val summaryLabel : string
                 val gradeLabel : string

                 (* What is the current user allowed to do within this assignment process? *)
                 val access : transaction access
             end) : sig
    include Ui.S where type input = M.summaries

    val grades : M.summaries -> transaction (list ($M.key * variant (mapU unit M.grades)))
end
