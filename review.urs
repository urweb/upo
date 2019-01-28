(* Abstraction of submitting evaluations of database objects *)

functor Make(M : sig
                 con when :: Name
                 con reviewer :: Name
                 con reviewed :: {Type}
                 con other :: {(Type * Type * Type)}
                 constraint reviewed ~ other
                 constraint [reviewer] ~ [when]
                 constraint [reviewer, when] ~ reviewed
                 constraint [reviewer, when] ~ other
                 constraint [Channel] ~ reviewed
                 table tab : ([when = time, reviewer = string] ++ reviewed ++ map fst3 other)

                 val widgets : $(map Widget.t' other)
                 val reviewedFl : folder reviewed
                 val otherFl : folder other
                 val reviewedInj : $(map sql_injectable reviewed)
                 val otherInj : $(map (fn p => sql_injectable p.1) other)
                 val labels : $(map (fn _ => string) other)

                 val show_reviewed : show $reviewed
                 val summarize : $(map fst3 other) -> xbody
                 val whoami : transaction (option string)

                 (* Adjust values after they are entered. *)
                 val adjust : $reviewed -> $(map fst3 other) -> transaction $(map fst3 other)
             end) : sig
    structure One : Ui.S where type input = $M.reviewed
    (* Viewing all reviews of one thing *)

    con hidden_fields :: {Type}
    constraint hidden_fields ~ M.reviewed
    structure Several : Ui.S where type input = sql_exp [T = M.reviewed ++ hidden_fields] [] [] bool
    (* Viewing all reviews matching a condition on the reviewed items *)
end

