(* Abstraction of submitting evaluations of database objects *)

functor Make(M : sig
                 con reviewer :: Name
                 con reviewed :: {Type}
                 con other :: {(Type * Type)}
                 constraint reviewed ~ other
                 constraint [reviewer] ~ [When]
                 constraint [reviewer, When] ~ reviewed
                 constraint [reviewer, When] ~ other
                 table tab : ([When = time, reviewer = string] ++ reviewed ++ map fst other)

                 val widgets : $(map Widget.t' other)
                 val reviewedFl : folder reviewed
                 val otherFl : folder other
                 val reviewedInj : $(map sql_injectable reviewed)
                 val otherInj : $(map (fn p => sql_injectable p.1) other)
                 val labels : $(map (fn _ => string) other)

                 val show_reviewed : show $reviewed
                 val summarize : $(map fst other) -> xbody
             end) : sig
    structure One : Ui.S where type input = $M.reviewed
    (* Viewing all reviews of one thing *)

    style summary
    (* For one <div> of a review that isn't expanded to full detail *)

    style full
    style fullHeader
    (* For an expanded review (full block and header row, respectively) *)
end

