(* Generalized voting, where every authorized user may see who voted for what *)

functor Make(M : sig
                 con voterKey1 :: Name
                 type voterKeyT
                 con voterKeyR :: {Type}
                 constraint [voterKey1] ~ voterKeyR
                 con voterKey = [voterKey1 = voterKeyT] ++ voterKeyR
                 con voterRest :: {Type}
                 constraint voterKey ~ voterRest
                 con voterKeyName :: Name
                 con voterOtherConstraints :: {{Unit}}
                 constraint [voterKeyName] ~ voterOtherConstraints
                 val voter : sql_table (voterKey ++ voterRest) ([voterKeyName = map (fn _ => ()) voterKey] ++ voterOtherConstraints)
                 val voterKeyInj : $(map sql_injectable_prim voterKey)
                 val voterKeyFl : folder voterKey
                 val voterKeyShow : show $voterKey
                 val voterKeyEq : $(map eq voterKey)
                 val voterKeyOrd : $(map ord voterKey)

                 con choiceBallot :: {Type} (* Identifies subsets of the choices that should be considered together *)
                 con choiceKey1 :: Name
                 type choiceKeyT
                 con choiceKeyR :: {Type}
                 constraint [choiceKey1] ~ choiceKeyR
                 con choiceKey = [choiceKey1 = choiceKeyT] ++ choiceKeyR
                 con choiceRest :: {Type}
                 constraint choiceBallot ~ choiceKey
                 constraint (choiceKey ++ choiceBallot) ~ choiceRest
                 con choiceKeyName :: Name
                 con choiceOtherConstraints :: {{Unit}}
                 constraint [choiceKeyName] ~ choiceOtherConstraints
                 val choice : sql_table (choiceBallot ++ choiceKey ++ choiceRest) ([choiceKeyName = map (fn _ => ()) (choiceBallot ++ choiceKey)] ++ choiceOtherConstraints)
                 val choiceKeyInj : $(map sql_injectable_prim choiceKey)
                 val choiceKeyFl : folder choiceKey
                 val choiceKeyShow : show $choiceKey
                 val choiceKeyEq : $(map eq choiceKey)
                 val choiceBallotInj : $(map sql_injectable_prim choiceBallot)
                 val choiceBallotFl : folder choiceBallot

                 constraint voterKey ~ (choiceBallot ++ choiceKey)
                 constraint (voterKey ++ choiceBallot ++ choiceKey) ~ [Votes]
                 constraint choiceBallot ~ [Channel]

                 val amVoter : transaction (option $voterKey)
                 val maxVotesPerVoter : option int
                 val keyFilter : sql_exp [Choice = choiceBallot ++ choiceKey ++ choiceRest] [] [] bool
                 (* Only show choices matching this filter. *)

                 val alwaysShowVotes : bool
                 (* Next to each item, show who votes for it, without requiring user to click a button first? *)
             end) : sig

    include Ui.S where type input = {Ballot : $M.choiceBallot, Voter : $M.voterKey}

    val removeVotesFor : $(M.choiceBallot ++ M.choiceKey) -> transaction unit

    structure OneChoice : Ui.S where type input = {Ballot : $M.choiceBallot,
                                                   Choice : $M.choiceKey,
                                                   Voter : $M.voterKey}

end
