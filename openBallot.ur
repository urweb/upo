open Bootstrap3

functor Make(M : sig
                 val voterLabel : string
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
                 val choice : sql_table (choiceBallot ++ choiceKey ++ choiceRest) ([choiceKeyName = map (fn _ => ()) choiceKey] ++ choiceOtherConstraints)
                 val choiceKeyInj : $(map sql_injectable_prim choiceKey)
                 val choiceKeyFl : folder choiceKey
                 val choiceKeyShow : show $choiceKey
                 val choiceKeyEq : $(map eq choiceKey)
                 val choiceBallotInj : $(map sql_injectable_prim choiceBallot)
                 val choiceBallotFl : folder choiceBallot

                 constraint voterKey ~ (choiceBallot ++ choiceKey)
                 constraint (voterKey ++ choiceBallot ++ choiceKey) ~ [Votes]

                 val amVoter : transaction (option $voterKey)
             end) = struct

    open M

    val voterKeyEq : eq $voterKey = @@Record.eq [voterKey] voterKeyEq voterKeyFl
    val choiceKeyEq : eq $choiceKey = @@Record.eq [choiceKey] choiceKeyEq choiceKeyFl

    val voterKeyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim voterKeyFl voterKeyInj
    val choiceKeyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim choiceKeyFl choiceKeyInj
    val choiceBallotInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim choiceBallotFl choiceBallotInj

    open Option

    table vote : (choiceBallot ++ choiceKey ++ voterKey ++ [Votes = int])
      PRIMARY KEY {{@primary_key [choiceKey1] [choiceKeyR ++ choiceBallot ++ voterKey] ! !
                    (choiceKeyInj ++ choiceBallotInj ++ voterKeyInj)}},
      {{one_constraint [#Choice] (@Sql.easy_foreign ! ! ! ! ! ! choiceKeyFl choice)}},
      {{one_constraint [#Voter] (@Sql.easy_foreign ! ! ! ! ! ! voterKeyFl voter)}}

    type choice = {Key : $choiceKey,
                   Votes : source (list ($voterKey * int))}

    type input = _
    type a = {Voter : $voterKey,
              Ballot : $choiceBallot,
              Choices : source (list choice)}

    fun create r =
        let
            fun doVotes (ls : list { Choice : $choiceKey, Vote : $(map option voterKey ++ [Votes = option int]) })
                        (acc : list choice)
                        (lastChoice : option $choiceKey)
                        (votes : list ($voterKey * int)) =
                case ls of
                    [] =>
                    (case lastChoice of
                         None => return acc
                       | Some ch =>
                         votes <- source votes;
                         return ({Key = ch, Votes = votes} :: acc))
                  | {Choice = ch, Vote = v} :: ls =>
                    case @Sql.unnull (@Folder.concat ! (_ : folder [Votes = _]) voterKeyFl) v of
                        None =>
                        (* This is a choice with no votes yet. *)
                        (case lastChoice of
                             None =>
                             votes <- source [];
                             doVotes ls ({Key = ch, Votes = votes} :: acc) None []
                           | Some ch' =>
                             votes' <- source votes;
                             votes <- source [];
                             doVotes ls ({Key = ch, Votes = votes} :: {Key = ch', Votes = votes'} :: acc) None [])
                      | Some v =>
                        (* This is a vote for the current choice. *)
                        if lastChoice = Some ch then
                            (* We're continuing with the last choice processed. *)
                            doVotes ls acc lastChoice ((v -- #Votes, v.Votes) :: votes)
                        else
                            case lastChoice of
                                None =>
                                (* There was no last choice needing further processing. *)
                                doVotes ls acc (Some ch) ((v -- #Votes, v.Votes) :: [])
                              | Some lastChoice =>
                                votes <- source votes;
                                doVotes ls ({Key = lastChoice, Votes = votes} :: acc)
                                        (Some ch) ((v -- #Votes, v.Votes) :: [])
        in
            votes <- queryL ({{{sql_query1 [[]]
                                    {Distinct = False,
                                     From = @@sql_left_join [[]]
                                              [[Choice = choiceBallot ++ choiceKey ++ choiceRest]]
                                              [[Vote = map (fn t => (t, option t)) (choiceBallot ++ choiceKey ++ [Votes = int] ++ voterKey)]]
                                              ! ! !
                                              {Vote = @Top.mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                                @@nullify_prim (@Folder.concat ! (_ : folder [Votes = _])
                                                                 (@Folder.concat !
                                                                   (@Folder.concat ! choiceBallotFl choiceKeyFl)
                                                                   voterKeyFl))
                                                (_ ++ voterKeyInj
                                                   ++ choiceBallotInj ++ choiceKeyInj)}
                                              (FROM choice) (FROM vote)
                                              (@@Sql.easy_join [#Choice] [#Vote]
                                                 [choiceKey] [choiceBallot ++ choiceRest]
                                                 [choiceBallot ++ [Votes = _] ++ voterKey]
                                                 [[]] [[]] [[]]
                                                 ! ! ! ! choiceKeyFl),
                                     Where = @@Sql.easy_where [#Choice] [choiceBallot] [_] [_] [_] [_]
                                               ! ! choiceBallotInj' choiceBallotFl r.Ballot,
                                     GroupBy = sql_subset_all [_],
                                     Having = (WHERE TRUE),
                                     SelectFields = sql_subset [[Choice = (choiceKey, _),
                                                                 Vote = ([Votes = _] ++ map option voterKey, _)]],
                                     SelectExps = {}} }}}
                             ORDER BY {{{@Sql.order_by (@Folder.concat ! choiceKeyFl (@Folder.mp voterKeyFl))
                                          (@Sql.some_fields [#Choice] [choiceKey] ! ! choiceKeyFl
                                            ++ @Sql.some_fields [#Vote] [map option voterKey] ! ! (@Folder.mp voterKeyFl))
                                          sql_desc}}});

            choices <- doVotes votes [] None [];
            choices <- source choices;
            return (r ++ {Choices = choices})
        end

    fun ui r = {Create = create r,
                Onload = fn _ => return (),
                Render = fn _ _ => <xml/>}

end
