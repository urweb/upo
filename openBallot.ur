open Bootstrap4

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

                 val alwaysShowVotes : bool
             end) = struct

    open M

    val voterKeyEq : eq $voterKey = @@Record.eq [voterKey] voterKeyEq voterKeyFl
    val voterKeyOrd : ord $voterKey = @@Record.ord [voterKey] voterKeyOrd voterKeyFl
    val choiceKeyEq : eq $choiceKey = @@Record.eq [choiceKey] choiceKeyEq choiceKeyFl

    val voterKeyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim voterKeyFl voterKeyInj
    val choiceKeyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim choiceKeyFl choiceKeyInj
    val choiceBallotInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim choiceBallotFl choiceBallotInj

    open Option

    table vote : (choiceBallot ++ choiceKey ++ voterKey ++ [Votes = int])
      PRIMARY KEY {{@primary_key [choiceKey1] [choiceKeyR ++ choiceBallot ++ voterKey] ! !
                    (choiceKeyInj ++ choiceBallotInj ++ voterKeyInj)}},
      {{one_constraint [#Choice] (@Sql.easy_foreign ! ! ! ! ! ! (@Folder.concat ! choiceBallotFl choiceKeyFl) choice)}},
      {{one_constraint [#Voter] (@Sql.easy_foreign ! ! ! ! ! ! voterKeyFl voter)}}

    type choice = {Key : $choiceKey,
                   Votes : source (list ($voterKey * int)),
                   ShowVoters : source bool}

    datatype operation = Vote | Unvote
    type action = { Operation : operation, Voter : $voterKey, Choice : $choiceKey }
    table listeners : (choiceBallot ++ [Channel = channel action])

    type input = _
    type a = {Voter : $voterKey,
              Ballot : $choiceBallot,
              Choices : source (list choice),
              Channel : channel action}

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
                         sv <- source False;
                         return ({Key = ch, Votes = votes, ShowVoters = sv} :: acc))
                  | {Choice = ch, Vote = v} :: ls =>
                    case @Sql.unnull (@Folder.concat ! (_ : folder [Votes = _]) voterKeyFl) v of
                        None =>
                        (* This is a choice with no votes yet. *)
                        (case lastChoice of
                             None =>
                             votes <- source [];
                             sv <- source False;
                             doVotes ls ({Key = ch, Votes = votes, ShowVoters = sv} :: acc) None []
                           | Some ch' =>
                             votes' <- source votes;
                             sv' <- source False;
                             votes <- source [];
                             sv <- source False;
                             doVotes ls ({Key = ch, Votes = votes, ShowVoters = sv} :: {Key = ch', Votes = votes', ShowVoters = sv'} :: acc) None [])
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
                                sv <- source False;
                                doVotes ls ({Key = lastChoice, Votes = votes, ShowVoters = sv} :: acc)
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
                                              (WHERE {@@Sql.easy_join [#Choice] [#Vote]
                                                 [choiceKey] [choiceBallot ++ choiceRest]
                                                 [choiceBallot ++ [Votes = _] ++ voterKey]
                                                 [[]] [[]] [[]]
                                                 ! ! ! ! choiceKeyFl}),
                                     Where = (WHERE {@@Sql.easy_where [#Choice] [choiceBallot] [_] [_] [_] [_]
                                                ! ! choiceBallotInj' choiceBallotFl r.Ballot}
                                                AND {sql_exp_weaken keyFilter}),
                                     GroupBy = sql_subset_all [_],
                                     Having = (WHERE TRUE),
                                     SelectFields = sql_subset [[Choice = (choiceKey, _),
                                                                 Vote = ([Votes = _] ++ map option voterKey, _)]],
                                     SelectExps = {}} }}}
                             ORDER BY {{{@Sql.order_by (@Folder.concat ! choiceKeyFl (@Folder.mp voterKeyFl))
                                          (@Sql.some_fields [#Choice] [choiceKey] ! ! choiceKeyFl
                                            ++ @Sql.some_fields [#Vote] [map option voterKey] ! ! (@Folder.mp voterKeyFl))
                                          sql_asc}}});

            choices <- doVotes votes [] None [];
            choices <- source choices;

            chan <- channel;
            @@Sql.easy_insert [[Channel = channel action] ++ choiceBallot] [_]
              ({Channel = _ : sql_injectable (channel action)} ++ choiceBallotInj')
              (@Folder.cons [#Channel] [_] ! choiceBallotFl)
              listeners ({Channel = chan} ++ r.Ballot);

            return (r ++ {Choices = choices, Channel = chan})
        end

    fun onload a =
        let
            fun loop () =
                act <- recv a.Channel;
                (case act.Operation of
                     Vote =>
                     chs <- Basis.get a.Choices;
                     List.app (fn ch =>
                                  if ch.Key <> act.Choice then
                                      return ()
                                  else
                                      votes <- Basis.get ch.Votes;
                                      oldcount <- return (Option.get 0 (List.assoc act.Voter votes));
                                      votes <- return ((act.Voter, oldcount+1)
                                                           :: List.filter (fn (v, _) =>
                                                                              v <> act.Voter) votes);
                                      set ch.Votes (List.sort (fn (v1, _) (v2, _) => v1 > v2) votes)) chs
                   | Unvote =>
                     chs <- Basis.get a.Choices;
                     List.app (fn ch =>
                                  if ch.Key <> act.Choice then
                                      return ()
                                  else
                                      votes <- Basis.get ch.Votes;
                                      oldcount <- return (Option.get 0 (List.assoc act.Voter votes));
                                      votes <- return (List.filter (fn (v, _) =>
                                                                       v <> act.Voter) votes);
                                      votes <- return (if oldcount > 1 then
                                                           (act.Voter, oldcount-1) :: votes
                                                       else
                                                           votes);
                                      set ch.Votes (List.sort (fn (v1, _) (v2, _) => v1 > v2) votes)) chs);
                loop ()
        in
            spawn (loop ())
        end

    fun del r =
        v <- amVoter;
        case v of
            None => error <xml>Not authenticated as a voter</xml>
          | Some v =>
            oldCount <- oneOrNoRowsE1 (SELECT (vote.Votes)
                                       FROM vote
                                       WHERE {@@Sql.easy_where [#Vote] [choiceBallot ++ choiceKey ++ voterKey]
                                         [[Votes = _]] [[]] [[]] [[]] ! !
                                         (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                                         (@Folder.concat ! choiceBallotFl
                                           (@Folder.concat ! choiceKeyFl voterKeyFl))
                                         (r.Ballot ++ r.Choice ++ v)});
            case oldCount of
                None => return ()
              | Some 1 =>
                dml (DELETE FROM vote
                     WHERE {@@Sql.easy_where [#T] [choiceBallot ++ choiceKey ++ voterKey]
                       [[Votes = _]] [[]] [[]] [[]] ! !
                       (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                       (@Folder.concat ! choiceBallotFl
                         (@Folder.concat ! choiceKeyFl voterKeyFl))
                       (r.Ballot ++ r.Choice ++ v)});
                queryI1 (SELECT listeners.Channel
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                (fn l => send l.Channel {Operation = Unvote, Voter = v, Choice = r.Choice})
              | Some oldCount =>
                dml (UPDATE vote
                     SET Votes = Votes - 1
                     WHERE {@@Sql.easy_where [#T] [choiceBallot ++ choiceKey ++ voterKey]
                       [[Votes = _]] [[]] [[]] [[]] ! !
                       (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                       (@Folder.concat ! choiceBallotFl
                         (@Folder.concat ! choiceKeyFl voterKeyFl))
                       (r.Ballot ++ r.Choice ++ v)});
                queryI1 (SELECT listeners.Channel
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                (fn l => send l.Channel {Operation = Unvote, Voter = v, Choice = r.Choice})

    fun add r =
        v <- amVoter;
        case v of
            None => error <xml>Not authenticated as a voter</xml>
          | Some v =>
            oldCount <- oneOrNoRowsE1 (SELECT (vote.Votes)
                                       FROM vote
                                       WHERE {@@Sql.easy_where [#Vote] [choiceBallot ++ choiceKey ++ voterKey]
                                         [[Votes = _]] [[]] [[]] [[]] ! !
                                         (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                                         (@Folder.concat ! choiceBallotFl
                                           (@Folder.concat ! choiceKeyFl voterKeyFl))
                                         (r.Ballot ++ r.Choice ++ v)});
            case oldCount of
                None =>
                @@Sql.easy_insert [choiceBallot ++ choiceKey ++ voterKey ++ [Votes = int]] [_]
                  ({Votes = _} ++ choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                  (@Folder.cons [#Votes] [_] ! (@Folder.concat ! choiceBallotFl
                                                 (@Folder.concat ! choiceKeyFl voterKeyFl)))
                  vote
                  (r.Ballot ++ r.Choice ++ v ++ {Votes = 1});
                queryI1 (SELECT listeners.Channel
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                        (fn l => send l.Channel {Operation = Vote, Voter = v, Choice = r.Choice})
              | Some n =>
                if (case maxVotesPerVoter of
                        None => False
                      | Some m => n >= m) then
                    return ()
                else
                    dml (UPDATE vote
                         SET Votes = Votes + 1
                         WHERE {@@Sql.easy_where [#T] [choiceBallot ++ choiceKey ++ voterKey]
                           [[Votes = _]] [[]] [[]] [[]] ! !
                           (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                           (@Folder.concat ! choiceBallotFl
                             (@Folder.concat ! choiceKeyFl voterKeyFl))
                           (r.Ballot ++ r.Choice ++ v)});
                    queryI1 (SELECT listeners.Channel
                             FROM listeners
                             WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                            (fn l => send l.Channel {Operation = Vote, Voter = v, Choice = r.Choice})

    fun oneChoice showKey showVotes a ch = <xml>
        <tr>
          <td>
            <dyn signal={votes <- signal ch.Votes;
                         myVotes <- return (Option.get 0 (List.assoc a.Voter votes));
                         return <xml>
                           <button class={if myVotes = 0 then
                                              CLASS "disabled btn"
                                          else
                                              CLASS "btn"}
                                   onclick={fn _ => rpc (del {Ballot = a.Ballot,
                                                              Choice = ch.Key})}>
                             <span class="glyphicon glyphicon-minus"/>
                           </button>
                           <button class={if (case maxVotesPerVoter of
                                                  None => False
                                                | Some n => myVotes >= n) then
                                              CLASS "disabled btn"
                                          else
                                              CLASS "btn"}
                                   onclick={fn _ => rpc (add {Ballot = a.Ballot,
                                                                     Choice = ch.Key})}>
                             <span class="glyphicon glyphicon-plus"/>
                           </button>
                         </xml>}/>
          </td>
          {if showKey then
               <xml><td>{[ch.Key]}</td></xml>
           else
               <xml/>}
          <td>
            <dyn signal={votes <- signal ch.Votes;
                         return <xml>{[List.foldl (fn (v, n) m => if v = a.Voter then n + m else m) 0 votes]}</xml>}/>
          </td>
          <td>
            <dyn signal={votes <- signal ch.Votes;
                         return <xml>
                           {[List.foldl (fn (_, n) m => n + m) 0 votes]}

                           <dyn signal={sv <- (if showVotes then
                                                   return True
                                               else
                                                   signal ch.ShowVoters);
                                        return (if sv then <xml>
                                          {if showVotes then
                                               <xml/>
                                           else
                                               <xml><button class="btn"
                                                            onclick={fn _ => set ch.ShowVoters False}>
                                                      <span class="glyphicon glyphicon-chevron-left"/>
                                                    </button>></xml>}

                                          {List.mapX (fn (k, n) => <xml><br/>{[k]}{case maxVotesPerVoter of
                                                                                       Some 1 => <xml/>
                                                                                     | _ => <xml> ({[n]})</xml>}</xml>) votes}
                                        </xml> else <xml>
                                          <button class="btn"
                                                  onclick={fn _ => set ch.ShowVoters True}>
                                            <span class="glyphicon glyphicon-chevron-right"/>
                                          </button>
                                        </xml>)}/>
                         </xml>}/>
          </td>
        </tr>
      </xml>

    fun render _ a = <xml>
      <table class="bs-table table-striped">
        <tr>
          <th/>
          <th>Choice</th>
          <th>Your Vote</th>
          <th>Votes</th>
        </tr>

        <dyn signal={choices <- signal a.Choices;
                     return (List.mapX (oneChoice True alwaysShowVotes a) choices)}/>
      </table>

      <dyn signal={choices <- signal a.Choices;
                   (count, choices) <- List.foldlM (fn ch (count, choices) =>
                                                       votes <- signal ch.Votes;
                                                       thisCount <- return (List.foldl (fn (_, n) m => n + m) 0 votes);
                                                       if thisCount > count then
                                                           return (thisCount, ch.Key :: [])
                                                       else if thisCount = count then
                                                           return (count, ch.Key :: choices)
                                                       else
                                                           return (count, choices))
                                                   (0, []) choices;
                   if count = 0 then
                       return <xml/>
                   else
                       return <xml>
                         <h3>Current leader{case choices of
                                                _ :: _ :: _ => <xml>s</xml>
                                              | _ => <xml/>}, with <i>{[count]}</i> vote{case count of
                                                                                             1 => <xml/>
                                                                                           | _ => <xml>s</xml>}:</h3>

                         {List.mapX (fn ch => <xml>{[ch]}<br/></xml>) choices}
                       </xml>}/>
    </xml>

    fun ui r = {Create = create r,
                Onload = onload,
                Render = render}

    fun removeVotesFor r =
        dml (DELETE FROM vote
                    WHERE {@@Sql.easy_where [#T] [choiceBallot ++ choiceKey] [voterKey ++ [Votes = _]]
                      [[]] [[]] [[]] ! !
                      (choiceBallotInj' ++ choiceKeyInj')
                       (@Folder.concat ! choiceBallotFl choiceKeyFl)
                       r})

    structure OneChoice = struct
        type input = _
        type a = {Base : a,
                  Choice : $choiceKey}

        val create r =
            base <- create (r -- #Choice);
            return {Base = base, Choice = r.Choice}

        val onload a = onload a.Base

        fun render _ a = <xml>
          <table class="bs-table table-striped">
            <tr>
              <th/>
              <th>Your Vote</th>
              <th>Votes</th>
            </tr>

            <dyn signal={choices <- signal a.Base.Choices;
                         return (List.mapX (fn ch =>
                                               if ch.Key <> a.Choice then
                                                   <xml/>
                                               else
                                                   oneChoice False True a.Base ch) choices)}/>
          </table>
        </xml>

        fun ui x = {Create = create x,
                    Onload = onload,
                    Render = render}
    end

end
