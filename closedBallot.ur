open Bootstrap3

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
                 constraint (voterKey ++ choiceBallot) ~ [Client, Channel]

                 val amVoter : transaction (option $voterKey)
                 val maxVotesPerVoter : option int
                 val keyFilter : sql_exp [Choice = choiceBallot ++ choiceKey ++ choiceRest] [] [] bool
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
                   Votes : source int,
                   MyVotes : source int}

    datatype operation = Vote | Unvote
    type action = { Operation : operation, IsItYou : bool, Choice : $choiceKey }
    table listeners : (choiceBallot ++ voterKey ++ [Channel = channel action])

    type input = _
    type a = {Voter : $voterKey,
              Ballot : $choiceBallot,
              Choices : list choice,
              Channel : channel action}

    fun create r =
        choices <- List.mapQueryM ({{{sql_query1 [[]]
                                                 {Distinct = False,
                                                  From = @@sql_left_join [[]]
                                                           [[Choice = choiceBallot ++ choiceKey ++ choiceRest,
                                                             Vote = map option (choiceBallot ++ choiceKey ++ [Votes = int] ++ voterKey)]]
                                                           [[YourVote = map (fn t => (t, option t)) (choiceBallot ++ choiceKey ++ [Votes = int] ++ voterKey)]]
                                                           ! ! !
                                                           {YourVote = @Top.mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                                                        @@nullify_prim (@Folder.concat ! (_ : folder [Votes = _])
                                                                                         (@Folder.concat !
                                                                                           (@Folder.concat ! choiceBallotFl choiceKeyFl)
                                                                                           voterKeyFl))
                                                                        (_ ++ voterKeyInj
                                                                           ++ choiceBallotInj ++ choiceKeyInj)}
                                                           (@@sql_left_join [[]]
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
                                                                 ! ! ! ! choiceKeyFl}))
                                                           (FROM vote AS YourVote)
                                                           (WHERE {@@Sql.easy_join [#Choice] [#YourVote]
                                                              [choiceKey] [choiceBallot ++ choiceRest]
                                                              [choiceBallot ++ [Votes = _] ++ voterKey]
                                                              [[Vote = map option (choiceBallot ++ choiceKey ++ [Votes = int] ++ voterKey)]] [[]] [[]]
                                                              ! ! ! ! choiceKeyFl}
                                                              AND {@@Sql.easy_where [#YourVote] [voterKey] [_]
                                                                [[Choice = _, Vote = map option (choiceBallot ++ choiceKey ++ [Votes = int] ++ voterKey)]] [_] [_]
                                                                ! ! voterKeyInj' voterKeyFl r.Voter}),
                                                  Where = (WHERE {@@Sql.easy_where [#Choice] [choiceBallot] [_] [_] [_] [_]
                                                             ! ! choiceBallotInj' choiceBallotFl r.Ballot}
                                                             AND {sql_exp_weaken keyFilter}),
                                                  GroupBy = sql_subset [[Choice = (choiceKey, _),
                                                                         Vote = ([], _),
                                                                         YourVote = ([], _)]],
                                                  Having = (WHERE TRUE),
                                                  SelectFields = sql_subset [[Choice = (choiceKey, _),
                                                                              Vote = ([], _),
                                                                              YourVote = ([], _)]],
                                                  SelectExps = {Votes = sql_window (SQL SUM(vote.Votes)),
                                                                MyVotes = sql_window (SQL SUM(YourVote.Votes))}} }}}
                                         ORDER BY {{{@Sql.order_by choiceKeyFl
                                                      (@Sql.some_fields [#Choice] [choiceKey] ! ! choiceKeyFl)
                                                      sql_asc}}})
                                  (fn {Choice = k, Votes = n, MyVotes = yn, ...} =>
                                      s <- source (Option.get 0 n);
                                      ys <- source (Option.get 0 yn);
                                      return {Key = k, Votes = s, MyVotes = ys});

        chan <- channel;
        @@Sql.easy_insert [[Channel = channel action] ++ choiceBallot ++ voterKey] [_]
          ({Channel = _ : sql_injectable (channel action)} ++ choiceBallotInj' ++ voterKeyInj')
          (@Folder.cons [#Channel] [_] ! (@Folder.concat ! choiceBallotFl voterKeyFl))
          listeners ({Channel = chan} ++ r.Voter ++ r.Ballot);

        return (r ++ {Choices = choices, Channel = chan})

    fun onload a =
        let
            fun loop () =
                act <- recv a.Channel;
                (case act.Operation of
                     Vote =>
                     List.app (fn ch =>
                                  if ch.Key <> act.Choice then
                                      return ()
                                  else
                                      (votes <- Basis.get ch.Votes;
                                       set ch.Votes (votes + 1);
                                       if act.IsItYou then
                                           (yourVotes <- Basis.get ch.MyVotes;
                                            set ch.MyVotes (yourVotes + 1))
                                       else
                                           return ())) a.Choices
                   | Unvote =>
                     List.app (fn ch =>
                                  if ch.Key <> act.Choice then
                                      return ()
                                  else
                                      (votes <- Basis.get ch.Votes;
                                       set ch.Votes (votes - 1);
                                       if act.IsItYou then
                                           (yourVotes <- Basis.get ch.MyVotes;
                                            set ch.MyVotes (yourVotes - 1))
                                       else
                                           return ())) a.Choices);
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
                queryI1 (SELECT listeners.Channel, listeners.{{voterKey}}
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                (fn l => send l.Channel {Operation = Unvote, IsItYou = (l -- #Channel = v), Choice = r.Choice})
              | Some oldCount =>
                dml (UPDATE vote
                     SET Votes = Votes - 1
                     WHERE {@@Sql.easy_where [#T] [choiceBallot ++ choiceKey ++ voterKey]
                       [[Votes = _]] [[]] [[]] [[]] ! !
                       (choiceBallotInj' ++ choiceKeyInj' ++ voterKeyInj')
                       (@Folder.concat ! choiceBallotFl
                         (@Folder.concat ! choiceKeyFl voterKeyFl))
                       (r.Ballot ++ r.Choice ++ v)});
                queryI1 (SELECT listeners.Channel, listeners.{{voterKey}}
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                (fn l => send l.Channel {Operation = Unvote, IsItYou = (l -- #Channel = v), Choice = r.Choice})

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
                queryI1 (SELECT listeners.Channel, listeners.{{voterKey}}
                         FROM listeners
                         WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                        (fn l => send l.Channel {Operation = Vote, IsItYou = (l -- #Channel = v), Choice = r.Choice})
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
                    queryI1 (SELECT listeners.Channel, listeners.{{voterKey}}
                             FROM listeners
                             WHERE {@Sql.easy_where [#Listeners] ! ! choiceBallotInj' choiceBallotFl r.Ballot})
                            (fn l => send l.Channel {Operation = Vote, IsItYou = (l -- #Channel = v), Choice = r.Choice})

    fun oneChoice showKey a ch = <xml>
        <tr>
          <td>
            <dyn signal={votes <- signal ch.Votes;
                         myVotes <- signal ch.MyVotes;
                         return <xml>
                           <button class={if myVotes = 0 then
                                              CLASS "disabled btn glyphicon glyphicon-minus"
                                          else
                                              CLASS "btn glyphicon glyphicon-minus"}
                                   onclick={fn _ => rpc (del {Ballot = a.Ballot,
                                                              Choice = ch.Key})}/>
                           <button class={if (case maxVotesPerVoter of
                                                  None => False
                                                | Some n => myVotes >= n) then
                                              CLASS "disabled btn glyphicon glyphicon-plus"
                                          else
                                              CLASS "btn glyphicon glyphicon-plus"}
                                   onclick={fn _ => rpc (add {Ballot = a.Ballot,
                                                              Choice = ch.Key})}/>
                         </xml>}/>
          </td>
          {if showKey then
               <xml><td>{[ch.Key]}</td></xml>
           else
               <xml/>}
          <td>
            <dyn signal={votes <- signal ch.MyVotes;
                         return <xml>{[votes]}</xml>}/>
          </td>
          <td>
            <dyn signal={votes <- signal ch.Votes;
                         return <xml>{[votes]}</xml>}/>
          </td>
        </tr>
      </xml>

    fun render _ a = <xml>
      <table class="bs3-table table-striped">
        <tr>
          <th/>
          <th>Choice</th>
          <th>Your Vote</th>
          <th>Total Votes</th>
        </tr>

        {List.mapX (oneChoice True a) a.Choices}
      </table>

      <dyn signal={(count, choices) <- List.foldlM (fn ch (count, choices) =>
                                                       thisCount <- signal ch.Votes;
                                                       if thisCount > count then
                                                           return (thisCount, ch.Key :: [])
                                                       else if thisCount = count then
                                                           return (count, ch.Key :: choices)
                                                       else
                                                           return (count, choices))
                                                   (0, []) a.Choices;
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

end
