open Bootstrap4

functor Make(M : sig
                 con choice :: Name
                 type choiceT
                 con choiceR :: {Type}
                 constraint [choice] ~ choiceR
                 table choice : ([choice = choiceT] ++ choiceR)
                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val eq_choiceT : eq choiceT
                 val inj_choiceT : sql_injectable_prim choiceT

                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 table pref : {user : string, slot : choiceT, preferred : bool}

                 con item :: Name
                 type itemT
                 con ichoice :: Name
                 con users :: {Unit}
                 con itemR :: {(Type * Type)}
                 constraint [item] ~ [ichoice]
                 constraint [item, ichoice] ~ users
                 constraint [item, ichoice] ~ itemR
                 constraint users ~ itemR
                 table item : ([item = itemT, ichoice = option choiceT] ++ mapU (option string) users ++ map fst itemR)
                 val fl : folder users
                 val show_itemT : show itemT
                 val eq_itemT : eq itemT
                 val inj_itemT : sql_injectable_prim itemT
                 val nullify_itemR : $(map (fn p => nullify p.1 p.2) itemR)
                 val labels : $(mapU string users)

                 val authorize : transaction bool
             end) = struct
    open M

    type a = {Choices : list (choiceT * source int),
              Items : list {Item : itemT,
                            Users : $(mapU (option string) users),
                            Choices : list {Choice : choiceT,
                                            Preferred : int,
                                            Available : int,
                                            NowChosen : source int
                                            (* How many times is it used so far? *)},
                            Choice : option choiceT}}

    (* Helper function to build an SQL expression indicating that one column
     * equals at least one of another sets of columns *)
    fun multijoin [r1 ::: {Type}] [r2 ::: {Type}] [r ::: {{Type}}]
                  [agg ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
                  [t1 :: Name] [col :: Name] [t2 :: Name] [cols :: {Unit}]
                  [[col] ~ r1] [cols ~ r2] [[t1] ~ [t2]] [[t1, t2] ~ r]
                  (fl : folder cols) (_ : sql_injectable_prim t)
        : sql_exp ([t1 = [col = t] ++ r1,
                    t2 = mapU (option t) cols ++ r2] ++ r) agg exps bool =
          @fold [fn cols => others :: {Unit} -> [cols ~ others] => [cols ++ others ~ r2]
                            => sql_exp ([t1 = [col = t] ++ r1,
                                         t2 = mapU (option t) (cols ++ others) ++ r2]
                                            ++ r) agg exps bool]
          (fn [nm ::_] [u ::_] [rest ::_] [[nm] ~ rest]
              (acc : others :: {Unit} -> [rest ~ others] => [rest ++ others ~ r2]
                     => sql_exp ([t1 = [col = t] ++ r1,
                                  t2 = mapU (option t) (rest ++ others) ++ r2]
                                     ++ r) agg exps bool)
              [others :: {Unit}] [[nm = u] ++ rest ~ others] [[nm = u] ++ rest ++ others ~ r2] =>
              (WHERE {acc [[nm = u] ++ others]}
                 OR {sql_nullable (SQL {{t1}}.{col})} = {{t2}}.{nm}))
          (fn [others ::_] [[] ~ others] [others ~ r2] => (WHERE FALSE)) fl [[]] ! !

    type a0 = list {Item : itemT,
                    Users : $(mapU (option string) users),
                    Choice : option choiceT,
                    Choices : list {Choice : choiceT,
                                    Available : int,
                                    Preferred : int}}
          
    val create =
        let
            fun items r (cs : a0) =
                return (case cs of
                            [] =>
                            {Item = r.Item.item,
                             Users = r.Item -- item -- ichoice,
                             Choice = r.Item.ichoice,
                             Choices = {Choice = r.Pref.slot,
                                        Available = 1,
                                        Preferred = if r.Pref.preferred then 1 else 0} :: []} :: []
                  | c :: cs' =>
                    if c.Item = r.Item.item then
                        {Item = c.Item,
                         Users = r.Item -- item -- ichoice,
                         Choice = c.Choice,
                         Choices = case c.Choices of
                                       [] => error <xml>Accumulator has choice-free item!</xml>
                                     | ch :: chs =>
                                       if ch.Choice = r.Pref.slot then
                                           {Choice = ch.Choice,
                                            Available = ch.Available + 1,
                                            Preferred = if r.Pref.preferred then
                                                            ch.Preferred + 1
                                                        else
                                                            ch.Preferred} :: chs
                                       else
                                           {Choice = r.Pref.slot,
                                            Available = 1,
                                            Preferred = if r.Pref.preferred then 1 else 0}
                                               :: c.Choices} :: cs'
                    else
                        {Item = r.Item.item,
                         Users = r.Item -- item -- ichoice,
                         Choice = r.Item.ichoice,
                         Choices = {Choice = r.Pref.slot,
                                    Available = 1,
                                    Preferred = if r.Pref.preferred then 1 else 0} :: []} :: cs)
        in
            items <- query (SELECT item.{item}, item.{{mapU (option string) users}}, item.{ichoice}, pref.{slot}, pref.{preferred}
                            FROM item JOIN pref
                              ON {@multijoin [#Pref] [user]
                                  [#Item] [users] ! ! ! ! fl _}
                            ORDER BY item.{item} DESC, pref.{preferred}, pref.{slot} DESC)
                           items [];
            choices <- List.mapQueryM (SELECT choice.{choice}, COUNT(item.{item}) AS Count
                                       FROM {{@@sql_left_join [[]] [[Choice = _]]
                                         [[Item = [item = (itemT, option itemT), ichoice = (option choiceT, option choiceT)]
                                                  ++ mapU (option string, option string) users ++ itemR]]
                                         ! ! !
                                         {Item = nullify_itemR
                                                 ++ @map0 [fn _ => nullify (option string) (option string)]
                                                    (fn [u ::_] => _) fl
                                                 ++ _}
                                         (FROM choice) (FROM item)
                                         (WHERE item.{ichoice} = {sql_nullable (SQL choice.{choice})})}}
                                       GROUP BY choice.{choice})
                       (fn r =>
                           c <- source r.Count;
                           return (r.Choice.choice, c));
            return {Choices = choices,
                    Items = List.mp (fn r =>
                                        r -- #Choices
                                          ++ {Choices = List.mp (fn ch =>
                                                                    ch ++ {NowChosen =
                                                                           case List.assoc ch.Choice choices of
                                                                               None => error <xml>Missing choice in initialization!</xml>
                                                                             | Some s => s})
                                                                (List.sort (fn ch1 ch2 => ch1.Preferred < ch2.Preferred
                                                                                          || (ch1.Preferred = ch2.Preferred
                                                                                              && ch1.Available < ch2.Available)) r.Choices)})
                                    items}
        end

    fun onload _ = return ()

    fun stars n =
        if n <= 0 then
            ""
        else
            "*" ^ stars (n - 1)

    val numUsers = @fold [fn _ => int]
                    (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] acc => acc + 1)
                    0 fl
                   
    fun save cs =
        auth <- authorize;
        if not auth then
            error <xml>Access denied</xml>
        else
            List.app (fn (i, c) => dml (UPDATE item
                                        SET {ichoice} = {[c]}
                                        WHERE T.{item} = {[i]})) cs

    fun render _ a = <xml>
      <active code={items <- List.mapM (fn i =>
                                           s <- source (case i.Choice of
                                                            None => ""
                                                          | Some v => show v);
                                           st <- source i.Choice;
                                           return (i -- #Choice ++ {Choice = s, StashedChoice = st})) a.Items;
                    return <xml>
                      <button class="btn btn-primary"
                              onclick={fn _ =>
                                          cs <- List.mapPartialM (fn i =>
                                                                     c <- get i.Choice;
                                                                     return (if c = "" then
                                                                                 None
                                                                             else
                                                                                 Some (i.Item, readError c)))
                                                                 items;
                                          rpc (save cs)}>
                        Save
                      </button>

                      <table class="bs-table">
                        <thead><tr>
                          <th/>
                          {@mapUX [string] [tr]
                           (fn [nm ::_] [r ::_] [[nm] ~ r] l => <xml><th>{[l]}</th></xml>)
                           fl labels}
                          <th>Choice</th>
                        </tr></thead>

                        <tbody>
                          {List.mapX (fn i => <xml><tr>
                            <td>{[i.Item]}</td>
                            {@mapUX [option string] [tr]
                              (fn [nm ::_] [r ::_] [[nm] ~ r] l => <xml><td>{[l]}</td></xml>)
                              fl i.Users}
                            <td><dyn signal={chs <- List.mapM (fn ch =>
                                                                  chosen <- signal ch.NowChosen;
                                                                  return (ch.Choice,
                                                                          show ch.Choice
                                                                          ^ stars ch.Preferred
                                                                          ^ (if ch.Available < numUsers then
                                                                                 " (" ^ show (numUsers - ch.Available) ^ " unavailable!)"
                                                                             else
                                                                                 "")
                                                                          ^ (if chosen = 0 then
                                                                                 ""
                                                                             else
                                                                                 " [chosen for " ^ show chosen ^ "]"))) i.Choices;
                                             return <xml>
                                               <cselect source={i.Choice}
                                                        onchange={old <- get i.StashedChoice;
                                                                  new <- get i.Choice;
                                                                  new <- return (case new of
                                                                                     "" => None
                                                                                   | _ => Some (readError new));
                                                                  set i.StashedChoice new;
                                                                  (case old of
                                                                       None => return ()
                                                                     | Some old =>
                                                                       List.app (fn (ch, c) =>
                                                                                    if ch = old then
                                                                                        n <- get c;
                                                                                        set c (n - 1)
                                                                                    else
                                                                                        return ()) a.Choices);
                                                                  (case new of
                                                                       None => return ()
                                                                     | Some new =>
                                                                       List.app (fn (ch, c) =>
                                                                                    if ch = new then
                                                                                        n <- get c;
                                                                                        set c (n + 1)
                                                                                    else
                                                                                        return ()) a.Choices)}>
                                                 <coption value="">unchosen</coption>
                                                 {List.mapX (fn (ch, s) => <xml><coption value={show ch}>{[s]}</coption></xml>) chs}
                                               </cselect>
                                             </xml>}/></td>
                          </tr></xml>) items}
                        </tbody>
                      </table>
                    </xml>}/>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification,
              Buttons = buttons}
end
