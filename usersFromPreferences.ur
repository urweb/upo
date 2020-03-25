open Bootstrap4

functor Make(M : sig
                 con choice :: Name
                 type choiceT
                 con users :: {{{Unit}}}
                 con choiceR :: {Type}
                 constraint [choice] ~ users
                 constraint [choice] ~ choiceR
                 constraint users ~ choiceR
                 con ckeys :: {{Unit}}
                 val choice : sql_table ([choice = choiceT] ++ map (fn _ => option string) users ++ choiceR) ckeys
                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val eq_choiceT : eq choiceT
                 val inj_choiceT : sql_injectable_prim choiceT
                 val cfl : folder choiceR
                 val fl : folder users
                 val labels : $(map (fn _ => string) users)

                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 val prefs : $(map (sql_table [user = string, slot = choiceT, preferred = bool]) users)

                 val whoami : transaction (option string)
             end) = struct
    open M

    type a = {Users : $(map (fn _ => list (string * source int)) users),
              (* ^-- tells us how many times each user is tapped, per role *)
              Choices : list {Choice : choiceT,
                              Users : $(map (fn _ => {Available : list {User : string,
                                                                        Preferred : bool,
                                                                        NowChosen : source int},
                                                      Selected : source string,
                                                      Stashed : source (option string)}) users)}}

    val create =
        choices <- List.mapQueryM (SELECT choice.{choice}, choice.{{map (fn _ => option string) users}}
                                   FROM choice
                                   ORDER BY choice.{choice})
                 (fn {Choice = r : $([choice = choiceT] ++ map (fn _ => option string) users)} =>
                     userss <-
                     @Monad.mapR2 _ [sql_table [user = string, slot = choiceT, preferred = bool]] [fn _ => option string]
                      [fn _ => {Available : list {User : string,
                                                  Preferred : bool},
                                Selected : source string,
                                Stashed : source (option string)}]
                     (fn [nm ::_] [t ::_]
                         (pref : sql_table [user = string, slot = choiceT, preferred = bool] t)
                         (assigned : option string) =>
                         assigned' <- source (case assigned of
                                                  None => ""
                                                | Some x => show x);
                         assigned <- source assigned;
                         users <- List.mapQuery (SELECT pref.{user}, pref.{preferred}
                                                 FROM pref
                                                 WHERE pref.{slot} = {[r.choice]}
                                                 ORDER BY pref.{preferred} DESC, pref.{user})
                                                (fn {Pref = r} => {User = r.user, Preferred = r.preferred});
                         return {Available = users,
                                 Selected = assigned',
                                 Stashed = assigned})
                     fl prefs (r -- choice);
                     return {Choice = r.choice, Users = userss});

        users <- @foldR [sql_table [user = string, slot = choiceT, preferred = bool]]
                  [fn us => others :: {{{Unit}}} -> [us ~ others] => [us ++ others ~ [choice]] => [us ++ others ~ choiceR]
                            => sql_table ([choice = choiceT] ++ map (fn _ => option string) (us ++ others) ++ choiceR) ckeys
                            -> $(map (fn _ => nullify (option string) (option string)) (us ++ others))
                            -> transaction ($(map (fn _ => list (string * source int)) us))]
                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                      (pref : sql_table [user = string, slot = choiceT, preferred = bool] t)
                      (acc : others :: {{{Unit}}} -> [r ~ others] => [r ++ others ~ [choice]] => [r ++ others ~ choiceR]
                             => sql_table ([choice = choiceT] ++ map (fn _ => option string) (r ++ others) ++ choiceR) ckeys
                                -> $(map (fn _ => nullify (option string) (option string)) (r ++ others))
                                -> transaction ($(map (fn _ => list (string * source int)) r)))
                      [others :: {{{Unit}}}] [[nm = t] ++ r ~ others] [[nm = t] ++ r ++ others ~ [choice]] [[nm = t] ++ r ++ others ~ choiceR]
                      (choice : sql_table ([choice = choiceT] ++ map (fn _ => option string) ([nm = t] ++ r ++ others) ++ choiceR) ckeys)
                      (injs : $(map (fn _ => nullify (option string) (option string)) ([nm = t] ++ r ++ others))) =>
                      users <- List.mapQueryM (SELECT DISTINCT pref.{user}, (SELECT COUNT( * )
                                                                             FROM choice
                                                                             WHERE choice.{nm} = {sql_nullable (SQL pref.{user})}) AS Count
                                               FROM pref)
                                              (fn r =>
                                                  c <- source (Option.get 0 r.Count);
                                                  return (r.Pref.user, c));
                      userss <- @acc [[nm = t] ++ others] ! ! ! choice injs;
                      return ({nm = users} ++ userss))
                     (fn [others ::_] [[] ~ others] [others ~ [choice]] [others ~ choiceR]
                                      _ _ => return {}) fl prefs [[]] ! ! !
                     choice (@map0 [fn _ => nullify (option string) (option string)]
                              (fn [u ::_] => _) fl);

        return {Users = users,
                Choices = List.mp (fn c =>
                                      c -- #Users
                                        ++ {Users = @map2 [fn _ => {Available : list {User : string, Preferred : bool}, Selected : _, Stashed : _}]
                                                     [fn _ => list (string * source int)]
                                                     [fn _ => {Available : list {User : string, Preferred : bool, NowChosen : source int}, Selected : _, Stashed : _}]
                                                     (fn [t] us uscs =>
                                                         us -- #Available
                                                            ++ {Available =
                                                                List.mp (fn u =>
                                                                            case List.assoc u.User uscs of
                                                                                None => error <xml>Unknown user found while counting assignments!</xml>
                                                                              | Some s => u ++ {NowChosen = s}) us.Available})
                                                     fl c.Users users}) choices}

    fun onload _ = return ()

    fun save cs =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            List.app (fn (c, us) =>
                         @@Sql.easy_update'' [[choice = choiceT]] [map (fn _ => option string) users] [_] [choiceR] ! !
                           _ (@map0 [fn _ => sql_injectable (option string)] (fn [u ::_] => _) (@@Folder.mp [fn _ => option string] [_] fl))
                           _ (@Folder.mp fl) choice {choice = c} us) cs

    fun render _ a = <xml>
      <button class="btn btn-primary"
              onclick={fn _ =>
                          cs <- List.mapM (fn c =>
                                              us <- @Monad.mapR _ [fn _ => {Available : _, Selected : source string, Stashed : source (option string)}]
                                                     [fn _ => option string]
                                                     (fn [nm ::_] [t ::_] {Stashed = stashed, ...} => get stashed)
                                                     fl c.Users;
                                              return (c.Choice, us))
                                          a.Choices;
                          rpc (save cs)}>
        Save
      </button>
      
      <table class="bs-table table-striped">
        <tr>
          <th/>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] l => <xml><th>{[l]}</th></xml>)
            fl labels}
        </tr>

        {List.mapX (fn c => <xml><tr>
          <td>{[c.Choice]}</td>
          {@mapX2 [fn _ => {Available : list {User : string,
                                              Preferred : bool,
                                              NowChosen : source int},
                            Selected : source string,
                            Stashed : source (option string)}]
            [fn _ => list (string * source int)] [tr]
            (fn [nm ::_] [t ::_]  [r ::_] [[nm] ~ r] {Available = us, Selected = s, Stashed = stashed} counts => <xml><td>
              <dyn signal={cs <- List.mapM (fn u =>
                                               nc <- signal u.NowChosen;
                                               return (u.User,
                                                       u.User
                                                       ^ (if u.Preferred then
                                                              "*"
                                                          else
                                                              "")
                                                       ^ (if nc > 0 then
                                                              " [chosen for " ^ show nc ^ "]"
                                                          else
                                                              ""))) us;
                           return <xml>
                             <cselect source={s}
                                      onchange={old <- get stashed;
                                                new <- get s;
                                                new <- return (case new of
                                                                   "" => None
                                                                 | _ => Some (readError new));
                                                set stashed new;
                                                (case old of
                                                     None => return ()
                                                   | Some old =>
                                                     List.app (fn (u, c) =>
                                                                  if u = old then
                                                                      n <- get c;
                                                                      set c (n - 1)
                                                                  else
                                                                      return ()) counts);
                                                (case new of
                                                     None => return ()
                                                   | Some new =>
                                                     List.app (fn (u, c) =>
                                                                  if u = new then
                                                                      n <- get c;
                                                                      set c (n + 1)
                                                                  else
                                                                      return ()) counts)}>
                               <coption value="">unchosen</coption>
                               {List.mapX (fn (u, t) => <xml><coption value={u}>{[t]}</coption></xml>) cs}
                             </cselect>
                           </xml>}/>
            </td></xml>) fl c.Users a.Users}
        </tr></xml>) a.Choices}
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
