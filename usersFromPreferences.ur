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
                 val inj_choiceR : $(map sql_injectable_prim choiceR)
                 val cfl : folder choiceR

                 con user :: Name
                 con slot :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [preferred]
                 con users :: {{{Unit}}}
                 val prefs : $(map (sql_table [user = string, slot = choiceT, preferred = bool]) users)

                 constraint [choice] ~ users
                 constraint choiceR ~ users
                 con akeys :: {{Unit}}
                 val assignment : sql_table ([choice = choiceT] ++ map (fn _ => string) users ++ choiceR) akeys
                 val fl : folder users
                 val labels : $(map (fn _ => string) users)
                                    
                 val whoami : transaction (option string)
             end) = struct
    open M

    type a = {Users : $(map (fn _ => list (string * source int)) users),
              (* ^-- tells us how many times each user is tapped, per role *)
              Choices : list {Choice : choiceT,
                              Users : $(map (fn _ => list {User : string,
                                                           Preferred : bool,
                                                           NowChosen : source int
                                                           (* How many times is this user
                                                            * tapped in thise role so far? *)}) users),
                              Selections : $(map (fn _ => source string * source (option string)) users)}}

    val create =
        choices <- List.mapQueryM (SELECT choice.{choice} AS Choic
                                   FROM choice
                                   ORDER BY choice.{choice})
                 (fn {Choic = c} =>
                     (userss, assigneds) <-
                     @foldR [sql_table [user = string, slot = choiceT, preferred = bool]]
                      [fn us => others :: {{{Unit}}} -> [us ~ others] => [us ++ others ~ [choice]] => [us ++ others ~ choiceR]
                              => sql_table ([choice = choiceT] ++ map (fn _ => string) (us ++ others) ++ choiceR) akeys
                              -> transaction ($(map (fn _ => list {User : string,
                                                                   Preferred : bool}) us)
                                              * $(map (fn _ => source string * source (option string)) us))]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                         (pref : sql_table [user = string, slot = choiceT, preferred = bool] t)
                         (acc : others :: {{{Unit}}} -> [r ~ others] => [r ++ others ~ [choice]] => [r ++ others ~ choiceR]
                                => sql_table ([choice = choiceT] ++ map (fn _ => string) (r ++ others) ++ choiceR) akeys
                                -> transaction ($(map (fn _ => list {User : string,
                                                                     Preferred : bool}) r)
                                                * $(map (fn _ => source string * source (option string)) r)))
                         [others :: {{{Unit}}}] [[nm = t] ++ r ~ others] [[nm = t] ++ r ++ others ~ [choice]] [[nm = t] ++ r ++ others ~ choiceR]
                         (assignment : sql_table ([choice = choiceT] ++ map (fn _ => string) ([nm = t] ++ r ++ others) ++ choiceR) akeys) =>
                         assigned <- oneOrNoRowsE1 (SELECT (assignment.{nm})
                                                    FROM assignment
                                                    WHERE assignment.{choice} = {[c]});
                         assigned' <- source (case assigned of
                                                  None => ""
                                                | Some x => show x);
                         assigned <- source assigned;
                         users <- List.mapQuery (SELECT pref.{user}, pref.{preferred}
                                                 FROM pref
                                                 WHERE pref.{slot} = {[c]}
                                                 ORDER BY pref.{preferred} DESC, pref.{user})
                                                (fn {Pref = r} => {User = r.user, Preferred = r.preferred});
                         (userss, assigneds) <- acc [[nm = t] ++ others] assignment;
                         return ({nm = users} ++ userss,
                                 {nm = (assigned', assigned)} ++ assigneds))
                     (fn [others ::_] [[] ~ others] [others ~ [choice]] [others ~ choiceR]
                                      _ => return ({}, {})) fl prefs [[]] ! ! ! assignment;
                     return {Choice = c, Users = userss, Selections = assigneds});

        users <- @foldR [sql_table [user = string, slot = choiceT, preferred = bool]]
                  [fn us => others :: {{{Unit}}} -> [us ~ others] => [us ++ others ~ [choice]] => [us ++ others ~ choiceR]
                            => sql_table ([choice = choiceT] ++ map (fn _ => string) (us ++ others) ++ choiceR) akeys
                            -> $(map (fn _ => nullify string (option string)) (us ++ others))
                            -> transaction ($(map (fn _ => list (string * source int)) us))]
                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                      (pref : sql_table [user = string, slot = choiceT, preferred = bool] t)
                      (acc : others :: {{{Unit}}} -> [r ~ others] => [r ++ others ~ [choice]] => [r ++ others ~ choiceR]
                             => sql_table ([choice = choiceT] ++ map (fn _ => string) (r ++ others) ++ choiceR) akeys
                                -> $(map (fn _ => nullify string (option string)) (r ++ others))
                                -> transaction ($(map (fn _ => list (string * source int)) r)))
                      [others :: {{{Unit}}}] [[nm = t] ++ r ~ others] [[nm = t] ++ r ++ others ~ [choice]] [[nm = t] ++ r ++ others ~ choiceR]
                      (assignment : sql_table ([choice = choiceT] ++ map (fn _ => string) ([nm = t] ++ r ++ others) ++ choiceR) akeys)
                      (injs : $(map (fn _ => nullify string (option string)) ([nm = t] ++ r ++ others))) =>
                      users <- List.mapQueryM (SELECT DISTINCT pref.{user}, (SELECT COUNT( * )
                                                                             FROM assignment
                                                                             WHERE assignment.{nm} = pref.{user}) AS Count
                                               FROM pref)
                                              (fn r =>
                                                  c <- source (Option.get 0 r.Count);
                                                  return (r.Pref.user, c));
                      userss <- @acc [[nm = t] ++ others] ! ! ! assignment injs;
                      return ({nm = users} ++ userss))
                     (fn [others ::_] [[] ~ others] [others ~ [choice]] [others ~ choiceR]
                                      _ _ => return {}) fl prefs [[]] ! ! !
                     assignment (@map0 [fn _ => nullify string (option string)]
                                  (fn [u ::_] => _) fl);

        return {Users = users,
                Choices = List.mp (fn c =>
                                      c -- #Users
                                        ++ {Users = @map2 [fn _ => list {User : string, Preferred : bool}]
                                                     [fn _ => list (string * source int)]
                                                     [fn _ => list {User : string, Preferred : bool, NowChosen : source int}]
                                                     (fn [t] us uscs =>
                                                         List.mp (fn u =>
                                                                     case List.assoc u.User uscs of
                                                                         None => error <xml>Unknown user found while counting assignments!</xml>
                                                                       | Some s => u ++ {NowChosen = s}) us)
                                                     fl c.Users users}) choices}

    fun onload _ = return ()

    fun save cs =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some _ =>
            dml (DELETE FROM assignment WHERE TRUE);
            List.app (fn (c, us) =>
                         rest <- oneRow1 (SELECT choice.{{choiceR}}
                                          FROM choice
                                          WHERE choice.{choice} = {[c]});
                         @@Sql.easy_insert [[choice = choiceT] ++ map (fn _ => string) users ++ choiceR] [akeys]
                           ({choice = _}
                                ++ @map0 [fn _ => sql_injectable string]
                                (fn [u ::_] => _) fl
                                ++ @mp [sql_injectable_prim] [sql_injectable]
                                (fn [t] => @sql_prim)
                                cfl inj_choiceR)
                           (@Folder.cons [choice] [_] ! (@Folder.concat ! cfl (@Folder.mp fl)))
                           assignment ({choice = c} ++ us ++ rest)) cs

    fun render _ a = <xml>
      <button class="btn btn-primary"
              onclick={fn _ =>
                          cs <- List.mapPartialM (fn c =>
                                                     us <- @Monad.foldR _ [fn _ => source string * source (option string)]
                                                            [fn r => option $(map (fn _ => string) r)]
                                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_, stashed) acc =>
                                                                case acc of
                                                                    None => return None
                                                                  | Some acc =>
                                                                    stashed <- get stashed;
                                                                    case stashed of
                                                                        None => return None
                                                                      | Some stashed => return (Some ({nm = stashed} ++ acc)))
                                                            (Some {}) fl c.Selections;
                                                     return (case us of
                                                                 None => None
                                                               | Some us => Some (c.Choice, us)))
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
          {@mapX3 [fn _ => list {User : string,
                                 Preferred : bool,
                                 NowChosen : source int}]
            [fn _ => source string * source (option string)]
            [fn _ => list (string * source int)] [tr]
            (fn [nm ::_] [t ::_]  [r ::_] [[nm] ~ r] us (s, stashed) counts => <xml><td>
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
            </td></xml>) fl c.Users c.Selections a.Users}
        </tr></xml>) a.Choices}
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
