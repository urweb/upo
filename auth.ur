(* Generic authentication backed by a database table *)

signature S = sig
    con groups :: {Unit}

    val whoami : transaction (option string)
    val whoamiWithMasquerade : transaction (option string)

    val getUser : transaction string
    val getUserWithMasquerade : transaction string
    val requireUser : transaction unit

    val masqueradeAs : string -> transaction unit
    val unmasquerade : transaction unit

    val inGroup : variant (mapU unit groups) -> transaction bool

    val requireGroup : variant (mapU unit groups) -> transaction unit

    val getGroup : variant (mapU unit groups) -> transaction string
    val getGroupWithMasquerade : variant (mapU unit groups) -> transaction string

    val inGroups : dummy ::: {Unit} -> folder dummy
                   -> $(mapU (variant (mapU unit groups)) dummy) -> transaction bool
    val requireGroups : dummy ::: {Unit} -> folder dummy
                        -> $(mapU (variant (mapU unit groups)) dummy) -> transaction unit
    val getGroups : dummy ::: {Unit} -> folder dummy
                    -> $(mapU (variant (mapU unit groups)) dummy) -> transaction string
    val getGroupsWithMasquerade : dummy ::: {Unit} -> folder dummy
                                  -> $(mapU (variant (mapU unit groups)) dummy) -> transaction string
end

functor Make(M : sig
                 con name :: Name
                 con setThese :: {Type}
                 con groups :: {Unit}
                 con others :: {Type}

                 constraint [name] ~ setThese
                 constraint ([name] ++ map (fn _ => ()) setThese) ~ groups
                 constraint ([name] ++ map (fn _ => ()) setThese ++ groups) ~ others

                 table users : ([name = string] ++ setThese ++ mapU bool groups ++ others)

                 val underlying : transaction (option $([name = string] ++ setThese))
                 val defaults : option $(mapU bool groups ++ others)
                 val allowMasquerade : option (variant (mapU unit groups))
                 val requireSsl : bool

                 val fls : folder setThese
                 val flg : folder groups
                 val flo : folder others

                 val injs : $(map sql_injectable setThese)
                 val injo : $(map sql_injectable others)

                 val eqs : $(map eq setThese)
             end) = struct

    open M

    cookie masquerade : string

    val eqs' = @Record.eq eqs fls

    val anyToSet = @fold [fn _ => bool] (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] _ => True) False fls

    con schema = [Users = [name = string] ++ setThese ++ mapU bool groups ++ others]

    fun variantToExp (g : variant (mapU unit groups))
        : sql_exp schema schema [] bool =
          @@match [mapU unit groups] [sql_exp schema schema [] bool] g
          (@fold [fn gs => gso :: {Unit} -> [gso ~ gs] => [[name = string] ++ setThese ++ others ~ gso ++ gs] => $(mapU (unit -> sql_exp [Users = [name = string] ++ setThese ++ mapU bool (gs ++ gso) ++ others] [Users = [name = string] ++ setThese ++ mapU bool (gs ++ gso) ++ others] [] bool) gs)]
            (fn [nm ::_] [u ::_] [gs ::_] [[nm] ~ gs]
                         (acc : gso :: {Unit} -> [gso ~ gs] => [[name = string] ++ setThese ++ others ~ gso ++ gs] => $(mapU (unit -> sql_exp [Users = [name = string] ++ setThese ++ mapU bool (gs ++ gso) ++ others] [Users = [name = string] ++ setThese ++ mapU bool (gs ++ gso) ++ others] [] bool) gs))
                         [gso ::_] [gso ~ [nm] ++ gs] [[name = string] ++ setThese ++ others ~ gso ++ [nm] ++ gs] =>
                {nm = fn () => (SQL users.{nm})}
                    ++ acc [[nm] ++ gso])
            (fn [gso ::_] [gso ~ []] [[name = string] ++ setThese ++ others ~ gso] => {}) flg [[]] ! !)

    fun whoami' masq =
        data <- underlying;
        case data of
            None => return None
          | Some data =>
            mq <- (if masq then getCookie masquerade else return None);
            case mq of
                Some mq =>
                (case allowMasquerade of
                     None => error <xml>Masquerade not allowed</xml>
                   | Some g =>
                     b <- oneOrNoRowsE1 (SELECT ({variantToExp g})
                                         FROM users
                                         WHERE users.{name} = {[data.name]});
                     case b of
                         None => error <xml>User not found</xml>
                       | Some b =>
                         if b then
                             return (Some mq)
                         else
                             error <xml>Access denied</xml>)
              | None =>
                if anyToSet then
                    inDb <- oneOrNoRows1 (SELECT users.{{setThese}}
                                          FROM users
                                          WHERE users.{name} = {[data.name]});
                    case inDb of
                        None =>
                        (case defaults of
                             None => return None
                           | Some defaults =>
                             @@Sql.easy_insert [[name = _] ++ setThese ++ mapU bool groups ++ others] [_]
                               ({name = _} ++ injs ++ injo ++ @map0 [fn _ => sql_injectable bool] (fn [u ::_] => _) flg)
                               (@Folder.cons [name] [_] ! (@Folder.concat ! (@Folder.mp flg)
                                                            (@Folder.concat ! fls flo)))
                               users (data ++ defaults);
                             return (Some data.name))
                      | Some r =>
                        (if r = data -- name then
                             return ()
                         else
                             @Sql.easy_update'' ! ! {name = _} injs _ fls users {name = data.name} (data -- name));
                        return (Some data.name)
                else
                    inDb <- oneRowE1 (SELECT COUNT( * ) > 0
                                      FROM users
                                      WHERE users.{name} = {[data.name]});
                    if inDb then
                        return (Some data.name)
                    else
                        case defaults of
                            None => return None
                          | Some defaults =>
                            @@Sql.easy_insert [[name = _] ++ setThese ++ mapU bool groups ++ others] [_]
                              ({name = _} ++ injs ++ injo ++ @map0 [fn _ => sql_injectable bool] (fn [u ::_] => _) flg)
                              (@Folder.cons [name] [_] ! (@Folder.concat ! (@Folder.mp flg)
                                                           (@Folder.concat ! fls flo)))
                              users (data ++ defaults);
                            return (Some data.name)

    val whoami = whoami' False
    val whoamiWithMasquerade = whoami' True

    val getUser =
        o <- whoami;
        case o of
            None => error <xml>Access denied (must be logged in)</xml>
          | Some s => return s

    val getUserWithMasquerade =
        o <- whoamiWithMasquerade;
        case o of
            None => error <xml>Access denied (must be logged in)</xml>
          | Some s => return s

    val requireUser =
        o <- whoami;
        case o of
            None => error <xml>Access denied (must be logged in)</xml>
          | Some _ => return ()

    fun inGroup g =
        u <- whoami;
        case u of
            None => return False
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({variantToExp g})
                                 FROM users
                                 WHERE users.{name} = {[u]});
            return (case bo of
                        None => False
                      | Some b => b)

    fun requireGroup g =
        b <- inGroup g;
        if b then
            return ()
        else
            error <xml>Access denied</xml>

    fun getGroup g =
        u <- whoami;
        case u of
            None => error <xml>Access denied</xml>
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({variantToExp g})
                                 FROM users
                                 WHERE users.{name} = {[u]});
            case bo of
                Some True => return u
              | _ => error <xml>Access denied</xml>

    fun getGroupWithMasquerade g =
        u <- whoamiWithMasquerade;
        case u of
            None => error <xml>Access denied</xml>
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({variantToExp g})
                                FROM users
                                WHERE users.{name} = {[u]});
            case bo of
                Some True => return u
              | _ => error <xml>Access denied</xml>

    fun inGroups [dummy] (fl : folder dummy) (gs : $(mapU _ dummy)) =
        u <- whoami;
        case u of
            None => return False
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({@foldUR [variant (mapU unit groups)] [fn _ => sql_exp schema schema [] bool]
                                     (fn [nm ::_] [r ::_] [[nm] ~ r] g e =>
                                       (SQL {e} OR {variantToExp g}))
                                     (SQL FALSE) fl gs})
                                      FROM users
                                      WHERE users.{name} = {[u]});
            return (Option.get False bo)

    fun requireGroups [dummy] (fl : folder dummy) (gs : $(mapU _ dummy)) =
        b <- @inGroups fl gs;
        if b then
            return ()
        else
            error <xml>Access denied</xml>

    fun getGroups [dummy] (fl : folder dummy) (gs : $(mapU _ dummy)) =
        u <- whoami;
        case u of
            None => error <xml>Access denied</xml>
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({@foldUR [variant (mapU unit groups)] [fn _ => sql_exp schema schema [] bool]
                                     (fn [nm ::_] [r ::_] [[nm] ~ r] g e =>
                                       (SQL {e} OR {variantToExp g}))
                                     (SQL FALSE) fl gs})
                                      FROM users
                                      WHERE users.{name} = {[u]});
            case bo of
                Some True => return u
              | _ => error <xml>Access denied</xml>

    fun getGroupsWithMasquerade [dummy] (fl : folder dummy) (gs : $(mapU _ dummy)) =
        u <- whoamiWithMasquerade;
        case u of
            None => error <xml>Access denied</xml>
          | Some u =>
            bo <- oneOrNoRowsE1 (SELECT ({@foldUR [variant (mapU unit groups)] [fn _ => sql_exp schema schema [] bool]
                                     (fn [nm ::_] [r ::_] [[nm] ~ r] g e =>
                                       (SQL {e} OR {variantToExp g}))
                                     (SQL FALSE) fl gs})
                                      FROM users
                                      WHERE users.{name} = {[u]});
            case bo of
                Some True => return u
              | _ => error <xml>Access denied</xml>

    val unmasquerade = clearCookie masquerade

    fun masqueradeAs u =
        case allowMasquerade of
            None => error <xml>Masquerading is not enabled.</xml>
          | Some g =>
            b <- inGroup g;
            if b then
                setCookie masquerade {Value = u,
                                      Expires = None,
                                      Secure = requireSsl}
            else
                error <xml>Access denied</xml>

end
