(* Generic authentication backed by a database table *)

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

                 val fls : folder setThese
                 val flg : folder groups
                 val flo : folder others

                 val injs : $(map sql_injectable setThese)
                 val injo : $(map sql_injectable others)

                 val eqs : $(map eq setThese)
             end) = struct

    open M

    val eqs' = @Record.eq eqs fls

    val whoami =
        data <- underlying;
        case data of
            None => return None
          | Some data =>
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

    fun inGroup g =
        u <- whoami;
        case u of
            None => return False
          | Some u =>
            oneRowE1 (SELECT ({variantToExp g})
                      FROM users
                      WHERE users.{name} = {[u]})

    fun requireGroup g =
        b <- inGroup g;
        if b then
            return ()
        else
            error <xml>Access denied</xml>

end
