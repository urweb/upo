open Bootstrap3

type tag (t :: Type) =
     {Label : string,
      Render : t -> option string -> xbody}

type t (keys :: {Type}) (tags :: {Type}) =
     [[Assignee, Due, Done, Kind] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                    -> option string (* username to restrict to *)
                 -> sql_query1 [] [] [] [] ([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ map option (keys ++ otherKeys)),
         Extract : otherTags :: {Type}
                   -> [otherTags ~ tags]
                   => $(map option keys) -> option (variant (tags ++ otherTags)),
         Tags : $(map tag tags)}

fun create [tag :: Name] [key] [[Assignee, Due, Done, Kind] ~ key] (fl : folder key)
           (f : otherKeys :: {Type}
                -> [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key) ~ otherKeys]
                => folder otherKeys
                -> $(map sql_injectable_prim otherKeys)
                   -> option string
                -> sql_query1 [] [] [] [] ([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ map option (key ++ otherKeys))) r
    : t key [tag = $key] =
    fn [[Assignee, Due, Done, Kind] ~ key] =>
    {Query = f,
     Extract = fn [otherTags ::_] [otherTags ~ [tag = _]] r =>
                  case @Sql.unopt fl r of
                      None => None
                    | Some v => Some (make [tag] v),
     Tags = {tag = r}}

functor WithDueDate(M : sig
                        con tag :: Name
                        con key :: {Type}
                        con due :: Name
                        con other :: {Type}
                        con user :: Name
                        con dother :: {Type}
                        con ukey :: Name
                        con uother :: {Type}
                        constraint key ~ other
                        constraint key ~ dother
                        constraint [due] ~ (key ++ other)
                        constraint [user] ~ (key ++ dother)
                        constraint [ukey] ~ uother
                        constraint [Assignee, Due, Done, Kind] ~ key
                        val fl : folder key
                        val inj : $(map sql_injectable_prim key)
                        table items : (key ++ [due = time] ++ other)
                        table done : (key ++ [user = string] ++ dother)
                        table users : ([ukey = string] ++ uother)
                        val title : string
                        val render : $key -> string -> xbody
                        val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                        val allowAnyUser : bool
                    end) = struct
    open M

    con private = $key

    type window_env = [Items = key ++ [due = time] ++ other,
                       Users = [ukey = string] ++ uother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t key [tag = private] =
        @@create [tag] [key] ! fl
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Items, Users]]
                                          {Distinct = False,
                                           From = (FROM items, users),
                                           Where = case uo of
                                                       None => sql_exp_weaken ucond
                                                     | Some u => (WHERE users.{ukey} = {[u]}
                                                                    AND {sql_exp_weaken ucond}),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Users = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (sql_nullable (SQL users.{ukey})) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL items.{due})) : expw (option time)),
                                                         Done = (sql_window (SQL (SELECT COUNT( * ) > 0
                                                                                  FROM done
                                                                                  WHERE {if allowAnyUser then (WHERE TRUE) else (WHERE done.{user} = users.{ukey})}
                                                                                    AND {@@Sql.easy_join [#Done] [#Items] [key] [_] [_] [_] [_] [_] ! ! ! ! fl}))
                                                                 : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string)}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t))
                                                           fl inj
                                                           (@@Sql.some_fields [#Items] [key]
                                                              [[due = _] ++ other]
                                                              [[Users = [ukey = string] ++ uother]] [window_env] [[]] ! ! fl)
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r u =>
                   case u of
                       None => error <xml>Todo: impossible lack of user</xml>
                     | Some u => render r u,
       Label = title}
end

functor WithForeignDueDate(M : sig
                               con tag :: Name
                               con key :: {Type}
                               con subkey :: {Type}
                               con due :: Name
                               con other :: {Type}
                               con pother :: {Type}
                               con user :: Name
                               con dother :: {Type}
                               con ukey :: Name
                               con uother :: {Type}
                               constraint key ~ other
                               constraint key ~ dother
                               constraint key ~ pother
                               constraint (key ++ other) ~ subkey
                               constraint [due] ~ (key ++ pother)
                               constraint [user] ~ (key ++ dother)
                               constraint [user] ~ other
                               constraint [ukey] ~ uother
                               constraint subkey ~ dother
                               constraint subkey ~ [user]
                               constraint [Assignee, Due, Done, Kind] ~ (key ++ subkey)
                               constraint [user] ~ (key ++ subkey)
                               val fl : folder key
                               val sfl : folder subkey
                               val inj : $(map sql_injectable_prim key)
                               val sinj : $(map sql_injectable_prim subkey)

                               table items : (key ++ [user = string] ++ subkey ++ other)
                               (* The set of items that must be done *)
                               table parent : (key ++ [due = time] ++ pother)
                               (* Look here for due dates. *)
                               table done : (key ++ subkey ++ [user = string] ++ dother)
                               (* Recording which users have done which items *)
                               table users : ([ukey = string] ++ uother)
                               (* Full set of users *)
                               val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                               (* Condition to narrow down to the ones who need to do these items *)

                               val title : string
                               val render : $(key ++ subkey) -> string (* username *) -> xbody
                    end) = struct
    open M

    con private = $(key ++ subkey)

    type window_env = [Items = key ++ [user = string] ++ subkey ++ other,
                       Parent = key ++ [due = time] ++ pother,
                       Users = [ukey = string] ++ uother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t (key ++ subkey) [tag = private] =
        @@create [tag] [key ++ subkey] ! (@Folder.concat ! fl sfl)
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key ++ subkey) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Items, Parent, Users]]
                                          {Distinct = False,
                                           From = (FROM items, parent, users),
                                           Where = let
                                               val base = (WHERE {sql_exp_weaken ucond}
                                                             AND {@@Sql.easy_join [#Parent] [#Items] [key] [_] [_] [_] [_] [_] ! ! ! ! fl}
                                                             AND users.{ukey} = items.{user})
                                           in
                                               case uo of
                                                   None => base
                                                 | Some u => (WHERE users.{ukey} = {[u]}
                                                                AND {base})
                                           end,
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Parent = ([], _), Users = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (sql_nullable (SQL users.{ukey})) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL parent.{due})) : expw (option time)),
                                                         Done = (sql_window (SQL (SELECT COUNT( * ) > 0
                                                                                  FROM done
                                                                                  WHERE done.{user} = users.{ukey}
                                                                                    AND {@@Sql.easy_join [#Done] [#Items] [key ++ subkey] [_] [_] [_] [_] [_] ! ! ! ! (@Folder.concat ! fl sfl)}))
                                                                 : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string)}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t))
                                                           (@Folder.concat ! fl sfl) (inj ++ sinj)
                                                           (@@Sql.some_fields [#Items] [key ++ subkey] [[user = _] ++ other]
                                                              [[Parent = key ++ [due = time] ++ pother,
                                                                Users = [ukey = string] ++ uother]] [window_env] [[]] ! ! (@Folder.concat ! fl sfl))
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r u =>
                   case u of
                       None => error <xml>Todo: impossible lack of user [3]</xml>
                     | Some u => render r u,
       Label = title}
end

functor WithCompletionFlag(M : sig
                               con tag :: Name
                               con key :: {Type}
                               con subkey :: {Type}
                               con done :: Name
                               con other :: {Type}
                               con user :: Name
                               con aother :: {Type}
                               constraint key ~ subkey
                               constraint (key ++ subkey) ~ other
                               constraint key ~ aother
                               constraint [done] ~ (key ++ subkey ++ other)
                               constraint [user] ~ (key ++ aother)
                               constraint [Assignee, Due, Done, Kind] ~ (key ++ subkey)
                               val fl : folder key
                               val sfl : folder subkey
                               val inj : $(map sql_injectable_prim key)
                               val sinj : $(map sql_injectable_prim subkey)

                               table items : (key ++ subkey ++ [done = bool] ++ other)
                               (* The set of items that must be done *)
                               table assignments : (key ++ [user = option string] ++ aother)
                               (* Recording who is responsible for which items *)

                               val title : string
                               val render : $(key ++ subkey) -> string (* username *) -> xbody
                    end) = struct
    open M

    con private = $(key ++ subkey)

    type window_env = [Items = key ++ subkey ++ [done = bool] ++ other,
                       Assignments = key ++ [user = option string] ++ aother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t (key ++ subkey) [tag = private] =
        @@create [tag] [key ++ subkey] ! (@Folder.concat ! fl sfl)
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key ++ subkey) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Items, Assignments]]
                                          {Distinct = False,
                                           From = (FROM items JOIN assignments ON
                                                     {@@Sql.easy_join [#Items] [#Assignments] [key] [_] [_] [_] [_] [_] ! ! ! ! fl}
                                                     AND NOT items.{done}),
                                           Where = case uo of
                                                       None => (WHERE NOT (assignments.{user} IS NULL))
                                                     | Some u => (WHERE assignments.{user} = {[Some u]}),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Assignments = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (SQL assignments.{user}) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL CURRENT_TIMESTAMP)) : expw (option time)),
                                                         Done = (sql_window (sql_nullable (SQL FALSE)) : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string)}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t))
                                                           (@Folder.concat ! fl sfl) (inj ++ sinj)
                                                           (@@Sql.some_fields [#Items] [key ++ subkey]
                                                              [[done = _] ++ other]
                                                              [[Assignments = key ++ [user = option string] ++ aother]] [window_env] [[]] ! ! (@Folder.concat ! fl sfl))
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r u =>
                   case u of
                       None => error <xml>Todo: impossible lack of user</xml>
                     | Some u => render r u,
       Label = title}
end

functor WithCompletionFlagAndDueDate(M : sig
                                         con tag :: Name
                                         con key :: {Type}
                                         con subkey :: {Type}
                                         con due :: Name
                                         con done :: Name
                                         con other :: {Type}
                                         con user :: Name
                                         con aother :: {Type}
                                         constraint key ~ subkey
                                         constraint (key ++ subkey) ~ other
                                         constraint key ~ aother
                                         constraint [due] ~ [done]
                                         constraint [due, done] ~ (key ++ subkey ++ other)
                                         constraint [user] ~ (key ++ aother)
                                         constraint [Assignee, Due, Done, Kind] ~ (key ++ subkey)
                                         val fl : folder key
                                         val sfl : folder subkey
                                         val inj : $(map sql_injectable_prim key)
                                         val sinj : $(map sql_injectable_prim subkey)

                                         table items : (key ++ subkey ++ [due = time, done = bool] ++ other)
                                         (* The set of items that must be done *)
                                         table assignments : (key ++ [user = option string] ++ aother)
                                         (* Recording who is responsible for which items *)

                                         val title : string
                                         val render : $(key ++ subkey) -> string (* username *) -> xbody
                    end) = struct
    open M

    con private = $(key ++ subkey)

    type window_env = [Items = key ++ subkey ++ [due = time, done = bool] ++ other,
                       Assignments = key ++ [user = option string] ++ aother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t (key ++ subkey) [tag = private] =
        @@create [tag] [key ++ subkey] ! (@Folder.concat ! fl sfl)
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key ++ subkey) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Items, Assignments]]
                                          {Distinct = False,
                                           From = (FROM items JOIN assignments ON
                                                     {@@Sql.easy_join [#Items] [#Assignments] [key] [_] [_] [_] [_] [_] ! ! ! ! fl}),
                                           Where = case uo of
                                                       None => (WHERE NOT (assignments.{user} IS NULL))
                                                     | Some u => (WHERE assignments.{user} = {[Some u]}),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Assignments = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (SQL assignments.{user}) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL items.{due})) : expw (option time)),
                                                         Done = (sql_window (sql_nullable (SQL items.{done})) : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string)}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t))
                                                           (@Folder.concat ! fl sfl) (inj ++ sinj)
                                                           (@@Sql.some_fields [#Items] [key ++ subkey]
                                                              [[due = _, done = _] ++ other]
                                                              [[Assignments = key ++ [user = option string] ++ aother]] [window_env] [[]] ! ! (@Folder.concat ! fl sfl))
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r u =>
                   case u of
                       None => error <xml>Todo: impossible lack of user</xml>
                     | Some u => render r u,
       Label = title}
end

functor Happenings(M : sig
                       con tag :: Name
                       con key :: {Type}
                       con when :: Name
                       con other :: {Type}
                       con ukey :: Name
                       con uother :: {Type}
                       constraint key ~ other
                       constraint [when] ~ (key ++ other)
                       constraint [ukey] ~ uother
                       constraint [Assignee, Due, Done, Kind] ~ key
                       val fl : folder key
                       val inj : $(map sql_injectable_prim key)

                       table items : (key ++ [when = time] ++ other)
                       (* The set of items that must be done *)
                       table users : ([ukey = string] ++ uother)
                       (* Full set of users *)
                       val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                       (* Condition to narrow down to the ones who need to do these items *)

                       val title : string
                       val render : $key -> xbody
                   end) = struct
    open M

    con private = $key

    type window_env = [Items = key ++ [when = time] ++ other,
                       Users = [ukey = string] ++ uother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t key [tag = private] =
        @@create [tag] [key] ! fl
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ key) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Items, Users]]
                                          {Distinct = False,
                                           From = (FROM items, users),
                                           Where = case uo of
                                                       None => sql_exp_weaken ucond
                                                     | Some u => (WHERE users.{ukey} = {[u]}
                                                                    AND {sql_exp_weaken ucond}),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Users = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (SQL NULL) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL items.{when})) : expw (option time)),
                                                         Done = (sql_window (SQL NULL) : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string)}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t))
                                                           fl inj
                                                           (@@Sql.some_fields [#Items] [key]
                                                              [[when = _] ++ other]
                                                              [[Users = [ukey = string] ++ uother]] [window_env] [[]] ! ! fl)
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r _ => render r,
       Label = title}
end

functor Grading(M : sig
                    con tag :: Name
                    con akey :: {Type}
                    con due :: Name
                    con aother :: {Type}
                    con ukey :: Name
                    con uother :: {Type}
                    con guser :: Name
                    con gother :: {Type}
                    constraint akey ~ aother
                    constraint [due] ~ (akey ++ aother)
                    constraint [ukey] ~ uother
                    constraint akey ~ gother
                    constraint [guser] ~ (akey ++ gother)
                    constraint [guser] ~ akey
                    constraint [Assignee, Due, Done, Kind] ~ ([guser = string] ++ akey)
                    val fl : folder akey
                    val inj : $(map sql_injectable_prim akey)

                    table assignments : (akey ++ [due = time] ++ aother)
                    (* The set of assignments to be graded *)
                    val acond : sql_exp [Assignments = akey ++ [due = time] ++ aother] [] [] bool
                    (* Condition to narrow down to the ones ready for grading *)
                    table users : ([ukey = string] ++ uother)
                    (* Full set of users *)
                    val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                    (* Condition to narrow down to the ones who get graded *)
                    table grades : ([guser = string] ++ akey ++ gother)
                    (* Recorded grades; if missing, generate a todo. *)
                    val gcond : sql_exp [Graders = [ukey = string] ++ uother] [] [] bool
                    (* Which users are responsible for grading? *)

                    val title : string
                    val render : $([guser = string] ++ akey) -> string (* username *) -> xbody
                end) = struct
    open M

    con private = $([guser = string] ++ akey)

    type window_env = [Assignments = akey ++ [due = time] ++ aother,
                       Users = [ukey = string] ++ uother,
                       Graders = [ukey = string] ++ uother]

    con expw = sql_expw window_env window_env []
    con exp = sql_exp window_env window_env []

    val todo : t ([guser = string] ++ akey) [tag = private] =
        @@create [tag] [[guser = string] ++ akey] ! (@Folder.cons [guser] [_] ! fl)
        (fn [otherKeys :: {Type}] [([Assignee = option string, Due = option time, Done = option bool, Kind = string, guser = string] ++ akey) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) uo =>
            sql_forget_tables (sql_query1 [[Assignments, Users, Graders]]
                                          {Distinct = False,
                                           From = (FROM assignments JOIN users ON {sql_exp_weaken ucond}
                                                     JOIN users AS Graders ON {sql_exp_weaken gcond}
                                                       AND {sql_exp_weaken acond}),
                                           Where = case uo of
                                                       None => (WHERE TRUE)
                                                     | Some u => (WHERE graders.{ukey} = {[u]}),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Assignments = ([], _), Users = ([], _), Graders = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (sql_nullable (SQL graders.{ukey})) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL assignments.{due})) : expw (option time)),
                                                         Done = (sql_window (SQL (SELECT COUNT( * ) > 0
                                                                                  FROM grades
                                                                                  WHERE grades.{guser} = users.{ukey}
                                                                                    AND {@@Sql.easy_join [#Grades] [#Assignments] [akey] [_] [_] [_] [_] [_] ! ! ! ! fl})) : expw (option bool)),
                                                         Kind = (sql_window (SQL {[title]}) : expw string),
                                                         guser = (sql_window (sql_nullable (SQL users.{ukey})) : expw (option string))}
                                                         ++ @map2 [sql_injectable_prim] [exp] [fn t => expw (option t)]
                                                           (fn [t] prim e => sql_window (@sql_nullable prim e) : expw (option t)) fl inj
                                                           (@@Sql.some_fields [#Assignments] [akey]
                                                              [[due = _] ++ aother]
                                                              [[Users = [ukey = string] ++ uother, Graders = [ukey = string] ++ uother]] [window_env] [[]] ! ! fl)
                                                         ++ @mp [sql_injectable_prim]
                                                           [fn t => expw (option t)]
                                                           (fn [t] (pr : sql_injectable_prim t) =>
                                                               sql_window (SQL NULL) : expw (option t))
                                                           flo primo}))
      {Render = fn r u =>
                   case u of
                       None => error <xml>Todo: impossible lack of user</xml>
                     | Some u => render r u,
       Label = title}
end

fun compose [keys1] [keys2] [tags1] [tags2] [keys1 ~ keys2] [tags1 ~ tags2]
            (fl1 : folder keys1) (fl2 : folder keys2)
            (prim1 : $(map sql_injectable_prim keys1))
            (prim2 : $(map sql_injectable_prim keys2))
            (t1 : t keys1 tags1) (t2 : t keys2 tags2) : t (keys1 ++ keys2) (tags1 ++ tags2) =
    fn [[Assignee, Due, Done, Kind] ~ (keys1 ++ keys2)] =>
       {Query = fn [otherKeys :: {Type}]
                   [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ keys1 ++ keys2) ~ otherKeys]
                   (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) uo =>
                   let
                       val q1 = t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo) uo
                       val q2 = t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo) uo
                   in
                       sql_relop sql_union False q1 q2
                   end,
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] r =>
                     case t1.Extract [otherTags ++ tags2] ! (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ tags1] ! (r --- map option keys1)
                       | x => x,
        Tags = t1.Tags ++ t2.Tags}

type todo tags = {Assignee : option string,
                  Due : option time,
                  Done : option bool,
                  Tag : variant tags}

fun items [keys ::: {Type}] [tags ::: {Type}] [[Assignee, Due, Done, Kind] ~ keys] (t : t keys tags)
    : transaction (list (todo tags)) =
    List.mapQuery ({{{t.Query [[]] ! _ {} None}}}
                   ORDER BY Due)
    (fn r => case t.Extract [[]] ! (r -- #Assignee -- #Due -- #Done -- #Kind) of
                 Some x => {Assignee = r.Assignee,
                            Due = r.Due,
                            Done = r.Done,
                            Tag = x}
               | None => error <xml>Todo: impossible: query result doesn't correspond to a tag</xml>)

fun itemsU [keys ::: {Type}] [tags ::: {Type}] [[Assignee, Due, Done, Kind] ~ keys] (t : t keys tags) (u : string)
    : transaction (list (todo tags)) =
    List.mapQuery ({{{t.Query [[]] ! _ {} (Some u)}}}
                   ORDER BY Due)
    (fn r => case t.Extract [[]] ! (r -- #Assignee -- #Due -- #Done -- #Kind) of
                 Some x => {Assignee = r.Assignee,
                            Due = r.Due,
                            Done = r.Done,
                            Tag = x}
               | None => error <xml>Todo: impossible: query result doesn't correspond to a tag</xml>)


functor Make(M : sig
                 con keys :: {Type}
                 con tags :: {Type}
                 constraint [Assignee, Due, Done, Kind] ~ keys
                 val t : t keys tags
                 val fl : folder tags
             end) = struct
    open M

    structure AllUsers = struct
        type a = _

        val create = items @t

        fun onload _ = return ()

        fun render _ a = <xml>
          <table class="bs3-table table-striped">
            <tr>
              <th>Task</th>
              <th>Due</th>
              <th>Assigned to</th>
              <th>Done?</th>
            </tr>

            {List.mapX (fn r : todo tags => <xml><tr>
              <td>{@Record.select [tag] [ident] fl
                    (fn [p] (t : tag p) (x : p) => t.Render x r.Assignee) t.Tags r.Tag}</td>
              <td>{[r.Due]}</td>
              <td>{[r.Assignee]}</td>
              <td>{case r.Done of
                       Some True => <xml><span class="glyphicon glyphicon-ok"/></xml>
                     | _ => <xml></xml>}</td>
            </tr></xml>) a}
          </table>
        </xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render}
    end

    structure OneUser = struct
        type a = _
        type input = _

        val create = itemsU @t

        fun onload _ = return ()

        fun render _ a = <xml>
          <table class="bs3-table table-striped">
            <tr>
              <th>Task</th>
              <th>Due</th>
              <th>Done?</th>
            </tr>

            {List.mapX (fn r : todo tags => <xml><tr>
              <td>{let
                       val x = @Record.select [tag] [ident] fl
                                (fn [p] (t : tag p) (x : p) => t.Render x r.Assignee) t.Tags r.Tag
                   in
                       if Option.isSome r.Assignee && r.Done <> Some True then
                           <xml><b>{x}</b></xml>
                       else
                           x
                   end}</td>
              <td>{if Option.isSome r.Assignee && r.Done <> Some True then
                       <xml><b>{[r.Due]}</b></xml>
                   else
                       <xml>{[r.Due]}</xml>}</td>
              <td>{case r.Done of
                       Some True => <xml><span class="glyphicon glyphicon-ok"/></xml>
                     | _ => <xml></xml>}</td>
            </tr></xml>) a}
          </table>
        </xml>

        fun ui u = {Create = create u,
                    Onload = onload,
                    Render = render}
    end

end
