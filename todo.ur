open Bootstrap3

type tag (t :: Type) =
     {Label : string,
      Eq : eq t,
      Show : show t}

type t (keys :: {Type}) (tags :: {Type}) =
     [[Assignee, Due, Done, Kind] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([Assignee = option string, Due = option time, Done = option bool, Kind = string] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
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
                        val flO : folder other
                        val inj : $(map sql_injectable_prim key)
                        val injO : $(map sql_injectable_prim other)
                        table items : (key ++ [due = time] ++ other)
                        table done : (key ++ [user = string] ++ dother)
                        table users : ([ukey = string] ++ uother)
                        val eqs : $(map eq key)
                        val title : string
                        val sh : show $key
                        val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
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
            (primo : $(map sql_injectable_prim otherKeys)) =>
            sql_forget_tables (sql_query1 [[Items, Users]]
                                          {Distinct = False,
                                           From = (FROM items, users),
                                           Where = sql_exp_weaken ucond,
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Items = ([], _), Users = ([], _)]],
                                           SelectExps = {Assignee = (sql_window (sql_nullable (SQL users.{ukey})) : expw (option string)),
                                                         Due = (sql_window (sql_nullable (SQL items.{due})) : expw (option time)),
                                                         Done = (sql_window (SQL (SELECT COUNT( * ) > 0
                                                                                  FROM done
                                                                                  WHERE done.{user} = users.{ukey}
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
      {Eq = @@Record.eq [key] eqs fl,
       Show = sh,
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
                   (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) =>
                   let
                       val q1 = t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo)
                       val q2 = t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo)
                   in
                       sql_relop sql_union False q1 q2
                   end,
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] r =>
                     case t1.Extract [otherTags ++ tags2] ! (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ tags1] ! (r --- map option keys1)
                       | x => x,
        Tags = t1.Tags ++ t2.Tags}

fun items [keys ::: {Type}] [tags ::: {Type}] [[Assignee, Due, Done, Kind] ~ keys] (t : t keys tags)
    : transaction (list {Assignee : option string,
                         Due : option time,
                         Done : option bool,
                         Tag : variant tags}) =
    List.mapQuery ({{{t.Query [[]] ! _ {}}}}
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
              <th>Due</th>
              <th>Assigned to</th>
              <th>Done?</th>
              <th>Task</th>
            </tr>

            {List.mapX (fn r => <xml><tr>
              <td>{[r.Due]}</td>
              <td>{[r.Assignee]}</td>
              <td>{[r.Done]}</td>
              <td>{[@Record.select [tag] [ident] fl
                     (fn [p] (t : tag p) => @show t.Show) t.Tags r.Tag]}</td>
            </tr></xml>) a}
          </table>
        </xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render}
    end

    structure OneUser = struct
        type a = unit
        type input = _

        val create = return ()

        fun onload _ = return ()

        fun render _ _ = <xml></xml>

        fun ui _ = {Create = create,
                    Onload = onload,
                    Render = render}
    end

end
