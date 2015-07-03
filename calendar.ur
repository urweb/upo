type t (keys :: {Type}) (tags :: {Type}) =
     [[When] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([When = time] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                 -> sql_query1 [] [] [] [] ([When = time] ++ map option (keys ++ otherKeys)),
         Extract : otherTags :: {Type}
                   -> [otherTags ~ tags]
                   => time -> $(map option keys) -> option (variant (tags ++ otherTags))}

fun unopt [fs ::: {Type}] (fl : folder fs) (r : $(map option fs)) : option $fs =
    @foldR [option] [fn r => option $r]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (x : option t) (acc : option $r) =>
        case x of
            None => None
          | Some x =>
            case acc of
                None => None
              | Some acc => Some ({nm = x} ++ acc))
    (Some {}) fl r

fun create [tag :: Name] [key] [[When] ~ key] (fl : folder key)
           (f : otherKeys :: {Type}
                -> [([When = time] ++ key) ~ otherKeys]
                => folder otherKeys
                -> $(map sql_injectable_prim otherKeys)
                -> sql_query1 [] [] [] [] ([When = time] ++ map option (key ++ otherKeys)))
    : t key [tag = $([When = time] ++ key)] =
    fn [[When] ~ key] =>
    {Query = f,
     Extract = fn [otherTags ::_] [otherTags ~ [tag = _]]
                  tm r => case @unopt fl r of
                              None => None
                            | Some v => Some (make [tag] ({When = tm} ++ v))}

fun fromTable [tag :: Name] [key :: {Type}] [when :: Name] [other ::: {Type}] [us ::: {{Unit}}]
              [key ~ other] [[when] ~ (key ++ other)] [[When] ~ (key ++ other)]
              (fl : folder key) (prim : $(map sql_injectable_prim key))
              (tab : sql_table (key ++ other ++ [when = time]) us)
    : t key [tag = $([When = time] ++ key)] =
      @create [tag] ! fl (fn [otherKeys :: {Type}] [([When = time] ++ key) ~ otherKeys]
                             (flo : folder otherKeys)
                             (primo : $(map sql_injectable_prim otherKeys)) =>
                             sql_forget_tables (sql_query1 [[Tab]]
                             {Distinct = False,
                              From = (FROM tab),
                              Where = (WHERE TRUE),
                              GroupBy = sql_subset_all [_],
                              Having = (WHERE TRUE),
                              SelectFields = sql_subset [[Tab = ([], _)]],
                              SelectExps = {When = (sql_window (SQL tab.{when})
                                                    : sql_expw
                                                          [Tab = [when = _] ++ key ++ other]
                                                          [Tab = [when = _] ++ key ++ other] []
                                                          time)}
                                               ++ @map2 [sql_injectable_prim]
                                               [sql_exp [Tab = [when = _] ++ key ++ other]
                                                        [Tab = [when = _] ++ key ++ other] []]
                                               [fn t => sql_expw
                                                            [Tab = [when = _] ++ key ++ other]
                                                            [Tab = [when = _] ++ key ++ other] []
                                                            (option t)]
                                               (fn [t] prim e => sql_window (@sql_nullable prim e)
                                                            : sql_expw [Tab = [when = time] ++ key ++ other] [Tab = [when = time] ++ key ++ other] [] (option t))
                                               fl prim
                                               (@@Sql.some_fields [#Tab] [key]
                                                  [[when = _] ++ other]
                                                  [[]] [[Tab = [when = _] ++ key ++ other]] [[]] ! ! fl)
                                               ++ @mp [sql_injectable_prim]
                                               [fn t => sql_expw [Tab = [when = time] ++ key ++ other] [Tab = [when = time] ++ key ++ other] [] (option t)]
                                               (fn [t] (pr : sql_injectable_prim t) =>
                                                   sql_window (SQL NULL)
                                                   : sql_expw [Tab = [when = time] ++ key ++ other] [Tab = [when = time] ++ key ++ other] [] (option t))
                                               flo primo}))

fun compose [keys1] [keys2] [tags1] [tags2] [keys1 ~ keys2] [tags1 ~ tags2]
            (fl1 : folder keys1) (fl2 : folder keys2)
            (prim1 : $(map sql_injectable_prim keys1))
            (prim2 : $(map sql_injectable_prim keys2))
            (t1 : t keys1 tags1) (t2 : t keys2 tags2) : t (keys1 ++ keys2) (tags1 ++ tags2) =
    fn [[When] ~ (keys1 ++ keys2)] =>
       {Query = fn [otherKeys :: {Type}]
                   [([When = time] ++ (keys1 ++ keys2)) ~ otherKeys]
                   (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) =>
                   sql_relop sql_union False
                   (t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo))
                   (t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo)),
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] tm r =>
                     case t1.Extract [otherTags ++ tags2] ! tm (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ tags1] ! tm (r --- map option keys1)
                       | x => x}

fun items [keys ::: {Type}] [tags ::: {Type}] [[When] ~ keys] (t : t keys tags) =
    List.mapQuery ({{{t.Query [[]] ! _ {}}}}
                   ORDER BY When)
    (fn r => case t.Extract [[]] ! r.When (r -- #When) of
                 Some x => x
               | None => error <xml>Calendar: impossible: query result doesn't correspond to a tag</xml>)

fun items' [keys ::: {Type}] [tags ::: {Type}] [[When] ~ keys] (t : t keys tags) =
    List.mapQuery ({{{t.Query [[]] ! _ {}}}}
                   ORDER BY When)
    (fn r => case t.Extract [[]] ! r.When (r -- #When) of
                 Some x => (r.When, x)
               | None => error <xml>Calendar: impossible: query result doesn't correspond to a tag</xml>)

con calendar (tags :: {Type}) = list (time * string * list (string * variant tags))
(* We render the times to strings server-side to avoid time-zone hang-ups. *)

fun dateify tm = Datetime.toTime (Datetime.fromTime tm -- #Hour -- #Minute -- #Second
                                                    ++ {Hour = 0, Minute = 0, Second = 0})

val oneDay = 24 * 60 * 60

fun extractWeek [a] (ls : list (time * string * list a)) : option {ThisWeek : list (time * string * list a), LaterWeeks : list (time * string * list a)} =
    case ls of
        [] => None
      | (tm, tmS, _) :: _ =>
        let
            val offsetStart = addSeconds tm (Datetime.dayOfWeekToInt (Datetime.dayOfWeek (Datetime.fromTime tm)) * (-oneDay))
            (* The beginning of this week *)

            fun getWeek daynum ls =
                if daynum >= 7 then
                    ([], ls)
                else
                    let
                        val dow = Datetime.intToDayOfWeek daynum
                        fun more ls' = getWeek (daynum + 1) ls'
                    in
                        case ls of
                            [] =>
                            let
                                val (ls'', left) = more ls
                                val tm' = addSeconds offsetStart (daynum * oneDay)
                            in
                                ((tm', timef "%b %e" tm', []) :: ls'', left)
                            end
                          | ent :: ls' =>
                            if Datetime.dayOfWeek (Datetime.fromTime ent.1) = dow then
                                let
                                    val (ls'', left) = more ls'
                                in
                                    (ent :: ls'', left)
                                end
                            else
                                let
                                    val (ls'', left) = more ls
                                    val tm' = addSeconds offsetStart (daynum * oneDay)
                                in
                                    ((tm', timef "%b %e" tm', []) :: ls'', left)
                                end
                    end

            val (ls'', left) = getWeek 0 ls
        in
            Some {ThisWeek = ls'', LaterWeeks = left}
        end

style calendar

val calendar [keys] [tags] [[When] ~ keys] (t : t keys tags) (sh : $(map show tags)) (fl : folder tags) {FromDay = from, ToDay = to} : Ui.t (calendar tags) =
    let
        val from = dateify from
        val to = dateify to

        val create =
            let
                fun loop day items acc =
                    let
                        val nextDay = addSeconds day oneDay
                    in
                        if day > to then
                            List.rev acc
                        else
                            let
                                fun loop' items acc' =
                                    case items of
                                        [] => loop nextDay items ((day, timef "%b %e" day, List.rev acc') :: acc)
                                      | item :: items' =>
                                        if item.1 < nextDay then
                                            loop' items' ((timef "%I:%M" item.1, item.2) :: acc')
                                        else
                                            loop nextDay items ((day, timef "%b %e" day, List.rev acc') :: acc)
                            in
                                loop' items []
                            end
                    end
            in
                items <- items' @t;
                return (loop from items [])
            end

        fun onload _ = return ()

        fun render ctx days =
            let
                fun render' days =
                    case extractWeek days of
                        None => <xml/>
                      | Some r => <xml>
                        <tr>{List.mapX (fn (_, tmS, items) => <xml><td>
                          {[tmS]}
                          {List.mapX (fn (tmS, d) => <xml><div>{[tmS]}: {[@Record.select [show] [ident] fl
                                                                           (fn [t] (_ : show t) (x : t) => show x) sh d]}</div></xml>) items}
                        </td></xml>) r.ThisWeek}</tr>
                        {render' r.LaterWeeks}
                      </xml>
            in
              <xml><table class={calendar}>
                <tr>
                  <th>Sunday</th>
                  <th>Monday</th>
                  <th>Tuesday</th>
                  <th>Wednesday</th>
                  <th>Thursday</th>
                  <th>Friday</th>
                  <th>Saturday</th>
                </tr>
                {render' days}
              </table></xml>
            end
    in
        {Create = create,
         Onload = onload,
         Render = render}
    end
