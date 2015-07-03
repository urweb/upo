open Bootstrap3

type t (keys :: {Type}) (tags :: {(Type * Type)}) =
     [[When] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([When = time] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                 -> sql_query1 [] [] [] [] ([When = time] ++ map option (keys ++ otherKeys)),
         Extract : otherTags :: {Type}
                   -> [otherTags ~ tags]
                   => time -> $(map option keys) -> option (variant (map fst tags ++ otherTags)),
         Tags : $(map (fn p => {Fresh : time -> transaction p.2,
                                Render : p.2 -> xbody,
                                Create : p.2 -> transaction unit}) tags)}

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

fun create [tag :: Name] [key] [widget] [[When] ~ key] (fl : folder key)
           (f : otherKeys :: {Type}
                -> [([When = time] ++ key) ~ otherKeys]
                => folder otherKeys
                -> $(map sql_injectable_prim otherKeys)
                -> sql_query1 [] [] [] [] ([When = time] ++ map option (key ++ otherKeys)))
           (r : {Fresh : time -> transaction widget,
                 Render : widget -> xbody,
                 Create : widget -> transaction unit})
    : t key [tag = ($([When = time] ++ key), widget)] =
    fn [[When] ~ key] =>
    {Query = f,
     Extract = fn [otherTags ::_] [otherTags ~ [tag = _]]
                  tm r => case @unopt fl r of
                              None => None
                            | Some v => Some (make [tag] ({When = tm} ++ v)),
     Tags = {tag = r}}

functor FromTable(M : sig
                      con tag :: Name
                      con key :: {(Type * Type)} (* Each 2nd component is a type of GUI widget private state. *)
                      con when :: Name
                      con other :: {(Type * Type)}
                      con us :: {{Unit}}
                      constraint key ~ other
                      constraint [when] ~ (key ++ other)
                      constraint [When] ~ (key ++ other)
                      val fl : folder key
                      val flO : folder other
                      val inj : $(map (fn p => sql_injectable_prim p.1) key)
                      val injO : $(map (fn p => sql_injectable_prim p.1) other)
                      val ws : $(map Widget.t' (key ++ other))
                      val tab : sql_table (map fst (key ++ other) ++ [when = time]) us
                      val labels : $([when = string] ++ map (fn _ => string) (key ++ other))
                  end) = struct
    open M

    type private = {Widgets : $(map (fn p => id * p.2) (key ++ other)),
                    Time : source string,
                    TimeId : id}

    val cal : t (map fst key) [tag = ($([When = time] ++ map fst key), private)] =
        @@create [tag] [map fst key] [private] ! (@Folder.mp fl)
        (fn [otherKeys :: {Type}] [([When = time] ++ map fst key) ~ otherKeys]
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
                                                                       [Tab = [when = _] ++ map fst (key ++ other)]
                                                                       [Tab = [when = _] ++ map fst (key ++ other)] []
                                                                       time)}
                                                            ++ @map2 [sql_injectable_prim]
                                                            [sql_exp [Tab = [when = _] ++ map fst (key ++ other)]
                                                                     [Tab = [when = _] ++ map fst (key ++ other)] []]
                                                            [fn t => sql_expw
                                                                         [Tab = [when = _] ++ map fst (key ++ other)]
                                                                         [Tab = [when = _] ++ map fst (key ++ other)] []
                                                                         (option t)]
                                                            (fn [t] prim e => sql_window (@sql_nullable prim e)
                                                                              : sql_expw [Tab = [when = time] ++ map fst (key ++ other)]
                                                                                         [Tab = [when = time] ++ map fst (key ++ other)] [] (option t))
                                                            (@@Folder.mp [fst] [_] fl) inj
                                                            (@@Sql.some_fields [#Tab] [map fst key]
                                                               [[when = _] ++ map fst other]
                                                               [[]] [[Tab = [when = _] ++ map fst (key ++ other)]] [[]] ! ! (@Folder.mp fl))
                                                            ++ @mp [sql_injectable_prim]
                                                            [fn t => sql_expw [Tab = [when = time] ++ map fst (key ++ other)]
                                                                              [Tab = [when = time] ++ map fst (key ++ other)] [] (option t)]
                                                            (fn [t] (pr : sql_injectable_prim t) =>
                                                                sql_window (SQL NULL)
                                                                : sql_expw [Tab = [when = time] ++ map fst (key ++ other)]
                                                                           [Tab = [when = time] ++ map fst (key ++ other)] [] (option t))
                                                            flo primo}))
      {Fresh = fn tm =>
         w <- @Monad.mapR _ [Widget.t'] [fn p => id * p.2] (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                                                               id <- fresh;
                                                               w <- @Widget.create w;
                                                               return (id, w)) (@Folder.concat ! fl flO) ws;
         tm <- source (show tm);
         tmi <- fresh;
         return {Widgets = w, Time = tm, TimeId = tmi},
       Render = fn self => <xml>
         <div class="form-group">
           <label class="control-label" for={self.TimeId}>{[labels.when]}</label>
           <ctextbox id={self.TimeId} source={self.Time}/>
         </div>
         {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
           (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, x) => <xml>
             <div class="form-group">
               <label class="control-label" for={id}>{[lab]}</label>
               <span id={id}>{@Widget.asWidget w x}</span>
             </div>
           </xml>)
           (@Folder.concat ! fl flO) (labels -- when) ws self.Widgets}
       </xml>,
       Create = let
           fun create r = @Sql.easy_insert
                           ({when = _} ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] (@Folder.concat ! fl flO)) (inj ++ injO))
                           (@Folder.cons [when] [_] ! (@Folder.mp (@Folder.concat ! fl flO))) tab r
       in
           fn self =>
              tm <- get self.Time;
              case read tm of
                  None => alert "Invalid time!"
                | Some tm =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (create ({when = tm} ++ r))
       end}
end

fun compose [keys1] [keys2] [tags1] [tags2] [keys1 ~ keys2] [tags1 ~ tags2]
            (fl1 : folder keys1) (fl2 : folder keys2)
            (prim1 : $(map sql_injectable_prim keys1))
            (prim2 : $(map sql_injectable_prim keys2))
            (t1 : t keys1 tags1) (t2 : t keys2 tags2) : t (keys1 ++ keys2) (tags1 ++ tags2) =
    fn [[When] ~ (keys1 ++ keys2)] =>
       {Query = fn [otherKeys :: {Type}]
                   [([When = time] ++ keys1 ++ keys2) ~ otherKeys]
                   (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) =>
                   sql_relop sql_union False
                   (t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo))
                   (t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo)),
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] tm r =>
                     case t1.Extract [otherTags ++ map fst tags2] ! tm (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ map fst tags1] ! tm (r --- map option keys1)
                       | x => x,
        Tags = t1.Tags ++ t2.Tags}

fun items [keys ::: {Type}] [tags ::: {(Type * Type)}] [[When] ~ keys] (t : t keys tags) =
    List.mapQuery ({{{t.Query [[]] ! _ {}}}}
                   ORDER BY When)
    (fn r => case t.Extract [[]] ! r.When (r -- #When) of
                 Some x => x
               | None => error <xml>Calendar: impossible: query result doesn't correspond to a tag</xml>)

fun items' [keys ::: {Type}] [tags ::: {(Type * Type)}] [[When] ~ keys] (t : t keys tags) =
    List.mapQuery ({{{t.Query [[]] ! _ {}}}}
                   ORDER BY When)
    (fn r => case t.Extract [[]] ! r.When (r -- #When) of
                 Some x => (r.When, x)
               | None => error <xml>Calendar: impossible: query result doesn't correspond to a tag</xml>)

con calendar (tags :: {(Type * Type)}) = list (time * string * list (string * variant (map fst tags)))
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

style date
style calendar
style time
style item
style add

val calendar [keys] [tags] [[When] ~ keys] (t : t keys tags) (sh : $(map (fn p => show p.1) tags)) (fl : folder tags) {Labels = labels, FromDay = from, ToDay = to} : Ui.t (calendar tags) =
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
                                        if item.1 < from || item.1 > to then
                                            loop' items' acc'
                                        else if item.1 < nextDay then
                                            loop' items' ((timef "%l:%M" item.1, item.2) :: acc')
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
                          <span class={date}>{[tmS]}</span>
                          {Ui.modalButton ctx (CLASS "add btn btn-default btn-xs glyphicon glyphicon-plus-sign")
                                          <xml/>
                                          (return (Ui.modal (alert "Good work!")
                                                            <xml>Adding an item to the calendar</xml>
                                                            <xml>
                                                              <ul class="bs3-nav nav-tabs">
                                                                {@mapX [fn _ => string] [body]
                                                                  (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (lab : string) => <xml>
                                                                    <li><a>{[lab]}</a></li>
                                                                  </xml>) fl labels}
                                                              </ul>
                                                            </xml>
                                                            <xml>Add to Calendar</xml>))}
                          {List.mapX (fn (tmS, d) => <xml><div><span class={time}>{[tmS]}</span>:
                            <span class={item}>{[@Record.select [fn p => show p.1] [fst] fl
                               (fn [p] (s : show p.1) => @show s) sh d]}</span></div></xml>) items}
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
