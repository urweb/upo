open Bootstrap3

type tag (p :: (Type * Type)) =
     {Fresh : string -> transaction p.2,
      FromDb : p.1 -> transaction p.2,
      Render : p.2 -> xbody,
      Create : p.2 -> transaction (time * string * p.1),
      Save : p.1 -> p.2 -> transaction (time * string * p.1),
      Delete : p.1 -> transaction unit,
      Eq : eq p.1}

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
         Tags : $(map tag tags)}

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
                -> sql_query1 [] [] [] [] ([When = time] ++ map option (key ++ otherKeys))) r
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
                      val eqs : $(map (fn p => eq p.1) key)
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
      {Fresh = fn tmS =>
         w <- @Monad.mapR _ [Widget.t'] [fn p => id * p.2] (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                                                               id <- fresh;
                                                               w <- @Widget.create w;
                                                               return (id, w)) (@Folder.concat ! fl flO) ws;
         tm <- source tmS;
         tmi <- fresh;
         return {Widgets = w, Time = tm, TimeId = tmi},
       FromDb = let
           fun lookup r =
               oneRow1 (SELECT tab.{when}, tab.{{map fst other}}
                        FROM tab
                        WHERE {@@Sql.easy_where [#Tab] [map fst key] [_] [_] [_] [_] ! !
                          (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                          (@Folder.mp fl) r})
       in
        fn r =>
           r' <- rpc (lookup (r -- #When));
           w <- @Monad.mapR2 _ [Widget.t'] [fst] [fn p => id * p.2]
               (fn [nm ::_] [p ::_] (w : Widget.t' p) (x : p.1) =>
                   id <- fresh;
                   w <- @Widget.initialize w x;
                   return (id, w)) (@Folder.concat ! fl flO) ws ((r -- #When) ++ (r' -- when));
           tm <- source (show r'.when);
           tmi <- fresh;
           return {Widgets = w, Time = tm, TimeId = tmi}
       end,
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
           fun create r =
               @Sql.easy_insert
                ({when = _} ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] (@Folder.concat ! fl flO)) (inj ++ injO))
                (@Folder.cons [when] [_] ! (@Folder.mp (@Folder.concat ! fl flO))) tab r
       in
           fn self =>
              tm <- get self.Time;
              case read tm of
                  None => error <xml>Invalid time!</xml>
                | Some tm =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (create ({when = tm} ++ r));
                  return (tm, timef "%l:%M" tm, r --- (map fst other) ++ {When = tm})
       end,
       Save = let
           fun save k r =
               @@Sql.easy_update' [map fst key] [[when = _] ++ map fst other] [_] !
                 (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                 ({when = _} ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] flO) injO)
                 (@Folder.mp fl) (@Folder.cons [when] [_] ! (@Folder.mp flO)) tab (k -- #When) r
       in
           fn k self =>
              tm <- get self.Time;
              case read tm of
                  None => error <xml>Invalid time!</xml>
                | Some tm =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (save k ({when = tm} ++ r));
                  return (tm, timef "%l:%M" tm, r --- (map fst other) ++ {When = tm})
       end,
       Delete = let
           fun delete k =
               dml (DELETE FROM tab
                    WHERE {@@Sql.easy_where [#T] [map fst key] [_] [_] [_] [_] ! !
                          (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                          (@Folder.mp fl) (k -- #When)})
       in
           fn k =>
              rpc (delete k)
       end,
       Eq = @Record.eq ({When = _} ++ eqs) (@Folder.cons [#When] [_] ! (@Folder.mp fl))}
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

fun dateify tm = Datetime.toTime (Datetime.fromTime tm -- #Hour -- #Minute -- #Second
                                                    ++ {Hour = 0, Minute = 0, Second = 0})

val oneDay = 24 * 60 * 60

fun extractWeek [a] (ls : list (time * string * string * list a)) : option {ThisWeek : list (time * string * string * list a), LaterWeeks : list (time * string * string * list a)} =
    case ls of
        [] => None
      | (tm, tmS, _, _) :: _ =>
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
                                ((tm', timef "%b %e" tm', show (dateify tm'), []) :: ls'', left)
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
                                    ((tm', timef "%b %e" tm', show (dateify tm'), []) :: ls'', left)
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
style fields

style buttn

functor Make(M : sig
                 con keys :: {Type}
                 con tags :: {(Type * Type)}
                 constraint [When] ~ keys
                 val t : t keys tags
                 val sh : $(map (fn p => show p.1) tags)
                 val fl : folder tags
                 val labels : $(map (fn _ => string) tags)
             end) = struct
open M
type input = _

type add = time * string * variant (map fst tags)
type del = variant (map fst tags)

datatype action =
         Add of add
       | Del of del
       | Mod of del * add

table listeners : { Kind : serialized (variant (map (fn _ => unit) tags)),
                    Channel : channel action }

type a = channel action * source (list (time * string * string * list (time * string * variant (map fst tags))))
(* We render the times to strings server-side to avoid time-zone hang-ups. *)

fun ui {FromDay = from, ToDay = to} : Ui.t a =
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
                                        [] => loop nextDay items ((day, timef "%b %e" day, show (dateify day), List.rev acc') :: acc)
                                      | item :: items' =>
                                        if item.1 < from || item.1 > to then
                                            loop' items' acc'
                                        else if item.1 < nextDay then
                                            loop' items' ((item.1, timef "%l:%M" item.1, item.2) :: acc')
                                        else
                                            loop nextDay items ((day, timef "%b %e" day, show (dateify day), List.rev acc') :: acc)
                            in
                                loop' items []
                            end
                    end
            in
                items <- items' @t;
                ds <- source (loop from items []);
                ch <- channel;
                @Variant.withAll fl (fn k => dml (INSERT INTO listeners(Kind, Channel)
                                                  VALUES ({[serialize k]}, {[ch]})));
                return (ch, ds)
            end

        fun onload (ch, ds) =
            let
                fun doAdd (tm, tmS, r) =
                    days <- get ds;
                    set ds (List.mp (fn (tm', tmS', longS, items) =>
                                        (tm', tmS', longS,
                                         if tm' <= tm && tm < addSeconds tm' oneDay then
                                             List.sort (fn (tm1, _, _) (tm2, _, _) => tm1 > tm2) ((tm, tmS, r) :: items)
                                         else
                                             items)) days)

                fun doDel r =
                    days <- get ds;
                    set ds (List.mp (fn (tm', tmS', longS, items) =>
                                        (tm', tmS', longS,
                                         List.filter (fn (_, _, r') =>
                                                         not (@eq (@@Variant.eq [map fst tags]
                                                                     (@mp [tag] [fn p => eq p.1]
                                                                       (fn [p] (t : tag p) => t.Eq)
                                                                       fl t.Tags) (@Folder.mp fl)) r' r)) items)) days)

                fun loop () =
                    msg <- recv ch;
                    (case msg of
                         Add c => doAdd c
                       | Del c => doDel c
                       | Mod (c1, c2) => doDel c1; doAdd c2);
                    loop ()
            in
                spawn (loop ())
            end

        fun notify (k : variant (map fst tags)) (act : action) =
            queryI1 (SELECT listeners.Channel
                     FROM listeners
                     WHERE listeners.Kind = {[serialize (@Variant.erase (@Folder.mp fl) k)]})
                    (fn r => send r.Channel act)

        fun render ctx (_, ds) =
            let
                fun render' days =
                    case extractWeek days of
                        None => <xml/>
                      | Some r => <xml>
                        <tr>{List.mapX (fn (_, tmS, longS, items) => <xml><td>
                          <span class={date}>{[tmS]}</span>
                          {Ui.modalButton ctx (CLASS "buttn btn btn-default btn-xs glyphicon glyphicon-plus-sign")
                                          <xml/>
                                          (widgets <- @Monad.mapR _ [tag] [snd]
                                                       (fn [nm ::_] [p ::_] (r : tag p) => r.Fresh longS)
                                                       fl t.Tags;
                                           (whichTab : source int) <- source (@fold [fn _ => int]
                                                                (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (n : int) => n + 1)
                                                                (-1) fl);
                                           return (Ui.modal (wt <- get whichTab;
                                                             (@foldR2 [tag] [snd]
                                                               [fn r => o :: {Type} -> [o ~ r] => (variant (map fst r ++ o) -> variant (map fst tags))
                                                                        -> int * transaction unit]
                                                               (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (x : p.2)
                                                                            (acc : o :: {Type} -> [o ~ r] => (variant (map fst r ++ o) -> variant (map fst tags))
                                                                                   -> int * transaction unit)
                                                                            [o ::_] [o ~ [nm = p] ++ r]
                                                                            (maker : variant (map fst ([nm = p] ++ r) ++ o) -> variant (map fst tags)) =>
                                                                   let
                                                                       val (n, xact) = acc [[nm = p.1] ++ o] maker
                                                                   in
                                                                       (n+1,
                                                                        if n = wt then
                                                                            (tm, tmS, k) <- t.Create x;
                                                                            rpc (notify (maker (make [nm] k)) (Add (tm, tmS, maker (make [nm] k))))
                                                                        else
                                                                            xact)
                                                                   end) (fn [o ::_] [o ~ []] _ => (0, alert "Impossible tab!")) fl t.Tags widgets [[]] ! (fn x => x)).2)
                                                            <xml>Adding an item to the calendar</xml>
                                                            <xml>
                                                              <ul class="bs3-nav nav-tabs">
                                                                {(@foldR [fn _ => string] [fn _ => int * xbody]
                                                                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (n, b) =>
                                                                       (n+1,
                                                                        <xml>
                                                                          <li dynClass={wt <- signal whichTab;
                                                                                        return (if n = wt then
                                                                                                    CLASS "bs3-active"
                                                                                                else
                                                                                                    CLASS "")}><a onclick={fn _ => set whichTab n}>{[lab]}</a></li>
                                                                          {b}
                                                                        </xml>)) (0, <xml/>) fl labels).2}
                                                              </ul>
                                                              
                                                              <div class={fields}>
                                                                <dyn signal={wt <- signal whichTab;
                                                                             return (@foldR2 [tag] [snd] [fn _ => int * xbody]
                                                                                      (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (x : p.2) (n, b) =>
                                                                                          (n+1,
                                                                                           if n = wt then
                                                                                               t.Render x
                                                                                           else
                                                                                               b)) (0, <xml/>) fl t.Tags widgets).2}/>
                                                              </div>
                                                            </xml>
                                                            <xml>Add to Calendar</xml>))}
                          {List.mapX (fn (tm, tmS, d) => <xml><div class={item}><span class={time}>{[tmS]}</span>:
                            <span class={item}>{[@Record.select [fn p => show p.1] [fst] fl
                                                  (fn [p] (s : show p.1) => @show s) sh d]}
                              <span class={buttn}>
                              {Ui.modalButton ctx (CLASS "btn btn-default btn-xs glyphicon glyphicon-edit")
                                              <xml/>
                                              (@Record.select' [tag] [fst] [fst] fl
                                              (fn [p] (t : tag p) (maker : p.1 -> variant (map fst tags)) (x : p.1) =>
                                                 widget <- t.FromDb x;
                                                 return (Ui.modal ((tm, tmS, k2) <- t.Save x widget;
                                                                   rpc (notify d (Mod (maker x, (tm, tmS, maker k2)))))
                                                                  <xml>Editing a calendar item ({[@Record.select [fn _ => string] [fst] fl (fn [p] (lab : string) _ => lab) labels d]})</xml>
                                                                  <xml>
                                                                    <div class={fields}>
                                                                      {t.Render widget}
                                                                    </div>
                                                                  </xml>
                                                                  <xml>Save Calendar Entry</xml>))
                                              t.Tags d)}
                              {Ui.modalButton ctx (CLASS "btn btn-default btn-xs glyphicon glyphicon-trash")
                                              <xml/>
                                              (return (@Record.select2 [tag] [fn p => show p.1] [fst] fl
                                               (fn [p] (t : tag p) (_ : show p.1) (x : p.1) =>
                                               Ui.modal (t.Delete x;
                                                         rpc (notify d (Del d)))
                                               <xml>Deleting a calendar item ({[@Record.select [fn _ => string] [fst] fl (fn [p] (lab : string) _ => lab) labels d]})</xml>
                                               <xml>Are you sure you want to delete <b>{[x]}</b>?</xml>
                                               <xml>Yes, Delete It</xml>)
                                               t.Tags sh d))}
                              </span>
                            </span></div></xml>) items}
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
                <dyn signal={days <- signal ds;
                             return (render' days)}/>
              </table></xml>
            end
    in
        {Create = create,
         Onload = onload,
         Render = render}
    end
end
