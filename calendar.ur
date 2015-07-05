open Bootstrap3

datatype level = Forbidden | Read | Write

fun levelToInt lv =
    case lv of
        Forbidden => 0
      | Read => 1
      | Write => 2

val level_eq = mkEq (fn x y => levelToInt x = levelToInt y)
val level_ord = mkOrd {Lt = fn x y => levelToInt x < levelToInt y,
                       Le = fn x y => levelToInt x <= levelToInt y}

type tag (p :: (Type * Type)) =
     {Label : string,
      Fresh : string -> transaction p.2,
      FromDb : p.1 -> transaction p.2,
      Render : p.2 -> xbody,
      Create : p.2 -> transaction (list (time * string * string) * p.1),
      Save : p.1 -> p.2 -> transaction (list (time * string * string) * p.1),
      Delete : p.1 -> transaction unit,
      Eq : eq p.1,
      Show : show p.1,
      Display : p.1 -> transaction xbody,
      MaySee : transaction bool,
      MayModify : transaction bool}

type t (keys :: {Type}) (tags :: {(Type * Type)}) =
     [[When, Kind] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([When = time, Kind = string] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                 -> transaction (sql_query1 [] [] [] [] ([When = time, Kind = string] ++ map option (keys ++ otherKeys))),
         Extract : otherTags :: {Type}
                   -> [otherTags ~ tags]
                   => $(map option keys) -> option (variant (map fst tags ++ otherTags)),
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

fun create [tag :: Name] [key] [widget] [[When, Kind] ~ key] (fl : folder key)
           (f : otherKeys :: {Type}
                -> [([When = time, Kind = string] ++ key) ~ otherKeys]
                => folder otherKeys
                -> $(map sql_injectable_prim otherKeys)
                -> transaction (sql_query1 [] [] [] [] ([When = time, Kind = string] ++ map option (key ++ otherKeys)))) r
    : t key [tag = ($key, widget)] =
    fn [[When, Kind] ~ key] =>
    {Query = f,
     Extract = fn [otherTags ::_] [otherTags ~ [tag = _]] r =>
                  case @unopt fl r of
                      None => None
                    | Some v => Some (make [tag] v),
     Tags = {tag = r}}

functor FromTable(M : sig
                      con tag :: Name
                      con key :: {(Type * Type)} (* Each 2nd component is a type of GUI widget private state. *)
                      con times :: {Unit}
                      con other :: {(Type * Type)}
                      con us :: {{Unit}}
                      constraint key ~ times
                      constraint key ~ other
                      constraint times ~ other
                      constraint [When, Kind] ~ key
                      val fl : folder key
                      val flO : folder other
                      val flT : folder times
                      val inj : $(map (fn p => sql_injectable_prim p.1) key)
                      val injO : $(map (fn p => sql_injectable_prim p.1) other)
                      val ws : $(map Widget.t' (key ++ other))
                      val tab : sql_table (map fst (key ++ other) ++ mapU time times) us
                      val labels : $(map (fn _ => string) (key ++ other) ++ mapU string times)
                      val eqs : $(map (fn p => eq p.1) key)
                      val title : string
                      val display : $(map fst key) -> transaction xbody
                      val auth : transaction level
                      val kinds : $(mapU string times)
                      val sh : show $(map fst key)
                  end) = struct
    open M

    type private1 = {Widgets : $(map (fn p => id * p.2) (key ++ other)),
                     Times : $(map (fn _ => id * source string) times)}
    con private = ($(map fst key), private1)

    val cal : t (map fst key) [tag = private] =
        @@create [tag] [map fst key] [private1] ! (@Folder.mp fl)
        (fn [otherKeys :: {Type}] [([When = time, Kind = string] ++ map fst key) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) =>
            lv <- auth;
            ao <- return (@fold [fn r => $(mapU string r) -> o :: {Unit} -> [o ~ r] => [o ~ key] => [o ~ other] => [r ~ key] => [r ~ other]
                              => sql_table (map fst (key ++ other) ++ mapU time (r ++ o)) us
                              -> option (sql_query1 [] [] [] [] ([When = time, Kind = string]
                                                                     ++ map (fn p => option p.1) key ++ map option otherKeys))]
                     (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r]
                                  (acc : $(mapU string r) -> o :: {Unit} -> [o ~ r] => [o ~ key] => [o ~ other] => [r ~ key] => [r ~ other]
                                   => sql_table (map fst (key ++ other) ++ mapU time (r ++ o)) us
                                   -> option (sql_query1 [] [] [] [] ([When = time, Kind = string]
                                                                          ++ map (fn p => option p.1) key ++ map option otherKeys)))
                                  (ks : $(mapU string ([nm] ++ r))) [o ::_] [o ~ [nm] ++ r] [o ~ key] [o ~ other] [[nm] ++ r ~ key] [[nm] ++ r ~ other]
                                  (tab : sql_table (map fst (key ++ other) ++ mapU time ([nm] ++ r ++ o)) us) =>
                         let
                             val q =
                                 sql_forget_tables (sql_query1 [[Tab]]
                                                               {Distinct = False,
                                                                From = (FROM tab),
                                                                Where = (WHERE {[lv >= Read]}),
                                                                GroupBy = sql_subset_all [_],
                                                                Having = (WHERE TRUE),
                                                                SelectFields = sql_subset [[Tab = ([], _)]],
                                                                SelectExps = {When = (sql_window (SQL tab.{nm})
                                                                                      : sql_expw
                                                                                            [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                            [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)] []
                                                                                            time),
                                                                              Kind = (sql_window (SQL {[ks.nm]})
                                                                                      : sql_expw
                                                                                            [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                            [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)] []
                                                                                            string)}
                                                                             ++ @map2 [sql_injectable_prim]
                                                                             [sql_exp [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                      [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)] []]
                                                                             [fn t => sql_expw
                                                                                          [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                          [Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)] []
                                                                                          (option t)]
                                                                             (fn [t] prim e => sql_window (@sql_nullable prim e)
                                                                                               : sql_expw [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                                          [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)] [] (option t))
                                                                             (@@Folder.mp [fst] [_] fl) inj
                                                                             (@@Sql.some_fields [#Tab] [map fst key]
                                                                                [[nm = _] ++ map fst other ++ mapU time (r ++ o)]
                                                                                [[]] [[Tab = [nm = _] ++ map fst (key ++ other) ++ mapU time (r ++ o)]] [[]] ! ! (@Folder.mp fl))
                                                                             ++ @mp [sql_injectable_prim]
                                                                             [fn t => sql_expw [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                               [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)] [] (option t)]
                                                                             (fn [t] (pr : sql_injectable_prim t) =>
                                                                                 sql_window (SQL NULL)
                                                                                 : sql_expw [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)]
                                                                                            [Tab = [nm = time] ++ map fst (key ++ other) ++ mapU time (r ++ o)] [] (option t))
                                                                             flo primo})
                         in
                             Some (case acc (ks -- nm) [[nm] ++ o] tab of
                                       None => q
                                     | Some acc' => sql_relop sql_union False q acc')
                         end)
                     (fn _ [o ::_] [o ~ []] [o ~ key] [o ~ other] [[] ~ key] [[] ~ other] _ => None) flT kinds [[]] ! ! ! ! ! tab);
            case ao of
                None => error <xml>Calendar.FromTable: empty times</xml>
              | Some a => return a)
      {Fresh = fn tmS =>
         w <- @Monad.mapR _ [Widget.t'] [fn p => id * p.2] (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                                                               id <- fresh;
                                                               w <- @Widget.create w;
                                                               return (id, w)) (@Folder.concat ! fl flO) ws;
         tms <- @Monad.mapR0 _ [fn _ => id * source string]
                 (fn [nm ::_] [u ::_] =>
                     id <- fresh;
                     s <- source tmS;
                     return (id, s)) flT;
         return {Widgets = w, Times = tms},
       FromDb = let
           fun lookup k =
               lv <- auth;
               if lv >= Read then
                   oneRow1 (SELECT tab.{{mapU time times}}, tab.{{map fst other}}
                            FROM tab
                            WHERE {@@Sql.easy_where [#Tab] [map fst key] [_] [_] [_] [_] ! !
                              (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                              (@Folder.mp fl) k})
               else
                   error <xml>Not authorized</xml>
       in
        fn r =>
           r' <- rpc (lookup r);
           w <- @Monad.mapR2 _ [Widget.t'] [fst] [fn p => id * p.2]
               (fn [nm ::_] [p ::_] (w : Widget.t' p) (x : p.1) =>
                   id <- fresh;
                   w <- @Widget.initialize w x;
                   return (id, w)) (@Folder.concat ! fl flO) ws (r ++ (r' --- mapU time times));
           tms <- @Monad.mapR _ [fn _ => time] [fn _ => id * source string]
                 (fn [nm ::_] [u ::_] (tm : time) =>
                     id <- fresh;
                     s <- source (show tm);
                     return (id, s)) flT (r' --- _);
           return {Widgets = w, Times = tms}
       end,
       Render = fn self => <xml>
         {@mapX2 [fn _ => string] [fn _ => id * source string] [body]
           (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (lab : string) (id, s) => <xml>
             <div class="form-group">
               <label class="control-label" for={id}>{[lab]}</label>
               <ctextbox id={id} class="form-control" source={s}/>
             </div>
           </xml>)
           flT (labels --- map (fn _ => string) (key ++ other)) self.Times}
         {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
           (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, x) => <xml>
             <div class="form-group">
               <label class="control-label" for={id}>{[lab]}</label>
               <span id={id}>{@Widget.asWidget w x}</span>
             </div>
           </xml>)
           (@Folder.concat ! fl flO) (labels --- mapU string times) ws self.Widgets}
       </xml>,
       Create = let
           fun create r =
               lv <- auth;
               if lv >= Write then
                   @Sql.easy_insert
                    (@map0 [fn _ => sql_injectable time] (fn [u ::_] => _ : sql_injectable time) flT
                      ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] (@Folder.concat ! fl flO)) (inj ++ injO))
                    (@Folder.concat ! (@Folder.mp flT) (@Folder.mp (@Folder.concat ! fl flO))) tab r
               else
                   error <xml>Not authorized</xml>
       in
           fn self =>
              tms <- @Monad.foldR2 _ [fn _ => string] [fn _ => id * source string]
                      [fn r => option ($(mapU time r) * list (time * string * string))]
                      (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] k (_, s) acc =>
                          tmS <- get s;
                          return (case (read tmS, acc) of
                                      (Some tm, Some (r, l)) => Some ({nm = tm} ++ r, (tm, timef "%l:%M" tm, k) :: l)
                                    | _ => None))
                      (Some ({}, [])) flT kinds self.Times;
              case tms of
                  None => error <xml>Invalid time!</xml>
                | Some (tms, tml) =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (create (tms ++ r));
                  return (tml, r --- map fst other)
       end,
       Save = let
           fun save k r =
               lv <- auth;
               if lv >= Write then
                   @@Sql.easy_update' [map fst key] [mapU time times ++ map fst other] [_] !
                     (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                     (@map0 [fn _ => sql_injectable time] (fn [u ::_] => _ : sql_injectable time) flT
                       ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] flO) injO)
                     (@Folder.mp fl) (@Folder.concat ! (@Folder.mp flT) (@Folder.mp flO)) tab k r
               else
                   error <xml>Not authorized</xml>
       in
           fn k self =>
              tms <- @Monad.foldR2 _ [fn _ => string] [fn _ => id * source string]
                      [fn r => option ($(mapU time r) * list (time * string * string))]
                      (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] k (_, s) acc =>
                          tmS <- get s;
                          return (case (read tmS, acc) of
                                      (Some tm, Some (r, l)) => Some ({nm = tm} ++ r, (tm, timef "%l:%M" tm, k) :: l)
                                    | _ => None))
                      (Some ({}, [])) flT kinds self.Times;
              case tms of
                  None => error <xml>Invalid time!</xml>
                | Some (tms, tml) =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (save k (tms ++ r));
                  return (tml, r --- map fst other)
       end,
       Delete = let
           fun delete k =
               lv <- auth;
               if lv >= Write then
                   dml (DELETE FROM tab
                               WHERE {@@Sql.easy_where [#T] [map fst key] [_] [_] [_] [_] ! !
                                 (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst] [_] fl) inj)
                                 (@Folder.mp fl) k})
               else
                   error <xml>Not authorized</xml>
       in
           fn k =>
              rpc (delete k)
       end,
       Eq = @@Record.eq [map fst key] eqs (@Folder.mp fl),
       Show = sh,
       Label = title,
       Display = display,
       MaySee =
         lv <- auth;
         return (lv >= Read),
       MayModify =
         lv <- auth;
         return (lv >= Write)
      }
end

fun compose [keys1] [keys2] [tags1] [tags2] [keys1 ~ keys2] [tags1 ~ tags2]
            (fl1 : folder keys1) (fl2 : folder keys2)
            (prim1 : $(map sql_injectable_prim keys1))
            (prim2 : $(map sql_injectable_prim keys2))
            (t1 : t keys1 tags1) (t2 : t keys2 tags2) : t (keys1 ++ keys2) (tags1 ++ tags2) =
    fn [[When, Kind] ~ (keys1 ++ keys2)] =>
       {Query = fn [otherKeys :: {Type}]
                   [([When = time, Kind = string] ++ keys1 ++ keys2) ~ otherKeys]
                   (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) =>
                   q1 <- t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo);
                   q2 <- t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo);
                   return (sql_relop sql_union False q1 q2),
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] r =>
                     case t1.Extract [otherTags ++ map fst tags2] ! (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ map fst tags1] ! (r --- map option keys1)
                       | x => x,
        Tags = t1.Tags ++ t2.Tags}

fun items [keys ::: {Type}] [tags ::: {(Type * Type)}] [[When, Kind] ~ keys] (t : t keys tags)
    : transaction (list (time * string * variant (map fst tags))) =
    q <- t.Query [[]] ! _ {};
    List.mapQuery ({{{q}}}
                   ORDER BY When)
    (fn r => case t.Extract [[]] ! (r -- #When -- #Kind) of
                 Some x => (r.When, r.Kind, x)
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
                 constraint [When, Kind] ~ keys
                 val t : t keys tags
                 val fl : folder tags
             end) = struct
open M
type input = _

type add = list (time * string * string) * variant (map fst tags)
type del = variant (map fst tags)

datatype action =
         Add of add
       | Del of del
       | Mod of del * add

table listeners : { Kind : serialized (variant (map (fn _ => unit) tags)),
                    Channel : channel action }

type a = channel action
         * source (list (time * string * string * list (time * string * string * variant (map fst tags))))
           (* We render the times to strings server-side to avoid time-zone hang-ups. *)
         * $(map (fn _ => bool) tags) (* Allowed to modify this kind of entry? *)

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
                                      | (tm, k, item) :: items' =>
                                        if tm < from || tm > to then
                                            loop' items' acc'
                                        else if tm < nextDay then
                                            loop' items' ((tm, timef "%l:%M" tm, k, item) :: acc')
                                        else
                                            loop nextDay items ((day, timef "%b %e" day, show (dateify day), List.rev acc') :: acc)
                            in
                                loop' items []
                            end
                    end
            in
                items <- items @t;
                ds <- source (loop from items []);
                ch <- channel;
                mm <- @Monad.mapR _ [tag] [fn _ => bool]
                       (fn [nm ::_] [p ::_] (t : tag p) => t.MayModify)
                       fl t.Tags;
                @Variant.withAll fl (fn k =>
                                        b <- @Record.select [tag] [fn _ => unit] fl
                                            (fn [p] (t : tag p) _ => t.MaySee)
                                            t.Tags k;
                                        if b then
                                            dml (INSERT INTO listeners(Kind, Channel)
                                                 VALUES ({[serialize k]}, {[ch]}))
                                        else
                                            return ());
                return (ch, ds, mm)
            end

        fun onload (ch, ds, _) =
            let
                fun doAdd (tml, r) =
                    days <- get ds;
                    set ds (List.foldl (fn (tm, tmS, k) days =>
                                           List.mp (fn (tm', tmS', longS, items) =>
                                                       (tm', tmS', longS,
                                                        if tm' <= tm && tm < addSeconds tm' oneDay then
                                                            List.sort (fn (tm1, _, _, _) (tm2, _, _, _) => tm1 > tm2)
                                                                      ((tm, tmS, k, r) ::items)
                                                        else
                                                            items)) days) days tml)

                fun doDel r =
                    days <- get ds;
                    set ds (List.mp (fn (tm', tmS', longS, items) =>
                                        (tm', tmS', longS,
                                         List.filter (fn (_, _, _, r') =>
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

        fun render ctx (_, ds, mm) =
            let
                fun render' days =
                    case extractWeek days of
                        None => <xml/>
                      | Some r => <xml>
                        <tr>{List.mapX (fn (_, tmS, longS, items) => <xml><td>
                          <span class={date}>{[tmS]}</span>
                          {if @foldR [fn _ => bool] [fn _ => bool]
                               (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (b : bool) (acc : bool) => not b && acc)
                               True fl mm then
                               <xml/>
                           else
                               Ui.modalButton ctx (CLASS "buttn btn btn-default btn-xs glyphicon glyphicon-plus-sign")
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
                                                                 (tml, k) <- t.Create x;
                                                                 rpc (notify (maker (make [nm] k)) (Add (tml, maker (make [nm] k))))
                                                             else
                                                                 xact)
                                                        end) (fn [o ::_] [o ~ []] _ => (0, alert "Impossible tab!")) fl t.Tags widgets [[]] ! (fn x => x)).2)
                                                 <xml>Adding an item to the calendar</xml>
                                                 <xml>
                                                   <ul class="bs3-nav nav-tabs">
                                                     {(@foldR [tag] [fn _ => int * xbody]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (n, b) =>
                                                            (n+1,
                                                             <xml>
                                                               <li dynClass={wt <- signal whichTab;
                                                                             return (if n = wt then
                                                                                         CLASS "bs3-active"
                                                                                     else
                                                                                         CLASS "")}><a onclick={fn _ => set whichTab n}>{[t.Label]}</a></li>
                                                               {b}
                                                             </xml>)) (0, <xml/>) fl t.Tags).2}
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
                          {List.mapX (fn (tm, tmS, k, d) =>
                                         let
                                             val maymod = @Record.select [fn _ => bool] [fst] fl
                                                           (fn [p] (b : bool) _ => b)
                                                           mm d
                                         in
                                             <xml><div class={item}><span class={time}>{[tmS]}</span>:
                                               <span class={item}>
                                                 {Ui.modalButton ctx (CLASS "btn btn-link")
                                                                 <xml>{[@Record.select [tag] [fst] fl
                                                                         (fn [p] (t : tag p) => @show t.Show) t.Tags d]} {[k]}</xml>
                                                                 (@Record.select [tag] [fst] fl
                                                                   (fn [p] (t : tag p) (x : p.1) =>
                                                                       xm <- t.Display x;
                                                                       return (Ui.simpleModal xm <xml>Close</xml>))
                                                                   t.Tags d)}
                                                   {if not maymod then
                                                        <xml/>
                                                    else <xml>
                                                      <span class={buttn}>
                                                        {Ui.modalButton ctx (CLASS "btn btn-default btn-xs glyphicon glyphicon-edit")
                                                        <xml/>
                                                        (@Record.select' [tag] [fst] [fst] fl
                                                        (fn [p] (t : tag p) (maker : p.1 -> variant (map fst tags)) (x : p.1) =>
                                                            widget <- t.FromDb x;
                                                            return (Ui.modal ((tml, k2) <- t.Save x widget;
                                                                              rpc (notify d (Mod (maker x, (tml, maker k2)))))
                                                                             <xml>Editing a calendar item ({[t.Label]})</xml>
                                                                             <xml>
                                                                               <div class={fields}>
                                                                                 {t.Render widget}
                                                                               </div>
                                                                             </xml>
                                                                             <xml>Save Calendar Entry</xml>))
                                                        t.Tags d)}
                                                        {Ui.modalButton ctx (CLASS "btn btn-default btn-xs glyphicon glyphicon-trash")
                                                                        <xml/>
                                                                        (return (@Record.select [tag] [fst] fl
                                                                        (fn [p] (t : tag p) (x : p.1) =>
                                                                        Ui.modal (t.Delete x;
                                                                        rpc (notify d (Del d)))
                                                                        <xml>Deleting a calendar item ({[t.Label]})</xml>
                                                                        <xml>Are you sure you want to delete <b>{[@show t.Show x]}</b>?</xml>
                                                                        <xml>Yes, Delete It</xml>)
                                                                        t.Tags d))}
                                                        </span>
                                                        </xml>}
                                                   </span></div></xml>
                                         end) items}
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
