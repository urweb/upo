open Bootstrap4

datatype level = Forbidden | Read | Write

fun levelToInt lv =
    case lv of
        Forbidden => 0
      | Read => 1
      | Write => 2

val level_eq = mkEq (fn x y => levelToInt x = levelToInt y)
val level_ord = mkOrd {Lt = fn x y => levelToInt x < levelToInt y,
                       Le = fn x y => levelToInt x <= levelToInt y}

type tag (p :: (Type * Type * Type)) =
     {Label : string,
      Configure : transaction p.3,
      Fresh : p.3 -> string -> transaction p.2,
      FromDb : p.3 -> p.1 -> transaction p.2,
      Render : p.2 -> xbody,
      Create : p.2 -> transaction (list (time * option string * string) * p.1),

      Save : p.1 -> p.2 -> string -> time -> transaction (list (time * option string * string) * p.1),
      Delete : p.1 -> string -> time -> transaction unit,
      (* For these two, the [string] names a time kind, asking only to touch entries where that kind
       * is associated with the [time] from another parameter. *)

      Eq : eq p.1,
      Show : show p.1,
      Display : option (Ui.context -> p.1 -> transaction xbody),
      MaySee : transaction bool,
      MayModify : transaction bool,
      ShowTime : bool,
      MultipleKinds : bool}

type t (keys :: {Type}) (tags :: {(Type * Type * Type)}) =
     [[When, Kind, ShowTime] ~ keys]
     => {Query : otherKeys :: {Type}
                 -> [([When = time, Kind = string, ShowTime = bool] ++ keys) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                    -> transaction (option (sql_query1 [] [] [] [] ([When = time, Kind = string, ShowTime = bool] ++ map option (keys ++ otherKeys)))),
         Extract : otherTags :: {Type}
                   -> [otherTags ~ tags]
                   => $(map option keys) -> option (variant (map fst3 tags ++ otherTags)),
         Tags : $(map tag tags)}

fun create [tag :: Name] [key] [widget] [config] [[When, Kind, ShowTime] ~ key] (fl : folder key)
           (f : otherKeys :: {Type}
                -> [([When = time, Kind = string, ShowTime = bool] ++ key) ~ otherKeys]
                => folder otherKeys
                -> $(map sql_injectable_prim otherKeys)
                -> transaction (option (sql_query1 [] [] [] [] ([When = time, Kind = string, ShowTime = bool] ++ map option (key ++ otherKeys))))) r
    : t key [tag = ($key, widget, config)] =
    fn [[When, Kind, ShowTime] ~ key] =>
    {Query = f,
     Extract = fn [otherTags ::_] [otherTags ~ [tag = _]] r =>
                  case @Sql.unopt fl r of
                      None => None
                    | Some v => Some (make [tag] v),
     Tags = {tag = r}}

fun pickFieldFromString [tab :: Name] [t ::: Type] [choosable ::: {Unit}] [others ::: {Type}]
    [choosable ~ others] (fl : folder choosable) (names : $(mapU string choosable)) (name : string)
    : sql_exp [tab = mapU t choosable ++ others] [] [] t =
      let
          val opt = @foldUR [string] [fn ch => others :: {Type} -> [ch ~ others] => option (sql_exp [tab = mapU t ch ++ others] [] [] t)]
                    (fn [nm ::_] [r ::_] [[nm] ~ r] (thisName : string)
                        (acc : others :: {Type} -> [r ~ others] => option (sql_exp [tab = mapU t r ++ others] [] [] t))
                        [others :: {Type}] [([nm] ++ r) ~ others] =>
                        if thisName = name then
                            Some (WHERE {{tab}}.{nm})
                        else
                            acc [[nm = t] ++ others])
                    (fn [others ::_] [[] ~ others] => None)
                    fl names
      in
          case opt [others] of
              None => error <xml>pickFieldFromString: field doesn't exist</xml>
            | Some e => e
      end

functor FromTable(M : sig
                      con tag :: Name
                      con key :: {(Type * Type * Type)}
                      con times :: {Unit}
                      con other :: {(Type * Type * Type)}
                      con us :: {{Unit}}
                      constraint key ~ times
                      constraint key ~ other
                      constraint times ~ other
                      constraint [When, Kind, ShowTime] ~ key
                      val fl : folder key
                      val flO : folder other
                      val flT : folder times
                      val inj : $(map (fn p => sql_injectable_prim p.1) key)
                      val injO : $(map (fn p => sql_injectable p.1) other)
                      val ws : $(map Widget.t' (key ++ other))
                      val tab : sql_table (map fst3 (key ++ other) ++ mapU time times) us
                      val labels : $(map (fn _ => string) (key ++ other) ++ mapU string times)
                      val eqs : $(map (fn p => eq p.1) key)
                      val title : string
                      val display : option (Ui.context -> $(map fst3 key) -> transaction xbody)
                      val auth : transaction level
                      val showTime : bool
                      val kinds : $(mapU string times)
                      val sh : show $(map fst3 key)
                  end) = struct
    open M

    type private1 = {Widgets : $(map (fn p => id * p.2) (key ++ other)),
                     Times : $(map (fn _ => id * source string) times)}
    type config1 = $(map thd3 (key ++ other))
    con private = ($(map fst3 key), private1, config1)

    val cal : t (map fst3 key) [tag = private] =
        @@create [tag] [map fst3 key] [private1] [config1] ! (@Folder.mp fl)
        (fn [otherKeys :: {Type}] [([When = time, Kind = string, ShowTime = bool] ++ map fst3 key) ~ otherKeys]
            (flo : folder otherKeys)
            (primo : $(map sql_injectable_prim otherKeys)) =>
            lv <- auth;
            return (@fold [fn r => $(mapU string r) -> o :: {Unit} -> [o ~ r] => [o ~ key] => [o ~ other] => [r ~ key] => [r ~ other]
                        => sql_table (map fst3 (key ++ other) ++ mapU time (r ++ o)) us
                        -> option (sql_query1 [] [] [] [] ([When = time, Kind = string, ShowTime = bool]
                                                               ++ map (fn p => option p.1) key ++ map option otherKeys))]
               (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r]
                            (acc : $(mapU string r) -> o :: {Unit} -> [o ~ r] => [o ~ key] => [o ~ other] => [r ~ key] => [r ~ other]
                             => sql_table (map fst3 (key ++ other) ++ mapU time (r ++ o)) us
                             -> option (sql_query1 [] [] [] [] ([When = time, Kind = string, ShowTime = bool]
                                                                    ++ map (fn p => option p.1) key ++ map option otherKeys)))
                            (ks : $(mapU string ([nm] ++ r))) [o ::_] [o ~ [nm] ++ r] [o ~ key] [o ~ other] [[nm] ++ r ~ key] [[nm] ++ r ~ other]
                            (tab : sql_table (map fst3 (key ++ other) ++ mapU time ([nm] ++ r ++ o)) us) =>
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
                                                                                      [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                      [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] []
                                                                                      time),
                                                                        Kind = (sql_window (SQL {[ks.nm]})
                                                                                : sql_expw
                                                                                      [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                      [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] []
                                                                                      string),
                                                                       ShowTime = (sql_window (SQL {[showTime]})
                                                                                   : sql_expw
                                                                                         [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                         [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] []
                                                                                         bool)}
                                                                       ++ @map2 [sql_injectable_prim]
                                                                       [sql_exp [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] []]
                                                                       [fn t => sql_expw
                                                                                    [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                    [Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] []
                                                                                    (option t)]
                                                                       (fn [t] prim e => sql_window (@sql_nullable prim e)
                                                                                         : sql_expw [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                                    [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] [] (option t))
                                                                       (@@Folder.mp [fst3] [_] fl) inj
                                                                       (@@Sql.some_fields [#Tab] [map fst3 key]
                                                                          [[nm = _] ++ map fst3 other ++ mapU time (r ++ o)]
                                                                          [[]] [[Tab = [nm = _] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]] [[]] ! ! (@Folder.mp fl))
                                                                       ++ @mp [sql_injectable_prim]
                                                                       [fn t => sql_expw [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                         [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] [] (option t)]
                                                                       (fn [t] (pr : sql_injectable_prim t) =>
                                                                           sql_window (SQL NULL)
                                                                           : sql_expw [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)]
                                                                                      [Tab = [nm = time] ++ map fst3 (key ++ other) ++ mapU time (r ++ o)] [] (option t))
                                                                       flo primo})
                   in
                       Some (case acc (ks -- nm) [[nm] ++ o] tab of
                                 None => q
                               | Some acc' => sql_relop sql_union False q acc')
                   end)
               (fn _ [o ::_] [o ~ []] [o ~ key] [o ~ other] [[] ~ key] [[] ~ other] _ => None) flT kinds [[]] ! ! ! ! ! tab))
      {Configure = @Monad.mapR _ [Widget.t'] [thd3] (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w) (@Folder.concat ! fl flO) ws,
       Fresh = fn cfg tmS =>
         w <- @Monad.mapR2 _ [Widget.t'] [thd3] [fn p => id * p.2] (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) =>
                                                                       id <- fresh;
                                                                       w <- @Widget.create w cfg;
                                                                       return (id, w)) (@Folder.concat ! fl flO) ws cfg;
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
                   oneRow1 (SELECT tab.{{mapU time times}}, tab.{{map fst3 other}}
                            FROM tab
                            WHERE {@@Sql.easy_where [#Tab] [map fst3 key] [_] [_] [_] [_] ! !
                              (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst3] [_] fl) inj)
                              (@Folder.mp fl) k})
               else
                   error <xml>Not authorized</xml>
       in
        fn cfg r =>
           r' <- rpc (lookup r);
           w <- @Monad.mapR3 _ [Widget.t'] [fst3] [thd3] [fn p => id * p.2]
               (fn [nm ::_] [p ::_] (w : Widget.t' p) (x : p.1) (cfg : p.3) =>
                   id <- fresh;
                   w <- @Widget.initialize w cfg x;
                   return (id, w)) (@Folder.concat ! fl flO) ws (r ++ (r' --- mapU time times)) cfg;
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
               {@Widget.asWidget w x (Some id)}
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
                      ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst3] [_] fl) inj ++ injO)
                    (@Folder.concat ! (@Folder.mp flT) (@Folder.mp (@Folder.concat ! fl flO))) tab r
               else
                   error <xml>Not authorized</xml>
       in
           fn self =>
              tms <- @Monad.foldR2 _ [fn _ => string] [fn _ => id * source string]
                      [fn r => option ($(mapU time r) * list (time * option string * string))]
                      (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] k (_, s) acc =>
                          tmS <- get s;
                          return (case (read tmS, acc) of
                                      (Some tm, Some (r, l)) => Some ({nm = tm} ++ r, (tm, if showTime then Some (timef "%l:%M" tm) else None, k) :: l)
                                    | _ => None))
                      (Some ({}, [])) flT kinds self.Times;
              case tms of
                  None => error <xml>Invalid time!</xml>
                | Some (tms, tml) =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst3]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (create (tms ++ r));
                  return (tml, r --- map fst3 other)
       end,
       Save = let
           fun save k r kind tm =
               lv <- auth;
               if lv >= Write then
                   @@Sql.easy_update' [map fst3 key] [mapU time times ++ map fst3 other] [_] !
                     (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst3] [_] fl) inj)
                     (@map0 [fn _ => sql_injectable time] (fn [u ::_] => _ : sql_injectable time) flT
                       ++ injO)
                     (@Folder.mp fl) (@Folder.concat ! (@Folder.mp flT) (@Folder.mp flO)) tab k r
                     (WHERE {@pickFieldFromString [#T] ! flT kinds kind} = {[tm]})
               else
                   error <xml>Not authorized</xml>
       in
           fn k self kind tm =>
              tms <- @Monad.foldR2 _ [fn _ => string] [fn _ => id * source string]
                      [fn r => option ($(mapU time r) * list (time * option string * string))]
                      (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] k (_, s) acc =>
                          tmS <- get s;
                          return (case (read tmS, acc) of
                                      (Some tm, Some (r, l)) => Some ({nm = tm} ++ r, (tm, if showTime then Some (timef "%l:%M" tm) else None, k) :: l)
                                    | _ => None))
                      (Some ({}, [])) flT kinds self.Times;
              case tms of
                  None => error <xml>Invalid time!</xml>
                | Some (tms, tml) =>
                  r <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst3]
                        (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                        (@Folder.concat ! fl flO) ws self.Widgets;
                  rpc (save k (tms ++ r) kind tm);
                  return (tml, r --- map fst3 other)
       end,
       Delete = let
           fun delete k kind tm =
               lv <- auth;
               if lv >= Write then
                   dml (DELETE FROM tab
                        WHERE {@@Sql.easy_where [#T] [map fst3 key] [_] [_] [_] [_] ! !
                          (@mp [sql_injectable_prim] [sql_injectable] @@sql_prim (@@Folder.mp [fst3] [_] fl) inj)
                          (@Folder.mp fl) k}
                          AND {@pickFieldFromString [#T] ! flT kinds kind} = {[tm]})
               else
                   error <xml>Not authorized</xml>
       in
           fn k kind tm =>
              rpc (delete k kind tm)
       end,
       Eq = @@Record.eq [map fst3 key] eqs (@Folder.mp fl),
       Show = sh,
       Label = title,
       Display = display,
       MaySee =
         lv <- auth;
         return (lv >= Read),
       MayModify =
         lv <- auth;
         return (lv >= Write),
       ShowTime = showTime,
       MultipleKinds = @fold [fn _ => int]
                        (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] n => n + 1)
                        0 flT > 1
      }
end

fun compose [keys1] [keys2] [tags1] [tags2] [keys1 ~ keys2] [tags1 ~ tags2]
            (fl1 : folder keys1) (fl2 : folder keys2)
            (prim1 : $(map sql_injectable_prim keys1))
            (prim2 : $(map sql_injectable_prim keys2))
            (t1 : t keys1 tags1) (t2 : t keys2 tags2) : t (keys1 ++ keys2) (tags1 ++ tags2) =
    fn [[When, Kind, ShowTime] ~ (keys1 ++ keys2)] =>
       {Query = fn [otherKeys :: {Type}]
                       [([When = time, Kind = string, ShowTime = bool] ++ keys1 ++ keys2) ~ otherKeys]
                       (flo : folder otherKeys) (primo : $(map sql_injectable_prim otherKeys)) =>
                   q1 <- t1.Query [keys2 ++ otherKeys] ! (@Folder.concat ! fl2 flo) (prim2 ++ primo);
                   q2 <- t2.Query [keys1 ++ otherKeys] ! (@Folder.concat ! fl1 flo) (prim1 ++ primo);
                   case q1 of
                       None => return q2
                     | Some q1' =>
                       case q2 of
                           None => return q1
                         | Some q2' => return (Some (sql_relop sql_union False q1' q2')),
        Extract = fn [otherTags ::_] [otherTags ~ tags1 ++ tags2] r =>
                     case t1.Extract [otherTags ++ map fst3 tags2] ! (r --- map option keys2) of
                         None => t2.Extract [otherTags ++ map fst3 tags1] ! (r --- map option keys1)
                       | x => x,
        Tags = t1.Tags ++ t2.Tags}

fun items [keys ::: {Type}] [tags ::: {(Type * Type * Type)}] [[When, Kind, ShowTime] ~ keys] (t : t keys tags)
    : transaction (list (time * string * bool * variant (map fst3 tags))) =
    q <- t.Query [[]] ! _ {};
    case q of
        None => return []
      | Some q =>
        List.mapQuery ({{{q}}}
                        ORDER BY When, Kind)
                      (fn r => case t.Extract [[]] ! (r -- #When -- #Kind -- #ShowTime) of
                                   Some x => (r.When, r.Kind, r.ShowTime, x)
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
                 con tags :: {(Type * Type * Type)}
                 constraint [When, Kind, ShowTime] ~ keys
                 val t : t keys tags
                 val fl : folder tags
             end) = struct
open M
type input = _

type add = list (time * option string * string) * variant (map fst3 tags)
type del = variant (map fst3 tags)

datatype action =
         Add of add
       | Del of del * option {Kind : string, Day : string, Time : string}
       | Mod of del * add * option {Kind : string, Day : string, Time : string}

table listeners : { Kind : serialized (variant (map (fn _ => unit) tags)),
                    Channel : channel action }

type a = $(map thd3 tags) (* configuration *)
         * channel action
         * source (list (time * string * string * list (time * option string * string * variant (map fst3 tags))))
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
                                      | (tm, k, showTime, item) :: items' =>
                                        if tm < from || tm > to then
                                            loop' items' acc'
                                        else if tm < nextDay then
                                            loop' items' ((tm, if showTime then Some (timef "%l:%M" tm) else None, k, item) :: acc')
                                        else
                                            loop nextDay items ((day, timef "%b %e" day, show (dateify day), List.rev acc') :: acc)
                            in
                                loop' items []
                            end
                    end
            in
                cfg <- @Monad.mapR _ [tag] [thd3] (fn [nm ::_] [p ::_] (t : tag p) => t.Configure) fl t.Tags;
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
                return (cfg, ch, ds, mm)
            end

        fun onload ((_, ch, ds, _) : a) =
            let
                fun doAdd (tml, r) =
                    days <- get ds;
                    set ds (List.foldl (fn (tm, tmS, k) days =>
                                           List.mp (fn (tm', tmS', longS, items) =>
                                                       (tm', tmS', longS,
                                                        if tm' <= tm && tm < addSeconds tm' oneDay then
                                                            List.sort (fn (tm1, _, _, _) (tm2, _, _, _) => tm1 > tm2)
                                                                      ((tm, tmS, k, r) :: items)
                                                        else
                                                            items)) days) days tml)

                fun doDel (r, sto) =
                    days <- get ds;
                    set ds (List.mp (fn (tm', tmS', longS, items) =>
                                        (tm', tmS', longS,
                                         List.filter (fn (tm'', tmS'', k, r') =>
                                                         not (@eq (@@Variant.eq [map fst3 tags]
                                                                     (@mp [tag] [fn p => eq p.1]
                                                                       (fn [p] (t : tag p) => t.Eq)
                                                                       fl t.Tags) (@Folder.mp fl)) r' r
                                                              && case sto of
                                                                     None => True
                                                                   | Some st => k = st.Kind && tmS'' = Some st.Time && tmS' = st.Day)) items)) days)

                fun loop () =
                    msg <- recv ch;
                    (case msg of
                         Add c => doAdd c
                       | Del c => doDel c
                       | Mod (c1, c2, sto) => doDel (c1, sto); doAdd c2);
                    loop ()
            in
                spawn (loop ())
            end

        fun notify (k : variant (map fst3 tags)) (act : action) =
            queryI1 (SELECT listeners.Channel
                     FROM listeners
                     WHERE listeners.Kind = {[serialize (@Variant.erase (@Folder.mp fl) k)]})
                    (fn r => send r.Channel act)

        fun render ctx (cfg, _, ds, mm) =
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
                               Ui.modalButton ctx (CLASS "buttn btn btn-secondary btn-xs")
                               <xml><span class="glyphicon glyphicon-plus"/></xml>
                               (widgets <- @Monad.mapR2 _ [tag] [thd3] [snd3]
                                            (fn [nm ::_] [p ::_] (r : tag p) (cfg : p.3) => r.Fresh cfg longS)
                                            fl t.Tags cfg;
                                (whichTab : source int) <- source (@fold [fn _ => int]
                                                                    (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (n : int) => n + 1)
                                                                    (-1) fl);
                                return (Ui.modal (wt <- get whichTab;
                                                  (@foldR2 [tag] [snd3]
                                                    [fn r => o :: {Type} -> [o ~ r] => (variant (map fst3 r ++ o) -> variant (map fst3 tags))
                                                                                       -> int * transaction unit]
                                                    (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (x : p.2)
                                                                 (acc : o :: {Type} -> [o ~ r] => (variant (map fst3 r ++ o) -> variant (map fst3 tags))
                                                                                                  -> int * transaction unit)
                                                                 [o ::_] [o ~ [nm = p] ++ r]
                                                                 (maker : variant (map fst3 ([nm = p] ++ r) ++ o) -> variant (map fst3 tags)) =>
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
                                                   <ul class="bs-nav nav-tabs">
                                                     {(@foldR [tag] [fn _ => int * xbody]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (n, b) =>
                                                            (n+1,
                                                             <xml>
                                                               <li class="nav-item">
                                                                 <a dynClass={wt <- signal whichTab;
                                                                              return (if n = wt then
                                                                                          CLASS "nav-link bs-active"
                                                                                      else
                                                                                          CLASS "nav-link")}
                                                                    onclick={fn _ => set whichTab n}>{[t.Label]}</a>
                                                               </li>
                                                               {b}
                                                             </xml>)) (0, <xml/>) fl t.Tags).2}
                                                     </ul>
                                                              
                                                     <div class={fields}>
                                                       <dyn signal={wt <- signal whichTab;
                                                                    return (@foldR2 [tag] [snd3] [fn _ => int * xbody]
                                                                             (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (t : tag p) (x : p.2) (n, b) =>
                                                                                 (n+1,
                                                                                  if n = wt then
                                                                                      t.Render x
                                                                                  else
                                                                                      b)) (0, <xml/>) fl t.Tags widgets).2}/>
                                                     </div>
                                                     </xml>
                                                     <xml>Add to Calendar</xml>))}
                          {List.mapX (fn (tm, tmS', k, d) =>
                                         let
                                             val maymod = @Record.select [fn _ => bool] [fst3] fl
                                                           (fn [p] (b : bool) _ => b)
                                                           mm d
                                         in
                                             <xml><div class={item}>{case tmS' of
                                                                         None => <xml></xml>
                                                                       | Some tmS' =>
                                                                         <xml><span class={time}>{[tmS']}</span>:</xml>}
                                               <span class={item}>
                                                 {@Record.select [tag] [fst3] fl
                                                   (fn [p] (t : tag p) (x : p.1) =>
                                                       case t.Display of
                                                           None => <xml><i>{[@show t.Show x]} {[k]}</i></xml>
                                                         | Some display =>
                                                           Ui.modalButton ctx (CLASS "btn btn-link")
                                                                          <xml>{[@show t.Show x]} {[k]}</xml>
                                                                          (display ctx x))
                                                   t.Tags d}
                                                   {if not maymod then
                                                        <xml/>
                                                    else <xml>
                                                      <span class={buttn}>
                                                        {Ui.modalButton ctx (CLASS "btn btn-secondary btn-xs")
                                                        <xml><span class="glyphicon glyphicon-edit"/></xml>
                                                        (@Record.select2' [tag] [thd3] [fst3] [fst3] fl
                                                        (fn [p] (t : tag p) (cfg : p.3) (maker : p.1 -> variant (map fst3 tags)) (x : p.1) =>
                                                            widget <- t.FromDb cfg x;
                                                            return (Ui.modal ((tml, k2) <- t.Save x widget k tm;
                                                                              sto <- return (if t.MultipleKinds then
                                                                                                None
                                                                                            else
                                                                                                case tmS' of
                                                                                                    None => None
                                                                                                  | Some tmS' => Some {Kind = k, Time = tmS', Day = tmS});
                                                                              rpc (notify d (Mod (maker x, (tml, maker k2), sto))))
                                                                             <xml>Editing a calendar item ({[t.Label]})</xml>
                                                                             <xml>
                                                                               <div class={fields}>
                                                                                 {t.Render widget}
                                                                               </div>
                                                                             </xml>
                                                                             <xml>Save Calendar Entry</xml>))
                                                        t.Tags cfg d)}
                                                        {Ui.modalButton ctx (CLASS "btn btn-secondary btn-xs")
                                                                        <xml><span class="glyphicon glyphicon-trash"/></xml>
                                                                        (return (@Record.select [tag] [fst3] fl
                                                                        (fn [p] (t : tag p) (x : p.1) =>
                                                                        Ui.modal (t.Delete x k tm;
                                                                        sto <- return (if t.MultipleKinds then
                                                                                           None
                                                                                       else
                                                                                           case tmS' of
                                                                                               None => None
                                                                                             | Some tmS' => Some {Kind = k, Time = tmS', Day = tmS});
                                                                        rpc (notify d (Del (d, sto))))
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
                <thead><tr>
                  <th>Sunday</th>
                  <th>Monday</th>
                  <th>Tuesday</th>
                  <th>Wednesday</th>
                  <th>Thursday</th>
                  <th>Friday</th>
                  <th>Saturday</th>
                </tr></thead>
                <tbody>
                  <dyn signal={days <- signal ds;
                               return (render' days)}/>
                </tbody>
              </table></xml>
            end
    in
        {Create = create,
         Onload = onload,
         Render = render}
    end
end
