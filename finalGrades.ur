open Bootstrap4

datatype access = Forbidden | Read | Write

fun accessToInt x =
    case x of
        Forbidden => 0
      | Read => 1
      | Write => 2

val access_ord : ord access = mkOrd {Lt = fn x y => accessToInt x < accessToInt y,
                                     Le = fn x y => accessToInt x <= accessToInt y}

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 con rest :: {Type}
                 constraint key ~ rest
                 constraint key ~ [Grade, Min, Max]
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val tab : sql_table (key ++ rest) ([keyName = map (fn _ => ()) key] ++ otherConstraints)
                 val filter : sql_exp [Tab = key ++ rest] [] [] bool
                 val inj : $(map sql_injectable_prim key)
                 val fl : folder key
                 val keyShow : show $key
                 val keyEq : $(map eq key)

                 type summaries
                 type summary
                 val summary : summaries -> $key -> summary
                 val ord_summary : ord summary
                 val show_summary : show summary
                 val eq_summary : eq summary
                 val inj_summary : sql_injectable_prim summary

                 con grades :: {Unit}
                 val grades : $(mapU string grades)
                 val gfl : folder grades

                 val keyLabel : string
                 val summaryLabel : string
                 val gradeLabel : string

                 val access : transaction access
             end) = struct

    open M

    type grade = variant (mapU unit grades)

    val keyEq : eq $key = @@Record.eq [key] keyEq fl
    val gradeEq : eq grade = @@Variant.eq [mapU unit grades]
                               (@map0 [fn _ => eq unit] (fn [u ::_] => mkEq (fn () () => True)) gfl)
                               (@Folder.mp gfl)

    val inj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim fl inj

    table specialCase : (key ++ [Grade = serialized grade])
      PRIMARY KEY {{@primary_key [key1] [keyR ] ! ! inj}},
      {{one_constraint [#Key] (@Sql.easy_foreign ! ! ! ! ! ! fl tab)}}

    table ranges : {Min : summary, Max : summary, Grade : serialized grade}
      PRIMARY KEY (Min, Max)

    type input = summaries

    datatype change =
             SpecialCase of $key * option grade
           | Ranges of list (summary * summary * grade)

    type entry = $key
                 * summary

                 * source (option (source string))
                 (* A blank to solicit a new range boundary *)

                 * source (option grade)
                 (* Current special-case grade, if any *)

                 * source (option (source string))
                 (* A blank to solicit a new special-case grade *)

    type a = {Entries : list entry,
              Ranges : source (list (summary * summary * grade)),
              Channel : channel change}

    table listeners : {Channel : channel change}

    fun create sms =
        keys <- List.mapQueryM ({{{sql_query1 [[]]
                        {Distinct = False,
                         From = @@sql_left_join [[]]
                                  [[Tab = key ++ rest]]
                                  [[SpecialCase = map (fn t => (t, option t)) (key ++ [Grade = serialized grade])]]
                                  ! ! !
                                  {SpecialCase = @Top.mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                    @@nullify_prim (@Folder.concat ! (_ : folder [Grade = _]) fl)
                                    (_ ++ inj)}
                                  (FROM tab) (FROM specialCase)
                                  (WHERE {@@Sql.easy_join [#Tab] [#SpecialCase]
                                     [key] [rest] [[Grade = _]]
                                     [[]] [[]] [[]]
                                     ! ! ! ! fl}),
                         Where = sql_exp_weaken filter,
                         GroupBy = sql_subset_all [_],
                         Having = (WHERE TRUE),
                         SelectFields = sql_subset [[Tab = (key, _),
                                                     SpecialCase = ([Grade = _], _)]],
                         SelectExps = {}} }}})
                 (fn r =>
                     rb <- source None;
                     gr <- source (Option.mp deserialize r.SpecialCase.Grade);
                     newgr <- source None;
                     return (r.Tab, summary sms r.Tab, rb, gr, newgr));
        keys <- return (List.sort (fn (_, sm1, _, _, _) (_, sm2, _, _, _) => sm1 < sm2) keys);
        ranges <- List.mapQuery (SELECT *
                                 FROM ranges
                                 ORDER BY ranges.Max DESC, ranges.Min DESC)
                  (fn {Ranges = r} => (r.Min, r.Max, deserialize r.Grade));
        ranges <- source ranges;
        ch <- channel;
        dml (INSERT INTO listeners(Channel)
             VALUES ({[ch]}));
        return {Entries = keys, Ranges = ranges, Channel = ch}

    fun onload a =
        let
            fun loop () =
                change <- recv a.Channel;
                (case change of
                     SpecialCase (key, g) =>
                     List.app (fn (key', _, s1, sg, s2) =>
                                  if key' = key then
                                      set s1 None;
                                      set sg g;
                                      set s2 None
                                  else
                                      return ()) a.Entries
                   | Ranges rs => set a.Ranges rs);
                loop ()
        in
            spawn (loop ())
        end

    fun skipRanges sm ranges =
        case ranges of
            [] => []
          | (min, _, _) :: ranges' =>
            if min > sm then
                skipRanges sm ranges'
            else
                ranges

    val checkAccess =
        level <- access;
        if level < Write then
            error <xml>Access denied</xml>
        else
            return ()

    fun setRanges rs =
        checkAccess;
        dml (DELETE FROM ranges
             WHERE TRUE);
        List.app (fn (min, max, g) => debug ("[" ^ show min ^ "," ^ show max ^ "]"); dml (INSERT INTO ranges(Min, Max, Grade)
                                           VALUES ({[min]}, {[max]}, {[serialize g]}))) rs;
        queryI1 (SELECT * FROM listeners)
                (fn r => send r.Channel (Ranges rs))

    fun setSpecialCase (key : $key) g =
        checkAccess;
        dml (DELETE FROM specialCase
             WHERE {@@Sql.easy_where [#T] [key] [[Grade = _]] [[]] [[]] [[]] ! ! inj' fl key});
        (case g of
             None => return ()
           | Some g => @@Sql.easy_insert [key ++ [Grade = _]] [_] (inj' ++ {Grade = _})
                         (@Folder.cons [#Grade] [_] ! fl) specialCase (key ++ {Grade = serialize g}));
        queryI1 (SELECT * FROM listeners)
                (fn r => send r.Channel (SpecialCase (key, g)))

    fun render' allRanges lastSm ranges (keys : list entry) =
        case keys of
            [] => <xml></xml>
          | (key, sm, rb, gr, newgr) :: keys' =>
            let
                val ranges = skipRanges sm ranges
            in
                <xml><tr>
                  <td>
                    <dyn signal={rbv <- signal rb;
                                 return (case rbv of
                                             Some rbs => <xml>
                                               <cselect source={rbs}>
                                                 <coption></coption>
                                                 {@mapUX [string] [_]
                                                   (fn [nm ::_] [r ::_] [[nm] ~ r] name => <xml>
                                                     <coption>First {[name]}</coption>
                                                     <coption>Last {[name]}</coption>
                                                   </xml>)
                                                   gfl grades}
                                               </cselect>
                                               <button class="btn btn-secondary btn-sm"
                                                       onclick={fn _ =>
                                                                   set rb None;
                                                                   rbv <- get rbs;
                                                                   case String.split rbv #" " of
                                                                       None =>
                                                                       grs <- get allRanges;
                                                                       let
                                                                           val ars' = List.filter (fn (min, _, _) => min <> sm) grs
                                                                       in
                                                                           set allRanges ars';
                                                                           rpc (setRanges ars')
                                                                       end
                                                                     | Some (which, g) =>
                                                                       case @Variant.fromString gfl grades g of
                                                                           None => error <xml>FinalGrades: Unknown grade string</xml>
                                                                         | Some g =>
                                                                           case which of
                                                                               "Last" =>
                                                                               ar <- get allRanges;
                                                                               let
                                                                                   fun setFirst' ranges =
                                                                                       case ranges of
                                                                                           [] => (sm, sm, g) :: []
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if min < sm then
                                                                                               (sm, sm, g) :: ranges
                                                                                           else
                                                                                               (min, max, g') :: setFirst' ranges'

                                                                                   fun setFirst ranges acc =
                                                                                       case ranges of
                                                                                           [] => setFirst' ar
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if g' = g then
                                                                                               List.revAppend acc ((sm, max, g) :: ranges')
                                                                                           else
                                                                                               setFirst ranges' ((min, max, g') :: acc)

                                                                                   val ar' = setFirst ar []
                                                                               in
                                                                                   set allRanges ar';
                                                                                   rpc (setRanges ar')
                                                                               end
                                                                             | "First" =>
                                                                               ar <- get allRanges;
                                                                               let
                                                                                   fun setLast' ranges =
                                                                                       case ranges of
                                                                                           [] => (sm, sm, g) :: []
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if min < sm then
                                                                                               (sm, sm, g) :: ranges
                                                                                           else
                                                                                               (min, max, g') :: setLast' ranges'

                                                                                   fun setLast ranges acc =
                                                                                       case ranges of
                                                                                           [] => setLast' ar
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if g' = g then
                                                                                               List.revAppend acc ((min, sm, g) :: ranges')
                                                                                           else
                                                                                               setLast ranges' ((min, max, g') :: acc)
                                                                                   val ar' = setLast ar []
                                                                               in
                                                                                   set allRanges ar';
                                                                                   rpc (setRanges ar')
                                                                               end
                                                                             | _ => error <xml>FinalGrades: Unknown bound spec</xml>}>
                                                 <span class="glyphicon glyphicon-check"/>
                                               </button>
                                               <button class="btn btn-secondary btn-sm"
                                                       onclick={fn _ => set rb None}>
                                                 <span class="glyphicon glyphicon-trash"/>
                                               </button>
                                             </xml>
                                           | None =>
                                             let
                                                 val rng =
                                                     case ranges of
                                                         [] => ""
                                                       | (min, max, g) :: _ =>
                                                         if sm = max && (case keys' of
                                                                                  (_, sm', _, _, _) :: _ => sm' <> sm
                                                                                | [] => True) then
                                                             "First " ^ @Record.select [fn _ => string] [fn _ => unit]
                                                                         gfl (fn [t] (lab : string) () => lab)
                                                                         grades g
                                                         else if sm = min && lastSm <> Some sm then
                                                             "Last " ^ @Record.select [fn _ => string] [fn _ => unit]
                                                                        gfl (fn [t] (lab : string) () => lab)
                                                                        grades g
                                                         else
                                                             ""
                                             in
                                                 <xml>
                                                   {[rng]}
                                                   <button class="btn btn-secondary btn-sm"
                                                           onclick={fn _ =>
                                                                       s <- source rng;
                                                                       set rb (Some s)}>
                                                     <span class="glyphicon glyphicon-pencil-alt"/>
                                                   </button>
                                                 </xml>
                                             end)}/>
                  </td>

                  <td>
                    <dyn signal={newgrv <- signal newgr;
                                 case newgrv of
                                     None =>
                                     grv <- signal gr;
                                     return (case grv of
                                                 Some g =>
                                                 let
                                                     val text = @Record.select [fn _ => string] [fn _ => unit]
                                                                 gfl (fn [t] (lab : string) () => lab)
                                                                 grades g
                                                 in
                                                     <xml>
                                                       *{[text]}
                                                       <button class="btn btn-secondary btn-sm"
                                                               onclick={fn _ =>
                                                                           s <- source text;
                                                                           set newgr (Some s)}>
                                                         <span class="glyphicon glyphicon-pencil-alt"/>
                                                       </button>
                                                     </xml>
                                                 end
                                               | None =>
                                                 case ranges of
                                                     [] => <xml>
                                                       -
                                                       <button class="btn btn-secondary btn-sm"
                                                               onclick={fn _ =>
                                                                           s <- source "";
                                                                           set newgr (Some s)}>
                                                         <span class="glyphicon glyphicon-pencil-alt"/>
                                                       </button>
                                                     </xml>
                                                   | (min, max, g) :: _ =>
                                                     if min <= sm && sm <= max then
                                                         let
                                                             val text = @Record.select [fn _ => string] [fn _ => unit]
                                                                         gfl (fn [t] (lab : string) () => lab)
                                                                         grades g
                                                         in
                                                             <xml>
                                                               {[text]}
                                                               <button class="btn btn-secondary btn-sm"
                                                                       onclick={fn _ =>
                                                                                   s <- source text;
                                                                                   set newgr (Some s)}>
                                                                 <span class="glyphicon glyphicon-pencil-alt"/>
                                                               </button>
                                                             </xml>
                                                         end
                                                     else
                                                         <xml>
                                                           -
                                                           <button class="btn btn-secondary btn-sm"
                                                                   onclick={fn _ =>
                                                                               s <- source "";
                                                                               set newgr (Some s)}>
                                                             <span class="glyphicon glyphicon-pencil-alt"/>
                                                           </button>
                                                         </xml>)
                                   | Some gs =>
                                     grv <- signal gr;
                                     return <xml>
                                       <cselect source={gs}>
                                         <coption></coption>
                                         {@mapUX [string] [_]
                                           (fn [nm ::_] [r ::_] [[nm] ~ r] name => <xml>
                                             <coption>{[name]}</coption>
                                           </xml>)
                                           gfl grades}
                                       </cselect>

                                       <button class="btn btn-secondary btn-sm"
                                               onclick={fn _ =>
                                                           set newgr None;
                                                           g <- get gs;
                                                           g <- return (@Variant.fromString gfl grades g);
                                                           set gr g;
                                                           rpc (setSpecialCase key g)}>
                                         <span class="glyphicon glyphicon-check"/>
                                       </button>
                                       <button class="btn btn-secondary btn-sm"
                                               onclick={fn _ => set newgr None}>
                                         <span class="glyphicon glyphicon-trash"/>
                                       </button>
                                     </xml>}/>
                  </td>

                  <td>{[sm]}</td>
                  <td>{[key]}</td>
                </tr>{render' allRanges (Some sm) ranges keys'}</xml>
            end

    fun render ctx (a : a) = <xml>
      <table class="bs-table">
        <thead><tr>
          <th>Range?</th>
          <th>{[gradeLabel]}</th>
          <th>{[summaryLabel]}</th>
          <th>{[keyLabel]}</th>
        </tr></thead>

        <tbody>
          <dyn signal={ranges <- signal a.Ranges;
                       return (render' a.Ranges None ranges a.Entries)}/>
        </tbody>
      </table>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui sms = {Create = create sms,
                  Onload = onload,
                  Render = render,
                  Notification = notification,
                  Buttons = buttons}

    fun grades sms =
        ranges <- queryL1 (SELECT *
                           FROM ranges
                           ORDER BY ranges.Min ASC, ranges.Max ASC);

        List.mapQuery ({{{sql_query1 [[]]
                        {Distinct = False,
                         From = @@sql_left_join [[]]
                                  [[Tab = key ++ rest]]
                                  [[SpecialCase = map (fn t => (t, option t)) (key ++ [Grade = serialized grade])]]
                                  ! ! !
                                  {SpecialCase = @Top.mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                    @@nullify_prim (@Folder.concat ! (_ : folder [Grade = _]) fl)
                                    (_ ++ inj)}
                                  (FROM tab) (FROM specialCase)
                                  (WHERE {@@Sql.easy_join [#Tab] [#SpecialCase]
                                     [key] [rest] [[Grade = _]]
                                     [[]] [[]] [[]]
                                     ! ! ! ! fl}),
                         Where = sql_exp_weaken filter,
                         GroupBy = sql_subset_all [_],
                         Having = (WHERE TRUE),
                         SelectFields = sql_subset [[Tab = (key, _),
                                                     SpecialCase = ([Grade = _], _)]],
                         SelectExps = {}} }}}
                 ORDER BY {{{@Sql.order_by fl
                                       (@Sql.some_fields [#Tab] [key] ! ! fl)
                                       sql_asc}}})
                 (fn r =>
                     let
                         val sm = summary sms r.Tab

                         fun tryRanges ranges =
                             case ranges of
                                 [] => error <xml>No grade chosen yet for {[r.Tab]}</xml>
                               | r :: ranges' =>
                                 if r.Min <= sm && sm <= r.Max then
                                     deserialize r.Grade
                                 else
                                     tryRanges ranges'
                     in
                         (r.Tab, case r.SpecialCase.Grade of
                                     None => tryRanges ranges
                                   | Some g => deserialize g)
                     end)

end
