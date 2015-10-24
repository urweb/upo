open Bootstrap3

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
             end) = struct

    open M

    type grade = variant (mapU unit grades)

    val keyEq : eq $key = @@Record.eq [key] keyEq fl
    val gradeEq : eq grade = @@Variant.eq [mapU unit grades]
                               (@map0 [fn _ => eq unit] (fn [u ::_] => mkEq (fn () () => True)) gfl)
                               (@Folder.mp gfl)

    table specialCase : (key ++ [Grade = serialized grade])
      PRIMARY KEY {{@primary_key [key1] [keyR ] ! ! inj}},
      {{one_constraint [#Key] (@Sql.easy_foreign ! ! ! ! ! ! fl tab)}}

    table ranges : {Min : summary, Max : summary, Grade : serialized grade}
      PRIMARY KEY (Min, Max)

    type input = summaries

    type a = {Entries : list ($key
                              * summary

                              * source (option (source string))
                              (* A blank to solicit a new range boundary *)

                              * source (option grade)
                              (* Current special-case grade, if any *)

                              * source (option (source string))
                              (* A blank to solicit a new special-case grade *)),
              Ranges : source (list (summary * summary * grade))}

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
        return {Entries = keys, Ranges = ranges}

    fun onload _ = return ()

    fun skipRanges sm ranges =
        case ranges of
            [] => []
          | (min, _, _) :: ranges' =>
            if min > sm then
                skipRanges sm ranges'
            else
                ranges

    fun render' allRanges lastSm ranges keys =
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
                                               <button class="btn btn-sm glyphicon glyphicon-ok"
                                                       onclick={fn _ =>
                                                                   set rb None;
                                                                   rbv <- get rbs;
                                                                   case String.split rbv #" " of
                                                                       None =>
                                                                       grs <- get allRanges;
                                                                       set allRanges (List.filter (fn (min, _, _) => min <> sm) grs)
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
                                                                                           if min > sm then
                                                                                               (sm, sm, g) :: ranges'
                                                                                           else
                                                                                               (min, max, g') :: setFirst' ranges'

                                                                                   fun setFirst ranges =
                                                                                       case ranges of
                                                                                           [] => setFirst' ar
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if g' = g then
                                                                                               (sm, max, g) :: ranges'
                                                                                           else
                                                                                               (min, max, g') :: setFirst ranges'
                                                                               in
                                                                                   set allRanges (setFirst ar)
                                                                               end
                                                                             | "First" =>
                                                                               ar <- get allRanges;
                                                                               let
                                                                                   fun setLast' ranges =
                                                                                       case ranges of
                                                                                           [] => (sm, sm, g) :: []
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if min > sm then
                                                                                               (sm, sm, g) :: ranges'
                                                                                           else
                                                                                               (min, max, g') :: setLast' ranges'

                                                                                   fun setLast ranges =
                                                                                       case ranges of
                                                                                           [] => setLast' ar
                                                                                         | (min, max, g') :: ranges' =>
                                                                                           if g' = g then
                                                                                               (min, sm, g) :: ranges'
                                                                                           else
                                                                                               (min, max, g') :: setLast ranges'
                                                                               in
                                                                                   set allRanges (setLast ar)
                                                                               end
                                                                             | _ => error <xml>FinalGrades: Unknown bound spec</xml>}/>
                                               <button class="btn btn-sm glyphicon glyphicon-remove"
                                                       onclick={fn _ => set rb None}/>
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
                                                   <button class="btn btn-sm glyphicon glyphicon-pencil"
                                                           onclick={fn _ =>
                                                                       s <- source rng;
                                                                       set rb (Some s)}/>
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
                                                       <button class="btn btn-sm glyphicon glyphicon-pencil"
                                                               onclick={fn _ =>
                                                                           s <- source text;
                                                                           set newgr (Some s)}/>
                                                     </xml>
                                                 end
                                               | None =>
                                                 case ranges of
                                                     [] => <xml>
                                                       -
                                                       <button class="btn btn-sm glyphicon glyphicon-pencil"
                                                               onclick={fn _ =>
                                                                           s <- source "";
                                                                           set newgr (Some s)}/>
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
                                                               <button class="btn btn-sm glyphicon glyphicon-pencil"
                                                                       onclick={fn _ =>
                                                                                   s <- source text;
                                                                                   set newgr (Some s)}/>
                                                             </xml>
                                                         end
                                                     else
                                                         <xml>
                                                           -
                                                           <button class="btn btn-sm glyphicon glyphicon-pencil"
                                                                   onclick={fn _ =>
                                                                               s <- source "";
                                                                               set newgr (Some s)}/>
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

                                       <button class="btn btn-sm glyphicon glyphicon-ok"
                                               onclick={fn _ =>
                                                           set newgr None;
                                                           g <- get gs;
                                                           set gr (@Variant.fromString gfl grades g)}/>
                                       <button class="btn btn-sm glyphicon glyphicon-remove"
                                               onclick={fn _ => set newgr None}/>
                                     </xml>}/>
                  </td>

                  <td>{[sm]}</td>
                  <td>{[key]}</td>
                </tr>{render' allRanges (Some sm) ranges keys'}</xml>
            end

    fun render ctx a = <xml>
      <table class="bs3-table table-striped">
        <tr>
          <th>Range?</th>
          <th>{[gradeLabel]}</th>
          <th>{[summaryLabel]}</th>
          <th>{[keyLabel]}</th>
        </tr>

        <dyn signal={ranges <- signal a.Ranges;
                     return (render' a.Ranges None ranges a.Entries)}/>
      </table>
    </xml>

    fun ui sms = {Create = create sms,
                  Onload = onload,
                  Render = render}

end
