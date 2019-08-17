open Bootstrap4

type month = int * Datetime.month

val _ : eq month = Record.eq
val _ : ord month = mkOrd {Lt = fn (y1, m1) (y2, m2) => y1 < y2 || (y1 = y2 && m1 < m2),
                           Le = fn (y1, m1) (y2, m2) => y1 < y2 || (y1 = y2 && m1 <= m2)}
val _ : show month = mkShow (fn (y, m) => show m ^ " " ^ show y)
                   
fun monthIn t =
    let
        val t = Datetime.fromTime t
    in
        (t.Year, t.Month)
    end

fun monthOut (y, m) =
    Datetime.toTime {Year = y,
                     Month = m,
                     Day = 1,
                     Hour = 0,
                     Minute = 0,
                     Second = 0}
                   
fun oneMonthLater (y, m) =
    if m = Datetime.December then
        (y+1, Datetime.January)
    else
        (y, Datetime.intToMonth (Datetime.monthToInt m + 1))

fun oneMonthEarlier (y, m) =
    if m = Datetime.January then
        (y-1, Datetime.December)
    else
        (y, Datetime.intToMonth (Datetime.monthToInt m - 1))

type entry = {When : month,
              What : xbody,  (* description of transaction category *)
              HowMany : int, (* how many constituent transactions *)
              HowMuch : int} (* total value of all constituents *)
        
type t (ps, vs) = {Create : transaction ps,
                   Settings : ps -> xbody,
                   Parameters : ps -> signal vs,
                   Calculate : vs -> {From : month, To : month}
                               -> transaction {StartingBalance : int,
                                               Entries : list (list entry)}}
(* Invariant: each output sublist is sorted by When in descending order,
 * so we will be able to do an efficient merge into one sorted list. *)

type fromGroupedTable = (unit, unit)
fun fromGroupedTable [when :: Name] [k] [fs] [ks]
    [k ~ fs] [[when] ~ k ++ fs]
    (tab : sql_table ([when = time] ++ k ++ fs) ks)
    (eqs : $(map eq k)) (fl : folder k) (what : $k -> xbody)
    (howMuch : $([when = time] ++ k ++ fs) -> int) : t fromGroupedTable =
    {Create = return (),
     Settings = fn () => <xml></xml>,
     Parameters = fn () => return (),
     Calculate = fn () (bounds : {From : _, To : _}) =>
                    (startbal, months) <- query1' (SELECT *
                                                   FROM tab
                                                   WHERE tab.{when} < {[monthOut bounds.To]}
                                                   ORDER BY tab.{when})
                                          (fn r (startbal, months) =>
                                              if r.when < monthOut bounds.From then
                                                  (startbal + howMuch r, months)
                                              else
                                                  let
                                                      val g = r -- when --- fs
                                                      val m = monthIn r.when
                                                      val _ = @Record.eq eqs fl
                                                  in
                                                      case months of
                                                          [] => (startbal, (m, (g, (1, howMuch r)) :: []) :: [])
                                                        | (m', gs) :: months' =>
                                                          if m' = m then
                                                              case List.assoc g gs of
                                                                  None =>
                                                                  (startbal, (m', (g, (1, howMuch r)) :: gs) :: months')
                                                                | Some (count, bal) =>
                                                                  let
                                                                      val gs = List.filter (fn (g', _) => g' <> g) gs
                                                                  in
                                                                      (startbal, (m, (g, (count + 1, bal + howMuch r)) :: gs) :: months')
                                                                  end
                                                          else
                                                              (startbal, (m, (g, (1, howMuch r)) :: []) :: months)
                                                  end)
                                          (0, []);
                    let
                        fun makeMonths months cur acc =
                            if cur > bounds.To then
                                List.rev acc
                            else case months of
                                     [] => makeMonths months (oneMonthLater cur) acc
                                   | (m, gs) :: months' =>
                                     if m = cur then
                                         makeMonths months' (oneMonthLater cur)
                                                    (List.revAppend
                                                         (List.mp (fn (g, (count, amt)) =>
                                                                      {When = cur,
                                                                       What = what g,
                                                                       HowMany = count,
                                                                       HowMuch = amt}) gs)
                                                         acc)
                                     else
                                         makeMonths months (oneMonthLater cur) acc
                    in
                        return {StartingBalance = startbal,
                                Entries = makeMonths (List.rev months) bounds.From [] :: []}
                    end}

type fromFlatTable = (unit, unit)
fun fromFlatTable [when :: Name] [fs] [ks] [[when] ~ fs]
    (tab : sql_table ([when = time] ++ fs) ks) what
    (howMuch : $([when = time] ++ fs) -> int) : t fromFlatTable =
    fromGroupedTable [when] tab (fn () => what) howMuch

type flatAveragedEstimate = ({Lookback : source string,
                              Add : source string,
                              Max : source string,
                              Percent : source string},
                             {Lookback : int,
                              Add : int,
                              Max : int,
                              Percent : int})
fun flatAveragedEstimate [monthsToAverage :: Name] [addToCountMonthly :: Name] [maxCount :: Name] [monthlyPercentChangeInMultiplier :: Name] [ks]
    [[monthsToAverage] ~ [addToCountMonthly]] [[monthsToAverage, addToCountMonthly] ~ [maxCount]]
    [[monthsToAverage, addToCountMonthly, maxCount] ~ [monthlyPercentChangeInMultiplier]]
    (params : sql_table [monthsToAverage = int, addToCountMonthly = int, maxCount = int, monthlyPercentChangeInMultiplier = int] ks)
    [when :: Name] [fs] [gks] [[when] ~ fs]
    (tab : sql_table ([when = time] ++ fs) gks) what
    (howMuch : $([when = time] ++ fs) -> int) : t flatAveragedEstimate =
    {Create = ro <- oneOrNoRows1 (SELECT * FROM params);
              r <- return (Option.get (@@map0 [fn _ :: Unit => int] (fn [u ::_] => 0) [[monthsToAverage, addToCountMonthly, maxCount, monthlyPercentChangeInMultiplier]] _) ro);
              r <- Monad.mapR [fn _ :: Unit => int] [fn _ => source string] (fn [nm ::_] [u ::_] n => source (show n)) r;
              return {Lookback = r.monthsToAverage, Add = r.addToCountMonthly, Max = r.maxCount, Percent = r.monthlyPercentChangeInMultiplier},
     Settings = fn r => <xml>
       <h2>{[what]}</h2>
       <div class="form-group">
         <label class="control-label">How many recent months to average:</label>
         <div><ctextbox source={r.Lookback}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">How much to increase count per month:</label>
         <div><ctextbox source={r.Add}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">Maximum count:</label>
         <div><ctextbox source={r.Max}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">Percent increase in multiplier per month:</label>
         <div><ctextbox source={r.Percent}/></div>
       </div>
     </xml>,
     Parameters = Monad.mapR [fn _ :: Unit => source string] [fn _ => int] (fn [nm ::_] [u ::_] s => s <- signal s; return (Option.get 0 (read s))),
     Calculate = fn r bounds =>
                    let
                        fun oneMonth m =
                            (count, sum) <- query1 (SELECT *
                                                    FROM tab
                                                    WHERE {[monthOut m]} <= tab.{when}
                                                      AND tab.{when} < {[monthOut (oneMonthLater m)]})
                                                   (fn r (count, sum) => return (count + 1, sum + howMuch r))
                                                   (0, 0);
                            return (count,
                                    if count = 0 then
                                        0
                                    else
                                        sum / count)

                        fun sumMonths m remaining =
                            if remaining <= 0 then
                                return (0, 0)
                            else
                                (count1, avg1) <- oneMonth m;
                                (_, avgs) <- sumMonths (oneMonthEarlier m) (remaining - 1);
                                return (count1, avg1 + avgs)

                        fun buildMonths cur count multiplier acc =
                            if cur > bounds.To then
                                List.rev acc
                            else
                                let
                                    val count = min (count + r.Add) r.Max
                                    val multiplier = multiplier * (100 + r.Percent) / 100
                                in
                                    buildMonths (oneMonthLater cur) count multiplier
                                    ({When = cur, What = what, HowMany = count, HowMuch = count * multiplier} :: acc)
                                end
                    in
                        m <- now;
                        m <- return (monthIn m);
                        (count, sum) <- sumMonths m r.Lookback;
                        avg <- return (if r.Lookback <= 0 then
                                           0
                                       else
                                           sum / r.Lookback);
                        return {StartingBalance = 0,
                                Entries = buildMonths (max bounds.From (oneMonthLater m)) count avg [] :: []}
                    end}

type groupedAveragedEstimate (kt :: Type) =
     (list (kt * {Lookback : source string,
                  Add : source string,
                  Max : source string,
                  Percent : source string}),
      list (kt * {Lookback : int,
                  Add : int,
                  Max : int,
                  Percent : int}))
fun groupedAveragedEstimate [k ::: Name] [kt ::: Type] [monthsToAverage :: Name] [addToCountMonthly :: Name] [maxCount :: Name] [monthlyPercentChangeInMultiplier :: Name] [ks]
    [[monthsToAverage] ~ [addToCountMonthly]] [[monthsToAverage, addToCountMonthly] ~ [maxCount]]
    [[monthsToAverage, addToCountMonthly, maxCount] ~ [monthlyPercentChangeInMultiplier]]
    [[monthsToAverage, addToCountMonthly, maxCount, monthlyPercentChangeInMultiplier] ~ [k]]
    (eqk : eq kt)
    (params : sql_table ([k = kt, monthsToAverage = int, addToCountMonthly = int, maxCount = int, monthlyPercentChangeInMultiplier = int]) ks)
    [k2 :: Name] [when :: Name] [fs] [gks] [[k2] ~ fs] [[when] ~ [k2 = kt] ++ fs]
    (tab : sql_table ([when = time, k2 = kt] ++ fs) gks) (what : {k2 : kt} -> xbody)
    (howMuch : $([when = time, k2 = kt] ++ fs) -> int) : t (groupedAveragedEstimate kt) =
    {Create = rs <- List.mapQuery (SELECT * FROM params) (fn {Params = r} => (r.k, r -- k));
              rs <- query1 (SELECT DISTINCT tab.{k2}
                            FROM tab)
                           (fn rk rs =>
                               return (case List.assoc rk.k2 rs of
                                           None => (rk.k2, @@map0 [fn _ :: Unit => int] (fn [u ::_] => 0) [[monthsToAverage, addToCountMonthly, maxCount, monthlyPercentChangeInMultiplier]] _) :: rs
                                         | Some _ => rs)) rs;
              rs <- List.mapM (fn (k, r) => r <- Monad.mapR [fn _ :: Unit => int] [fn _ => source string] (fn [nm ::_] [u ::_] n => source (show n)) r; return (k, r)) rs;
              return (List.mp (fn (k, r) => (k, {Lookback = r.monthsToAverage, Add = r.addToCountMonthly, Max = r.maxCount, Percent = r.monthlyPercentChangeInMultiplier})) rs),
     Settings = List.mapX (fn (k, r) => <xml>
       <h2>{what {k2 = k}}</h2>
       <div class="form-group">
         <label class="control-label">How many recent months to average:</label>
         <div><ctextbox source={r.Lookback}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">How much to increase count per month:</label>
         <div><ctextbox source={r.Add}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">Maximum count:</label>
         <div><ctextbox source={r.Max}/></div>
       </div>
       <div class="form-group">
         <label class="control-label">Percent increase in multiplier per month:</label>
         <div><ctextbox source={r.Percent}/></div>
       </div>
     </xml>),
     Parameters = List.mapM (fn (k, r) => r <- Monad.mapR [fn _ :: Unit => source string] [fn _ => int] (fn [nm ::_] [u ::_] s => s <- signal s; return (Option.get 0 (read s))) r;
                                return (k, r)),
     Calculate = fn params bounds =>
                    let
                        fun oneMonth m remaining =
                            cs <- query1 (SELECT *
                                          FROM tab
                                          WHERE {[monthOut m]} <= tab.{when}
                                            AND tab.{when} < {[monthOut (oneMonthLater m)]})
                                         (fn r cs =>
                                             let
                                                 val k = r.k2
                                             in
                                                 return (case List.assoc k cs of
                                                             None => (k, (1, howMuch r)) :: cs
                                                           | Some (count, sum) => List.mp (fn (k', v) => if k' = k then (k, (count + 1, sum + howMuch r)) else (k', v)) cs)
                                             end)
                                         [];
                            return (List.mapPartial (fn (k, (count, sum)) =>
                                                        case List.assoc k remaining of
                                                            None => None
                                                          | Some _ =>
                                                            Some (k,
                                                                  (count,
                                                                   if count = 0 then
                                                                       0
                                                                   else
                                                                       sum / count))) cs)

                        fun sumMonths m remaining =
                            case remaining of
                                [] => return []
                              | _ =>
                                cs1 <- oneMonth m remaining;
                                cs <- sumMonths (oneMonthEarlier m) (List.mapPartial (fn (k, lookback) =>
                                                                                         if lookback <= 1 then
                                                                                             None
                                                                                         else
                                                                                             Some (k, lookback - 1)) remaining);
                                cs' <- return (List.mp (fn (k, (count, sum)) =>
                                                           let
                                                               val sum = case List.assoc k cs of
                                                                             None => sum
                                                                           | Some (_, sum') => sum + sum'
                                                           in
                                                               (k, (count, sum))
                                                           end) cs1);
                                return (List.foldl (fn (k, (_, sum)) cs' =>
                                                       case List.assoc k cs' of
                                                           None => (k, (0, sum)) :: cs'
                                                         | Some _ => cs') cs' cs)

                        fun buildMonths cur cs acc =
                            if cur > bounds.To then
                                List.rev acc
                            else
                                let
                                    val (cs, acc) = List.foldl (fn (k, (count, multiplier)) (cs, acc) =>
                                                                   case List.assoc k params of
                                                                       None => (cs, acc)
                                                                     | Some ps =>
                                                                       let
                                                                           val count = min (count + ps.Add) ps.Max
                                                                           val multiplier = multiplier * (100 + ps.Percent) / 100
                                                                       in
                                                                           ((k, (count, multiplier)) :: cs,
                                                                            {When = cur, What = what {k2 = k}, HowMany = count, HowMuch = count * multiplier} :: acc)
                                                                       end) ([], acc) cs
                                in
                                    buildMonths (oneMonthLater cur) cs acc
                                end
                    in
                        m <- now;
                        m <- return (monthIn m);
                        cs <- sumMonths m (List.mapPartial (fn (k, pr) => if pr.Lookback <= 0 then
                                                                              None
                                                                          else
                                                                              Some (k, pr.Lookback)) params);
                        cs <- return (List.mp (fn (k, (count, sum)) =>
                                                  (k, (count,
                                                       case List.assoc k params of
                                                           None => sum
                                                         | Some pr => if pr.Lookback <= 0 then
                                                                          0
                                                                      else
                                                                          sum / pr.Lookback))) cs);
                        return {StartingBalance = 0,
                                Entries = buildMonths (max bounds.From (oneMonthLater m)) cs [] :: []}
                    end}
     
fun merge (lss : list (list entry)) (acc : list entry) : list entry =
    let
        val earliest = List.foldl (fn ls earliest =>
                                      case ls of
                                          [] => earliest
                                        | e :: ls' =>
                                          Some (case earliest of
                                                    None => e.When
                                                  | Some earliest => min earliest e.When))
                                  None lss
    in
        case earliest of
            None => List.rev acc
          | Some earliest =>
            let
                fun findEarliest (lss : list (list entry)) (acc : list (list entry)) =
                    case lss of
                        [] => error <xml>Found no ledger entry matching computed earliest time!</xml>
                      | [] :: lss' => findEarliest lss' acc
                      | (e :: ls) :: lss' =>
                        if e.When = earliest then
                            (e, List.revAppend acc (ls :: lss'))
                        else
                            findEarliest lss' ((e :: ls) :: acc)

                val (e, lss) = findEarliest lss []
            in
                merge lss (e :: acc)
            end
    end

con compose (p1, v1) (p2, v2) = ((p1 * p2), (v1 * v2))
    
fun compose [a] [b] (a : t a) (b : t b) =
    {Create = a <- a.Create; b <- b.Create; return (a, b),
     Settings = fn (psa, psb) => <xml>{a.Settings psa}{b.Settings psb}</xml>,
     Parameters = fn (psa, psb) => a <- a.Parameters psa; b <- b.Parameters psb; return (a, b),
     Calculate = fn (psa, psb) bounds =>
                    a <- a.Calculate psa bounds;
                    b <- b.Calculate psb bounds;
                    return {StartingBalance = a.StartingBalance + b.StartingBalance,
                            Entries = List.append a.Entries b.Entries}}
    
functor Make(M : sig
                 con ps :: (Type * Type)
                 val t : t ps
                 val authorized : transaction bool
             end) = struct
    open M

    type a = {Parameters : ps.1,
              StartYear : source string,
              EndYear : source string,
              StartMonth : source string,
              EndMonth : source string,
              Months : source (list {Month : month,
                                     Balance : int,
                                     Entries : list {What : xbody, HowMany : int, HowMuch : int},
                                     Expanded : source bool})}

    val create =
        ps <- t.Create;
        sy <- source "";
        ey <- source "";
        sm <- source "";
        em <- source "";
        ms <- source [];
        return {Parameters = ps,
                StartYear = sy,
                EndYear = ey,
                StartMonth = sm,
                EndMonth = em,
                Months = ms}

    fun onload _ = return ()

    fun generate vs bounds =
        authed <- authorized;
        if authed then
            t.Calculate vs bounds
        else
            error <xml>Access denied</xml>

    fun monthsFromEntries (* current month *) cur
                          (* end month *) end_
                          es bal acc =
        if cur = end_ then
            return (List.rev acc)
        else
            let
                fun extractFromMonth es bal acc =
                    case es of
                        [] => ([], bal, List.rev acc)
                      | e :: es' =>
                        if e.When = cur then
                            extractFromMonth es' (bal + e.HowMuch) ((e -- #When) :: acc)
                        else
                            (es, bal, List.rev acc)

                val (remaining, bal, thisMonth) = extractFromMonth es bal []
            in
                expanded <- source False;
                monthsFromEntries (oneMonthLater cur) end_ remaining bal
                                  ({Month = cur,
                                    Balance = bal,
                                    Entries = thisMonth,
                                    Expanded = expanded} :: acc)
            end

    fun monthOf y m =
        y <- get y;
        case read y of
            None => error <xml>Invalid year</xml>
          | Some y =>
            m <- get m;
            case read m of
                None => error <xml>Invalid month</xml>
              | Some m =>
                let
                    val m = Datetime.intToMonth (m - 1)
                in
                    return (y, m)
                end
                    
    fun render _ a = <xml>
      <div class="form-group">
        <label class="control-label"><h5>Start year:</h5></label>
        <div><ctextbox source={a.StartYear}/></div>
      </div>
      <div class="form-group">
        <label class="control-label"><h5>Start month:</h5></label>
        <div><ctextbox source={a.StartMonth}/></div>
      </div>
      <div class="form-group">
        <label class="control-label"><h5>End year:</h5></label>
        <div><ctextbox source={a.EndYear}/></div>
      </div>
      <div class="form-group">
        <label class="control-label"><h5>End month:</h5></label>
        <div><ctextbox source={a.EndMonth}/></div>
      </div>
      {t.Settings a.Parameters}
      <button class="btn btn-primary"
              value="Calculate"
              onclick={fn _ =>
                          vs <- current (t.Parameters a.Parameters);
                          start <- monthOf a.StartYear a.StartMonth;
                          end_ <- monthOf a.EndYear a.EndMonth;
                          data <- rpc (generate vs {From = start, To = end_});
                          ms <- monthsFromEntries start (oneMonthLater end_) (merge data.Entries []) data.StartingBalance [];
                          set a.Months ms}/>

      <hr/>

      <table class="bs-table table-striped">
        <tr>
          <th class="col-sm-2">Month</th>
          <th class="col-sm-1">Balance</th>
          <th>Entries</th>
        </tr>
        <dyn signal={ms <- signal a.Months;
                     return (List.mapX (fn m => <xml><tr>
                       <td class="col_sm-1">{[m.Month]}</td>
                       <td class="col-sm-1">{[m.Balance]}</td>
                       <td>
                         <dyn signal={expd <- signal m.Expanded;
                                      return (if not expd then
                                                  <xml><button class="btn"
                                                               onclick={fn _ => set m.Expanded True}>
                                                    <span class="glyphicon glyphicon-chevron-down"/>
                                                  </button></xml>
                                              else
                                                  <xml>
                                                    <button class="btn"
                                                            onclick={fn _ => set m.Expanded False}>
                                                      <span class="glyphicon glyphicon-chevron-up"/>
                                                    </button>

                                                    <table class="bs-table table-striped">
                                                      <tr>
                                                        <th class="col-sm-3">What</th>
                                                        <th class="col-sm-2">How Many</th>
                                                        <th class="col-sm-2">How Much</th>
                                                      </tr>
                                                      {List.mapX (fn e => <xml><tr>
                                                        <td class="col-sm-3">{e.What}</td>
                                                        <td class="col-sm-2">{[e.HowMany]}</td>
                                                        <td class="col-sm-2">{[e.HowMuch]}</td>
                                                      </tr></xml>) m.Entries}
                                                    </table>
                                                  </xml>)}/>
                       </td>
                     </tr></xml>) ms)}/>
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
