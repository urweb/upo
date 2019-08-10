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

type entry = {When : month,
              What : xbody,  (* description of transaction category *)
              HowMany : int, (* how many constituent transactions *)
              HowMuch : int} (* total value of all constituents *)
        
type t = {From : month, To : month}
         -> transaction {StartingBalance : int,
                         Entries : list (list entry)}
(* Invariant: each output sublist is sorted by When in descending order,
 * so we will be able to do an efficient merge into one sorted list. *)

fun fromGroupedTable [when :: Name] [k] [fs] [ks]
    [k ~ fs] [[when] ~ k ++ fs]
    (tab : sql_table ([when = time] ++ k ++ fs) ks)
    (eqs : $(map eq k)) (fl : folder k) (what : $k -> xbody)
    (howMuch : $([when = time] ++ k ++ fs) -> int) (bounds : {From : _, To : _}) =
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
    end

fun fromFlatTable [when :: Name] [fs] [ks] [[when] ~ fs]
    (tab : sql_table ([when = time] ++ fs) ks) what
    (howMuch : $([when = time] ++ fs) -> int) : t =
    fromGroupedTable [when] tab (fn () => what) howMuch
    
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

fun compose (a : t) (b : t) bounds =
    a <- a bounds;
    b <- b bounds;
    return {StartingBalance = a.StartingBalance + b.StartingBalance,
            Entries = List.append a.Entries b.Entries}
    
functor Make(M : sig
                 val t : t
                 val authorized : transaction bool
             end) = struct
    open M

    type a = {StartYear : source string,
              EndYear : source string,
              StartMonth : source string,
              EndMonth : source string,
              Months : source (list {Month : month,
                                     Balance : int,
                                     Entries : list {What : xbody, HowMany : int, HowMuch : int},
                                     Expanded : source bool})}

    val create =
        sy <- source "";
        ey <- source "";
        sm <- source "";
        em <- source "";
        ms <- source [];
        return {StartYear = sy,
                EndYear = ey,
                StartMonth = sm,
                EndMonth = em,
                Months = ms}

    fun onload _ = return ()

    fun generate bounds =
        authed <- authorized;
        if authed then
            t bounds
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
      <button class="btn btn-primary"
              value="Calculate"
              onclick={fn _ =>
                          start <- monthOf a.StartYear a.StartMonth;
                          end_ <- monthOf a.EndYear a.EndMonth;
                          data <- rpc (generate {From = start, To = end_});
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
