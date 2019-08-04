open Bootstrap4

type entry = {When : time,
              What : xbody,
              HowMuch : int}
type t = {From : time, To : time} -> transaction (list (list entry))
(* Invariant: each output sublist is sorted by When in descending order,
 * so we will be able to do an efficient merge into one sorted list. *)

fun oneMonthLater (y, m) =
    if m = Datetime.December then
        (y+1, Datetime.January)
    else
        (y, Datetime.intToMonth (Datetime.monthToInt m + 1))

val _ : eq (int * Datetime.month) = Record.eq
        
fun ymIn t =
    let
        val t = Datetime.fromTime t
    in
        (t.Year, t.Month)
    end

fun ymOut (y, m) =
    Datetime.toTime {Year = y,
                     Month = m,
                     Day = 1,
                     Hour = 0,
                     Minute = 0,
                     Second = 0}
        
fun fromTable [when :: Name] [fs] [ks] [[when] ~ fs]
    (tab : sql_table ([when = time] ++ fs) ks)
    (howMuch : $([when = time] ++ fs) -> int)
    (what : $([when = time] ++ fs) -> xbody) bounds =
    ls <- List.mapQuery (SELECT *
                         FROM tab
                         WHERE {[bounds.From]} <= tab.{when}
                           AND tab.{when} <= {[bounds.To]}
                         ORDER BY tab.{when} DESC)
                        (fn {Tab = r} => {When = r.when,
                                          What = what r,
                                          HowMuch = howMuch r});
    return (ls :: [])

fun deltaAndMultiplier [kt] [k1 :: Name] [when1 :: Name] [change :: Name] [rest1]
    [k2 :: Name] [when2 :: Name] [multiplier :: Name] [rest2] [ks1] [ks2]
    [[when1] ~ [change]] [[when1, change] ~ [k1]] [[when1, change] ~ rest1] [[k1] ~ rest1]
    [[when2] ~ [multiplier]] [[when2, multiplier] ~ [k2]] [[when2, multiplier] ~ rest2] [[k2] ~ rest2]
    (_ : eq kt) (_ : show kt)
    (deltas : sql_table ([k1 = kt, when1 = time, change = int] ++ rest1) ks1)
    (multipliers : sql_table ([k2 = kt, when2 = time, multiplier = int] ++ rest2) ks2) bounds =
    let
        val b = ymIn bounds.From
        val e = ymIn bounds.To

        fun assemble cur groups ds ms acc =
            if cur = e then
                acc
            else
                let
                    val relevant =
                        case ms of
                            [] => None
                          | r :: ms' =>
                            if r.when2 > ymOut cur then
                                None
                            else
                                Some (ms', ds,
                                      case List.assoc r.k2 groups of
                                          None => ((r.k2, {Multiplier = r.multiplier, Count = 0}) :: groups)
                                        | Some _ => List.mp (fn (k', g) => (k', if k' = r.k2 then {Multiplier = r.multiplier, Count = g.Count} else g)) groups)

                    val relevant =
                        case relevant of
                            Some _ => relevant
                          | None =>
                            case ds of
                                [] => None
                              | r :: ds' =>
                                if r.when1 > ymOut cur then
                                    None
                                else
                                    Some (ms, ds',
                                          case List.assoc r.k1 groups of
                                              None => ((r.k1, {Count = r.change, Multiplier = 0}) :: groups)
                                            | Some _ => List.mp (fn (k', g) => (k', if k' = r.k1 then {Count = g.Count + r.change, Multiplier = g.Multiplier} else g)) groups)
                in
                    case relevant of
                        None => assemble (oneMonthLater cur) groups ds ms
                                (List.append
                                     (List.mapPartial (fn (k, g) =>
                                                          let
                                                              val hm = g.Multiplier * g.Count
                                                          in
                                                              if hm = 0 then
                                                                  None
                                                              else
                                                                  Some {When = ymOut cur,
                                                                        What = txt k,
                                                                        HowMuch = hm}
                                                          end) groups)
                                 acc)
                      | Some (ms', ds', groups') => assemble cur groups' ds' ms' acc
                end
    in
        ds <- queryL1 (SELECT deltas.{k1}, deltas.{when1}, deltas.{change}
                       FROM deltas
                       WHERE deltas.{when1} <= {[bounds.To]}
                       ORDER BY deltas.{when1});
        ms <- queryL1 (SELECT multipliers.{k2}, multipliers.{when2}, multipliers.{multiplier}
                       FROM multipliers
                       WHERE multipliers.{when2} <= {[bounds.To]}
                       ORDER BY multipliers.{when2});
        return (assemble b [] ds ms [] :: [])
    end
    
fun merge (lss : list (list entry)) (acc : list entry) : list entry =
    let
        val latest = List.foldl (fn ls latest =>
                                      case ls of
                                          [] => latest
                                        | e :: ls' =>
                                          Some (case latest of
                                                    None => e.When
                                                  | Some latest => max latest e.When))
                                  None lss
    in
        case latest of
            None => acc
          | Some latest =>
            let
                fun findLatest (lss : list (list entry)) (acc : list (list entry)) =
                    case lss of
                        [] => error <xml>Found no ledger entry matching computed latest time!</xml>
                      | [] :: lss' => findLatest lss' acc
                      | (e :: ls) :: lss' =>
                        if e.When = latest then
                            (e, List.revAppend acc (ls :: lss'))
                        else
                            findLatest lss' ((e :: ls) :: acc)

                val (e, lss) = findLatest lss []
            in
                merge lss (e :: acc)
            end
    end

fun compose (a : t) (b : t) bounds =
    a <- a bounds;
    b <- b bounds;
    return (List.append a b)
    
functor Make(M : sig
                 val t : t
                 val authorized : transaction bool
             end) = struct
    open M

    type a = {StartYear : source string,
              EndYear : source string,
              StartMonth : source string,
              EndMonth : source string,
              Months : source (list {Year : int,
                                     Month : Datetime.month,
                                     Balance : int,
                                     Entries : list entry,
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

    fun timeOf y m =
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
                    val p = (y, m)
                in
                    return (p, ymOut p)
                end

    fun generate bounds =
        authed <- authorized;
        if authed then
            t bounds
        else
            error <xml>Access denied</xml>

    fun monthsFromEntries (* current month, as year and month *) cur
                          (* end month *) end_
                          es bal acc =
        if cur = end_ then
            return (List.rev acc)
        else
            let
                fun extractFromMonth es bal acc =
                    case es of
                        [] => ([], bal, acc)
                      | e :: es' =>
                        let
                            val dt = Datetime.fromTime e.When
                        in
                            if (dt.Year, dt.Month) = cur then
                                extractFromMonth es' (bal + e.HowMuch) (e :: acc)
                            else
                                (es, bal, acc)
                        end

                val (remaining, bal, thisMonth) = extractFromMonth es bal []
            in
                expanded <- source False;
                monthsFromEntries (oneMonthLater cur) end_ remaining bal
                                  ({Year = cur.1,
                                    Month = cur.2,
                                    Balance = bal,
                                    Entries = thisMonth,
                                    Expanded = expanded} :: acc)
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
                          (startp, start) <- timeOf a.StartYear a.StartMonth;
                          (endp, end_) <- timeOf a.EndYear a.EndMonth;
                          es <- rpc (generate {From = start, To = addSeconds end_ (31 * 24 * 60 * 60)});
                          ms <- monthsFromEntries startp (oneMonthLater endp) (merge es []) 0 [];
                          set a.Months ms}/>

      <hr/>

      <table class="bs-table table-striped">
        <tr>
          <th class="col-sm-1">Month</th>
          <th class="col-sm-1">Balance</th>
          <th>Entries</th>
        </tr>
        <dyn signal={ms <- signal a.Months;
                     return (List.mapX (fn m => <xml><tr>
                       <td class="col_sm-1">{[m.Year]}/{[Datetime.monthToInt m.Month + 1]}</td>
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
                                                        <th>When</th>
                                                        <th>What</th>
                                                        <th>How Much</th>
                                                      </tr>
                                                      {List.mapX (fn e => <xml><tr>
                                                        <td>{[e.When]}</td>
                                                        <td>{e.What}</td>
                                                        <td>{[e.HowMuch]}</td>
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
