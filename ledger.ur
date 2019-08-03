open Bootstrap4

type entry = {When : time,
              What : xbody,
              HowMuch : int}
type t = {From : time, To : time} -> transaction (list (list entry))
(* Invariant: each output sublist is sorted by When in descending order,
 * so we will be able to do an efficient merge into one sorted list. *)
 
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
                in
                    return (y, m,
                            Datetime.toTime {Year = y,
                                             Month = m,
                                             Day = 1,
                                             Hour = 0,
                                             Minute = 0,
                                             Second = 0})
                end

    fun generate bounds =
        authed <- authorized;
        if authed then
            t bounds
        else
            error <xml>Access denied</xml>

    fun monthsFromEntries (* current month, as year and month *) cy cm
                          (* end month *) ey em
                          es bal acc =
        if (if em = Datetime.December then
                cy = ey+1 && cm = Datetime.January
            else
                cy = ey && cm = Datetime.intToMonth (Datetime.monthToInt em + 1)) then
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
                            if dt.Year = cy && dt.Month = cm then
                                extractFromMonth es' (bal + e.HowMuch) (e :: acc)
                            else
                                (es, bal, acc)
                        end

                val (remaining, bal, thisMonth) = extractFromMonth es bal []

                val (ny, nm) =
                    if cm = Datetime.December then
                        (cy+1, Datetime.January)
                    else
                        (cy, Datetime.intToMonth (Datetime.monthToInt cm + 1))
            in
                expanded <- source False;
                monthsFromEntries ny nm ey em remaining bal
                                  ({Year = cy,
                                    Month = cm,
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
                          (sy, sm, start) <- timeOf a.StartYear a.StartMonth;
                          (ey, em, end_) <- timeOf a.EndYear a.EndMonth;
                          es <- rpc (generate {From = start, To = addSeconds end_ (31 * 24 * 60 * 60)});
                          ms <- monthsFromEntries sy sm ey em (merge es []) 0 [];
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
