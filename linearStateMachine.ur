(* Moving through steps in the life cycle of an application *)

open Bootstrap3

datatype activatedAs = NextStep | FastForward | Rewind

type metadata = {Label : string,
                 WhenEntered : activatedAs -> transaction unit}

style downArrow
style label

functor Make(M : sig
                 con steps :: {Unit}
                 val fl : folder steps

                 val steps : $(mapU metadata steps)
             end) = struct
    
    open M

    type step = variant (mapU unit steps)

    fun toInt (s : step) =
        match s
              (@fold [fn r => int * $(mapU (unit -> int) r)]
                (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (n, r) =>
                    (n-1,
                     r ++ {nm = fn () => n}))
                (0, {}) fl).2

    val step_eq = mkEq (fn s1 s2 => toInt s1 = toInt s2)
    val step_ord = mkOrd {Lt = fn s1 s2 => toInt s1 < toInt s2,
                          Le = fn s1 s2 => toInt s1 <= toInt s2}

    table step : { Step : serialized step }

    val firstStep =
        case @fold [fn r => option (variant (mapU unit r))]
             (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] _ =>
                 Some (make [nm] ()))
             None fl of
            None => error <xml>Empty state machine passed to LinearStateMachine.Make</xml>
          | Some x => x

    task initialize = fn () =>
         b <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM step);
         if b then
             return ()
         else
             dml (INSERT INTO step(Step)
                  VALUES ({[serialize firstStep]}))

    val current =
        s <- oneRowE1 (SELECT (step.Step)
                       FROM step);
        return (deserialize s)

    type a = source step

    val create =
        st <- current;
        source st

    fun onload _ = return ()

    fun render _ st = <xml>
      <table>
        {@Variant.withAllX fl
          (fn st' => <xml>
            <tr>
              <td></td>
              <td class={downArrow}><div class="glyphicon glyphicon-arrow-down"></div></td>
            </tr>
            <tr>
              <td>
                <div dynClass={cur <- signal st;
                               return (if cur = st' then
                                           CLASS "glyphicon glyphicon-arrow-right"
                                       else
                                           CLASS "")}></div>
              </td>
              <td class={label}>
                <button dynClass={cur <- signal st;
                                  return (if cur = st' then
                                              CLASS ""
                                          else
                                              CLASS "btn")}>{[@Record.select [fn _ => metadata] [fn _ => unit] fl
                                                               (fn [t] r () => r.Label)
                                                               steps st']}</button></td>
            </tr>
          </xml>)}
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
