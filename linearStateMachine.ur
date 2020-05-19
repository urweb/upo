(* Moving through steps in the life cycle of an application *)

open Bootstrap4

datatype activatedAs = NextStep | FastForward | Rewind

type metadata = {Label : string,
                 WhenEntered : activatedAs -> transaction unit}

style downArrow
style label

functor Make(M : sig
                 con steps :: {Unit}
                 val fl : folder steps

                 val mayChange : transaction bool
             end) = struct

    open M

    type step = variant (mapU unit steps)

    fun toInt (s : step) =
        match s
              (@fold [fn r => int * $(mapU (unit -> int) r)]
                (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] p =>
                    (p.1-1,
                     p.2 ++ {nm = fn () => p.1}))
                (0, {}) fl).2

    fun fromInt (n : int) =
        (@fold [fn r => int * (r' :: {Unit} -> [r' ~ r] => option (variant (mapU unit (r ++ r'))))]
          (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (p : int * (r' :: {Unit} -> [r' ~ r] => option (variant (mapU unit (r ++ r'))))) =>
              (p.1-1,
               if p.1 = n then
                fn [r' ::_] [r' ~ [nm] ++ r] => Some (make [nm] ())
               else
                fn [r' ::_] [r' ~ [nm] ++ r] => p.2 [[nm] ++ r'] !))
          (0, fn [r' ::_] [r' ~ []] => None) fl).2 [[]] !

    val step_eq = mkEq (fn s1 s2 => toInt s1 = toInt s2)
    val step_ord = mkOrd {Lt = fn s1 s2 => toInt s1 < toInt s2,
                          Le = fn s1 s2 => toInt s1 <= toInt s2}
    fun next (s : step) : option step = fromInt (toInt s + 1)

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

    functor MakeUi(N : sig
                       val steps : $(mapU metadata steps)
                   end) = struct
        open N

        table listeners : { Channel : channel step }

        type a = {Step : source step,
                  Channel : channel step}

        val create =
            st <- current;
            sts <- source st;
            ch <- channel;
            dml (INSERT INTO listeners(Channel) VALUES ({[ch]}));
            return {Step = sts, Channel = ch}

        fun onload a =
            let
                fun loop () =
                    st <- recv a.Channel;
                    set a.Step st;
                    loop ()
            in
                spawn (loop ())
            end

        fun change {From = fr, To = to} =
            mc <- mayChange;
            if not mc then
                error <xml>Access denied</xml>
            else
                st <- current;
                if st <> fr then
                    error <xml>Somebody else beat you to it!</xml>
                else
                    dml (UPDATE step
                         SET Step = {[serialize to]}
                         WHERE TRUE);
                    queryI1 (SELECT * FROM listeners)
                    (fn r => send r.Channel to);

                    @Record.select [fn _ => metadata] [fn _ => unit] fl
                    (fn [u] r () => r.WhenEntered (if next fr = Some to then
                                                       NextStep
                                                   else if to < fr then
                                                       Rewind
                                                   else
                                                       FastForward))
                    steps to

        val labelOf = @Record.select [fn _ => metadata] [fn _ => unit] fl
                       (fn [u] r () => r.Label)
                       steps

        fun render ctx a = <xml>
          <table>
            {@Variant.withAllX fl
              (fn st' => <xml>
                <tr>
                  <td></td>
                  <td class={downArrow}><div class="glyphicon glyphicon-arrow-down"></div></td>
                </tr>
                <tr>
                  <td>
                    <div dynClass={cur <- signal a.Step;
                                   return (if cur = st' then
                                               CLASS "glyphicon glyphicon-arrow-right"
                                           else
                                               CLASS "")}></div>
                  </td>
                  <td class={label}>
                    <dyn signal={cur <- signal a.Step;
                                 return (if cur = st' then
                                             txt (labelOf cur)
                                         else
                                             Ui.modalButton ctx (CLASS "btn btn-secondary")
                                                            (txt (labelOf st'))
                                                            (return (Ui.modal
                                                                         (rpc (change {From = cur, To = st'}))
                                                                         (if next cur = Some st' then
                                                                              <xml>Are you sure you want to advance to the next step?</xml>
                                                                          else if st' < cur then
                                                                              <xml>Warning: traveling back in time!</xml>
                                                                          else
                                                                              <xml>Warning: fast-forwarding through time!</xml>)
                                                                         <xml>
                                                                           You are about to move from <b>{[labelOf cur]}</b> to <b>{[labelOf st']}</b>.
                                                                         </xml>
                                                                         <xml>Change</xml>)))}/>
                  </td>
                </tr>
              </xml>)}
          </table>
        </xml>

        fun notification _ = <xml></xml>

        val ui = {Create = create,
                  Onload = onload,
                  Render = render,
                  Notification = notification}
    end
end
