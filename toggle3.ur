open Bootstrap4

type xbody' = xml [Body] [] []

functor Make(M : sig
                 con user :: Name
                 con flagT :: Name
                 con flagF :: Name
                 con others :: {Type}
                 constraint [flagT] ~ [flagF]
                 constraint [user] ~ [flagT, flagF]
                 constraint [user, flagT, flagF] ~ others
                 table t : ([user = string, flagT = bool, flagF = bool] ++ others)

                 val whoami : transaction (option string)

                 val noAnswerYetHeadingText : xbody
                 val trueHeadingText : xbody
                 val falseHeadingText : xbody

                 val trueButtonText : xbody'
                 val falseButtonText : xbody'

                 val onRsvp : string -> transaction unit
                 val onUnrsvp : string -> transaction unit

                 val confirmUnrsvp : transaction bool
             end) = struct
    open M

    type input = string
    type a = source {flagT : bool, flagF : bool}

    fun create u =
        b <- oneOrNoRows1 (SELECT t.{flagT}, t.{flagF}
                           FROM t
                           WHERE t.{user} = {[u]});
        case b of
            None => error <xml>Toggle: user not found.</xml>
          | Some b => source b

    fun onload _ = return ()

    val rsvp =
        me <- whoami;
        case me of
            None => error <xml>Toggle: not logged in</xml>
          | Some me =>
            dml (UPDATE t
                 SET {flagT} = TRUE, {flagF} = FALSE
                 WHERE t.{user} = {[me]});
            onRsvp me

    val unrsvp =
        me <- whoami;
        case me of
            None => error <xml>Toggle: not logged in</xml>
          | Some me => 
            dml (UPDATE t
                 SET {flagT} = FALSE, {flagF} = TRUE
                 WHERE t.{user} = {[me]});
            onUnrsvp me

    fun render _ a = <xml>
      <dyn signal={av <- signal a;
                   return <xml>
                     {if av.flagT then
                          trueHeadingText
                      else if av.flagF then
                          falseHeadingText
                      else
                          noAnswerYetHeadingText}
                     {if not av.flagT then
                          <xml>
                            <button class="btn btn-primary"
                                    onclick={fn _ =>
                                                rpc rsvp;
                                                set a {flagT = True, flagF = False}}>
                              {falseButtonText}
                            </button>
                          </xml>
                      else
                          <xml></xml>}
                     {if not av.flagF then
                          <xml>
                            <button class="btn btn-primary"
                                    onclick={fn _ =>
                                                doit <- confirmUnrsvp;
                                                if doit then
                                                    rpc unrsvp;
                                                    set a {flagT = False, flagF = True}
                                                else
                                                    return ()}>
                              {trueButtonText}
                            </button>
                          </xml>
                      else
                          <xml></xml>}
                   </xml>}/>
    </xml>

    fun notification _ = <xml></xml>

    fun ui u = {Create = create u,
                Onload = onload,
                Render = render,
                Notification = notification}
end
