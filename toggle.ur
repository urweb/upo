open Bootstrap4

type xbody' = xml [Body] [] []

functor Make(M : sig
                 con user :: Name
                 con flag :: Name
                 con others :: {Type}
                 constraint [user] ~ [flag]
                 constraint [user, flag] ~ others
                 table t : ([user = string, flag = bool] ++ others)

                 val whoami : transaction (option string)

                 val trueHeadingText : xbody
                 val trueButtonText : xbody'
                 val falseHeadingText : xbody
                 val falseButtonText : xbody'

                 val onRsvp : string -> transaction unit
                 val onUnrsvp : string -> transaction unit

                 val confirmUnrsvp : transaction bool
             end) = struct
    open M

    type input = string
    type a = source bool

    fun create u =
        b <- oneOrNoRowsE1 (SELECT (t.{flag})
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
                 SET {flag} = TRUE
                 WHERE t.{user} = {[me]});
            onRsvp me

    val unrsvp =
        me <- whoami;
        case me of
            None => error <xml>Toggle: not logged in</xml>
          | Some me => 
            dml (UPDATE t
                 SET {flag} = FALSE
                 WHERE t.{user} = {[me]});
            onUnrsvp me

    fun render _ a = <xml>
      <dyn signal={av <- signal a;
                   return (if av then <xml>
                     {trueHeadingText}
                     <button class="btn btn-primary"
                             onclick={fn _ =>
                                         doit <- confirmUnrsvp;
                                         if doit then
                                             rpc unrsvp;
                                             set a False
                                         else
                                             return ()}>
                       {trueButtonText}
                     </button>
                   </xml> else <xml>
                     {falseHeadingText}
                     <button class="btn btn-primary"
                             onclick={fn _ =>
                                         rpc rsvp;
                                         set a True}>
                       {falseButtonText}
                     </button>
                   </xml>)}/>
    </xml>

    fun ui u = {Create = create u,
                Onload = onload,
                Render = render}
end
