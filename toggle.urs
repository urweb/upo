(* Simple toggling of user-specific Boolean flags *)

type xbody' = xml [Body] [] []

functor Make(M : sig
                 con user :: Name
                 con flag :: Name
                 con others :: {Type}
                 constraint [user] ~ [flag]
                 constraint [user, flag] ~ others
                 table t : ([user = string, flag = bool] ++ others)

                 val whoami : transaction (option string)
                 (* Which user is logged in? *)

                 (* Text to show, as a heading and as a button label, depending on the flag value *)
                 val trueHeadingText : xbody
                 val trueButtonText : xbody'
                 val falseHeadingText : xbody
                 val falseButtonText : xbody'

                 (* Callbacks for when flags change *)
                 val onRsvp : string -> transaction unit
                 val onUnrsvp : string -> transaction unit
             end) : Ui.S where type input = string (* username *)
