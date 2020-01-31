(* Simple toggling of user-specific Boolean flags,
 * also recognizing a separate state for "didn't respond yet" *)

type xbody' = xml [Body] [] []

functor Make(M : sig
                 con user :: Name
                 con flagT :: Name (* set if the user definitely answers "yes" *)
                 con flagF :: Name (* set if the user definitely answers "no" *)
                 con others :: {Type}
                 constraint [flagT] ~ [flagF]
                 constraint [user] ~ [flagT, flagF]
                 constraint [user, flagT, flagF] ~ others
                 table t : ([user = string, flagT = bool, flagF = bool] ++ others)

                 val whoami : transaction (option string)
                 (* Which user is logged in? *)

                 (* Text to show, as heading and button labels, depending on the flag value *)
                 val noAnswerYetHeadingText : xbody
                 val trueHeadingText : xbody
                 val falseHeadingText : xbody
                                        
                 val trueButtonText : xbody'
                 val falseButtonText : xbody'

                 (* Callbacks for when flags change to and away from "yes" *)
                 val onRsvp : string -> transaction unit
                 val onUnrsvp : string -> transaction unit

                 (* For the latter, first run this procedure to check if the user really meant it. *)
                 val confirmUnrsvp : transaction bool
             end) : Ui.S where type input = string (* username *)
