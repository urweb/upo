(* Generic authentication backed by a database table *)

signature S = sig
    con groups :: {Unit}

    val whoami : transaction (option string)
    (* Which user (by name), if any, is logged in? *)

    val getUser : transaction string
    val requireUser : transaction unit
    (* Like above, but fails if not logged in *)

    val inGroup : variant (mapU unit groups) -> transaction bool
    (* Does the current user belong to the specified group? *)

    val requireGroup : variant (mapU unit groups) -> transaction unit
    (* Fail if the current user doesn't to the group. *)

    val getGroup : variant (mapU unit groups) -> transaction string
    (* Like above, but also returns username. *)

    val inGroups : dummy ::: {Unit} -> folder dummy
                   -> $(mapU (variant (mapU unit groups)) dummy) -> transaction bool
    val requireGroups : dummy ::: {Unit} -> folder dummy
                        -> $(mapU (variant (mapU unit groups)) dummy) -> transaction unit
    val getGroups : dummy ::: {Unit} -> folder dummy
                    -> $(mapU (variant (mapU unit groups)) dummy) -> transaction string
    (* Like the above, but based on checking whether the user belongs to at least one of a set of groups *)        
end

functor Make(M : sig
                 con name :: Name
                 (* Which column gives us the primary identifier for a user? *)

                 con setThese :: {Type}
                 (* Based on the underlying authentication method, we also learn the values of these fields.
                  * Example: client SSL certificate gives additional personal data. *)

                 con groups :: {Unit}
                 (* Boolean flags indicating membership in classes of users *)

                 con others :: {Type}
                 (* Miscellaneous remaining fields of the users table *)

                 constraint [name] ~ setThese
                 constraint ([name] ++ map (fn _ => ()) setThese) ~ groups
                 constraint ([name] ++ map (fn _ => ()) setThese ++ groups) ~ others

                 table users : ([name = string] ++ setThese ++ mapU bool groups ++ others)

                 val underlying : transaction (option $([name = string] ++ setThese))
                 (* Data of confirmed current user, if any *)

                 val defaults : option $(mapU bool groups ++ others)
                 (* If provided, automatically creates accounts for unknown usernames.
                  * Fields are initialized from these defaults. *)

                 val fls : folder setThese
                 val flg : folder groups
                 val flo : folder others

                 val injs : $(map sql_injectable setThese)
                 val injo : $(map sql_injectable others)

                 val eqs : $(map eq setThese)
             end) : S where con groups = M.groups
