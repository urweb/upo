(* Generic authentication backed by a database table *)

signature S = sig
    con groups :: {Unit}

    val whoami : transaction (option string)
    (* Which user (by name), if any, is logged in? *)
    val whoamiWithMasquerade : transaction (option string)
    (* Might return a fake, if the user is masquerading. *)

    val getUser : transaction string
    val getUserWithMasquerade : transaction string
    val requireUser : transaction unit
    (* Like above, but fails if not logged in *)

    val masqueradeAs : string -> transaction unit
    val unmasquerade : transaction unit
    (* Begin/end masquerading *)

    val inGroup : variant (mapU unit groups) -> transaction bool
    val inGroupWithMasquerade : variant (mapU unit groups) -> transaction bool
    (* Does the current user belong to the specified group? *)

    val requireGroup : variant (mapU unit groups) -> transaction unit
    (* Fail if the current user doesn't to the group. *)

    val getGroup : variant (mapU unit groups) -> transaction string
    (* Like above, but also returns username. *)
    val getGroupWithMasquerade : variant (mapU unit groups) -> transaction string
    (* Like last one, but will return fake name if masquerading. *)

    val inGroups : dummy ::: {Unit} -> folder dummy
                   -> $(mapU (variant (mapU unit groups)) dummy) -> transaction bool
    val requireGroups : dummy ::: {Unit} -> folder dummy
                        -> $(mapU (variant (mapU unit groups)) dummy) -> transaction unit
    val getGroups : dummy ::: {Unit} -> folder dummy
                    -> $(mapU (variant (mapU unit groups)) dummy) -> transaction string
    val getGroupsWithMasquerade : dummy ::: {Unit} -> folder dummy
                                  -> $(mapU (variant (mapU unit groups)) dummy) -> transaction string
    (* Like the above, but based on checking whether the user belongs to at least one of a set of groups *)


    (* Help setting up masquerades with nice UIs *)
    functor Masquerade(N : sig
                           con fs :: {Type}
                           con tab :: Name
                           val query : sql_query [] [] [tab = fs] []
                           val fl : folder fs
                           val render : $fs -> xbody
                           val target : $fs -> transaction page
                       end) : Ui.S0
end

functor Make(M : sig
                 con name :: Name
                 (* Which column gives us the primary identifier for a user? *)

                 con key :: {Type}
                 (* Based on the underlying authentication method, we also learn the values of these fields.
                  * Example: client SSL certificate gives additional personal data. *)

                 con groups :: {Unit}
                 (* Boolean flags indicating membership in classes of users *)

                 con others :: {Type}
                 (* Miscellaneous remaining fields of the users table *)

                 constraint [name] ~ key
                 constraint ([name] ++ map (fn _ => ()) key) ~ groups
                 constraint ([name] ++ map (fn _ => ()) key ++ groups) ~ others

                 table users : ([name = string] ++ key ++ mapU bool groups ++ others)

                 val underlying : transaction (option $([name = string] ++ key))
                 (* Data of confirmed current user, if any *)

                 val defaults : option $(mapU bool groups ++ others)
                 (* If provided, automatically creates accounts for unknown usernames.
                  * Fields are initialized from these defaults. *)

                 val allowMasquerade : option (list (variant (mapU unit groups)))
                 (* If present, members of this group can pretend to be anyone else.
                  * We assume that this is an uber-group that will always pass access-control checks! *)

                 val requireSsl : bool

                 val fls : folder key
                 val flg : folder groups
                 val flo : folder others

                 val injs : $(map sql_injectable key)
                 val injo : $(map sql_injectable others)

                 val eqs : $(map eq key)
             end) : S where con groups = M.groups
