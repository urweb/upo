(* Combinators to describe what extra goodies to show in a FullCalendar widget *)

con t :: {Type} (* fields of rows we are visualizing *) -> Type
val empty : r ::: {Type} -> t r
val compose : r ::: {Type} -> t r -> t r -> t r

type event_data = {
     Id : option string,
     Start : time,
     End : option time,
     Title : string,
     Background : bool,
     TextColor : option string,
     BackgroundColor : option string
}

val extraEvents : r ::: {Type} -> t r -> option string (* user *) -> transaction (list event_data)
val aboveCalendar : r ::: {Type} -> t r -> Ui.context -> FullCalendar.t -> xbody
val withinEvent : r ::: {Type} -> t r -> Ui.context -> FullCalendar.t -> FullCalendar.event -> {Header : xbody, Body : xbody}
val maySee : r ::: {Type} -> t r -> tab :: Name -> tab' :: Name -> [[tab] ~ [tab']] => option string (* user *) -> option (sql_exp [tab = r] [] [] bool)
val mayEdit : r ::: {Type} -> t r -> tab :: Name -> tab' :: Name -> [[tab] ~ [tab']] => option string (* user *) -> option (sql_exp [tab = r] [] [] bool)

functor EventSource(M : sig
                        con r :: {Type}
                        con key :: Name
                        type keyT
                        con tm :: Name
                        con others :: {Type}
                        constraint [key] ~ [tm]
                        constraint [key, tm] ~ others
                        con keyName :: Name
                        con otherKeys :: {{Unit}}
                        constraint [keyName] ~ otherKeys
                        val source : sql_table ([key = keyT, tm = option time] ++ others)
                                               ([keyName = [key]] ++ otherKeys)
                        val title : string
                        val show_key : show keyT
                        val prefix : string (* string to put before title *)
                        val background : bool
                        val textColor : option string
                        val backgroundColor : option string
                        val addons : t ([key = keyT, tm = option time] ++ others)
                        val duration : option string
                    end) : sig
    val t : t M.r
end

functor OnlyOwnerMaySee(M : sig
                            con owner :: Name
                            con r :: {Type}
                            constraint [owner] ~ r
                        end) : sig
    val t : t ([M.owner = option string] ++ M.r)
end

functor OnlyOwnerMayEdit(M : sig
                             con owner :: Name
                             con r :: {Type}
                             constraint [owner] ~ r
                         end) : sig
    val t : t ([M.owner = option string] ++ M.r)
end

functor ConnectedMaySee(M : sig
                            con key :: Name
                            type keyT
                            con r :: {Type}
                            constraint [key] ~ r
                            con keyLabels = [key]

                            con cev :: Name
                            con cuser :: Name
                            con cother :: {Type}
                            constraint [cev] ~ [cuser]
                            constraint [cev, cuser] ~ cother
                            table ctr : ([cev = keyT, cuser = string] ++ cother)
                        end) : sig
    val t : t ([M.key = M.keyT] ++ M.r)
end

functor ShowExternalCalendar(M : sig
                                 con r :: {Type}

                                 type calendar_id
                                 val calendars : transaction (list (calendar_id * string (* description *)))
                                 (* Retrieve user's own calendars. *)
                                 val events : calendar_id -> {Min : option time, Max : option time}
                                              -> transaction (list {Summary : option string, Start : option time, End : option time})
                                 (* Get all events (in some time wondow) from a calendar. *)

                                 val label : string
                                 (* Text on button to show calendars *)
                                 val idPrefix : string
                                 (* Used for internal ID generation *)
                             end) : sig
    val t : t M.r
end
