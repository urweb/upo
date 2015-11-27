(* Extensible todo lists: read assigned tasks out of database tables *)

(* Generators of todo entries *)
con t :: {Type}    (* Dictionary of all key fields used across all sources of events *)
         -> {Type} (* Mapping user-meaningful tags (for event kinds) to associated data *)
         -> Type

(* Every user in a certain set must submit something associated with every row of a certain table. *)
functor WithDueDate(M : sig
                        con tag :: Name
                        con key :: {Type}
                        con due :: Name
                        con other :: {Type}
                        con user :: Name
                        con dother :: {Type}
                        con ukey :: Name
                        con uother :: {Type}
                        constraint key ~ other
                        constraint key ~ dother
                        constraint [due] ~ (key ++ other)
                        constraint [user] ~ (key ++ dother)
                        constraint [ukey] ~ uother
                        constraint [Assignee, Due, Done, Kind] ~ key
                        val fl : folder key
                        val inj : $(map sql_injectable_prim key)

                        table items : (key ++ [due = time] ++ other)
                        (* The set of items that must be done *)
                        table done : (key ++ [user = string] ++ dother)
                        (* Recording which users have done which items *)
                        table users : ([ukey = string] ++ uother)
                        (* Full set of users *)
                        val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                        (* Condition to narrow down to the ones who need to do these items *)

                        val title : string
                        val render : $key -> string (* username *) -> xbody
                    end) : sig
    con private
    con tag = M.tag
    val todo : t M.key [tag = private]
end

(* Like above, but we need to follow a foreign-key link to find the due date. *)
functor WithForeignDueDate(M : sig
                               con tag :: Name
                               con key :: {Type}
                               con due :: Name
                               con other :: {Type}
                               con subkey :: {Type}
                               con pother :: {Type}
                               con user :: Name
                               con dother :: {Type}
                               con ukey :: Name
                               con uother :: {Type}
                               constraint key ~ other
                               constraint key ~ dother
                               constraint key ~ pother
                               constraint (key ++ other) ~ subkey
                               constraint [due] ~ (key ++ pother)
                               constraint [user] ~ (key ++ dother)
                               constraint [user] ~ other
                               constraint [ukey] ~ uother
                               constraint subkey ~ dother
                               constraint subkey ~ [user]
                               constraint [Assignee, Due, Done, Kind] ~ (key ++ subkey)
                               constraint [user] ~ (key ++ subkey)
                               val fl : folder key
                               val sfl : folder subkey
                               val inj : $(map sql_injectable_prim key)
                               val sinj : $(map sql_injectable_prim subkey)

                               table items : (key ++ [user = string] ++ subkey ++ other)
                               (* The set of items that must be done *)
                               table parent : (key ++ [due = time] ++ pother)
                               (* Look here for due dates. *)
                               table done : (key ++ subkey ++ [user = string] ++ dother)
                               (* Recording which users have done which items *)
                               table users : ([ukey = string] ++ uother)
                               (* Full set of users *)
                               val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                               (* Condition to narrow down to the ones who need to do these items *)

                               val title : string
                               val render : $(key ++ subkey) -> string (* username *) -> xbody
                           end) : sig
    con private
    con tag = M.tag
    val todo : t (M.key ++ M.subkey) [tag = private]
end

(* Every row in a table, whose Boolean flag is not set, must be done by the associated users, indicated by other table rows. *)
functor WithCompletionFlag(M : sig
                               con tag :: Name
                               con key :: {Type}
                               con subkey :: {Type}
                               con done :: Name
                               con other :: {Type}
                               con user :: Name
                               con aother :: {Type}
                               constraint key ~ subkey
                               constraint (key ++ subkey) ~ other
                               constraint key ~ aother
                               constraint [done] ~ (key ++ subkey ++ other)
                               constraint [user] ~ (key ++ aother)
                               constraint [Assignee, Due, Done, Kind] ~ (key ++ subkey)
                               val fl : folder key
                               val sfl : folder subkey
                               val inj : $(map sql_injectable_prim key)
                               val sinj : $(map sql_injectable_prim subkey)

                               table items : (key ++ subkey ++ [done = bool] ++ other)
                               (* The set of items that must be done *)
                               table assignments : (key ++ [user = option string] ++ aother)
                               (* Recording who is responsible for which items *)

                               val title : string
                               val render : $(key ++ subkey) -> string (* username *) -> xbody
                    end) : sig
    con private
    con tag = M.tag
    val todo : t (M.key ++ M.subkey) [tag = private]
end

(* Every user in a certain set should be aware of the contents of a certain table, interpreted as todos. *)
functor Happenings(M : sig
                       con tag :: Name
                       con key :: {Type}
                       con when :: Name
                       con other :: {Type}
                       con ukey :: Name
                       con uother :: {Type}
                       constraint key ~ other
                       constraint [when] ~ (key ++ other)
                       constraint [ukey] ~ uother
                       constraint [Assignee, Due, Done, Kind] ~ key
                       val fl : folder key
                       val inj : $(map sql_injectable_prim key)

                       table items : (key ++ [when = time] ++ other)
                       (* The set of items that must be done *)
                       table users : ([ukey = string] ++ uother)
                       (* Full set of users *)
                       val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
                       (* Condition to narrow down to the ones who need to do these items *)

                       val title : string
                       val render : $key -> xbody
                   end) : sig
    con private
    con tag = M.tag
    val todo : t M.key [tag = private]
end

val compose : keys1 ::: {Type} -> keys2 ::: {Type}
              -> tags1 ::: {Type} -> tags2 ::: {Type}
              -> [keys1 ~ keys2] => [tags1 ~ tags2]
              => folder keys1 -> folder keys2
              -> $(map sql_injectable_prim keys1)
              -> $(map sql_injectable_prim keys2)
              -> t keys1 tags1 -> t keys2 tags2 -> t (keys1 ++ keys2) (tags1 ++ tags2)

functor Make(M : sig
                 con keys :: {Type}
                 con tags :: {Type}
                 constraint [Assignee, Due, Done, Kind] ~ keys
                 val t : t keys tags
                 val fl : folder tags
             end) : sig
    structure AllUsers : Ui.S0
    structure OneUser : Ui.S where type input = string
end
