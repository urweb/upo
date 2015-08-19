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
