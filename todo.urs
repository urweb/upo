(* Extensible todo lists: read assigned tasks out of database tables *)

(* Generators of todo entries *)
con t :: {Type}    (* Dictionary of all key fields used across all sources of events *)
         -> {Type} (* Mapping user-meaningful tags (for event kinds) to associated data *)
         -> Type

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
                        val flO : folder other
                        val inj : $(map sql_injectable_prim key)
                        val injO : $(map sql_injectable_prim other)
                        table items : (key ++ [due = time] ++ other)
                        table done : (key ++ [user = string] ++ dother)
                        table users : ([ukey = string] ++ uother)
                        val eqs : $(map eq key)
                        val title : string
                        val sh : show $key
                        val ucond : sql_exp [Users = [ukey = string] ++ uother] [] [] bool
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
