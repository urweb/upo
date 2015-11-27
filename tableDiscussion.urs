(* Specialization of Discussion using foreign keys *)

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 constraint key ~ [Thread, When, Who, Text]
                 val fl : folder key
                 val kinj : $(map sql_injectable_prim key)
                 con rest :: {Type}
                 constraint rest ~ key
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val parent : sql_table (key ++ rest) ([keyName = map (fn _ => ()) key] ++ otherConstraints)

                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 val access : $key -> transaction Discussion.access
                 val showOpenVsClosed : bool
             end) : sig
    include Ui.S where type input = $M.key

    (* Generate todo items for users tasked with responding to messages. *)
    functor Todo(N : sig
                     con tag :: Name
                     con user :: Name
                     con aother :: {Type}
                     constraint M.key ~ aother
                     constraint [user] ~ (M.key ++ aother)
                     constraint [Assignee, Due, Done, Kind] ~ (M.key ++ [Thread = time])
                     val inj : $(map sql_injectable_prim M.key)

                     table assignments : (M.key ++ [user = option string] ++ aother)
                     (* Recording who is responsible for which items *)

                     val title : string
                     val render : $(M.key ++ [Thread = time]) -> string (* username *) -> xbody
                 end) : sig
        type private
        con tag = N.tag
        val todo : Todo.t ([Thread = time] ++ M.key) [tag = private]
    end
end
