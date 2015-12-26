(* Discussions with topics described by a flexible type of keys *)

(* Levels of authorization to interact with the list of posts *)
datatype access =
         Forbidden
         (* May not have anything to do with this key *)
       | Read
         (* May only read posts *)
       | Post of { User : string, MayEdit : bool, MayDelete : bool, MayMarkClosed : bool}
         (* May read and make own posts, possibly with the ability to edit, delete, or close own posts/threads *)
       | Admin of { User : string }
         (* Full privileges *)

style post
style post_header
style post_body

functor Make(M : sig
                 con key :: {Type}
                 con thread :: Name
                 constraint [thread] ~ [When, Who, Text, Closed, Private, Subject]
                 constraint key ~ [thread, When, Who, Text, Closed, Private]
                 val fl : folder key
                 val kinj : $(map sql_injectable key)

                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 table message : (key ++ [thread = time, When = time, Who = string, Text = string])

                 val access : $key -> transaction access

                 val showOpenVsClosed : bool
                 (* Should we expose to users the idea of marking threads open/closed? *)

                 val allowPrivate : bool
                 (* May users create threads visible only to themselves and admins? *)

                 val onNewMessage : transaction (list string)
                    (* Run to get list of all users who have posted in the thread. *)
                    -> {thread : time, Subject : string, Who : string, Text : string}
                    -> transaction unit
                 (* Callback for every new message posted in any thread *)
             end) : sig
    include Ui.S where type input = $M.key

    con thread = M.thread

    (* Generate todo items for users tasked with responding to messages. *)
    functor Todo(N : sig
                     con tag :: Name
                     con user :: Name
                     con aother :: {Type}
                     constraint M.key ~ aother
                     constraint [user] ~ (M.key ++ aother)
                     constraint [Assignee, Due, Done, Kind] ~ (M.key ++ [thread = time])
                     val inj : $(map sql_injectable_prim M.key)

                     table assignments : (M.key ++ [user = option string] ++ aother)
                     (* Recording who is responsible for which items *)

                     val title : string
                     val render : $(M.key ++ [thread = time]) -> string (* username *) -> xbody
                 end) : sig
        type private
        con tag = N.tag
        val todo : Todo.t ([thread = time] ++ M.key) [tag = private]
    end
end
