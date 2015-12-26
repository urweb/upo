(* Specialization of Discussion that creates just one global set of threads *)

functor Make(M : sig
                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 val access : transaction Discussion.access
                 val showOpenVsClosed : bool
                 val allowPrivate : bool
                 val onNewMessage : transaction (list string)
                    -> {Thread : time, Subject : string, Who : string, Text : string}
                    -> transaction unit
             end) : sig
    include Ui.S0

    (* Generate todo items for users tasked with responding to messages. *)
    functor Todo(N : sig
                     con tag :: Name
                     con user :: Name
                     con aother :: {Type}
                     constraint [user] ~ aother

                     table assignments : ([user = option string] ++ aother)
                     (* Recording who is responsible for which items *)

                     val title : string
                     val render : {Thread : time} -> string (* username *) -> xbody
                 end) : sig
        type private
        con tag = N.tag
        val todo : Todo.t [Thread = time] [tag = private]
    end
end
