functor Make(M : sig
                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 val access : transaction Discussion.access
                 val showOpenVsClosed : bool
                 val allowPrivate : bool
                 val onNewMessage : {Thread : time, Subject : string, Who : string, Text : string} -> transaction unit
             end) = struct

    open M

    table message : {Thread : time, When : time, Who : string, Text : string}
      PRIMARY KEY(Thread, When)

    open Discussion.Make(struct
                             open M

                             con key = []
                             con thread = #Thread
                             constraint [thread] ~ [When, Who, Text, Closed, Private]
                             val fl = _

                             con message_hidden_constraints = _
                             con empty :: {{Unit}} = []
                             constraint empty ~ message_hidden_constraints
                             val message = message

                             val kinj = {}

                             constraint key ~ [Thread, When, Who, Text, Closed]

                             val access () = access
                         end)

    val ui = ui ()

    functor Todo(N : sig
                     con tag :: Name
                     con user :: Name
                     con aother :: {Type}
                     constraint [user] ~ aother
                                
                     table assignments : ([user = option string] ++ aother)
                                         
                     val title : string
                     val render : {Thread : time} -> string (* username *) -> xbody
                 end) = struct
        open Todo(struct
                      open N

                      constraint [Assignee, Due, Done, Kind] ~ [Thread = time]
                      val inj = _
                  end)
    end

end
