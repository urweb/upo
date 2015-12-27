(* Blog-style sequences of posted blurbs *)

style item
style header

datatype access =
         Forbidden
       | Read
       | Post of {User : string, MayEdit : bool, MayDelete : bool}
       | Admin of {User : string}

functor Make(M : sig
                 type title
                 type title_internal
                 type title_config
                 val title : Widget.t title title_internal title_config
                 val title_inj : sql_injectable title

                 type body
                 type body_internal
                 type body_config
                 val body : Widget.t body body_internal body_config
                 val body_inj : sql_injectable body

                 val access : transaction access

                 val onNewPost : {Title : title, Poster : string, Body : body}
                                 -> transaction unit
                 (* Callback for every new post *)
             end) : Ui.S0
