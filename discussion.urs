(* Discussions with topics described by a flexible type of keys *)

(* Levels of authorization to interact with the list of posts *)
datatype access =
         Forbidden
         (* May not have anything to do with this key *)
       | Read
         (* May only read posts *)
       | Post of { User : string, MayEdit : bool, MayDelete : bool}
         (* May read and make own posts, possibly with the ability to edit or delete own posts *)
       | Admin of { User : string }
         (* Full privileges *)

style post
style post_header
style post_body

functor Make(M : sig
                 con key :: {Type}
                 constraint key ~ [Thread, When, Who, Text]
                 val fl : folder key
                 val kinj : $(map sql_injectable key)

                 type text
                 type text_internal
                 type text_config
                 val text : Widget.t text text_internal text_config
                 val inj : sql_injectable text

                 table message : (key ++ [Thread = time, When = time, Who = string, Text = text])

                 val access : $key -> transaction access
             end) : Ui.S where type input = $M.key
