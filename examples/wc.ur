structure Theme = Ui.Make(Default)

table user : { User : string }
  PRIMARY KEY User

val show_user : show {User : string} = mkShow (fn r => r.User)

table widget : { Title : string, Size : int }
  PRIMARY KEY Title

cookie userC : { User : string }

structure Commented = WithComments.Make(struct
                                            val key = widget
                                            val user = user

                                            val query = (SELECT (widget.Title) AS Title,
                                                           (widget.Size) AS Size
                                                         FROM widget)

                                            fun render r = <xml><b>{[r.Title]}</b> ({[r.Size]})</xml>

                                            val amUser = getCookie userC
                                        end)

structure Widgets = EditableTable.Make(struct
                                           val tab = widget
                                           val labels = {Title = "Title",
                                                         Size = "Size"}
                                           val permission = return {Add = True,
                                                                    Delete = True,
                                                                    Modify = True}

                                           fun shrink w = w -- #Size
                                           fun onAdd w = Commented.add (shrink w)
                                           fun onDelete w = Commented.delete (shrink w)
                                           fun onModify w = Commented.modify {Old = shrink w.Old,
                                                                              New = shrink w.New}
                                       end)

fun auth s =
    alreadyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                              FROM user
                              WHERE user.User = {[s]});
    (if alreadyThere then
         return ()
     else
         dml (INSERT INTO user(User) VALUES ({[s]})));
    setCookie userC {Value = {User = s},
                     Expires = None,
                     Secure = False}

val main =
    newuname <- source "";
    uname <- getCookie userC;
    Theme.tabbed "OV"
    ((Some "Login",
      Ui.const <xml>
        <ctextbox source={newuname}/>
        <button value="Set" onclick={fn _ => s <- get newuname; rpc (auth s)}/>
      </xml>),
     (Some "Widgets",
      Widgets.ui),
     (Some "Commented",
      Commented.ui uname))
