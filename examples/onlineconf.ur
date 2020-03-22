structure Theme = Ui.Make(Default)

table user : { Username : string,
               EmailAddress : string }
  PRIMARY KEY Username

table category : { Category : string }
  PRIMARY KEY Category

table paper : { Title : string }
  PRIMARY KEY Title

table author : { Paper : string,
                 User : string,
                 SeqNum : int }
  PRIMARY KEY (Paper, User),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE

table paperCategory : { Paper : string,
                        Category : string }
  PRIMARY KEY (Paper, Category),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Category FOREIGN KEY Category REFERENCES category(Category) ON UPDATE CASCADE ON DELETE CASCADE

table slot : { Begin : time,
               End : time }
  PRIMARY KEY Begin

structure Slots = FillTimeRange.Make(struct
                                         val slot = slot
                                         val initial = readError "2020-4-1 0:00:00"
                                         val final = readError "2020-4-3 11:59:59"
                                         val duration = 1800
                                     end)
             
table preference : { User : string,
                     Slot : time,
                     Available : bool,
                     Preferred : bool }
  PRIMARY KEY (User, Slot),
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Slot FOREIGN KEY Slot REFERENCES slot(Begin),
  CONSTRAINT Preferred CHECK NOT (Preferred AND NOT Available)
      
open Explorer

cookie userC : string
val whoami = getCookie userC

structure Exp = Make(struct
                         structure Theme = Default

                         val title = "Papers"

                         val t = none
                                     |> one [#User] [#Username] user "Users" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Category] [#Category] category "Categories" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Paper] [#Title] paper "Papers" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Slot] [#Begin] slot "Time slots" (return <xml></xml>) (Default (WHERE TRUE))

                                     |> text [#User] [#Username] "Name"
                                     |> text [#User] [#EmailAddress] "E-mail address"

                                     |> text [#Category] [#Category] "Name"

                                     |> text [#Paper] [#Title] "Title"
                                     |> manyToManyOrdered [#Paper] [#Title] [#Paper] [#User] [#Username] [#User] author "Authors" "Papers" {}
                                     |> manyToMany [#Paper] [#Title] [#Paper] [#Category] [#Category] [#Category] paperCategory "Categories" "Papers" {}

                                     |> text [#Slot] [#Begin] "Begins"
                                     |> text [#Slot] [#End] "Ends"

                         fun authorize _ = return True

                         val preTabs = {}
                         val postTabs = {}
                         val hiddenTabs = {}
                     end)

structure Prefs = Preferences.Make(struct
                                       con choice = #Begin
                                       val choice = slot

                                       con user = #User
                                       con slot = #Slot
                                       con available = #Available
                                       con preferred = #Preferred
                                       val pref = preference

                                       val whoami = whoami
                                   end)

val explore = Exp.index (make [#Category] ())

fun login {Nam = s} =
    ex <- oneRowE1 (SELECT COUNT( * ) > 0
                    FROM user
                    WHERE user.Username = {[s]});
    (if ex then
         return ()
     else
         dml (INSERT INTO user(Username, EmailAddress)
              VALUES ({[s]}, "")));
    
    setCookie userC {Value = s, Expires = None, Secure = False};
    redirect (url (main ()))

and main () =
    u <- whoami;
    case u of
        None => Theme.simple "OnlineConf"
                (Ui.const <xml>
                  <h3>Better log in!</h3>

                  <form>
                    <textbox{#Nam}/>
                    <submit action={login} value="Log in"/>
                  </form>
                </xml>)
      | Some u =>
        Theme.tabbed "OnlineConf"
        ((Some "Explore", Ui.h4 <xml><a link={explore}>Begin exploring!</a></xml>),
         (Some "Availability", Prefs.ui u))
