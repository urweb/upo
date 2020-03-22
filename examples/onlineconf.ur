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
                                         val initial = readError "2020-4-1 12:00:00"
                                         val final = readError "2020-4-1 17:00:00"
                                         val duration = 3600
                                     end)
             
table preference : { User : string,
                     Slot : time,
                     Preferred : bool }
  PRIMARY KEY (User, Slot),
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Slot FOREIGN KEY Slot REFERENCES slot(Begin)

table speakingInterest : { Title : string,
                           User : string,
                           Preferred : bool }
  PRIMARY KEY (Title, User),
  CONSTRAINT Paper FOREIGN KEY Title REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE ON DELETE CASCADE

table moderatingInterest : { Title : string,
                             User : string,
                             Preferred : bool }
  PRIMARY KEY (Title, User),
  CONSTRAINT Paper FOREIGN KEY Title REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE ON DELETE CASCADE
  
table talk : { Title : string,
               Speaker : string,
               Moderator : string }
  PRIMARY KEY Title,
  CONSTRAINT Paper FOREIGN KEY Title REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Speaker FOREIGN KEY Speaker REFERENCES user(Username) ON UPDATE CASCADE,
  CONSTRAINT Moderator FOREIGN KEY Moderator REFERENCES user(Username) ON UPDATE CASCADE

table talkTime : { Title : string,
                   Begin : time }
  PRIMARY KEY Title,
  CONSTRAINT Paper FOREIGN KEY Title REFERENCES talk(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT Begin FOREIGN KEY Begin REFERENCES slot(Begin)
      
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
                                     |> one [#Talk] [#Title] talk "Talks" (return <xml></xml>) (Default (WHERE TRUE))

                                     |> text [#User] [#Username] "Name"
                                     |> text [#User] [#EmailAddress] "E-mail address"

                                     |> text [#Category] [#Category] "Name"

                                     |> text [#Paper] [#Title] "Title"
                                     |> manyToManyOrdered [#Paper] [#Title] [#Paper] [#User] [#Username] [#User] author "Authors" "Papers" {}
                                     |> manyToMany [#Paper] [#Title] [#Paper] [#Category] [#Category] [#Category] paperCategory "Categories" "Papers" {}

                                     |> text [#Slot] [#Begin] "Begins"
                                     |> text [#Slot] [#End] "Ends"

                                     |> foreign [#Talk] [#Title] [#Paper] [#Title] "Paper" "Talks"
                                     |> foreign [#Talk] [#Speaker] [#User] [#Username] "Speaker" "Speaker for"
                                     |> foreign [#Talk] [#Moderator] [#User] [#Username] "Moderator" "Moderator for"
                                     |> manyToMany [#Talk] [#Title] [#Title] [#Slot] [#Begin] [#Begin] talkTime "Times" "Talks" {}

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
                                       con preferred = #Preferred
                                       val pref = preference

                                       val whoami = whoami
                                   end)

structure SlotPref = ChoicesFromPreferences.Make(struct
                                                     con choice = #Begin
                                                     val choice = slot

                                                     con user = #User
                                                     con slot = #Slot
                                                     con preferred = #Preferred
                                                     val pref = preference

                                                     con item = #Title
                                                     con users = [Speaker, Moderator]
                                                     val item = talk
                                                     val labels = {Speaker = "Speaker",
                                                                   Moderator = "Moderator"}

                                                     val itemChoice = talkTime

                                                     val authorize = return True
                                                 end)

structure SpeakerInterest = Preferences.Make(struct
                                                 con choice = #Title
                                                 val choice = paper

                                                 con user = #User
                                                 con slot = #Title
                                                 con preferred = #Preferred
                                                 val pref = speakingInterest

                                                 val whoami = whoami
                                             end)

structure ModeratorInterest = Preferences.Make(struct
                                                   con choice = #Title
                                                   val choice = paper

                                                   con user = #User
                                                   con slot = #Title
                                                   con preferred = #Preferred
                                                   val pref = moderatingInterest

                                                   val whoami = whoami
                                               end)

structure AssignTalks = UsersFromPreferences.Make(struct
                                                      con choice = #Title
                                                      val choice = paper

                                                      con user = #User
                                                      con slot = #Title
                                                      con preferred = #Preferred
                                                      val prefs = {Speaker = speakingInterest,
                                                                   Moderator = moderatingInterest}

                                                      val assignment = talk
                                                      val labels = {Speaker = "Speaker",
                                                                    Moderator = "Moderator"}

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

and logout () =
    clearCookie userC;
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
         (Some "Availability", Prefs.ui u),
         (Some "Speaker interest", SpeakerInterest.ui u),
         (Some "Moderator interest", ModeratorInterest.ui u),
         (Some "Assign talks", AssignTalks.ui),
         (Some "Talk times", SlotPref.ui),
         (Some "Log out", Ui.h4 <xml><a link={logout ()}>Log out</a></xml>))
