open Bootstrap4
structure Theme = Ui.Make(Default)

table user : { Username : string }
  PRIMARY KEY Username

table slot : { Begin : time,
               End : time }
  PRIMARY KEY Begin

table paper : { Title : string,
                Abstract : string,
                Speaker : option string,
                TalkBegins : option time }
  PRIMARY KEY Title,
  CONSTRAINT Speaker FOREIGN KEY Speaker REFERENCES user(Username) ON UPDATE CASCADE,
  CONSTRAINT TalkBegins FOREIGN KEY TalkBegins REFERENCES slot(Begin)

table author : { Paper : string,
                 User : string,
                 SeqNum : int }
  PRIMARY KEY (Paper, User),
  CONSTRAINT Paper FOREIGN KEY Paper REFERENCES paper(Title) ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT User FOREIGN KEY User REFERENCES user(Username) ON UPDATE CASCADE

structure Slots = FillTimeRange.Make(struct
                                         val slot = slot
                                         val initial = readError "2020-3-23 12:00:00"
                                         val final = readError "2020-3-23 17:00:00"
                                         val duration = 3600
                                     end)
             
table timePreference : { User : string,
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
      
open Explorer

cookie userC : string
val whoami = getCookie userC

structure Exp = Make(struct
                         structure Theme = Default

                         val title = "Papers"

                         val t = none
                                     |> one [#User] [#Username] user "Users" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Paper] [#Title] paper "Papers" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Slot] [#Begin] slot "Time slots" (return <xml></xml>) (Default (WHERE TRUE))

                                     |> text [#User] [#Username] "Name"

                                     |> text [#Paper] [#Title] "Title"
                                     |> manyToManyOrdered [#Paper] [#Title] [#Paper] [#User] [#Username] [#User] author "Authors" "Papers" {}
                                     |> foreign [#Paper] [#Speaker] [#User] [#Username] "Speaker" "Speaker for"
                                     |> foreign [#Paper] [#TalkBegins] [#Slot] [#Begin] "Talk begins" "Talks"
                                     |> text [#Paper] [#Abstract] "Abstract"

                                     |> text [#Slot] [#Begin] "Begins"
                                     |> text [#Slot] [#End] "Ends"

                         fun authorize _ = return True

                         val preTabs = {}
                         val postTabs = {}
                         val hiddenTabs = {}
                     end)

structure SpeakerInterest = Preferences.Make(struct
                                                 con choice = #Title
                                                 val choice = paper

                                                 con user = #User
                                                 con slot = #Title
                                                 con preferred = #Preferred
                                                 val pref = speakingInterest

                                                 val whoami = whoami
                                                 fun eligible u = (WHERE (SELECT COUNT( * ) > 0
                                                                          FROM author
                                                                          WHERE author.Paper = Choice.Title
                                                                            AND author.User = {[u]}) = {[Some True]})
                                             end)

structure AssignTalks = UsersFromPreferences.Make(struct
                                                      con choice = #Title
                                                      val choice = paper
                                                      val labels = {Speaker = "Speaker"}
                                                                   
                                                      con user = #User
                                                      con slot = #Title
                                                      con preferred = #Preferred
                                                      val prefs = {Speaker = speakingInterest}

                                                      val whoami = whoami
                                                  end)
                
structure UsersEnterAvailability = Preferences.Make(struct
                                                        con choice = #Begin
                                                        val choice = slot

                                                        con user = #User
                                                        con slot = #Slot
                                                        con preferred = #Preferred
                                                        val pref = timePreference

                                                        val whoami = whoami
                                                        fun eligible _ = (WHERE TRUE)
                                                    end)

structure AssignTalkTimes = ChoicesFromPreferences.Make(struct
                                                            con choice = #Begin
                                                            val choice = slot

                                                            con user = #User
                                                            con slot = #Slot
                                                            con preferred = #Preferred
                                                            val pref = timePreference

                                                            con item = #Title
                                                            con users = [Speaker]
                                                            val item = paper
                                                            val labels = {Speaker = "Speaker"}

                                                            val authorize = return True
                                                        end)

structure HotcrpImport : Ui.S0 = struct
    type a = source string

    val create = source ""
    fun onload _ = return ()

    fun import s =
        List.app (fn p : Hotcrp.paper =>
                     ex <- oneRowE1 (SELECT COUNT( * ) > 0
                                     FROM paper
                                     WHERE paper.Title = {[p.Title]});
                     if ex then
                         return ()
                     else
                         dml (INSERT INTO paper(Title, Abstract, Speaker, TalkBegins)
                              VALUES ({[p.Title]}, {[p.Abstract]}, NULL, NULL));
                         List.appi (fn i a =>
                                      let
                                          val name = case (a.First, a.Last) of
                                                         (Some f, None) => f
                                                       | (None, Some l) => l
                                                       | (Some f, Some l) => f ^ " " ^ l
                                                       | (None, None) => "Anonymous"
                                      in
                                          ex <- oneRowE1 (SELECT COUNT( * ) > 0
                                                          FROM user
                                                          WHERE user.Username = {[name]});
                                          (if ex then
                                               return ()
                                           else
                                               dml (INSERT INTO user(Username)
                                                    VALUES ({[name]})));
                                          dml (INSERT INTO author(Paper, User, SeqNum)
                                               VALUES ({[p.Title]}, {[name]}, {[i]}))
                                      end) (Option.get [] p.Authors))
                 (Json.fromJson s)
                   
    fun render _ s = <xml>
      <ctextarea source={s} class="form-control"/>
      <button class="btn btn-primary"
              onclick={fn _ => s <- get s; rpc (import s)}>
        Import
      </button>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end

fun info title =
    Theme.simple "Paper" (Exp.ui (make [#Paper] title))
                                 
structure PaperList : Ui.S0 = struct
    type a = list string

    val create = List.mapQuery (SELECT paper.Title
                                FROM paper
                                ORDER BY paper.Title)
                               (fn {Paper = {Title = s}} => s)

    fun onload _ = return ()

    fun render _ ls = <xml><ul class="list-group">
      {List.mapX (fn s => <xml><li class="list-group-item"><a link={info s}>{[s]}</a></li></xml>) ls}
    </ul></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
                            
fun login {Nam = s} =
    ex <- oneRowE1 (SELECT COUNT( * ) > 0
                    FROM user
                    WHERE user.Username = {[s]});
    (if ex then
         return ()
     else
         dml (INSERT INTO user(Username)
              VALUES ({[s]})));
    
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
        ((Some "HotCRP import", HotcrpImport.ui),
         (Some "Speaker interest", SpeakerInterest.ui u),
         (Some "Assign talks", AssignTalks.ui),
         (Some "Availability", UsersEnterAvailability.ui u),
         (Some "Talk times", AssignTalkTimes.ui),
         (Some "Paper list", PaperList.ui),
         (Some "Log out", Ui.h4 <xml><a link={logout ()}>Log out</a></xml>))
