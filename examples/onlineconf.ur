(* Create onlineconfSecret.ur with Zoom parameters, plus start time! *)

open Bootstrap4
structure Theme = Ui.Make(Default)
structure Z = Zoom.Make(Zoom.TwoLegged(OnlineconfSecret))
                  
table user : { Username : string }
  PRIMARY KEY Username

table slot : { Begin : time,
               End : time }
  PRIMARY KEY Begin

table paper : { Title : string,
                Abstract : string,
                Speaker : option string,
                TalkBegins : option time,
                ZoomMeetingId : option string,
                StartUrl : option string,
                JoinUrl : option string,
                ShareUrl : option string }
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
                                         val initial = OnlineconfSecret.start
                                         val final = addSeconds OnlineconfSecret.start (5 * 3600)
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

table chat : { Title : string,
               Creator : string,
               ZoomMeetingId : int,
               StartUrl : string,
               JoinUrl : string,
               Active : bool }
  PRIMARY KEY Title,
  CONSTRAINT Creator FOREIGN KEY Creator REFERENCES user(Username) ON UPDATE CASCADE
      
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
                                     |> ignored [#Paper] [#ZoomMeetingId]
                                     |> ignored [#Paper] [#StartUrl]
                                     |> ignored [#Paper] [#JoinUrl]
                                     |> ignored [#Paper] [#ShareUrl]

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
                         dml (INSERT INTO paper(Title, Abstract, Speaker, TalkBegins,
                                  ZoomMeetingId, StartUrl, JoinUrl, ShareUrl)
                              VALUES ({[p.Title]}, {[p.Abstract]}, NULL, NULL,
                                  NULL, NULL, NULL, NULL));
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

structure PaperList = SmartList.Make(struct
                                         con sortBy = #TalkBegins
                                         val tab = paper
                                         val wher = (WHERE NOT (tab.TalkBegins IS NULL))

                                         val t = SmartList.iconButtonInHeader
                                                     whoami
                                                     (fn u tm {TalkBegins = begins,
                                                               Speaker = speaker,
                                                               StartUrl = start,
                                                               JoinUrl = join,
                                                               ShareUrl = share} =>
                                                         case share of
                                                             Some share => Some (glyphicon_film, bless share)
                                                           | None =>
                                                             case begins of
                                                                 None => None
                                                               | Some begins =>
                                                                 if begins > addSeconds tm (15 * 60) then
                                                                     None
                                                                 else if speaker = u then
                                                                     case start of
                                                                         None => None
                                                                       | Some start => Some (glyphicon_play_circle_o, bless start)
                                                                 else
                                                                     case join of
                                                                         None => None
                                                                       | Some join => Some (glyphicon_video_camera, bless join))
                                                 |> SmartList.compose (SmartList.columnInHeader [#Title])
                                                 |> SmartList.compose (SmartList.columnInBody [#TalkBegins] "Begins")
                                                 |> SmartList.compose (SmartList.orderedLinked [#Title] [#Paper] [#User] author "Authors")
                                     end)

structure ChatList = SmartList.Make(struct
                                        con sortBy = #Title
                                        val tab = chat
                                        val wher = (WHERE tab.Active)

                                        val t = SmartList.iconButtonInHeader
                                                    whoami
                                                    (fn u tm {Active = actv,
                                                              Creator = creator,
                                                              StartUrl = start,
                                                              JoinUrl = join} =>
                                                        if not actv then
                                                            None
                                                        else if Some creator = u then
                                                            Some (glyphicon_play_circle_o, bless start)
                                                        else
                                                            Some (glyphicon_video_camera, bless join))
                                                |> SmartList.compose (SmartList.columnInHeader [#Title])
                                                |> SmartList.compose (SmartList.columnInBody [#Creator] "Creator")
                                    end)

structure CreateChat = SmartInsert.Make(struct
                                            val tab = chat
                                            val labels = {Title = "Title"}

                                            fun custom r =
                                                uo <- whoami;
                                                case uo of
                                                    None => error <xml>Must be logged in.</xml>
                                                  | Some u =>
                                                    m <- Z.Meetings.create ({Topic = r.Title,
                                                                             Typ = Zoom.Instant}
                                                                                ++ Api.optionals {});
                                                    case (m.Id, m.StartUrl, m.JoinUrl) of
                                                        (Some id, Some start, Some join) =>
                                                        return {Creator = u,
                                                                ZoomMeetingId = id,
                                                                StartUrl = start,
                                                                JoinUrl = join,
                                                                Active = True}
                                                      | _ => error <xml>Missing field in record for new Zoom meeting!</xml>
                                        end)

val maybeCreateOneChannel =
    tm <- now;
    create_threshold <- return (addSeconds tm (24 * 60 * 60));
    next <- oneOrNoRows1 (SELECT paper.Title, paper.TalkBegins
                          FROM paper
                          WHERE NOT (paper.TalkBegins IS NULL)
                            AND paper.ZoomMeetingId IS NULL
                            AND paper.TalkBegins < {[Some create_threshold]}
                          ORDER BY paper.TalkBegins);
    case next of
        None => return ()
      | Some next =>
        m <- Z.Meetings.create ({Topic = "Onlineconf: " ^ next.Title,
                                 Typ = Zoom.Scheduled,
                                 StartTime = next.TalkBegins}
                                    ++ Api.optionals {Duration = 60,
                                                      Settings = Api.optionals {AutoRecording = Zoom.Cloud}});
        dml (UPDATE paper
             SET ZoomMeetingId = {[m.Uuid]}, StartUrl = {[m.StartUrl]}, JoinUrl = {[m.JoinUrl]}
             WHERE Title = {[next.Title]})

val checkForFinishedRecordings =
    rs <- Z.CloudRecordings.list;
    List.app (fn r =>
                 case List.search (fn f =>
                                      case f.Status of
                                          Some Zoom.Completed => f.MeetingId
                                        | _ => None) (Option.get [] r.RecordingFiles) of
                     None => return ()
                   | Some id =>
                     dml (UPDATE paper
                          SET ShareUrl = {[r.ShareUrl]}
                          WHERE ZoomMeetingId = {[Some id]})) rs

val updateChatStatuses =
    queryI1 (SELECT chat.ZoomMeetingId
             FROM chat
             WHERE chat.Active)
    (fn {ZoomMeetingId = id} =>
        m <- Z.Meetings.get id;
        if (case m of None => False | Some m => case m.Status of Some Zoom.Waiting => True | Some Zoom.Started => True | _ => False) then
            return ()
        else
            dml (UPDATE chat
                 SET Active = FALSE
                 WHERE ZoomMeetingId = {[id]}))

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
         (Some "Chat list", ChatList.ui),
         (Some "Create chat", CreateChat.ui),
         (Some "Provision next",
          Ui.const <xml>
            <button class="btn btn-primary"
                    onclick={fn _ => rpc maybeCreateOneChannel}>
              Maybe create a channel
            </button>
            <button class="btn btn-primary"
                    onclick={fn _ => rpc checkForFinishedRecordings}>
              Check for finished recordings
            </button>
            <button class="btn btn-primary"
                    onclick={fn _ => rpc updateChatStatuses}>
              Update chat statuses
            </button>
          </xml>),
         (Some "Log out", Ui.h4 <xml><a link={logout ()}>Log out</a></xml>))
