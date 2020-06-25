(* Create onlineconfSecret.ur with Zoom parameters, plus start time! *)

open Bootstrap4
structure Theme = Ui.Make(Default)
structure Z = Zoom.Make(Zoom.TwoLegged(OnlineconfSecret))
structure S = Slack.Make(Slack.TwoLegged(OnlineconfSecret))

(*fun sendEmail hs htmsg =
    Mail.send "smtp://localhost" False None "" "" hs (Widget.textFromHtml (show htmsg)) (Some htmsg)
fun headers0 () =
    let
        val hs = Mail.empty
        val hs = Mail.from ("OnlineConf <" ^ OnlineconfSecret.admin_email ^ ">") hs
    in
        hs
    end*)

table user : { Username : string,
               Email : string,
               ClaimCode : option int,
               Admin : bool }
  PRIMARY KEY Username,
  CONSTRAINT Email UNIQUE Email,
  CONSTRAINT ClaimCode UNIQUE ClaimCode

task initialize = fn () =>
                     ex <- oneRowE1 (SELECT COUNT( * ) > 0
                                     FROM user);
                     if ex then
                         return ()
                     else
                         dml (INSERT INTO user(Username, Email, ClaimCode, Admin)
                              VALUES ({[OnlineconfSecret.admin_name]}, {[OnlineconfSecret.admin_email]}, NULL, TRUE))

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
                ShareUrl : option string,
                SlackChannelId : option string }
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
                                         val duration = 5400
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
               Starts : option time,
               Creator : option string,
               ZoomMeetingId : option int,
               StartUrl : option string,
               JoinUrl : option string,
               Active : option bool }
  PRIMARY KEY Title,
  CONSTRAINT Creator FOREIGN KEY Creator REFERENCES user(Username) ON UPDATE CASCADE

open Explorer

cookie claimCode : int

val claimed =
    code <- getCookie claimCode;
    (case code of
         None => return ()
       | Some code =>
         ex <- oneRowE1 (SELECT COUNT( * ) > 0
                         FROM user
                         WHERE user.ClaimCode = {[Some code]});
         if not ex then
             error <xml>No such claim code</xml>
         else
             dml (UPDATE user
                  SET ClaimCode = NULL
                  WHERE ClaimCode = {[Some code]});
             clearCookie claimCode);
    redirect (bless "/main")

structure Auth = Google.ThreeLegged(struct
                                        open OnlineconfSecret
                                        val https = False
                                        val scopes = Google.Scope.empty
                                        val onCompletion = claimed
                                    end)
structure G = Google.Make(Auth)

val whoami =
    addro <- G.emailAddress;
    case addro of
        None => return None
      | Some addr => oneOrNoRowsE1 (SELECT (user.Username)
                                    FROM user
                                    WHERE user.Email = {[addr]})

val amAdmin =
    addro <- G.emailAddress;
    case addro of
        None => return False
      | Some addr =>
        r <- oneOrNoRowsE1 (SELECT (user.Admin)
                            FROM user
                            WHERE user.Email = {[addr]});
        return (r = Some True)

val requireAdmin =
    b <- amAdmin;
    if b then
        return ()
    else
        error <xml>Access denied</xml>

val whoamiAdmin =
    addro <- G.emailAddress;
    case addro of
        None => return None
      | Some addr =>
        ro <- oneOrNoRows1 (SELECT user.Username, user.Admin
                            FROM user
                            WHERE user.Email = {[addr]});
        case ro of
            None => return None
          | Some r =>
            if r.Admin then
                return (Some r.Username)
            else
                error <xml>Access denied</xml>

fun authorize [a] [b] (act : Explorer.action a b) =
    case act of
        Explorer.Read _ => return True
      | _ => amAdmin

structure Exp = Make(struct
                         structure Theme = Default

                         val title = "Papers"

                         val t = none
                                     |> one [#User] [#Username] user "Users" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Paper] [#Title] paper "Papers" (return <xml></xml>) (Default (WHERE TRUE))
                                     |> one [#Slot] [#Begin] slot "Time slots" (return <xml></xml>) (Default (WHERE TRUE))

                                     |> text [#User] [#Username] "Name"
                                     |> ignored [#User] [#Email] "bogus@bogus.com"
                                     |> ignored [#User] [#ClaimCode] None
                                     |> ignored [#User] [#Admin] False

                                     |> text [#Paper] [#Title] "Title"
                                     |> manyToManyOrdered [#Paper] [#Title] [#Paper] [#User] [#Username] [#User] author "Authors" "Papers" {}
                                     |> foreign [#Paper] [#Speaker] [#User] [#Username] "Speaker" "Speaker for"
                                     |> foreign [#Paper] [#TalkBegins] [#Slot] [#Begin] "Talk begins" "Talks"
                                     |> text [#Paper] [#Abstract] "Abstract"
                                     |> ignored [#Paper] [#ZoomMeetingId] None
                                     |> ignored [#Paper] [#StartUrl] None
                                     |> ignored [#Paper] [#JoinUrl] None
                                     |> ignored [#Paper] [#ShareUrl] None
                                     |> ignored [#Paper] [#SlackChannelId] None

                                     |> text [#Slot] [#Begin] "Begins"
                                     |> text [#Slot] [#End] "Ends"

                         val authorize = authorize

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

                                                      val whoami = whoamiAdmin
                                                  end)

structure UsersEnterAvailability = TimePreferences.Make(struct
                                                            val times = slot

                                                            con user = #User
                                                            con tcol = #Slot
                                                            con preferred = #Preferred
                                                            val prefs = timePreference
                                                            val title = "TimePreference"

                                                            val whoami = whoami
                                                            val addon = CalendarAddons.empty
                                                        end)

structure AssignTalkTimes = AssignTimes.Make(struct
                                                 val times = slot
                                                 fun others tm = {End = addSeconds tm (60 * 60)}

                                                 con user = #User
                                                 con btime = #Slot
                                                 con preferred = #Preferred
                                                 val bid = timePreference
                                                 val bidTitle = "TimePreference"

                                                 con this = #Title
                                                 con ttime = #TalkBegins
                                                 val t = paper
                                                 val tTitle = "Paper"
                                                 val assignees = {Speaker = "Speaker"}

                                                 val whoami = whoami
                                                 val addon = CalendarAddons.empty
                                             end)

fun claim code =
    ex <- oneRowE1 (SELECT COUNT( * ) > 0
                    FROM user
                    WHERE user.ClaimCode = {[Some code]});
    if not ex then
        error <xml>No such claim code</xml>
    else
        setCookie claimCode {Value = code, Expires = None, Secure = False};
        Auth.authorize

fun addUser name email =
    code <- rand;
    dml (INSERT INTO user(Username, Email, ClaimCode, Admin)
         VALUES ({[name]}, {[email]}, {[Some code]}, FALSE))(*;
    let
        val hs = headers0 ()
        val hs = Mail.subject "Claim your OnlineConf account" hs
        val hs = Mail.to email hs

        val htmlm = <xml>
          Hurry on up and <a href={url (claim code)}>claim your account</a>!
        </xml>
    in
        sendEmail hs htmlm
    end*)

structure HotcrpImport : Ui.S0 = struct
    type a = source string

    val create = source ""
    fun onload _ = return ()

    fun import s =
        requireAdmin;
        List.app (fn p : Hotcrp.paper =>
                     ex <- oneRowE1 (SELECT COUNT( * ) > 0
                                     FROM paper
                                     WHERE paper.Title = {[p.Title]});
                     if ex then
                         return ()
                     else
                         dml (INSERT INTO paper(Title, Abstract, Speaker, TalkBegins,
                                  ZoomMeetingId, StartUrl, JoinUrl, ShareUrl, SlackChannelId)
                              VALUES ({[p.Title]}, {[p.Abstract]}, NULL, NULL,
                                  NULL, NULL, NULL, NULL, NULL));
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
                                               addUser name a.Email);
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

    fun notification _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification}
end

structure PaperList = SmartList.Make(struct
                                         con sortBy = #TalkBegins
                                         val tab = paper
                                         val title = "Paper"
                                         val notifyOnNonempty = False
                                         val authorized = return True

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
                                                                       | Some start => Some (glyphicon_play_circle, bless start)
                                                                 else
                                                                     case join of
                                                                         None => None
                                                                       | Some join => Some (glyphicon_video, bless join))
                                                 |> SmartList.compose (SmartList.iconButtonInHeader
                                                                           whoami
                                                                           (fn u tm {SlackChannelId = chid} =>
                                                                               case chid of
                                                                                   None => None
                                                                                 | Some chid => Some (glyphicon_comment, S.Conversations.url {Channel = chid, Team = None})))
                                                 |> SmartList.compose (SmartList.columnInHeader [#Title])
                                                 |> SmartList.compose (SmartList.columnInBody [#TalkBegins] "Begins")
                                                 |> SmartList.compose (SmartList.orderedLinked [#Title] [#Paper] [#User] author "Authors")
                                                 |> SmartList.compose (SmartList.nonnull [#TalkBegins])
                                     end)

structure ChatList = SmartTable.Make(struct
                                         val tab = chat
                                         val title = "Chat"
                                         val notifyOnNonempty = False
                                         val authorized = return True
                                         val allowCreate = True
                                         val labels = {Title = "Title",
                                                       Starts = "Starts",
                                                       Creator = "Creator",
                                                       ZoomMeetingId = "Zoom meeting ID",
                                                       StartUrl = "Start URL",
                                                       JoinUrl = "Join URL",
                                                       Active = "Active?"}

                                         val t = SmartTable.iconButton
                                                     whoami
                                                     (fn u tm {Active = actv,
                                                               Creator = creator,
                                                               StartUrl = start,
                                                               JoinUrl = join} =>
                                                         case (actv, start, join) of
                                                             (Some True, Some start, Some join) =>
                                                             if creator = u then
                                                                 Some (glyphicon_play_circle, bless start)
                                                             else
                                                                 Some (glyphicon_video, bless join)
                                                           | _ => None)
                                                     "Zoom"
                                                 |> SmartTable.compose (SmartTable.column [#Starts] "Starts")
                                                 |> SmartTable.compose (SmartTable.owner [#Creator] whoami "Creator")
                                                 |> SmartTable.compose (SmartTable.column [#Title] "Title")
                                     end)

task periodic 10 = fn () =>
                      tm <- now;
                      soon <- return (addSeconds tm ((-30) * 60)); (* Check for chats about to start in under half an hour. *)
                      chatReady <- oneOrNoRows1 (SELECT chat.Title, chat.Starts
                                                 FROM chat
                                                 WHERE chat.ZoomMeetingId IS NULL
                                                   AND NOT (chat.Starts IS NULL)
                                                   AND chat.Starts > {[Some soon]}
                                                 ORDER BY chat.Starts
                                                 LIMIT 1);
                      case chatReady of
                          None => return ()
                        | Some r =>
                          m <- Z.Meetings.create ({Topic = r.Title,
                                                   Typ = Zoom.Instant,
                                                   StartTime = r.Starts}
                                                      ++ Api.optionals {});
                          dml (UPDATE chat
                               SET ZoomMeetingId = {[m.Id]},
                                 StartUrl = {[m.StartUrl]},
                                 JoinUrl = {[m.JoinUrl]},
                                 Active = {[Some True]}
                               WHERE Title = {[r.Title]})

structure ScheduleChats = SetTimes.Make(struct
                                            val t = chat
                                            val whoami = whoami
                                            val addon = CalendarAddons.empty
                                        end)

structure Calendar = SmartCalendar.Make(struct
                                            structure T = CalendarAddons.EventSource(struct
                                                                                         val source = paper
                                                                                         val title = "Paper"
                                                                                         val prefix = "Talk: "
                                                                                         val background = False
                                                                                         val textColor = None
                                                                                         val backgroundColor = None
                                                                                         val addons = CalendarAddons.empty
                                                                                     end)

                                            structure C = CalendarAddons.EventSource(struct
                                                                                         val source = chat
                                                                                         val title = "Chat"
                                                                                         val prefix = "Chat: "
                                                                                         val background = True
                                                                                         val textColor = Some "blue"
                                                                                         val backgroundColor = Some "gray"
                                                                                         val addons = CalendarAddons.empty
                                                                                     end)

                                            val addon = T.t |> CalendarAddons.compose C.t
                                            val whoami = whoami
                                        end)

task periodic 1 = fn () =>
    tm <- now;
    create_threshold <- return (addSeconds tm (24 * 60 * 60));
    next <- oneOrNoRows1 (SELECT paper.Title, paper.TalkBegins
                          FROM paper
                          WHERE NOT (paper.TalkBegins IS NULL)
                            AND paper.ZoomMeetingId IS NULL
                            AND paper.TalkBegins < {[Some create_threshold]}
                          ORDER BY paper.TalkBegins
                          LIMIT 1);
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

task periodic 60 = fn () =>
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

task periodic 60 = fn () =>
    queryI1 (SELECT chat.ZoomMeetingId
             FROM chat
             WHERE COALESCE(chat.Active, FALSE))
    (fn {ZoomMeetingId = ido} =>
        case ido of
            None => error <xml>No Zoom meeting ID set.</xml>
          | Some id =>
            m <- Z.Meetings.get id;
            if (case m of None => False | Some m => case m.Status of Some Zoom.Waiting => True | Some Zoom.Started => True | _ => False) then
                return ()
            else
                dml (UPDATE chat
                     SET Active = {[Some False]}
                     WHERE ZoomMeetingId = {[ido]}))

task periodic 1 = fn () =>
    next <- oneOrNoRowsE1 (SELECT (paper.Title)
                           FROM paper
                           WHERE NOT (paper.TalkBegins IS NULL)
                             AND paper.SlackChannelId IS NULL
                           ORDER BY paper.TalkBegins
                           LIMIT 1);
    case next of
        None => return ()
      | Some next =>
        ch <- S.Conversations.create (Slack.suggestChannelName next);
        dml (UPDATE paper
             SET SlackChannelId = {[Some ch.Id]}
             WHERE Title = {[next]})

fun login () = Auth.authorize

and logout () =
    Auth.logout;
    redirect (url (main ()))

and main () =
    u <- whoami;
    case u of
        None => Theme.simple "OnlineConf"
                (Ui.const <xml>
                  <h3>Better log in!</h3>

                  <form>
                    <submit class="btn btn-primary" action={login} value="Log in at Google"/>
                  </form>
                </xml>)
      | Some u =>
        Theme.tabbed "OnlineConf"
        ((Some "Calendar", Calendar.ui),
         (Some "Paper list", PaperList.ui),
         (Some "Speaker interest", SpeakerInterest.ui u),
         (Some "Availability", UsersEnterAvailability.ui u),
         (Some "Chat list", ChatList.ui),
         (Some "Schedule chats", ScheduleChats.ui),
         (Some "Log out", Ui.h4 <xml>
           <form>
             <submit class="btn btn-primary" action={logout} value="Log out"/>
           </form>
         </xml>))

and admin () =
    u <- whoamiAdmin;
    case u of
        None => Theme.simple "OnlineConf Admin"
                (Ui.const <xml>
                  <h3>Better log in!</h3>

                  <form>
                    <submit class="btn btn-primary" action={login} value="Log in at Google"/>
                  </form>
                </xml>)
      | Some u =>
        Theme.tabbed "OnlineConf Admin"
        ((Some "HotCRP import", HotcrpImport.ui),
         (Some "Assign talks", AssignTalks.ui),
         (Some "Assign talk times", AssignTalkTimes.ui),
         (Some "Log out", Ui.h4 <xml>
           <form>
             <submit class="btn btn-primary" action={logout} value="Log out"/>
           </form>
         </xml>))
