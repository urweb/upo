structure Theme = Ui.Make(Default)

table session : { Session : string }
  PRIMARY KEY Session

val show_session : show {Session : string} = mkShow (fn r => r.Session)
val read_session : read {Session : string} = mkRead' (fn s => Some {Session = s}) "session"

table speaker : { Speaker : string }
  PRIMARY KEY Speaker

val show_speaker : show {Speaker : string} = mkShow (fn r => r.Speaker)
val read_speaker : read {Speaker : string} = mkRead' (fn s => Some {Speaker = s}) "speaker"

table talk : { Session : string, Time : time, Speaker : option string }
  PRIMARY KEY (Session, Time),
  CONSTRAINT Session FOREIGN KEY Session REFERENCES session(Session) ON UPDATE CASCADE,
  CONSTRAINT Speaker FOREIGN KEY Speaker REFERENCES speaker(Speaker) ON UPDATE CASCADE

val show_time : show {Time : time} = mkShow (fn r => show r.Time)
val read_time : read {Time : time} = mkRead' (fn s => Option.mp (fn t => {Time = t}) (read s)) "time"

structure S = ScheduleSessions.Make(struct
                                        con sessionKey1 = #Session
                                        con sessionKeyR = []
                                        val session = session

                                        con speakerKey1 = #Speaker
                                        con speakerKeyR = []
                                        val speaker = speaker

                                        val talk = talk
                                    end)

structure Sessions = EditableTable.Make(struct
                                            val tab = session
                                            val labels = {Session = "Session"}
                                            val permission = return {Add = True,
                                                                     Delete = True,
                                                                     Modify = True}
                                            fun onAdd _ = return ()
                                            fun onDelete _ = return ()
                                            fun onModify _ = return ()
                                        end)

structure Speakers = EditableTable.Make(struct
                                            val tab = speaker
                                            val labels = {Speaker = "Speaker"}
                                            val permission = return {Add = True,
                                                                     Delete = True,
                                                                     Modify = True}
                                            fun onAdd _ = return ()
                                            fun onDelete _ = return ()
                                            fun onModify _ = return ()
                                        end)
                     
val main = Theme.tabbed "SS"
           ((Some "Sessions",
             Sessions.ui),
            (Some "Speakers",
             Speakers.ui),
            (Some "Schedule",
             S.AllSessions.ui))
