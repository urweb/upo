table user : { User : string }
  PRIMARY KEY User

val show_user : show {User : string} = mkShow (fn r => r.User)

table ballot : { Ballot : string }
  PRIMARY KEY Ballot

table choice : { Ballot : string, City : string, Year : int }
  PRIMARY KEY (Ballot, City, Year),
  CONSTRAINT Ballot FOREIGN KEY Ballot REFERENCES ballot(Ballot) ON UPDATE CASCADE

val show_choice : show { City : string, Year : int} = mkShow (fn r => r.City ^ " (" ^ show r.Year ^ ")")

cookie userC : { User : string }

structure Ballots = EditableTable.Make(struct
                                           val tab = ballot
                                           val labels = {Ballot = "Ballot"}
                                           val permission = return {Add = True,
                                                                    Delete = True,
                                                                    Modify = True}
                                       end)

structure Choices = EditableTable.Make(struct
                                           val tab = choice
                                           val labels = {Ballot = "Ballot", City = "City", Year = "Year"}
                                           val permission = return {Add = True,
                                                                    Delete = True,
                                                                    Modify = True}
                                       end)

structure VotingUnlimited = OpenBallot.Make(struct
                                                con choiceBallot = [Ballot = _]
                                                con choiceKey = [City = _, Year = _]
                                                val voter = user
                                                val choice = choice
                                                val amVoter = getCookie userC
                                                val maxVotesPerVoter = None
                                            end)

structure VotingLimited = OpenBallot.Make(struct
                                              con choiceBallot = [Ballot = _]
                                              con choiceKey = [City = _, Year = _]
                                              val voter = user
                                              val choice = choice
                                              val amVoter = getCookie userC
                                              val maxVotesPerVoter = Some 2
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
    latest <- oneOrNoRows1 (SELECT * FROM ballot ORDER BY ballot.Ballot LIMIT 1);
    newuname <- source "";
    uname <- getCookie userC;
    uname <- return (Option.get {User = ""} uname);
    Ui.tabbed "OV"
    ((Some "Login",
      Ui.const <xml>
        <ctextbox source={newuname}/>
        <button value="Set" onclick={fn _ => s <- get newuname; rpc (auth s)}/>
      </xml>),
     (Some "Ballots",
      Ballots.ui),
     (Some "Choices",
      Choices.ui),
     (Some "Vote (unlimited)",
      VotingUnlimited.ui {Ballot = Option.get {Ballot = ""} latest,
                          Voter = uname}),
     (Some "Vote (limited)",
      VotingLimited.ui {Ballot = Option.get {Ballot = ""} latest,
                        Voter = uname}))
