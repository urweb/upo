(* Organizing dinners for the MIT EECS junior faculty *)

open Bootstrap3
structure Theme = Ui.Make(Default)

(* Local MIT people *)
table user : { HumanName : string, MitId : string, IsAdmin : bool, IsJfac : bool }
  PRIMARY KEY HumanName,
  CONSTRAINT MitId UNIQUE MitId

val userShow : show {HumanName : string} = mkShow (fn r => r.HumanName)
val userRead : read {HumanName : string} = mkRead' (fn s => Some {HumanName = s}) "user"

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(MitId, HumanName, IsAdmin, IsJfac)
           VALUES ('admin', 'Admin', TRUE, TRUE))

table restaurant : { RestaurantName : string, Neighborhood : string, Genre : string, Url : string }
  PRIMARY KEY (RestaurantName, Neighborhood)

val restaurantShow : show {RestaurantName : string, Neighborhood : string} =
    mkShow (fn r => case r.Neighborhood of
                        "" => r.RestaurantName
                      | s => r.RestaurantName ^ " (" ^ s ^ ")")

table time : { Time : time }
  PRIMARY KEY (Time)

val timeShow : show {Time : time} = mkShow (fn r => show r.Time)

table dinner : { RestaurantName : string, Neighborhood : string, Time : time }
  PRIMARY KEY (Time),
  CONSTRAINT Restaurant FOREIGN KEY (RestaurantName, Neighborhood) REFERENCES restaurant(RestaurantName, Neighborhood) ON UPDATE CASCADE,
  CONSTRAINT Time FOREIGN KEY Time REFERENCES time(Time) ON UPDATE CASCADE

(* The real app uses client certificates, but here we'll do cookies for convenience. *)
cookie localC : string

(* Find the common name of the authenticated user (via SSL certificates),
 * remembering this person in the DB, if successful. *)
val auth = return "Admin"

val requireAuth = Monad.ignore auth

(* Fail if not authenticated as an admin. *)
val amAdmin =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.HumanName = {[u]}
                AND user.IsAdmin)

val requireAdmin =
    isAdmin <- amAdmin;
    if isAdmin then
        return ()
    else
        error <xml>Access denied</xml>

val amUser = user <- auth; return (Some {HumanName = user})
val amJfac =
    u <- auth;
    jfac <- oneRowE1 (SELECT (user.IsJfac)
                      FROM user
                      WHERE user.HumanName = {[u]});
    if jfac then
        return (Some {HumanName = u})
    else
        error <xml>Sorry, you got yourself too much tenure.</xml>

val requireJfac = Monad.ignore amJfac

structure Commented = WithComments.Make(struct
                                            con key1 = #Time
                                            con keyR = []
                                            val key = dinner
                                            con user1 = #HumanName
                                            val user = user

                                            val query = (SELECT (dinner.Time) AS Time,
                                                           (dinner.RestaurantName) AS RestaurantName,
                                                           (dinner.Neighborhood) AS Neighborhood,
                                                           (restaurant.Genre) AS Genre,
                                                           (restaurant.Url) AS Url
                                                         FROM dinner, restaurant
                                                         WHERE dinner.RestaurantName = restaurant.RestaurantName
                                                           AND dinner.Neighborhood = restaurant.Neighborhood)

                                            fun render r = <xml>{[r.Time]}:
                                              {case checkUrl r.Url of
                                                   None => <xml><b>{[r.RestaurantName]}</b></xml>
                                                 | Some url => <xml><a href={url}><b>{[r.RestaurantName]}</b></a></xml>}
                                              {case (r.Neighborhood, r.Genre) of
                                                   ("", "") => <xml/>
                                                 | (s, "") => <xml>({[s]})</xml>
                                                 | ("", s) => <xml>({[s]})</xml>
                                                 | (s1, s2) => <xml>({[s1]}, {[s2]})</xml>}
                                            </xml>

                                            val amUser = amUser
                                        end)

structure VoteRestaurant = OpenBallot.Make(struct
                                               con choiceBallot = []

                                               val voter = user
                                               val choice = restaurant

                                               val amVoter = amJfac
                                               val maxVotesPerVoter = Some 1
                                               val keyFilter = (WHERE TRUE)
                                               val alwaysShowVotes = True
                                           end)

structure VoteTime = OpenBallot.Make(struct
                                         con choiceBallot = []

                                         val voter = user
                                         val choice = time

                                         val amVoter = amJfac
                                         val maxVotesPerVoter = None
                                         val keyFilter = (WHERE TRUE)
                                         val alwaysShowVotes = True
                                     end)

val rlabels = {RestaurantName = "Name",
               Neighborhood = "Neighborhood",
               Genre = "Genre",
               Url = "URL"}

structure Restaurants = EditableTable.Make(struct
                                               val tab = restaurant
                                               val labels = rlabels
                                               val permission =
                                                   requireAuth;
                                                   return {Add = True,
                                                           Delete = False,
                                                           Modify = True}

                                               val widgets = {Url = Widget.urlbox} ++ _

                                               fun onAdd _ = return ()
                                               fun onDelete _ = return ()
                                               fun onModify _ = return ()
                                       end)

val tlabels = {Time = "Time"}

structure Times = EditableTable.Make(struct
                                         val tab = time
                                         val labels = tlabels
                                         val permission =
                                             requireAuth;
                                             return {Add = True,
                                                     Delete = False,
                                                     Modify = True}

                                         fun onAdd _ = return ()
                                         fun onDelete _ = return ()
                                         fun onModify _ = return ()
                                     end)
                  
structure EditUsers = EditGrid.Make(struct
                                        con key = [MitId = _]
                                        val tab = user
                                        val labels = {MitId = "MIT ID",
                                                      HumanName = "Name",
                                                      IsAdmin = "Admin?",
                                                      IsJfac = "Still eligible?"}
                                        val authorized = amAdmin
                                    end)

structure EditRestaurants = EditGrid.Make(struct
                                              con key = [RestaurantName = _, Neighborhood = _]
                                              val tab = restaurant
                                              val labels = rlabels
                                              val authorized = amAdmin
                                          end)

structure EditTimes = EditGrid.Make(struct
                                        con key = [Time = _]
                                        val tab = time
                                        val labels = tlabels
                                        val authorized = amAdmin
                                    end)

structure EditDinners = EditGrid.Make(struct
                                          con key = [Time = _]
                                          val tab = dinner
                                          val labels = {Time = "Time",
                                                        RestaurantName = "Restaurant",
                                                        Neighborhood = "Neighborhood"}
                                          val authorized = amAdmin
                                      end)

val halfHour = 60 * 30
val oneHour = 60 * 60

fun addDinner r =
    requireJfac;
    dml (INSERT INTO dinner(RestaurantName, Neighborhood, Time)
         VALUES ({[r.RestaurantName]}, {[r.Neighborhood]}, {[r.Time]}));
    Commented.add {Time = r.Time}

fun addPastDinner r =
    timeExists <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM time
                            WHERE time.Time = {[r.Time]});
    (if timeExists then
         return ()
     else
         dml (INSERT INTO time(Time) VALUES ({[r.Time]})));
    addDinner r

val explainTime =
    Ui.h4 <xml>Your vote count is the number of people who would go, including you and your guests.</xml>

val main =
    user <- auth;
    key <- return {HumanName = user};

    times <- queryX1 (SELECT time.Time
                      FROM time
                      ORDER BY time.Time)
                   (fn r => <xml><coption>{[r.Time]}</coption></xml>);

    restaurants <- queryX1 (SELECT restaurant.RestaurantName, restaurant.Neighborhood
                            FROM restaurant
                            ORDER BY restaurant.RestaurantName, restaurant.Neighborhood)
                   (fn r => <xml><coption value={r.RestaurantName ^ "^"
                                                 ^ r.Neighborhood}>{[r.RestaurantName]}
                     {case r.Neighborhood of
                          "" => <xml/>
                        | s => <xml>({[s]})</xml>}</coption></xml>);

    whichT <- source "";
    whichR <- source "";
    whichTP <- source "";
    whichRP <- source "";

    nextDinner <- oneOrNoRows (SELECT dinner.Time, dinner.RestaurantName, dinner.Neighborhood,
                                  restaurant.Genre, restaurant.Url
                                FROM dinner, restaurant
                                WHERE dinner.RestaurantName = restaurant.RestaurantName
                                  AND dinner.Neighborhood = restaurant.Neighborhood
                                ORDER BY dinner.Time
                                LIMIT 1);

    Theme.tabbed "EECS Junior Faculty Dinners"
              ((case nextDinner of
                    None => None
                  | Some _ => Some "Next Dinner",
                Ui.seq
                    (Ui.const (case nextDinner of
                                   None => <xml/>
                                 | Some nd => <xml>
                                   <h2>When: {[nd.Dinner.Time]}</h2>
                                   <h2>Where:
                                     {case checkUrl nd.Restaurant.Url of
                                          None => <xml>{[nd.Dinner.RestaurantName]}</xml>
                                        | Some url => <xml><a href={url}>{[nd.Dinner.RestaurantName]}</a></xml>}
                                     {case (nd.Dinner.Neighborhood, nd.Restaurant.Genre) of
                                          ("", "") => <xml/>
                                        | ("", s) => <xml>({[s]})</xml>
                                        | (s, "") => <xml>({[s]})</xml>
                                        | (s1, s2) => <xml>({[s1]}, {[s2]})</xml>}</h2>
                                   </xml>),
                     explainTime,
                     VoteTime.OneChoice.ui {Ballot = (),
                                            Choice = case nextDinner of
                                                         None => {Time = minTime}
                                                       | Some nd => {Time = nd.Dinner.Time},
                                            Voter = key})),
               (Some "Vote on Times",
                Ui.seq
                    (explainTime,
                     VoteTime.ui {Ballot = (), Voter = key})),
               (Some "Vote on Restaurants",
                VoteRestaurant.ui {Ballot = (), Voter = key}),
               (Some "Restaurants",
                Restaurants.ui),
               (Some "Times",
                Ui.seq (Ui.h4 <xml>An example of the time format to use, applied to <i>right now</i>: </xml>,
                            Times.ui)),
               (Some "Dinners",
                Ui.seq
                    (Ui.const <xml>
                      <h2>Add a Future Dinner</h2>

                      <div class="form-group">
                        <label>When?</label>
                        <cselect source={whichT} class="form-control">
                          {times}
                        </cselect>
                      </div>

                      <div class="form-group">
                        <label>Where?</label>
                        <cselect source={whichR} class="form-control">
                          {restaurants}
                        </cselect>
                      </div>

                      <button class="btn btn-primary"
                              value="Add dinner"
                              onclick={fn _ =>
                                          t <- get whichT;
                                          r <- get whichR;
                                          case read t of
                                              None => error <xml>Bad time format</xml>
                                            | Some t =>
                                              let
                                                  val r = case String.split r #"^" of
                                                              None => {RestaurantName = r, Neighborhood = ""}
                                                            | Some (name, neighb) =>
                                                              {RestaurantName = name,
                                                               Neighborhood = neighb}
                                              in
                                                  rpc (addDinner (r ++ {Time = t}))
                                              end}/>

                      <hr/>

                      <h2>Add a Past Dinner</h2>

                      <div class="form-group">
                        <label>When? (format example:) </label>
                        <ctextbox source={whichTP} class="form-control"/>
                      </div>

                      <div class="form-group">
                        <label>Where?</label>
                        <cselect source={whichRP} class="form-control">
                          {restaurants}
                        </cselect>
                      </div>

                      <button class="btn btn-primary"
                              value="Add dinner"
                              onclick={fn _ =>
                                          t <- get whichTP;
                                          r <- get whichRP;
                                          case read t of
                                              None => error <xml>Bad time format</xml>
                                            | Some t =>
                                              let
                                                  val r = case String.split r #"^" of
                                                              None => {RestaurantName = r, Neighborhood = ""}
                                                            | Some (name, neighb) =>
                                                              {RestaurantName = name,
                                                               Neighborhood = neighb}
                                              in
                                                  rpc (addPastDinner (r ++ {Time = t}))
                                              end}/>

                      <hr/>
                     </xml>,
                     Commented.ui (Some key))))

val admin =
    requireAdmin;

    Theme.tabbed "EECS Junior Faculty Admin"
              ((Some "Users",
                EditUsers.ui),
               (Some "Times",
                EditTimes.ui),
               (Some "Restaurants",
                EditRestaurants.ui),
               (Some "Dinners",
                EditDinners.ui))

(* Every half hour, remove votes for restaurants where dinner started less than an hour ago. *)
task periodic halfHour = fn () =>
     tm <- now;
     queryI1 (SELECT dinner.RestaurantName, dinner.Neighborhood
              FROM dinner
              WHERE dinner.Time < CURRENT_TIMESTAMP
                AND dinner.Time > {[addSeconds tm (-oneHour)]})
             VoteRestaurant.removeVotesFor

fun setIt v =
    setCookie localC {Value = v,
                      Expires = None,
                      Secure = False}

val cookieSetup =
    sc <- source "";

    Theme.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={sc}/>
        <button value="Set" onclick={fn _ => v <- get sc; rpc (setIt v)}/>
        </xml>)}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={admin}>Admin</a></li>
  <li><a link={main}>Main</a></li>
</body></xml>
