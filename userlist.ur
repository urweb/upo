open Bootstrap4

functor Make(M : sig
                 con key :: Name
                 con rest :: {Type}
                 constraint [key] ~ rest
                 con keyName :: Name
                 con otherKeys :: {{Unit}}
                 constraint [keyName] ~ otherKeys
                 val user : sql_table ([key = string] ++ rest) ([keyName = [key]] ++ otherKeys)

                 val whoami : transaction (option string)
             end) = struct
    open M

    datatype delta =
             Joined of string
           | Left of string

    type a = {Users : source (list string),
              Channel : channel delta}

    table connected : {User : option string,
                       Client : client,
                       Channel : channel delta}

    val create =
        uo <- whoami;

        self <- self;
        ch <- channel;

        (case uo of
             None => return ()
           | Some u =>
             alreadyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                                       FROM connected
                                       WHERE connected.User = {[uo]});
             if alreadyThere then
                 return ()
             else
                 queryI1 (SELECT connected.Channel
                          FROM connected)
                 (fn r => send r.Channel (Joined u)));

        dml (INSERT INTO connected(User, Client, Channel)
             VALUES ({[uo]}, {[self]}, {[ch]}));

        users <- List.mapQuery (SELECT DISTINCT connected.User
                                FROM connected
                                WHERE NOT (connected.User IS NULL)
                                ORDER BY connected.User)
                 (fn r => case r.Connected.User of
                              None => error <xml>Impossible missing user</xml>
                            | Some u => u);
        users <- source users;

        return {Users = users, Channel = ch}

    fun onload self =
        let
            fun loop () =
                d <- recv self.Channel;
                users <- get self.Users;
                set self.Users (case d of
                                    Joined u => List.sort gt (u :: users)
                                  | Left u => List.filter (fn u' => u' <> u) users);
                loop ()
        in
            spawn (loop ())
        end

    fun render _ self = <xml>
      <ul class="list-group">
        <dyn signal={users <- signal self.Users;
                     return (List.mapX (fn u => <xml><li class="list-group-item">
                       {[u]}
                     </li></xml>) users)}/>
      </ul>
    </xml>

    fun notification _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification}

    task clientLeaves = fn self =>
                           uo <- oneOrNoRowsE1 (SELECT DISTINCT (connected.User)
                                                FROM connected
                                                WHERE connected.Client = {[self]});
                           case uo of
                               Some (Some u) =>
                               userElsewhere <- oneRowE1 (SELECT COUNT( * ) > 0
                                                          FROM connected
                                                          WHERE connected.User = {[Some u]}
                                                            AND connected.Client <> {[self]});
                               if userElsewhere then
                                   return ()
                               else
                                   queryI1 (SELECT connected.Channel
                                            FROM connected
                                            WHERE connected.Client <> {[self]})
                                           (fn r => send r.Channel (Left u))
                             | _ => return ()

end
