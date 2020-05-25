table listeners : { Topic : string, Channel : channel bool }
sequence channelIds
table channels : { Id : int, Client : client, Channel : channel bool }

fun changed topic =
    queryI1 (SELECT listeners.Channel
             FROM listeners
             WHERE listeners.Topic = {[topic]})
    (fn {Channel = ch} => send ch True)

fun changedBy id topic =
    me <- self;
    queryI1 (SELECT listeners.Channel
             FROM listeners, channels
             WHERE channels.Id = {[id]}
               AND channels.Client = {[me]}
               AND listeners.Topic = {[topic]}
               AND listeners.Channel <> channels.Channel)
    (fn {Channel = ch} => send ch True)

type server_part = int
type client_part = {Channel : channel bool,
                    Id : server_part}

fun server r = r.Id

fun listen topic =
    me <- self;
    id <- nextval channelIds;
    ch <- channel;
    dml (INSERT INTO channels(Id, Client, Channel)
         VALUES ({[id]}, {[me]}, {[ch]}));
    dml (INSERT INTO listeners(Topic, Channel)
         VALUES ({[topic]}, {[ch]}));
    return {Channel = ch, Id = id}

fun onChange cl f =
    let
        fun loop () =
            b <- recv cl.Channel;
            if b then
                f;
                loop ()
            else
                return ()
    in
        spawn (loop ())
    end

fun retire id =
    me <- self;
    cho <- oneOrNoRowsE1 (SELECT (channels.Channel)
                          FROM channels
                          WHERE channels.Client = {[me]}
                            AND channels.Id = {[id]});
    case cho of
        None => error <xml>That subscription doesn't exist.</xml>
      | Some ch =>
        send ch False;
        dml (DELETE FROM listeners
             WHERE Channel = {[ch]});
        dml (DELETE FROM channels
             WHERE Channel = {[ch]})
