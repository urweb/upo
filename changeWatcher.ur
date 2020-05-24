table listeners : { Topic : string, Channel : channel unit }

fun changed topic =
    debug ("CHANGE in " ^ topic);
    queryI1 (SELECT listeners.Channel
             FROM listeners
             WHERE listeners.Topic = {[topic]})
    (fn {Channel = ch} => debug "NOTIFY"; send ch ())

type t = channel unit

fun listen topic =
    ch <- channel;
    dml (INSERT INTO listeners(Topic, Channel)
         VALUES ({[topic]}, {[ch]}));
    return ch

fun onChange ch f =
    let
        fun loop () =
            recv ch;
            f;
            loop ()
    in
        spawn (loop ())
    end
