open Bootstrap3

datatype access =
         Forbidden
       | Read
       | Post of { User : string, MayEdit : bool, MayDelete : bool}
       | Admin of { User : string }

fun mayPost acc =
    case acc of
        Post r => Some r.User
      | Admin r => Some r.User
      | _ => None

style post
style post_header
style post_body

functor Make(M : sig
                 con key :: {Type}
                 constraint key ~ [Thread, When, Who, Text]
                 val fl : folder key
                 val kinj : $(map sql_injectable key)

                 type text
                 type text_internal
                 type text_config
                 val text : Widget.t text text_internal text_config
                 val inj : sql_injectable text

                 table message : (key ++ [Thread = time, When = time, Who = string, Text = text])

                 val access : $key -> transaction access
             end) = struct

    open M

    type input = $key

    type message = {When : time,
                    Who : string,
                    Text : text}

    datatype messages =
             Nil
           | Cons of source message * source messages

    type thread = {Thread : time,
                   Subject : string,
                   Head : source messages,
                   Tail : source (source messages)}
                  
    datatype threads =
             TNil
           | TCons of thread * source threads

    datatype update =
             New of { Thread : time, When : time, Who : string, Text : text }
           | Edit of { Thread: time, When : time, Text : text }
           | Delete of { Thread : time, When : time }

    type a = {Config : text_config,
              Key : $key,
              Access : access,
              Head : source threads,
              Tail : source (source threads),
              Thread : source string,
              NewPost : text_internal,
              Channel : channel update}

    table threads : (key ++ [Thread = time, Text = string])
    table listeners : (key ++ [Who = channel update])

    fun create k =
        acc <- access k;
        (case acc of
             Forbidden => error <xml>Access denied</xml>
           | _ => return ());

        cfg <- @Widget.configure text;
        np <- @Widget.create text cfg;

        tail' <- source TNil;
        tail <- source tail';
        (threads, unfinished) <- query (SELECT message.Thread, message.When, message.Who, message.Text, threads.Text
                                        FROM message JOIN threads ON threads.Thread = message.Thread
                                          AND {@@Sql.easy_join [#Message] [#Threads] [key]
                                            [[Thread = _, When = _, Who = _, Text = _]]
                                            [[Thread = _, Text = _]] [_] [_] [[]]
                                            ! ! ! ! fl}
                                        WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                                        ORDER BY message.Thread DESC, message.When DESC)
                                 (fn r (threads, unfinished) =>
                                     case unfinished of
                                         None =>
                                         tail' <- source Nil;
                                         tail <- source tail';
                                         msg <- source (r.Message -- #Thread);
                                         msgS <- source (Cons (msg, tail'));
                                         return (threads, Some (r.Message.Thread, r.Threads.Text, tail, msgS))
                                       | Some (thread, subj, ttail, msgS) =>
                                         if thread = r.Message.Thread then
                                             msg <- source (r.Message -- #Thread);
                                             msgS' <- source (Cons (msg, msgS));
                                             return (threads, Some (thread, subj, ttail, msgS'))
                                         else
                                             tail' <- source Nil;
                                             tail <- source tail';
                                             msg <- source (r.Message -- #Thread);
                                             msgS <- source (Cons (msg, tail'));

                                             ts <- source (TCons ({Thread = thread, Subject = subj, Head = msgS, Tail = ttail}, threads));

                                             return (ts, Some (r.Message.Thread, r.Threads.Text, tail, msgS)))
                                 (tail', None);
        threads <- (case unfinished of
                        None => return threads
                      | Some (thread, subj, ttail, msgS) => source (TCons ({Thread = thread, Subject = subj, Head = msgS, Tail = ttail}, threads)));

        ch <- channel;
        @@Sql.easy_insert [key ++ [Who = _]] [_]
              (kinj ++ {Who = _})
              (@Folder.concat ! _ fl)
              listeners
              (k ++ {Who = ch});

        thread <- source "";

        return {Config = cfg,
                Key = k,
                Access = acc,
                Head = threads,
                Tail = tail,
                Thread = thread,
                NewPost = np,
                Channel = ch}

    fun withThread f thread ls =
        lsV <- get ls;
        case lsV of
            TNil => return ()
          | TCons (th, ls') =>
            if th.Thread = thread then
                f th.Head th.Tail
            else
                withThread f thread ls'

    fun onload a =
        let
            fun loop () =
                upd <- recv a.Channel;
                (case upd of
                     New msg =>
                     withThread (fn head tail =>
                                    let
                                        fun atEnd ls =
                                            lsV <- get ls;
                                            case lsV of
                                                Nil =>
                                                (tail' <- source Nil;
                                                 msgS <- source (msg -- #Thread);
                                                 set ls (Cons (msgS, tail'));
                                                 set tail tail';

                                                 hdV <- get head;
                                                 case hdV of
                                                     Nil => set head (Cons (msgS, tail'))
                                                   | _ => return ())
                                              | Cons (_, ls') => atEnd ls'
                                    in
                                        atEnd head
                                    end) msg.Thread a.Head
                   | Edit r =>
                     withThread (fn head _ =>
                                    let
                                        fun findIt ls =
                                            lsV <- get ls;
                                            case lsV of
                                                Nil => return ()
                                              | Cons (msg, ls') =>
                                                msgV <- get msg;
                                                if msgV.When = r.When then
                                                    set msg (msgV -- #Text ++ {Text = r.Text})
                                                else
                                                    findIt ls'
                                    in
                                        findIt head
                                    end) r.Thread a.Head
                   | Delete r =>
                     withThread (fn head _ =>
                                    let
                                        fun findIt ls =
                                            lsV <- get ls;
                                            case lsV of
                                                Nil => return ()
                                              | Cons (msg, ls') =>
                                                msgV <- get msg;
                                                if msgV.When = r.When then
                                                    lsV' <- get ls';
                                                    set ls lsV'
                                                else
                                                    findIt ls'
                                    in
                                        findIt head
                                    end) r.Thread a.Head);

                loop ()
        in
            spawn (loop ())
        end

    fun postMsg k thread text =
        acc <- access k;
        case mayPost acc of
            None => error <xml>Access denied</xml>
          | Some u =>
            tm <- now;

            @@Sql.easy_insert [key ++ [Thread = _, When = _, Who = _, Text = _]] [_]
              (kinj ++ {Thread = _, When = _, Who = _, Text = _})
              (@Folder.concat ! _ fl)
              message
              (k ++ {Thread = thread, When = tm, Who = u, Text = text});

            queryI1 (SELECT listeners.Who
                     FROM listeners
                     WHERE {@Sql.easy_where [#Listeners] ! ! kinj fl k})
            (fn r => send r.Who (New {Thread = thread, When = tm, Who = u, Text = text}))

    fun render ctx a =
        let
            fun renderThreads ls =
                h <- signal ls;
                case h of
                    TNil => return <xml></xml>
                  | TCons (th, ls') =>
                    x <- renderThreads ls';
                    return <xml>
                      <coption value={show th.Thread}>{[th.Subject]}</coption>
                      {x}
                    </xml>

            fun renderPosts ls = <xml>
              <dyn signal={h <- signal ls;
                           return (case h of
                                       Nil => <xml></xml>
                                     | Cons (msg, ls') => <xml>
                                       <div class={post}>
                                         <dyn signal={r <- signal msg;
                                                      return <xml>
                                                        <div class={post_header}>{[r.Who]} at {[r.When]}</div>

                                                        <div class={post_body}>{@Widget.asValue text r.Text}</div>
                                                      </xml>}/>

                                         {renderPosts ls'}
                                       </div>
                                     </xml>)}/>
            </xml>

            fun renderPostsOfThread (th : time) ls =
                h <- signal ls;
                case h of
                    TNil => return <xml></xml>
                  | TCons (r, ls') =>
                    if r.Thread = th then
                        return (renderPosts r.Head)
                    else
                        renderPostsOfThread th ls'
        in
            <xml>
              Threads: <dyn signal={x <- renderThreads a.Head;
                                    return <xml>
                                      <cselect source={a.Thread}>
                                        {x}
                                      </cselect>
                                    </xml>}/>

              <dyn signal={th <- signal a.Thread;
                           case th of
                               "" => return <xml></xml>
                             | _ => renderPostsOfThread (readError th) a.Head}/>

              <dyn signal={th <- signal a.Thread;
                           return (case th of
                                       "" => <xml></xml>
                                     | _ =>
                                       case mayPost a.Access of
                                           None => <xml></xml>
                                         | Some u => <xml>
                                           <h2>Post Message</h2>

                                           {@Widget.asWidget text a.NewPost None}
                                           
                                           <button class="btn btn-primary"
                                                   value="Post"
                                                   onclick={fn _ =>
                                                               txt <- current (@Widget.value text a.NewPost);
                                                               @Widget.reset text a.NewPost;
                                                               rpc (postMsg a.Key (readError th) txt)}/>
                                         </xml>)}/>
            </xml>
        end

    fun ui k = {Create = create k,
                Onload = onload,
                Render = render}
    
end
