open Bootstrap

datatype access =
         Forbidden
       | Read
       | Post of { User : string, MayEdit : bool, MayDelete : bool, MayMarkClosed : bool }
       | Admin of { User : string }

fun mayPost acc =
    case acc of
        Post r => Some r.User
      | Admin r => Some r.User
      | _ => None

fun mayEdit acc poster =
    case acc of
        Post r => if r.MayEdit && poster = r.User then Some r.User else None
      | Admin r => Some r.User
      | _ => None

fun mayDelete acc poster =
    case acc of
        Post r => if r.MayDelete && poster = r.User then Some r.User else None
      | Admin r => Some r.User
      | _ => None

fun mayClose acc poster =
    case acc of
        Post r => if r.MayMarkClosed && poster = r.User then Some r.User else None
      | Admin r => Some r.User
      | _ => None

functor Make(M : sig
                 con key :: {Type}
                 con thread :: Name
                 constraint [thread] ~ [When, Who, Text, Closed, Private, Subject]
                 constraint key ~ [thread, When, Who, Text, Closed, Private, Subject]
                 val fl : folder key
                 val kinj : $(map sql_injectable key)

                 type text_internal
                 type text_config
                 val text : Widget.t string text_internal text_config

                 table message : (key ++ [thread = time, When = time, Who = string, Text = string])

                 val access : $key -> transaction access

                 val showOpenVsClosed : bool
                 val allowPrivate : bool
                 val onNewMessage : transaction (list string)
                    -> $(key ++ [thread = time, Subject = string, Who = string, Text = string])
                    -> transaction unit
             end) = struct

    open M

    type input = $key

    type message = {When : time,
                    Who : string,
                    Text : string}

    datatype messages =
             Nil
           | Cons of source message * source messages

    type threadr = {Thread : time,
                    Subject : string,
                    Private : bool,
                    Head : source messages,
                    Tail : source (source messages),
                    Closed : source bool,
                    FirstPoster : string}
                  
    datatype threads =
             TNil
           | TCons of threadr * source threads

    datatype update =
             NewThread of { Thread : time, Subject : string, Private : bool, FirstPoster : string }
           | SetClosed of { Thread : time, Closed : bool }
           | New of { Thread : time, When : time, Who : string, Text : string }
           | Edit of { Thread: time, When : time, Text : string }
           | Delete of { Thread : time, When : time }

    type a = {Config : text_config,
              Key : $key,
              Access : access,
              Head : source threads,
              Tail : source (source threads),
              Thread : source string,
              NewThread : source (option (source string * source bool)),
              NewPost : text_internal,
              Channel : channel update,
              ShowWhich : source string}

    table threads : (key ++ [thread = time, Who = string, Text = string, Closed = bool, Private = bool])

    table adminListeners : (key ++ [Who = channel update])
    (* These listeners can see all threads. *)

    table readonlyListeners : (key ++ [Who = channel update])
    (* These listeners can only see public threads. *)

    table loggedinListeners : (key ++ [Who = channel update, Private = string])
    (* These listeners can see public threads as well as private threads that they started. *)

    fun create k =
        acc <- access k;
        (case acc of
             Forbidden => error <xml>Access denied</xml>
           | _ => return ());

        cfg <- @Widget.configure text;
        np <- @Widget.create text cfg;
        nt <- source None;

        tail' <- source TNil;
        tail <- source tail';
        let
            fun thatBigQuery wc =
                queryL (SELECT message.{thread}, message.When, message.Who, message.Text, threads.Text, threads.Closed, threads.Private
                        FROM message JOIN threads ON threads.{thread} = message.{thread}
                          AND {@@Sql.easy_join [#Message] [#Threads] [key]
                          [[thread = _, When = _, Who = _, Text = _]]
                          [[thread = _, Text = _, Closed = _, Who = _, Private = _]] [_] [_] [[]]
                          ! ! ! ! fl}
                        WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                          AND {wc}
                        ORDER BY message.{thread}, message.When DESC)
        in
            rows <- (case acc of
                         Admin _ => thatBigQuery (SQL TRUE)
                       | Read => thatBigQuery (SQL NOT threads.Private)
                       | Post {User = u, ...} => thatBigQuery (SQL NOT threads.Private OR threads.Who = {[u]})
                       | Forbidden => error <xml>Discussion: impossible Forbidden</xml>);
            (threads, unfinished) <- List.foldlM
                (fn r (threads, unfinished) =>
                    case unfinished of
                        None =>
                        tail' <- source Nil;
                        tail <- source tail';
                        msg <- source (r.Message -- thread);
                        msgS <- source (Cons (msg, tail'));
                        cl <- source r.Threads.Closed;
                        return (threads, Some (r.Message.thread, r.Threads.Text, tail, msgS, cl, r.Message.Who, r.Threads.Private))
                      | Some (thread, subj, ttail, msgS, cl, fp, priv) =>
                        if thread = r.Message.thread then
                            msg <- source (r.Message -- thread);
                            msgS' <- source (Cons (msg, msgS));
                            return (threads, Some (thread, subj, ttail, msgS', cl, r.Message.Who, r.Threads.Private))
                        else
                            tail' <- source Nil;
                            tail <- source tail';
                            msg <- source (r.Message -- thread);
                            msgS' <- source (Cons (msg, tail'));
                            cl' <- source r.Threads.Closed;

                            ts <- source (TCons ({Thread = thread, Subject = subj, Private = priv, Head = msgS, Tail = ttail, Closed = cl, FirstPoster = fp}, threads));

                            return (ts, Some (r.Message.thread, r.Threads.Text, tail, msgS', cl', r.Message.Who, r.Threads.Private)))
                (tail', None) rows;

            threads <- (case unfinished of
                            None => return threads
                          | Some (thread, subj, ttail, msgS, cl, fp, priv) =>
                            source (TCons ({Thread = thread, Subject = subj, Private = priv, Head = msgS, Tail = ttail, Closed = cl, FirstPoster = fp}, threads)));

            ch <- channel;
            (case acc of
                 Admin _ =>
                 @@Sql.easy_insert [key ++ [Who = _]] [_]
                   (kinj ++ {Who = _})
                   (@Folder.concat ! _ fl)
                   adminListeners
                   (k ++ {Who = ch})
               | Read =>
                 @@Sql.easy_insert [key ++ [Who = _]] [_]
                   (kinj ++ {Who = _})
                   (@Folder.concat ! _ fl)
                   readonlyListeners
                   (k ++ {Who = ch})
               | Post {User = u, ...} =>
                 @@Sql.easy_insert [key ++ [Who = _, Private = _]] [_]
                   (kinj ++ {Who = _, Private = _})
                   (@Folder.concat ! _ fl)
                   loggedinListeners
                   (k ++ {Who = ch, Private = u})
               | Forbidden => error <xml>Discussion: impossible Forbidden</xml>);

            thread <- source "";
            sw <- source "Only show open";

            return {Config = cfg,
                    Key = k,
                    Access = acc,
                    Head = threads,
                    Tail = tail,
                    Thread = thread,
                    NewThread = nt,
                    NewPost = np,
                    Channel = ch,
                    ShowWhich = sw}
        end

    fun withThread f thread ls =
        lsV <- get ls;
        case lsV of
            TNil => return ()
          | TCons (th, ls') =>
            if th.Thread = thread then
                f th.Head th.Tail th.Closed
            else
                withThread f thread ls'

    fun onload a =
        let
            fun loop () =
                upd <- recv a.Channel;
                (case upd of
                     NewThread r =>
                     let
                         fun atEnd ls =
                             lsV <- get ls;
                             case lsV of
                                 TNil =>
                                 (head <- source Nil;
                                  tail <- source head;
                                  tail' <- source TNil;
                                  cl <- source False;
                                  cell <- return (TCons ({Thread = r.Thread,
                                                          Subject = r.Subject,
                                                          Private = r.Private,
                                                          Head = head,
                                                          Tail = tail,
                                                          Closed = cl,
                                                          FirstPoster = r.FirstPoster}, 
                                                         tail'));
                                  set ls cell;
                                  set a.Tail tail';

                                  hdV <- get a.Head;
                                  case hdV of
                                      TNil => set a.Head cell
                                    | _ => return ())
                               | TCons (_, ls') => atEnd ls'
                     in
                         atEnd a.Head
                     end
                   | SetClosed r =>
                     withThread (fn _ _ cl => set cl r.Closed) r.Thread a.Head
                   | New msg =>
                     withThread (fn head tail _ =>
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
                     withThread (fn head _ _ =>
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
                     withThread (fn head _ _ =>
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

    fun broadcast f k isPrivate u =
        if isPrivate then
            queryI (      SELECT (adminListeners.Who)
                          FROM adminListeners
                          WHERE {@Sql.easy_where [#AdminListeners] ! ! kinj fl k}
                    UNION SELECT (loggedinListeners.Who)
                          FROM loggedinListeners
                          WHERE {@Sql.easy_where [#LoggedinListeners] ! ! kinj fl k}
                            AND loggedinListeners.Private = {[u]})
                   (fn r => f r.1)
        else
            queryI (      SELECT (adminListeners.Who)
                          FROM adminListeners
                          WHERE {@Sql.easy_where [#AdminListeners] ! ! kinj fl k}
                    UNION SELECT (loggedinListeners.Who)
                          FROM loggedinListeners
                          WHERE {@Sql.easy_where [#LoggedinListeners] ! ! kinj fl k}
                    UNION SELECT (readonlyListeners.Who)
                          FROM readonlyListeners
                          WHERE {@Sql.easy_where [#ReadonlyListeners] ! ! kinj fl k})
                   (fn r => f r.1)

    fun newThread k subj isPrivate =
        isPrivate <- return (isPrivate && allowPrivate);
        acc <- access k;
        case mayPost acc of
            None => error <xml>Access denied</xml>
          | Some u =>
            tm <- now;

            @@Sql.easy_insert [key ++ [thread = _, Text = _, Closed = _, Who = _, Private = _]] [_]
              (kinj ++ {thread = _, Text = _, Closed = _, Who = _, Private = _})
              (@Folder.concat ! _ fl)
              threads
              (k ++ {thread = tm, Text = subj, Who = u, Closed = False, Private = isPrivate});

            broadcast (fn ch => send ch (NewThread {Thread = tm, Subject = subj, Private = isPrivate, FirstPoster = u}))
                      k isPrivate u;
            return tm

    fun postMsg k thread text =
        acc <- access k;
        case mayPost acc of
            None => error <xml>Access denied</xml>
          | Some u =>
            row <- oneOrNoRows1 (SELECT threads.Who, threads.Private, threads.Text
                                 FROM threads
                                 WHERE {@Sql.easy_where [#Threads] ! ! kinj fl k}
                                   AND threads.{thread} = {[thread]});

            case row of
                None => error <xml>Trying to post to nonexistent thread</xml>
              | Some row =>
                tm <- now;

                @@Sql.easy_insert [key ++ [thread = _, When = _, Who = _, Text = _]] [_]
                  (kinj ++ {thread = _, When = _, Who = _, Text = _})
                  (@Folder.concat ! _ fl)
                  message
                  (k ++ {thread = thread, When = tm, Who = u, Text = text});

                broadcast (fn ch => send ch (New {Thread = thread, When = tm, Who = u, Text = text}))
                          k row.Private row.Who;

                onNewMessage
                    (List.mapQuery (SELECT DISTINCT message.Who
                                    FROM message
                                    WHERE {@@Sql.easy_where [#Message] [key ++ [thread = _]]
                                      [[When = _, Who = _, Text = _]]
                                      [[]] [[]] [[]] ! !
                                      (kinj ++ {thread = _})
                                      (@Folder.concat ! _ fl)
                                      (k ++ {thread = thread})})
                                   (fn {Message = {Who = w}} => w))
                    (k ++ {thread = thread, Subject = row.Text, Who = u, Text = text})

    fun saveMsg k thread msg text =
        acc <- access k;
        wh <- oneOrNoRowsE1 (SELECT (message.Who)
                             FROM message
                             WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                               AND message.{thread} = {[thread]}
                               AND message.When = {[msg]});

        case wh of
            None => error <xml>Trying to edit nonexistent message</xml>
          | Some wh =>
            case mayEdit acc wh of
                None => error <xml>Access denied</xml>
              | Some u =>
                dml (UPDATE message
                     SET Text = {[text]}
                     WHERE {@@Sql.easy_where [#T] [key ++ [thread = _, When = _]] [[Who = _, Text = _]]
                       [[]] [[]] [[]] ! !
                       (kinj ++ {thread = _, When = _})
                       (@Folder.concat ! _ fl)
                       (k ++ {thread = thread, When = msg})});

                row <- oneOrNoRows1 (SELECT threads.Who, threads.Private
                                     FROM threads
                                     WHERE {@Sql.easy_where [#Threads] ! ! kinj fl k}
                                       AND threads.{thread} = {[thread]});

                case row of
                    None => error <xml>Trying to save message in nonexistent thread</xml>
                  | Some row =>
                    broadcast (fn ch => send ch (Edit {Thread = thread, When = msg, Text = text}))
                    k row.Private row.Who

    fun deleteMsg k thread msg =
        acc <- access k;
        wh <- oneOrNoRowsE1 (SELECT (message.Who)
                             FROM message
                             WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                               AND message.{thread} = {[thread]}
                               AND message.When = {[msg]});

        case wh of
            None => error <xml>Trying to delete nonexistent message</xml>
          | Some wh =>
            case mayDelete acc wh of
                None => error <xml>Access denied</xml>
              | Some u =>
                dml (DELETE FROM message
                     WHERE {@@Sql.easy_where [#T] [key ++ [thread = _, When = _]] [[Who = _, Text = _]]
                       [[]] [[]] [[]] ! !
                       (kinj ++ {thread = _, When = _})
                       (@Folder.concat ! _ fl)
                       (k ++ {thread = thread, When = msg})});

                row <- oneOrNoRows1 (SELECT threads.Who, threads.Private
                                     FROM threads
                                     WHERE {@Sql.easy_where [#Threads] ! ! kinj fl k}
                                       AND threads.{thread} = {[thread]});

                case row of
                    None => error <xml>Trying to delete message in nonexistent thread</xml>
                  | Some row =>
                    broadcast (fn ch => send ch (Delete {Thread = thread, When = msg}))
                    k row.Private row.Who

    fun setClosed k th cl =
        acc <- access k;
        wh <- oneOrNoRowsE1 (SELECT (message.Who)
                             FROM message
                             WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                               AND message.{thread} = {[th]}
                             ORDER BY message.When
                             LIMIT 1);

        case wh of
            None => error <xml>Access denied</xml>
          | Some wh =>
            case mayClose acc wh of
                None => error <xml>Access denied</xml>
              | Some u =>
                dml (UPDATE threads
                     SET Closed = {[cl]}
                     WHERE {@@Sql.easy_where [#T] [key ++ [thread = _]] [[Text = _, Closed = _, Who = _, Private = _]]
                       [[]] [[]] [[]] ! !
                       (kinj ++ {thread = _})
                       (@Folder.concat ! _ fl)
                       (k ++ {thread = th})});

                row <- oneOrNoRows1 (SELECT threads.Who, threads.Private
                                     FROM threads
                                     WHERE {@Sql.easy_where [#Threads] ! ! kinj fl k}
                                       AND threads.{thread} = {[th]});

                case row of
                    None => error <xml>Trying to close nonexistent thread</xml>
                  | Some row =>
                    broadcast (fn ch => send ch (SetClosed {Thread = th, Closed = cl}))
                    k row.Private row.Who

    fun render ctx a =
        let
            fun renderThreads onlyClosed ls =
                h <- signal ls;
                case h of
                    TNil => return <xml></xml>
                  | TCons (th, ls') =>
                    x <- renderThreads onlyClosed ls';
                    cl <- signal th.Closed;
                    copt <- return <xml>
                      <coption value={show (toMilliseconds th.Thread)}>{[th.Subject]}{[if th.Private then
                                                                                           " [private]"
                                                                                       else
                                                                                           ""]}</coption>
                    </xml>;
                    return <xml>
                      {x}
                      {if not showOpenVsClosed || Option.isNone onlyClosed
                          || onlyClosed = Some cl then
                           copt
                       else
                           <xml></xml>}
                    </xml>

            fun renderPostsOfThread (th : time) ls =
                let
                    fun renderPosts ls = <xml>
                      <dyn signal={h <- signal ls;
                                   return (case h of
                                               Nil => <xml></xml>
                                             | Cons (msg, ls') => <xml>
                                               <div class="card">
                                                 <dyn signal={r <- signal msg;
                                                              return <xml>
                                                                <div class="card-header">{[r.Who]} at {[r.When]}
                                                                  {case mayEdit a.Access r.Who of
                                                                       None => <xml></xml>
                                                                     | Some _ =>
                                                                       Ui.modalButton ctx (CLASS "btn btn-secondary")
                                                                                      <xml><span class="glyphicon glyphicon-edit"/></xml>
                                                                                      (newText <- @Widget.initialize text a.Config r.Text;
                                                                                       return (Ui.modal
                                                                                                   (text' <- current (@Widget.value text newText);
                                                                                                    rpc (saveMsg a.Key th r.When text'))
                                                                                                   <xml>
                                                                                                     <h2>Editing Post</h2>

                                                                                                     {@Widget.asWidget text newText None}
                                                                                                   </xml>
                                                                                                   <xml/>
                                                                                                   <xml>Save</xml>))}
                                                                  {case mayDelete a.Access r.Who of
                                                                       None => <xml></xml>
                                                                     | Some _ =>
                                                                       Ui.modalButton ctx close
                                                                                      <xml>&times;</xml>
                                                                                      (return (Ui.modal
                                                                                                   (rpc (deleteMsg a.Key th r.When))
                                                                                                   <xml>Are you sure you want to delete that post by {[r.Who]}?</xml>
                                                                                                   <xml/>
                                                                                                   <xml>Yes!</xml>))}
                                                                </div>

                                                                <div class="card-body">{@Widget.asValue text r.Text}</div>
                                                              </xml>}/>
                                               </div>

                                               {renderPosts ls'}

                                             </xml>)}/>
                    </xml>
                in
                    h <- signal ls;
                    case h of
                        TNil => return <xml></xml>
                      | TCons (r, ls') =>
                        if r.Thread = th then
                            return (renderPosts r.Head)
                        else
                            renderPostsOfThread th ls'
                end

            fun renderOpennessOfThread (th : time) ls =
                h <- signal ls;
                case h of
                    TNil => return <xml></xml>
                  | TCons (r, ls') =>
                    if r.Thread = th then
                        cl <- signal r.Closed;
                        return (if Option.isNone (mayClose a.Access r.FirstPoster) then
                                    <xml></xml>
                                else if cl then <xml>
                                  <b>Closed</b>
                                  <button class="btn btn-secondary"
                                  value="Mark open"
                                  onclick={fn _ => rpc (setClosed a.Key th False)}/>
                                </xml> else <xml>
                                  <b>Open</b>
                                  <button class="btn btn-secondary"
                                  value="Mark closed"
                                  onclick={fn _ => rpc (setClosed a.Key th True)}/>
                                </xml>)
                    else
                        renderOpennessOfThread th ls'
        in
            <xml>
              {if not showOpenVsClosed then
                   <xml></xml>
               else
                   <xml>
                     <cselect source={a.ShowWhich}>
                       <coption>Show all</coption>
                       <coption>Only show open</coption>
                       <coption>Only show closed</coption>
                     </cselect>
                   </xml>}

              Threads: <dyn signal={sw <- signal a.ShowWhich;
                                    x <- renderThreads (case sw of
                                                            "Only show open" => Some False
                                                          | "Only show closed" => Some True
                                                          | _ => None) a.Head;
                                    return <xml>
                                      <cselect source={a.Thread}>
                                        {x}
                                      </cselect>
                                    </xml>}/>
              <dyn signal={nt <- signal a.NewThread;
                           return (case nt of
                                       None => <xml>
                                         <button class="btn btn-secondary"
                                                 value="New Thread"
                                                 onclick={fn _ =>
                                                             s <- source "";
                                                             p <- source False;
                                                             set a.NewThread (Some (s, p))}/>
                                       </xml>
                                     | Some (s, priv) => <xml>
                                       New thread:
                                       <ctextbox source={s}/>
                                       {if allowPrivate then
                                            <xml>Private? <ccheckbox source={priv}/></xml>
                                        else
                                            <xml></xml>}
                                       <button class="btn btn-primary"
                                               value="Create"
                                               onclick={fn _ =>
                                                           subj <- get s;
                                                           priv <- get priv;
                                                           set a.NewThread None;
                                                           th <- rpc (newThread a.Key subj priv);
                                                           set a.Thread (show (toMilliseconds th))}/>
                                       <button class="btn btn-secondary"
                                               value="Cancel"
                                               onclick={fn _ => set a.NewThread None}/>
                                       </xml>)}/>

              <hr/>

              {if not showOpenVsClosed then
                   <xml></xml>
               else <xml>
                 <dyn signal={th <- signal a.Thread;
                              case th of
                                  "" => return <xml></xml>
                                | _ => renderOpennessOfThread (fromMilliseconds (readError th)) a.Head}/>
               </xml>}

              <dyn signal={th <- signal a.Thread;
                           case th of
                               "" => return <xml></xml>
                             | _ => renderPostsOfThread (fromMilliseconds (readError th)) a.Head}/>

              <dyn signal={th <- signal a.Thread;
                           return (case th of
                                       "" => <xml></xml>
                                     | _ =>
                                       case mayPost a.Access of
                                           None => <xml></xml>
                                         | Some u => <xml>
                                           <h2>Post Message</h2>

                                           {@Widget.asWidget text a.NewPost None}<br/>

                                           <button dynClass={v <- @Widget.value text a.NewPost;
                                                             return (case v of
                                                                         "" => CLASS "btn disabled"
                                                                       | _ => CLASS "btn btn-primary")}
                                                   value="Post"
                                                   onclick={fn _ =>
                                                               txt <- current (@Widget.value text a.NewPost);
                                                               @Widget.reset text a.NewPost;
                                                               rpc (postMsg a.Key (fromMilliseconds (readError th)) txt)}/>
                                         </xml>)}/>
            </xml>
        end

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui k = {Create = create k,
                Onload = onload,
                Render = render,
                Notification = notification,
                Buttons = buttons}

    functor Todo(N : sig
                     con tag :: Name
                     con user :: Name
                     con aother :: {Type}
                     constraint key ~ aother
                     constraint [user] ~ (key ++ aother)
                     constraint [Assignee, Due, Done, Kind] ~ (key ++ [thread = time])
                     val inj : $(map sql_injectable_prim M.key)

                     table assignments : (key ++ [user = option string] ++ aother)
                     (* Recording who is responsible for which items *)

                     val title : string
                     val render : $(key ++ [thread = time]) -> string (* username *) -> xbody
                 end) =
            Todo.WithCompletionFlag(struct
                                        open N

                                        con key = key
                                        con subkey = [thread = time]
                                        con done = #Closed
                                        con other = [Text = string, Private = bool, Who = string]
                                        constraint key ~ [thread = time]
                                        constraint (key ++ [thread = time]) ~ [Text = string, Private = bool, Who = string]
                                        constraint [Closed] ~ (key ++ [thread = time] ++ [Text = string, Private = bool, Who = string])

                                        val fl = fl
                                        val sfl = _
                                        val inj = inj
                                        val sinj = _

                                        con items_hidden_constraints = _
                                        con empty :: {Type} = []
                                        constraint empty ~ items_hidden_constraints
                                        val items = threads
                                    end)
    
end
