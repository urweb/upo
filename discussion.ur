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
                 constraint key ~ [When, Who, Text, Channel]
                 val fl : folder key
                 val kinj : $(map sql_injectable key)

                 type text
                 type text_internal
                 type text_config
                 val text : Widget.t text text_internal text_config
                 val inj : sql_injectable text

                 table message : (key ++ [When = time, Who = string, Text = text])

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

    datatype update =
             New of message
           | Edit of { When : time, Text : text }
           | Delete of time

    type a = {Config : text_config,
              Key : $key,
              Access : access,
              Head : source messages,
              Tail : source (source messages),
              NewPost : text_internal,
              Channel : channel update}

    table listeners : (key ++ [Who = channel update])

    fun create k =
        acc <- access k;
        (case acc of
             Forbidden => error <xml>Access denied</xml>
           | _ => return ());

        cfg <- @Widget.configure text;
        np <- @Widget.create text cfg;

        tail' <- source Nil;
        tail <- source tail';
        head <- query (SELECT message.When, message.Who, message.Text
                       FROM message
                       WHERE {@Sql.easy_where [#Message] ! ! kinj fl k}
                       ORDER BY message.When DESC)
                      (fn r ls =>
                          msg <- source r.Message;
                          source (Cons (msg, ls)))
                      tail';

        ch <- channel;
        @@Sql.easy_insert [key ++ [Who = _]] [_]
              (kinj ++ {Who = _})
              (@Folder.concat ! _ fl)
              listeners
              (k ++ {Who = ch});

        return {Config = cfg,
                Key = k,
                Access = acc,
                Head = head,
                Tail = tail,
                NewPost = np,
                Channel = ch}

    fun onload a =
        let
            fun loop () =
                upd <- recv a.Channel;
                (case upd of
                     New msg =>
                     let
                         fun atEnd ls =
                             lsV <- get ls;
                             case lsV of
                                 Nil =>
                                 (tail <- source Nil;
                                  msgS <- source msg;
                                  set ls (Cons (msgS, tail));
                                  set a.Tail tail;

                                  hdV <- get a.Head;
                                  case hdV of
                                      Nil => set a.Head (Cons (msgS, tail))
                                    | _ => return ())
                               | Cons (_, ls') => atEnd ls'
                     in
                         atEnd a.Head
                     end
                   | Edit r =>
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
                         findIt a.Head
                     end
                   | Delete tm =>
                     let
                         fun findIt ls =
                             lsV <- get ls;
                             case lsV of
                                 Nil => return ()
                               | Cons (msg, ls') =>
                                 msgV <- get msg;
                                 if msgV.When = tm then
                                     lsV' <- get ls';
                                     set ls lsV'
                                 else
                                     findIt ls'
                     in
                         findIt a.Head
                     end);

                loop ()
        in
            spawn (loop ())
        end

    fun postMsg k text =
        acc <- access k;
        case mayPost acc of
            None => error <xml>Access denied</xml>
          | Some u =>
            tm <- now;

            @@Sql.easy_insert [key ++ [When = _, Who = _, Text = _]] [_]
              (kinj ++ {When = _, Who = _, Text = _})
              (@Folder.concat ! _ fl)
              message
              (k ++ {When = tm, Who = u, Text = text});

            queryI1 (SELECT listeners.Who
                     FROM listeners
                     WHERE {@Sql.easy_where [#Listeners] ! ! kinj fl k})
            (fn r => send r.Who (New {When = tm, Who = u, Text = text}))

    fun render ctx a =
        let
            fun render' ls = <xml>
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

                                         {render' ls'}
                                       </div>
                                     </xml>)}/>
            </xml>
        in
            <xml>
              {render' a.Head}

              {case mayPost a.Access of
                   None => <xml></xml>
                 | Some u => <xml>
                   <h2>Post Message</h2>

                   {@Widget.asWidget text a.NewPost None}

                   <button class="btn btn-primary"
                           value="Post"
                           onclick={fn _ =>
                                       txt <- current (@Widget.value text a.NewPost);
                                       @Widget.reset text a.NewPost;
                                       rpc (postMsg a.Key txt)}/>
                 </xml>}
            </xml>
        end

    fun ui k = {Create = create k,
                Onload = onload,
                Render = render}
    
end
