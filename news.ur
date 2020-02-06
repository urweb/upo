open Bootstrap4

datatype access =
         Forbidden
       | Read
       | Post of {User : string, MayEdit : bool, MayDelete : bool}
       | Admin of {User : string}

functor Make(M : sig
                 type title
                 type title_internal
                 type title_config
                 val title : Widget.t title title_internal title_config
                 val title_inj : sql_injectable title

                 type body
                 type body_internal
                 type body_config
                 val body : Widget.t body body_internal body_config
                 val body_inj : sql_injectable body

                 val access : transaction access

                 val onNewPost : {Title : title, Poster : string, Body : body}
                                 -> transaction unit
             end) = struct

    open M

    table post : { Title : title, Body : body, Poster : string, When : time }
      PRIMARY KEY When

    type post = { Title : title, Body : body, Poster : string, When : time }

    type editing = {Title : title_internal, Body : body_internal, TitleId : id, BodyId : id}

    datatype mode =
             Hidden
           | Expanded
           | Editing of editing

    type postr = {Post : source post,
                  Mode : source mode}

    datatype posts =
             Nil
           | Cons of postr * source posts

    datatype action =
             Add of post
           | Delete of time
           | Modify of post

    table listeners : {Channel : channel action}

    type a = {TitleConfig : title_config,
              BodyConfig : body_config,
              Access : access,
              Head : source posts,
              Tail : source (source posts),
              NewPost : source (option editing),
              Channel : channel action}

    val create =
        acc <- access;
        case acc of
            Forbidden => error <xml>Access denied</xml>
          | _ =>
            tail <- source Nil;
            posts <- queryL1 (SELECT *
                              FROM post
                              ORDER BY post.When DESC);
            head <- List.foldlM (fn p ps =>
                                    p <- source p;
                                    mode <- source Hidden;
                                    source (Cons ({Post = p, Mode = mode}, ps)))
                                tail posts;
            tail <- source tail;

            ch <- channel;
            dml (INSERT INTO listeners(Channel)
                 VALUES ({[ch]}));

            title <- @Widget.configure title;
            body <- @Widget.configure body;
            newPost <- source None;

            return {TitleConfig = title,
                    BodyConfig = body,
                    Access = acc,
                    Head = head,
                    Tail = tail,
                    NewPost = newPost,
                    Channel = ch}

    fun onload a =
        let
            fun loop () =
                act <- recv a.Channel;
                (case act of
                     Add p =>
                     tl <- get a.Tail;
                     tl' <- source Nil;
                     set a.Tail tl';
                     p <- source p;
                     mode <- source Hidden;
                     cell <- return (Cons ({Post = p, Mode = mode}, tl'));
                     set tl cell;

                     hd <- get a.Head;
                     (case hd of
                          Nil => set a.Head cell
                        | _ => return ())
                   | Delete tm =>
                     let
                         fun del ps =
                             v <- get ps;
                             case v of
                                 Nil => return ()
                               | Cons (r, ps') =>
                                 p <- get r.Post;
                                 if p.When = tm then
                                     v <- get ps';
                                     set ps v
                                 else
                                     del ps'
                     in
                         del a.Head
                     end
                   | Modify p =>
                     let
                         fun mod ps =
                             v <- get ps;
                             case v of
                                 Nil => return ()
                               | Cons (r, ps') =>
                                 p' <- get r.Post;
                                 if p'.When = p.When then
                                     set r.Post p;
                                     set r.Mode Hidden
                                 else
                                     mod ps'
                     in
                         mod a.Head
                     end);
                loop ()
        in
            spawn (loop ())
        end

    fun mayAdd acc =
        case acc of
            Post r => Some r.User
          | Admin r => Some r.User
          | _ => None

    fun mayModify acc u u' =
        case acc of
            Post r => r.User = u && r.User = u' && r.MayEdit
          | Admin _ => True
          | _ => False

    fun mayDelete acc u =
        case acc of
            Post r => r.User = u && r.MayDelete
          | Admin _ => True
          | _ => False

    fun add p =
        tm <- now;
        acc <- access;
        case mayAdd acc of
            None => error <xml>Access denied</xml>
          | Some u =>
            p <- return (p ++ {Poster = u, When = tm});
            dml (INSERT INTO post(Poster, When, Title, Body)
                 VALUES ({[u]}, {[tm]}, {[p.Title]}, {[p.Body]}));
            queryI1 (SELECT listeners.Channel FROM listeners)
            (fn {Channel = ch} => send ch (Add p));

            onNewPost {Title = p.Title, Body = p.Body, Poster = u}

    fun delete tm =
        currentUser <- oneRowE1 (SELECT (post.Poster)
                                 FROM post
                                 WHERE post.When = {[tm]});
        acc <- access;
        if not (mayDelete acc currentUser) then
            error <xml>Access denied</xml>
        else
            dml (DELETE FROM post
                 WHERE When = {[tm]});
            queryI1 (SELECT listeners.Channel FROM listeners)
            (fn {Channel = ch} => send ch (Delete tm))

    fun modify p =
        currentUser <- oneRowE1 (SELECT (post.Poster)
                                 FROM post
                                 WHERE post.When = {[p.When]});
        acc <- access;
        if not (mayModify acc currentUser p.Poster) then
            error <xml>Access denied</xml>
        else
            dml (UPDATE post
                 SET Title = {[p.Title]}, Body = {[p.Body]}
                 WHERE When = {[p.When]});
            queryI1 (SELECT listeners.Channel FROM listeners)
            (fn {Channel = ch} => send ch (Modify p))

    fun render' ctx a ps = <xml>
      <dyn signal={v <- signal ps;
                   return (case v of
                               Nil => <xml></xml>
                             | Cons (r, ps') => <xml>
                               {render' ctx a ps'}

                               <div class="card">
                                 <div class="card-header">
                                   <dyn signal={mode <- signal r.Mode;
                                                return (case mode of
                                                            Hidden => <xml><button class="btn btn-secondary"
                                                                                   onclick={fn _ => set r.Mode Expanded}>
                                                                             <span class="glyphicon glyphicon-caret-down"/>
                                                                           </button></xml>
                                                          | _ => <xml><button class="btn btn-secondary"
                                                                              onclick={fn _ => set r.Mode Hidden}>
                                                                        <span class="glyphicon glyphicon-caret-up"/>
                                                                      </button></xml>)}/>
                                   <dyn signal={p <- signal r.Post;
                                                return <xml>
                                                  {[@Widget.asValue title p.Title]} -- {[p.Poster]} at {[p.When]}

                                                  {if not (mayModify a.Access p.Poster p.Poster) then
                                                     <xml></xml>
                                                   else
                                                     <xml>
                                                       <button class="btn btn-secondary"
                                                               onclick={fn _ =>
                                                                           title <- @Widget.initialize title a.TitleConfig p.Title;
                                                                           body <- @Widget.initialize body a.BodyConfig p.Body;
                                                                           tid <- fresh;
                                                                           bid <- fresh;
                                                                           set r.Mode (Editing {Title = title, Body = body, TitleId = tid, BodyId = bid})}>
                                                         <span class="glyphicon glyphicon-edit"/>
                                                       </button>
                                                     </xml>}

                                                  {if not (mayDelete a.Access p.Poster) then
                                                     <xml></xml>
                                                   else
                                                     Ui.modalButton ctx close
                                                                    <xml>&times;</xml>
                                                                    (return (Ui.modal
                                                                                 (rpc (delete p.When))
                                                                                 <xml>Are you sure you want to delete that post by {[p.Poster]}?</xml>
                                                                                 <xml/>
                                                                                 <xml>Yes!</xml>))}
                                                </xml>}/>

                                 </div>
                                 <dyn signal={mode <- signal r.Mode;
                                              return (case mode of
                                                          Hidden => <xml></xml>
                                                        | Expanded => <xml>
                                                          <div class="card-body">
                                                            <dyn signal={p <- signal r.Post;
                                                                         return (@Widget.asValue body p.Body)}/>
                                                          </div>
                                                        </xml>
                                                        | Editing ed => <xml>
                                                          <div class="card-body">
                                                            <div class="form-group">
                                                              <label class="control-label" for={ed.TitleId}>Title</label>
                                                              {@Widget.asWidget title ed.Title (Some ed.TitleId)}
                                                              <label class="control-label" for={ed.BodyId}>Body</label>
                                                              {@Widget.asWidget body ed.Body (Some ed.BodyId)}
                                                            </div>

                                                            <dyn signal={p <- signal r.Post;
                                                                         return <xml>
                                                                           <button class="btn btn-primary"
                                                                                   value="Save"
                                                                                   onclick={fn _ =>
                                                                                               title <- current (@Widget.value title ed.Title);
                                                                                               body <- current (@Widget.value body ed.Body);
                                                                                               rpc (modify (p -- #Title -- #Body ++ {Title = title, Body = body}));
                                                                                               set r.Mode Expanded}/>
                                                                         </xml>}/>
                                                            <button class="btn btn-secondary"
                                                                    value="Cancel"
                                                                    onclick={fn _ => set r.Mode Expanded}/>
                                                          </div>
                                                        </xml>)}/>
                               </div>
                             </xml>)}/>
    </xml>
                         
    fun render ctx a =
        if (case a.Access of Forbidden => True | Read => True | _ => False) then
            <xml></xml>
        else <xml>
          <dyn signal={np <- signal a.NewPost;
                       return (case np of
                                   None => <xml><p><button class="btn btn-primary"
                                                           value="New Post"
                                                                     onclick={fn _ =>
                                                                                 title <- @Widget.create title a.TitleConfig;
                                                                                 body <- @Widget.create body a.BodyConfig;
                                                                                 tid <- fresh;
                                                                                 bid <- fresh;
                                                                                 set a.NewPost (Some {Title = title, Body = body, TitleId = tid, BodyId = bid})}/></p></xml>
                                 | Some np => <xml>
                                   <div class="form-group">
                                     <label class="control-label" for={np.TitleId}>Title</label>
                                     {@Widget.asWidget title np.Title (Some np.TitleId)}
                                     <label class="control-label" for={np.BodyId}>Body</label>
                                     {@Widget.asWidget body np.Body (Some np.BodyId)}

                                     <button class="btn btn-primary"
                                             value="Add"
                                             onclick={fn _ =>
                                                         title <- current (@Widget.value title np.Title);
                                                         body <- current (@Widget.value body np.Body);
                                                         rpc (add {Title = title, Body = body});
                                                         set a.NewPost None}/>
                                     <button class="btn btn-secondary"
                                             value="Cancel"
                                             onclick={fn _ => set a.NewPost None}/>
                                   </div>
                                 </xml>)}/>

           {render' ctx a a.Head}
        </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end
