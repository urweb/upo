open Bootstrap3

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 con keyRest :: {Type}
                 constraint key ~ keyRest
                 con keyName :: Name
                 con keyOtherConstraints :: {{Unit}}
                 constraint [keyName] ~ keyOtherConstraints
                 val key : sql_table (key ++ keyRest) ([keyName = map (fn _ => ()) key] ++ keyOtherConstraints)
                 val keyInj : $(map sql_injectable_prim key)
                 val keyFl : folder key
                 val keyEq : $(map eq key)
                 val keyOrd : $(map ord key)

                 con user1 :: Name
                 type userT
                 con userR :: {Type}
                 constraint [user1] ~ userR
                 con user = [user1 = userT] ++ userR
                 con userRest :: {Type}
                 constraint user ~ userRest
                 con userKeyName :: Name
                 con userOtherConstraints :: {{Unit}}
                 constraint [userKeyName] ~ userOtherConstraints
                 val user : sql_table (user ++ userRest) ([userKeyName = map (fn _ => ()) user] ++ userOtherConstraints)
                 val userInj : $(map sql_injectable_prim user)
                 val userFl : folder user
                 val userEq : $(map eq user)
                 val userOrd : $(map ord user)
                 val userShow : show $user

                 con rest :: {Type}
                 constraint key ~ user
                 constraint (key ++ user) ~ rest
                 constraint (key ++ user ++ rest) ~ [Comment]
                 val query : sql_query [] [] [] (key ++ rest)
                 val restFl : folder rest

                 val render : $(key ++ rest) -> xbody
                 val amUser : transaction (option $user)
             end) = struct

    open M

    val keyEq : eq $key = @@Record.eq [key] keyEq keyFl
    val userEq : eq $user = @@Record.eq [user] userEq userFl

    val keyOrd : ord $key = @@Record.ord [key] keyOrd keyFl
    val userOrd : ord $user = @@Record.ord [user] userOrd userFl

    val keyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim keyFl keyInj
    val userInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim userFl userInj

    table comment : (key ++ user ++ [Comment = string])
      PRIMARY KEY {{@primary_key [key1] [keyR ++ user] ! !
                    (keyInj ++ userInj)}},
      {{one_constraint [#Key] (@Sql.easy_foreign ! ! ! ! ! ! keyFl key)}},
      {{one_constraint [#User] (@Sql.easy_foreign ! ! ! ! ! ! userFl user)}}

    datatype action =
             SetKey of { Key : $key, Rest : option $rest }
           | SetComment of { Key : $key, User : $user, Comment : option string }
    (* [None] for deletion *)

    table listeners : { Channel : channel action }

    type comment = {User : $user,
                    Comment : string}

    type entry = {Key : $key,
                  Rest : $rest,
                  Comments : source (list comment),
                  YourNewComment : source (option (source string)) }

    type a = {User : option $user,
              Entries : source (list entry),
              Channel : channel action}

    type input = _

    fun create iam =
        let
            fun setup (ls : list {Q : $(key ++ rest), Comment : $(map option user ++ [Comment = option string])})
                      (acc : list entry)
                      (thisKey : option ($key * $rest))
                      (comments : list comment)
                      : transaction (list entry) =
                case ls of
                    [] =>
                    (case thisKey of
                         None => return acc
                       | Some (key, rest) =>
                         comments <- source comments;
                         ync <- source None;
                         return ({Key = key, Rest = rest, Comments = comments, YourNewComment = ync} :: acc))
                  | {Q = q, Comment = c} :: ls' =>
                    case thisKey of
                        None =>
                        (case @Sql.unnull (@Folder.concat ! (_ : folder [Comment = _]) userFl) c of
                             None =>
                             comments <- source [];
                             ync <- source None;
                             setup ls' ({Key = q --- rest, Rest = q --- key, Comments = comments,
                                         YourNewComment = ync} :: acc)
                                   None []
                           | Some c =>
                             setup ls' acc (Some (q --- rest, q --- key))
                                   ({User = c -- #Comment, Comment = c.Comment} :: []))
                      | Some (key, rest) =>
                        if @eq keyEq key (q --- rest) then
                            case @Sql.unnull (@Folder.concat ! (_ : folder [Comment = _]) userFl) c of
                                None => error <xml>WithComments: Inconsistent left join</xml>
                              | Some c => setup ls' acc thisKey ({User = c -- #Comment, Comment = c.Comment} :: [])
                        else
                            comments <- source comments;
                            comments' <- return (case @Sql.unnull (@Folder.concat ! (_ : folder [Comment = _]) userFl) c of
                                                     None => []
                                                   | Some c => {User = c -- #Comment, Comment = c.Comment} :: []);
                            ync <- source None;
                            setup ls' ({Key = key, Rest = rest, Comments = comments, YourNewComment = ync} :: acc)
                                  (Some (q --- rest, q --- key)) comments'
        in
            ents <- queryL ({{{sql_query1 [[]]
                                          {Distinct = False,
                                           From = @@sql_left_join [[]]
                                                    [[Q = key ++ rest]]
                                                    [[Comment = map (fn t => (t, option t))
                                                                        (key ++ user ++ [Comment = string])]]
                                                    ! ! !
                                                    {Comment = @Top.mp [sql_injectable_prim] [fn t => nullify t (option t)]
                                                                @@nullify_prim (@Folder.concat ! (_ : folder [Comment = _])
                                                                                 (@Folder.concat ! keyFl userFl))
                                                                (_ ++ keyInj ++ userInj)}
                                                    (sql_from_query [#Q] query)
                                                    (FROM comment)
                                                    (@@Sql.easy_join [#Q] [#Comment]
                                                       [key] [rest] [user ++ [Comment = _]]
                                                       [[]] [[]] [[]]
                                                       ! ! ! ! keyFl),
                                           Where = (WHERE TRUE),
                                           GroupBy = sql_subset_all [_],
                                           Having = (WHERE TRUE),
                                           SelectFields = sql_subset [[Q = (key ++ rest, _),
                                                                       Comment = (map option user ++ [Comment = option string], _)]],
                                           SelectExps = {}} }}}
                            ORDER BY {{{@Sql.order_by (@Folder.concat ! keyFl (@Folder.mp userFl))
                                         (@Sql.some_fields [#Q] [key] ! ! keyFl
                                           ++ @Sql.some_fields [#Comment] [map option user] ! ! (@Folder.mp userFl))
                                         sql_desc}}});
            ents <- setup ents [] None [];
            ents <- source ents;

            ch <- channel;
            dml (INSERT INTO listeners(Channel) VALUES({[ch]}));

            return {User = iam, Entries = ents, Channel = ch}
        end

    fun onload a =
        let
            fun loop () =
                act <- recv a.Channel;
                (case act of
                     SetKey r =>
                     ents <- get a.Entries;
                     (case r.Rest of
                          None => set a.Entries (List.filter (fn ent => ent.Key <> r.Key) ents)
                        | Some rest =>
                          if List.exists (fn ent => ent.Key = r.Key) ents then
                              set a.Entries
                                  (List.mp (fn ent => if ent.Key = r.Key then
                                                          ent -- #Rest ++ {Rest = rest}
                                                      else
                                                          ent) ents)
                           else
                               comments <- source [];
                               ync <- source None;
                               set a.Entries (List.sort (fn r1 r2 => r1.Key > r2.Key)
                                                        ((r -- #Rest ++ {Rest = rest,
                                                                         Comments = comments,
                                                                         YourNewComment = ync}) :: ents)))
                   | SetComment r =>
                     ents <- get a.Entries;
                     List.app (fn ent =>
                                  if ent.Key <> r.Key then
                                      return ()
                                  else
                                      cs <- get ent.Comments;
                                      cs <- return (List.filter (fn c => c.User <> r.User) cs);
                                      cs <- return (case r.Comment of
                                                        None => cs
                                                      | Some s => {User = r.User, Comment = s} :: cs);
                                      set ent.Comments (List.sort (fn r1 r2 => r1.User > r2.User) cs)) ents);
                loop ()
        in
            spawn (loop ())
        end

    fun makeComment k s =
        u <- amUser;
        case u of
            None => error <xml>WithComments: Not authenticated as user</xml>
          | Some u =>
            dml (DELETE FROM comment
                 WHERE {@@Sql.easy_where [#T] [key ++ user] [[Comment = _]] [[]] [[]] [[]] ! !
                   (keyInj' ++ userInj')
                   (@Folder.concat ! keyFl userFl)
                   (k ++ u)});
            queryI1 (SELECT listeners.Channel
                     FROM listeners)
                    (fn l => send l.Channel (SetComment {Key = k, User = u, Comment = (case s of
                                                                                           "" => None
                                                                                         | _ => Some s)}));
            case s of
                "" => return ()
              | _ =>
                @@Sql.easy_insert [key ++ user ++ [Comment = string]] [_]
                  ({Comment = _} ++ keyInj' ++ userInj')
                  (@Folder.cons [#Comment] [_] ! (@Folder.concat ! keyFl userFl))
                  comment
                  (k ++ u ++ {Comment = s})

    style entry
    style comment

    open Option

    fun render _ a = <xml>
        <dyn signal={ents <- signal a.Entries;
                     return (List.mapX (fn ent => <xml>
                       <div class={entry}>
                         {M.render (ent.Key ++ ent.Rest)}<br/>
                         <dyn signal={cs <- signal ent.Comments;
                                      return (List.mapX (fn c => <xml>
                                        <div class={comment}>
                                          <b>{[c.User]}</b> writes: {[c.Comment]}<br/>
                                        </div>
                                      </xml>) cs)}/>

                         {case a.User of
                              None => <xml/>
                            | Some iam => <xml>
                              <dyn signal={ync <- signal ent.YourNewComment;
                                           case ync of
                                               None =>
                                               cs <- signal ent.Comments;
                                               cur <- return (case List.find (fn r => a.User = Some r.User) cs of
                                                                  None => None
                                                                | Some r => Some r.Comment);
                                               return (case cur of
                                                    None => <xml>
                                                      <button class="btn btn-sm"
                                                              value="Add your comment"
                                                              onclick={fn _ =>
                                                                          s <- source "";
                                                                          set ent.YourNewComment (Some s)}/>
                                                    </xml>
                                                  | Some cur => <xml>
                                                      <button class="btn btn-sm"
                                                              value="Edit your comment"
                                                              onclick={fn _ =>
                                                                          s <- source cur;
                                                                          set ent.YourNewComment (Some s)}/>
                                                    </xml>)

                                             | Some ync => return <xml>
                                               <ctextarea source={ync} class="form-control"/>
                                               <button class="btn btn-primary"
                                                       value="Save your comment"
                                                       onclick={fn _ =>
                                                                   s <- Basis.get ync;
                                                                   set ent.YourNewComment None;
                                                                   rpc (makeComment ent.Key s)}/>
                                               <button class="btn"
                                                       value="Cancel"
                                                       onclick={fn _ => set ent.YourNewComment None}/>
                                             </xml>}/>
                            </xml>}
                       </div>
                     </xml>) ents)}/>
    </xml>

    fun ui u = {Create = create u,
                Onload = onload,
                Render = render}

end
