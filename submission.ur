open Bootstrap3

functor Make(M : sig
                 con key1 :: Name
                 con keyT :: Type
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 constraint [Filename, Content, MimeType, When] ~ key
                 con other :: {Type}
                 constraint other ~ key
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val tab : sql_table (key ++ other) ([keyName = map (fn _ => ()) key] ++ otherConstraints)
                 val keyInj : $(map sql_injectable_prim key)
                 val keyFl : folder key
                 val keyShow : show $key

                 con ukey :: Name
                 con uother :: {Type}
                 con ukeyName :: Name
                 con uotherConstraints :: {{Unit}}
                 constraint [ukeyName] ~ uotherConstraints
                 constraint [ukey] ~ [Filename, Content, MimeType, When]
                 constraint [ukey] ~ uother
                 constraint [ukey] ~ key
                 val user : sql_table ([ukey = string] ++ uother) ([ukeyName = [ukey]] ++ uotherConstraints)
                 val whoami : transaction (option string)

                 con fs :: {(Type * Type)}
                 constraint [ukey, Filename, Content, MimeType, When] ~ fs
                 constraint key ~ fs
                 val widgets : $(map Widget.t' fs)
                 val fl : folder fs
                 val injs : $(map sql_injectable (map fst fs))
                 val labels : $(map (fn _ => string) fs)

                 val makeFilename : $key -> string (* username *) -> string
                 val mayInspect : transaction bool
             end) = struct

    open M

    con others = map fst fs ++ [Filename = option string, Content = blob, MimeType = string, When = time]
    constraint others ~ (key ++ [ukey = string])
    con submission_hidden_constraints = [Pkey = [ukey, When] ++ map (fn _ => ()) key,
                                         Key = [],
                                         User = []]
    con lame :: {{Unit}} = []
    constraint lame ~ submission_hidden_constraints
    table submission : (key ++ [ukey = string] ++ map fst fs
                            ++ [Filename = option string, Content = blob, MimeType = string, When = time])
      PRIMARY KEY {{@primary_key [ukey] [key ++ [When = time]] ! ! (_ ++ keyInj)}},
      {{one_constraint [#Key] (@Sql.easy_foreign ! ! ! ! ! ! keyFl tab)}},
      {{one_constraint [#User] (@Sql.easy_foreign ! ! ! ! ! ! (_ : folder [ukey = _]) user)}}

    datatype status = Idle | Uploading | Uploaded of AjaxUpload.handle | Error

    val keyInj' = @mp [sql_injectable_prim] [sql_injectable] @@sql_prim keyFl keyInj

    fun upload k h vs =
        cl <- AjaxUpload.claim h;
        case cl of
            AjaxUpload.NotFound => error <xml>Submission: upload not found</xml>
          | AjaxUpload.Found up =>
            u <- whoami;
            case u of
                None => error <xml>Submission: not logged in</xml>
              | Some u =>
                tm <- now;
                @@Sql.easy_insert
                  [key ++ map fst fs ++ [Filename = _, Content = _, MimeType = _, When = _, ukey = _]] [_]
                  (_ ++ injs ++ keyInj')
                  (@Folder.concat ! _ (@Folder.concat ! keyFl (@Folder.mp fl))) submission
                  (k ++ up ++ {When = tm, ukey = u} ++ vs)

    fun newUpload k =
        id <- fresh;
        st <- source Idle;
        ws <- @Monad.mapR _ [Widget.t'] [fn p => id * p.2]
              (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                  id <- fresh;
                  w <- @Widget.create w;
                  return (id, w))
              fl widgets;
        up <- AjaxUpload.render {SubmitLabel = None,
                                 OnBegin = set st Uploading,
                                 OnSuccess = fn h => set st (Uploaded h),
                                 OnError = set st Error};

        return (Ui.modal
                (st <- get st;
                 case st of
                     Uploaded h =>
                     vs <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (_, x) => current (@Widget.value w x))
                            fl widgets ws;
                     rpc (upload k h vs)
                   | _ => alert "Please select a file first.")
                <xml>New submission for {[k]}</xml>
                <xml>
                  <div class="form-group">
                    <label class="form-group" for={id}>File</label>
                    <span id={id}>
                      <span dynClass={st <- signal st;
                                      return (case st of
                                                  Idle => CLASS ""
                                                | Uploading => CLASS "glyphicon glyphicon-cloud-upload"
                                                | Uploaded _ => CLASS "glyphicon glyphicon-ok"
                                                | Error => CLASS "glyphicon glyphicon-fire")}/>
                        {up}
                    </span>
                  </div>

                  {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
                    (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] lab (w : Widget.t' p) (id, x) => <xml>
                      <div class="form-group">
                        <label class="control-label" for={id}>{[lab]}</label>
                        {@Widget.asWidget w x (Some id)}
                      </div>
                    </xml>)
                    fl labels widgets ws}
                </xml>
                <xml>Submit</xml>)

    fun latests f k =
        let
            fun retrieve u =
                b <- mayInspect;
                if not b then
                    error <xml>Submission: not authorized to retrieve submissions</xml>
                else
                    r <- oneRow1 (SELECT submission.Content, submission.MimeType
                                  FROM submission
                                  WHERE {@@Sql.easy_where [#Submission] [[ukey = _] ++ key] [_] [_] [_] [_] ! !
                                    ({ukey = _} ++ keyInj')
                                    (@Folder.cons [ukey] [_] ! keyFl) (k ++ {ukey = u})}
                                  ORDER BY submission.When DESC
                                  LIMIT 1);
                    case checkMime r.MimeType of
                        None => error <xml>Submission: bad MIME type on retrieve</xml>
                      | Some mt =>
                        setHeader (blessResponseHeader "Content-Disposition")
                                  ("attachment; filename=" ^ makeFilename k u);
                        returnBlob r.Content mt

            val listLatest =
                b <- mayInspect;
                if not b then
                    error <xml>Submission: not authorized to list submissions</xml>
                else
                    xm <- queryX1 (SELECT DISTINCT submission.{ukey}
                                   FROM submission
                                   WHERE {@@Sql.easy_where [#Submission] [key] [_] [_] [_] [_] ! ! keyInj' keyFl k}
                                   ORDER BY submission.{ukey})
                                  (fn r => <xml>
                                    <li><a link={retrieve r.ukey}>{[r.ukey]}</a> {f r.ukey}</li>
                                  </xml>);
                    return <xml>
                      <ul>
                        {xm}
                      </ul>
                    </xml>
        in
            rpc listLatest
        end
end
