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
             end) = struct

    open M

    table submission : (key ++ [ukey = string] ++ map fst fs
                            ++ [Filename = option string, Content = blob, MimeType = string, When = time])
      PRIMARY KEY {{@primary_key [ukey] [key ++ [When = time]] ! ! (_ ++ keyInj)}},
      {{one_constraint [#Key] (@Sql.easy_foreign ! ! ! ! ! ! keyFl tab)}},
      {{one_constraint [#User] (@Sql.easy_foreign ! ! ! ! ! ! (_ : folder [ukey = _]) user)}}

    datatype status = Idle | Uploading | Uploaded of AjaxUpload.handle | Error

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
                  (_ ++ injs ++ @mp [sql_injectable_prim] [sql_injectable] @@sql_prim keyFl keyInj)
                  (@Folder.concat ! _ (@Folder.concat ! keyFl (@Folder.mp fl))) submission
                  (k ++ up ++ {When = tm, ukey = u} ++ vs)

    fun render k =
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
                        <span id={id}>{@Widget.asWidget w x}</span>
                      </div>
                    </xml>)
                    fl labels widgets ws}
                </xml>
                <xml>Submit</xml>)

end
