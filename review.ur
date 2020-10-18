open Bootstrap4

functor Make(M : sig
                 con when :: Name
                 con reviewer :: Name
                 con reviewed :: {Type}
                 con other :: {(Type * Type * Type)}
                 constraint reviewed ~ other
                 constraint [reviewer] ~ [when]
                 constraint [reviewer, when] ~ reviewed
                 constraint [reviewer, when] ~ other
                 constraint [Channel] ~ reviewed
                 table tab : ([when = time, reviewer = string] ++ reviewed ++ map fst3 other)

                 val widgets : $(map Widget.t' other)
                 val reviewedFl : folder reviewed
                 val otherFl : folder other
                 val reviewedInj : $(map sql_injectable reviewed)
                 val otherInj : $(map (fn p => sql_injectable p.1) other)
                 val labels : $(map (fn _ => string) other)

                 val show_reviewed : show $reviewed
                 val summarize : $(map fst3 other) -> xbody
                 val whoami : transaction (option string)

                 val adjust : $reviewed -> $(map fst3 other) -> transaction $(map fst3 other)
             end) = struct

    open M

    type fields = {Reviewed : $reviewed, Reviewer : string, When : time, Other : $(map fst3 other)}

    datatype action =
             Add of fields
           | Edit of fields

    table specificListeners : (reviewed ++ [Channel = channel action])

    structure One = struct
        type input = _

        datatype review_state =
                 Summary of $(map (fn p => id * p.1) other)
               | Full of $(map (fn p => id * p.1) other)
               | Editing of $(map (fn p => id * p.1) other) * $(map (fn p => id * p.2) other)

        type review = {Reviewer : string,
                       When : time,
                       State : source review_state,
                       WaitingForRpc : source bool}

        type a = {Config : $(map thd3 other),
                  Whoami : string,
                  Reviewed : $reviewed,
                  Reviews : source (list review),
                  Widgets : $(map (fn p => id * p.2) other),
                  Channel : channel action}

        val freshWidgets =
            @Monad.mapR2 _ [Widget.t'] [thd3] [fn p => id * p.2]
             (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) =>
                 id <- fresh;
                 w <- @Widget.create w cfg;
                 return (id, w))
             otherFl widgets

        val widgetsFrom =
            @Monad.mapR3 _ [Widget.t'] [thd3] [fn p => id * p.1] [fn p => id * p.2]
             (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (_, x : p.1) =>
                 id <- fresh;
                 w <- @Widget.initialize w cfg x;
                 return (id, w))
             otherFl widgets

        val readWidgets =
            @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst3]
             (fn [nm ::_] [p ::_] (w : Widget.t' p) (id, x : p.2) =>
                 current (@Widget.value w x))
             otherFl widgets

        fun create key =
            u <- whoami;
            u <- (case u of
                      None => error <xml>Must be logged in</xml>
                    | Some u => return u);
            cfg <- @Monad.mapR _ [Widget.t'] [thd3]
                    (fn [nm ::_] [p ::_] (x : Widget.t' p) => @Widget.configure x)
                    otherFl widgets;
            rs <- List.mapQueryM (SELECT tab.{reviewer}, tab.{when}, tab.{{map fst3 other}}
                                  FROM tab
                                  WHERE {@Sql.easy_where [#Tab] ! ! reviewedInj reviewedFl key}
                                  ORDER BY tab.{when})
                                 (fn {Tab = r} =>
                                     other <- @Monad.mapR _ [fst3] [fn p => id * p.1]
                                               (fn [nm ::_] [p ::_] (x : p.1) =>
                                                   id <- fresh;
                                                   return (id, x))
                                               otherFl (r -- reviewer -- when);
                                     rs <- source (Full other);
                                     waiting <- source False;
                                     return {Reviewer = r.reviewer,
                                             When = r.when,
                                             State = rs,
                                             WaitingForRpc = waiting});
            rs <- source rs;
            ws <- freshWidgets cfg;
            ch <- channel;
            @@Sql.easy_insert [[Channel = _] ++ reviewed] [_]
              ({Channel = _} ++ reviewedInj)
              (@Folder.cons [#Channel] [_] ! reviewedFl)
              specificListeners ({Channel = ch} ++ key);
            return {Config = cfg,
                    Whoami = u,
                    Reviewed = key,
                    Reviews = rs,
                    Widgets = ws,
                    Channel = ch}

        fun onload a =
            let
                fun loop () =
                    act <- recv a.Channel;
                    (case act of
                         Add r =>
                         rs <- get a.Reviews;
                         other <- @Monad.mapR _ [fst3] [fn p => id * p.1]
                                   (fn [nm ::_] [p ::_] (x : p.1) =>
                                       id <- fresh;
                                       return (id, x))
                                   otherFl r.Other;
                         st <- source (Full other);
                         waiting <- source False;
                         set a.Reviews (List.append rs ({Reviewer = r.Reviewer,
                                                         When = r.When,
                                                         State = st,
                                                         WaitingForRpc = waiting} :: []))
                       | Edit r =>
                         rs <- get a.Reviews;
                         other <- @Monad.mapR _ [fst3] [fn p => id * p.1]
                                   (fn [nm ::_] [p ::_] (x : p.1) =>
                                       id <- fresh;
                                       return (id, x))
                                   otherFl r.Other;
                         st <- source (Full other);
                         rs <- List.mapM (fn r' =>
                                             if r'.Reviewer = r.Reviewer then
                                                 set r'.WaitingForRpc False;
                                                 return {Reviewer = r.Reviewer,
                                                         When = r.When,
                                                         State = st,
                                                         WaitingForRpc = r'.WaitingForRpc}
                                             else
                                                 return r') rs;
                         set a.Reviews (List.sort (fn r1 r2 => r1.When > r2.When) rs));
                    loop ()
            in
                spawn (loop ())
            end

        fun edit key other =
            u <- whoami;
            case u of
                None => error <xml>Must be logged in to edit a record</xml>
              | Some u =>
                tm <- now;
                other <- adjust key other;
                @@Sql.easy_update' [[reviewer = _] ++ reviewed]
                  [[when = _] ++ map fst3 other] [_] !
                  ({reviewer = _} ++ reviewedInj)
                  ({when = _} ++ otherInj)
                  (@Folder.cons [reviewer] [_] ! reviewedFl)
                  (@Folder.cons [when] [_] ! (@Folder.mp otherFl))
                  tab ({reviewer = u} ++ key)
                  ({when = tm, reviewer = u} ++ key ++ other)
                  (WHERE TRUE);
                queryI1 (SELECT specificListeners.Channel
                         FROM specificListeners
                         WHERE {@Sql.easy_where [#SpecificListeners] ! ! reviewedInj reviewedFl key})
                (fn r => send r.Channel (Edit {Reviewed = key,
                                               Reviewer = u,
                                               When = tm,
                                               Other = other}))

        fun add key other =
            u <- whoami;
            case u of
                None => error <xml>Must be logged in to add a record</xml>
              | Some u =>
                tm <- now;
                other <- adjust key other;
                @@Sql.easy_insert [[when = _, reviewer = _] ++ reviewed ++ map fst3 other] [_]
                  ({when = _, reviewer = _} ++ reviewedInj ++ otherInj)
                  (@Folder.cons [when] [_] ! (@Folder.cons [reviewer] [_] !
                    (@Folder.concat ! reviewedFl (@Folder.mp otherFl))))
                  tab ({when = tm, reviewer = u} ++ key ++ other);
                queryI1 (SELECT specificListeners.Channel
                         FROM specificListeners
                         WHERE {@Sql.easy_where [#SpecificListeners] ! ! reviewedInj reviewedFl key})
                (fn r => send r.Channel (Add {Reviewed = key,
                                              Reviewer = u,
                                              When = tm,
                                              Other = other}))

        fun render _ a = <xml>
          <dyn signal={rs <- signal a.Reviews;
                       return (List.mapX (fn r => <xml>
                         <dyn signal={st <- signal r.State;
                                      return (case st of
                                                  Summary o => <xml>
                                                    <div class="card"
                                                         onclick={fn _ => set r.State (Full o)}>
                                                      <div class="card-header"><button class="btn btn-link collapsed">
                                                        {[r.Reviewer]}
                                                        ({[r.When]})
                                                        {[summarize (@mp [fn p => id * p.1] [fst3]
                                                                      (fn [p] (_, x) => x)
                                                                      otherFl o)]}
                                                      </button></div>
                                                    </div>
                                                  </xml>
                                                | Full o => <xml>
                                                    <div class="card">
                                                      <div class="card-header"
                                                           onclick={fn _ => set r.State (Summary o)}>
                                                           <button class="btn btn-link">
                                                             {[r.Reviewer]}
                                                             ({[r.When]})
                                                           </button>
                                                      </div>

                                                      <div class="card-body">
                                                        {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.1] [body]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, v : p.1) => <xml>
                                                            <div class="form-group">
                                                              <label class="control-label" for={id}><h5>{[lab]}</h5></label>
                                                              <div id={id}>
                                                                {@Widget.asValue w v}
                                                              </div>
                                                            </div>
                                                          </xml>)
                                                          otherFl labels widgets o}

                                                        {if r.Reviewer <> a.Whoami then
                                                           <xml/>
                                                         else <xml>
                                                           <dyn signal={b <- signal r.WaitingForRpc;
                                                                        return (if b then
                                                                                  <xml/>
                                                                                else <xml>
                                                                                  <button class="card-link"
                                                                                          value="Edit Review"
                                                                                          onclick={fn _ =>
                                                                                                      ws <- widgetsFrom a.Config o;
                                                                                                      set r.State (Editing (o, ws))}/>
                                                                                </xml>)}/>
                                                         </xml>}
                                                      </div>
                                                    </div>
                                                  </xml>
                                                | Editing (o, ws) => <xml>
                                                    <div class="card">
                                                      <div class="card-header">
                                                        {[r.Reviewer]}
                                                        ({[r.When]})
                                                      </div>

                                                      <div class="card-body">
                                                        {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
                                                          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, v : p.2) => <xml>
                                                            <div class="form-group">
                                                              <label class="control-label" for={id}><h5>{[lab]}</h5></label>
                                                              {@Widget.asWidget w v (Some id)}
                                                            </div>
                                                          </xml>)
                                                          otherFl labels widgets ws}

                                                        <button class="card-link"
                                                                value="Save Review"
                                                                onclick={fn _ =>
                                                                            vs <- readWidgets ws;
                                                                            set r.State (Full o);
                                                                            set r.WaitingForRpc True;
                                                                            rpc (edit a.Reviewed vs)}/>
                                                        <button class="card-link"
                                                                value="Cancel Editing"
                                                                onclick={fn _ => set r.State (Full o)}/>
                                                      </div>
                                                    </div>
                                                  </xml>)}/>
                       </xml>) rs)}/>

          <dyn signal={rs <- signal a.Reviews;
                       return (if List.exists (fn r => r.Reviewer = a.Whoami) rs then
                                   <xml/>
                               else <xml>
                                 <hr/>

                                 <h2>Add Review</h2>

                                 {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
                                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, x : p.2) => <xml>
                                     <div class="form-group">
                                       <label class="control-label" for={id}><h5>{[lab]}</h5></label>
                                       {@Widget.asWidget w x (Some id)}
                                     </div>
                                   </xml>)
                                   otherFl labels widgets a.Widgets}

                                 <button class="btn btn-primary"
                                         value="Add Review"
                                         onclick={fn _ =>
                                                     vs <- readWidgets a.Widgets;
                                                     rpc (add a.Reviewed vs)}/>
                               </xml>)}/>
        </xml>

        fun notification _ _ = <xml></xml>
        fun buttons _ _ = <xml></xml>

        fun ui inp = {Create = create inp,
                      Onload = onload,
                      Render = render,
                      Notification = notification,
                      Buttons = buttons}
    end

    con hidden_fields = _
    constraint hidden_fields ~ reviewed

    structure Several = struct
        type input = _
        type a = _

        fun create e =
            queryL1 (SELECT *
                     FROM tab AS T
                     WHERE {e}
                     ORDER BY T.{when} DESC)

        fun onload _ = return ()

        fun render _ rs = <xml>
          {List.mapX (fn r => <xml>
            <div class="card">
              <div class="card-header">
                {[r -- when -- reviewer --- (map fst3 other)]}
                -- {[r.reviewer]}
                ({[r.when]})
              </div>

              <div class="card-body">
                {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                  (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (v : p.1) => <xml>
                    <div class="form-group">
                      <label class="control-label"><h5>{[lab]}</h5></label>
                      <div>
                        {@Widget.asValue w v}
                      </div>
                    </div>
                  </xml>)
                  otherFl labels widgets (r -- when -- reviewer --- reviewed)}
              </div>
            </div>
          </xml>) rs}
        </xml>

        fun notification _ _ = <xml></xml>
        fun buttons _ _ = <xml></xml>

        fun ui e = {Create = create e,
                    Onload = onload,
                    Render = render,
                    Notification = notification,
                    Buttons = buttons}
    end

end
