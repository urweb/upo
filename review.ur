open Bootstrap3

functor Make(M : sig
                 con reviewer :: Name
                 con reviewed :: {Type}
                 con other :: {(Type * Type)}
                 constraint reviewed ~ other
                 constraint [reviewer] ~ [When]
                 constraint [reviewer, When] ~ reviewed
                 constraint [reviewer, When] ~ other
                 constraint [Channel] ~ reviewed
                 table tab : ([When = time, reviewer = string] ++ reviewed ++ map fst other)

                 val widgets : $(map Widget.t' other)
                 val reviewedFl : folder reviewed
                 val otherFl : folder other
                 val reviewedInj : $(map sql_injectable reviewed)
                 val otherInj : $(map (fn p => sql_injectable p.1) other)
                 val labels : $(map (fn _ => string) other)

                 val show_reviewed : show $reviewed
                 val summarize : $(map fst other) -> xbody
                 val whoami : transaction (option string)
             end) = struct

    style summary
    style full
    style fullHeader

    open M

    datatype action =
             Add of {Reviewed : $reviewed, Reviewer : string, When : time, Other : $(map fst other)}

    table specificListeners : (reviewed ++ [Channel = channel action])

    structure One = struct
        type input = _

        datatype review_state =
                 Summary of $(map (fn p => id * p.1) other)
               | Full of $(map (fn p => id * p.1) other)
               | Editing of $(map (fn p => id * p.2) other)

        type review = {Reviewer : string,
                       When : time,
                       State : source review_state}

        type a = {Reviewed : $reviewed,
                  Reviews : source (list review),
                  Widgets : $(map (fn p => id * p.2) other),
                  Channel : channel action}

        fun create key =
            rs <- List.mapQueryM (SELECT tab.{reviewer}, tab.When, tab.{{map fst other}}
                                  FROM tab
                                  WHERE {@Sql.easy_where [#Tab] ! ! reviewedInj reviewedFl key}
                                  ORDER BY tab.When)
                                 (fn {Tab = r} =>
                                     other <- @Monad.mapR _ [fst] [fn p => id * p.1]
                                               (fn [nm ::_] [p ::_] (x : p.1) =>
                                                   id <- fresh;
                                                   return (id, x))
                                               otherFl (r -- reviewer -- #When);
                                     rs <- source (Summary other);
                                     return {Reviewer = r.reviewer,
                                             When = r.When,
                                             State = rs});
            rs <- source rs;
            ws <- @Monad.mapR _ [Widget.t'] [fn p => id * p.2]
                   (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                       id <- fresh;
                       w <- @Widget.create w;
                       return (id, w))
                   otherFl widgets;
            ch <- channel;
            @@Sql.easy_insert [[Channel = _] ++ reviewed] [_]
              ({Channel = _} ++ reviewedInj)
              (@Folder.cons [#Channel] [_] ! reviewedFl)
              specificListeners ({Channel = ch} ++ key);
            return {Reviewed = key,
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
                         other <- @Monad.mapR _ [fst] [fn p => id * p.1]
                                   (fn [nm ::_] [p ::_] (x : p.1) =>
                                       id <- fresh;
                                       return (id, x))
                                   otherFl r.Other;
                         st <- source (Summary other);
                         set a.Reviews (List.append rs ({Reviewer = r.Reviewer,
                                                         When = r.When,
                                                         State = st} :: [])));
                    loop ()
            in
                spawn (loop ())
            end

        fun add key other =
            u <- whoami;
            case u of
                None => error <xml>Must be logged in to add a record</xml>
              | Some u =>
                tm <- now;
                @@Sql.easy_insert [[When = _, reviewer = _] ++ reviewed ++ map fst other] [_]
                  ({When = _, reviewer = _} ++ reviewedInj ++ otherInj)
                  (@Folder.cons [#When] [_] ! (@Folder.cons [reviewer] [_] !
                    (@Folder.concat ! reviewedFl (@Folder.mp otherFl))))
                  tab ({When = tm, reviewer = u} ++ key ++ other);
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
                                                    <div class={summary}
                                                         onclick={fn _ => set r.State (Full o)}>
                                                      {[r.Reviewer]}
                                                      ({[r.When]})
                                                      {[summarize (@mp [fn p => id * p.1] [fst]
                                                                    (fn [p] (_, x) => x)
                                                                    otherFl o)]}
                                                    </div>
                                                  </xml>
                                                | Full o => <xml>
                                                    <div class={full}>
                                                      <div class={fullHeader}
                                                           onclick={fn _ => set r.State (Summary o)}>
                                                        {[r.Reviewer]}
                                                        ({[r.When]})
                                                      </div>

                                                      {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.1] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, v : p.1) => <xml>
                                                          <div class="form-group">
                                                            <label class="control-label" for={id}>{[lab]}</label>
                                                            <span class="form-control" id={id}>
                                                              {@Widget.asValue w v}
                                                            </span>
                                                          </div>
                                                        </xml>)
                                                        otherFl labels widgets o}
                                                    </div>
                                                  </xml>
                                                | Editing _ => <xml>Editing not implemented yet</xml>)}/>
                       </xml>) rs)}/>

          <hr/>

          <h2>Add Review</h2>

          {@mapX3 [fn _ => string] [Widget.t'] [fn p => id * p.2] [body]
           (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (id, x : p.2) => <xml>
             <div class="form-group">
               <label class="control-label" for={id}>{[lab]}</label>
               {@Widget.asWidget w x (Some id)}
             </div>
           </xml>)
           otherFl labels widgets a.Widgets}

          <button class="btn btn-primary"
                  value="Add Review"
                  onclick={fn _ =>
                              vs <- @Monad.mapR2 _ [Widget.t'] [fn p => id * p.2] [fst]
                                     (fn [nm ::_] [p ::_] (w : Widget.t' p) (id, x : p.2) =>
                                         current (@Widget.value w x))
                                     otherFl widgets a.Widgets;
                              rpc (add a.Reviewed vs)}/>
        </xml>

        fun ui inp = {Create = create inp,
                      Onload = onload,
                      Render = render}
    end

end
