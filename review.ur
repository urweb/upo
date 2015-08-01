open Bootstrap3

functor Make(M : sig
                 con reviewer :: Name
                 con reviewed :: {Type}
                 con other :: {(Type * Type)}
                 constraint reviewed ~ other
                 constraint [reviewer] ~ [When]
                 constraint [reviewer, When] ~ reviewed
                 constraint [reviewer, When] ~ other
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

    structure One = struct
        type input = _

        datatype review_state =
                 Summary of $(map fst other)
               | Full of $(map fst other)
               | Editing of $(map snd other)

        type review = {Reviewer : string,
                       When : time,
                       State : source review_state}

        type a = {Reviewed : $reviewed,
                  Reviews : source (list review),
                  Widgets : $(map (fn p => id * p.2) other)}

        fun create key =
            rs <- List.mapQueryM (SELECT tab.{reviewer}, tab.When, tab.{{map fst other}}
                                  FROM tab
                                  WHERE {@Sql.easy_where [#Tab] ! ! reviewedInj reviewedFl key}
                                  ORDER BY tab.When)
                                 (fn {Tab = r} =>
                                     rs <- source (Summary (r -- reviewer -- #When));
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
            return {Reviewed = key,
                    Reviews = rs,
                    Widgets = ws}

        fun onload _ = return ()

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
                  tab ({When = tm, reviewer = u} ++ key ++ other)

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
                                                      {[summarize o]}
                                                    </div>
                                                  </xml>
                                                | Full o => <xml>
                                                    <div class={full}>
                                                      <div class={fullHeader}
                                                           onclick={fn _ => set r.State (Summary o)}>
                                                        {[r.Reviewer]}
                                                        ({[r.When]})
                                                      </div>

                                                      {@mapX3 [fn _ => string] [Widget.t'] [fst] [body]
                                                        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (v : p.1) => <xml>
                                                          <div class="form-group">
                                                            <label class="control-label">{[lab]}</label>
                                                            {@Widget.asValue w v}
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
