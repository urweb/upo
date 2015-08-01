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
                  Reviews : source (list review)}

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
            return {Reviewed = key,
                    Reviews = rs}

        fun onload _ = return ()

        fun render _ a = <xml>
          <dyn signal={rs <- signal a.Reviews;
                       return (List.mapX (fn r => <xml>
                         <dyn signal={st <- signal r.State;
                                      return (case st of
                                                  Summary o => <xml>
                                                    <div class={summary}>
                                                      {[r.Reviewer]}
                                                      ({[r.When]})
                                                      {[summarize o]}
                                                    </div>
                                                  </xml>
                                                | Full o => <xml>
                                                    <div class={full}>
                                                      <div class={fullHeader}>
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
        </xml>

        fun ui inp = {Create = create inp,
                      Onload = onload,
                      Render = render}
    end

end
