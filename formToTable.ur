open Bootstrap4

functor Make(M : sig
                 con widgets :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' widgets)
                 val widgetsFl : folder widgets
                 val widgetsInj : $(map (fn p => sql_injectable p.1) widgets)
                 val labels : $(map (fn _ => string) widgets)

                 con constants :: {Type}
                 val constants : $constants
                 val constantsFl : folder constants
                 val constantsInj : $(map sql_injectable constants)

                 constraint constants ~ widgets
                                    
                 type context
                 val context : transaction (option context)
                      
                 con others :: {Type}
                 val others : context -> $(map (sql_exp [] [] []) others)
                 constraint widgets ~ others
                 constraint constants ~ others

                 table tab : $(map fst3 widgets ++ constants ++ others)

                 val onSubmit : context -> $(map fst3 widgets ++ constants) -> transaction unit
             end) = struct

    open M

    datatype formstate =
             Pending of $(map snd3 widgets)
           | Submitted

    type input = $(map (fn p => option p.1) widgets)

    type a = {Config : $(map thd3 widgets),
              Ids : $(map (fn _ => id) widgets),
              Formstate : source formstate}

    fun create ws =
        cfg <- @Monad.mapR _ [Widget.t'] [thd3]
                (fn [nm ::_] [p ::_] => @Widget.configure)
                widgetsFl widgets;
        ids <- @Monad.mapR0 _ [fn _ => id]
                (fn [nm ::_] [p ::_] => fresh)
                widgetsFl;
        ws <- @Monad.mapR3 _ [Widget.t'] [thd3] [fn p => option p.1] [snd3]
               (fn [nm ::_] [p ::_] w cfg vo =>
                   case vo of
                       None => @Widget.create w cfg
                     | Some v => @Widget.initialize w cfg v)
               widgetsFl widgets cfg ws;
        fs <- source (Pending ws);
        return {Config = cfg,
                Ids = ids,
                Formstate = fs}

    fun onload _ = return ()

    fun submit vs =
        ctx <- context;
        case ctx of
            None => error <xml>Access denied</xml>
          | Some ctx =>
            dml (insert tab (@map2 [fn p => sql_injectable p.1] [fst3] [fn p => sql_exp [] [] [] p.1]
                              (fn [p] (_ : sql_injectable p.1) => sql_inject)
                              widgetsFl widgetsInj vs
                              ++ @map2 [sql_injectable] [ident] [sql_exp [] [] []]
                              (fn [t] (_ : sql_injectable t) => sql_inject)
                              constantsFl constantsInj constants
                              ++ others ctx));
            onSubmit ctx (vs ++ constants)
                   
    fun render _ self = <xml>
      <dyn signal={sub <- signal self.Formstate;
                   return (case sub of
                               Submitted => <xml>
                                 <p>Thanks for submitting the form.</p>

                                 <button class="btn btn-primary"
                                         value="Submit Again"
                                         onclick={fn _ =>
                                                     ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                                                            (fn [nm ::_] [p ::_] => @Widget.create)
                                                            widgetsFl widgets self.Config;
                                                     set self.Formstate (Pending ws)}/>
                               </xml>
                             | Pending ws => <xml><div>
                                 {@mapX4 [fn _ => string] [Widget.t'] [fn _ => id] [snd3] [body]
                                     (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] lab w id v => <xml>
                                       <div class="form-group">
                                         <label class="control-label" for={id}>{[lab]}</label>
                                         {@Widget.asWidget w v (Some id)}
                                       </div>
                                     </xml>)
                                     widgetsFl labels widgets self.Ids ws}

                                 <button class="btn btn-primary"
                                         value="Submit"
                                         onclick={fn _ =>
                                                     vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.2) => current (@Widget.value w v))
                                                            widgetsFl widgets ws;
                                                     rpc (submit vs);
                                                     set self.Formstate Submitted}/>
                               </div></xml>)}/>
    </xml>

    fun notification _ = <xml></xml>

    fun ui ws = {Create = create ws,
                 Onload = onload,
                 Render = render,
                 Notification = notification}
end
