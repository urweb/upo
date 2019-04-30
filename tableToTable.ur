open Bootstrap4

functor Make(M : sig
                 con key :: {(Type * Type * Type)}
                 val keyFl : folder key
                 val keyInj : $(map (fn p => sql_injectable p.1) key)

                 con otherIn :: {(Type * Type * Type)}
                 val otherInFl : folder otherIn
                 val otherInInj : $(map (fn p => sql_injectable p.1) otherIn)

                 constraint key ~ otherIn

                 con inKeyName :: Name
                 con inOtherConstraints :: {{Unit}}
                 constraint [inKeyName] ~ inOtherConstraints
                 val tableIn : sql_table (map fst3 key ++ map fst3 otherIn) ([inKeyName = map (fn _ => ()) key] ++ inOtherConstraints)
                 val labelsIn :$(map (fn _ => string) (key ++ otherIn))
                 val widgetsIn : $(map Widget.t' (key ++ otherIn))
                                  
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
                 val context : $(map fst3 key) -> transaction (option context)
                             (* Return [None] if this user may not submit the form. *)
                      
                 con others :: {Type}
                 val others : context -> $(map (sql_exp [] [] []) others)
                 constraint widgets ~ others
                 constraint constants ~ others

                 con ported :: {Type}
                 constraint widgets ~ ported
                 constraint constants ~ ported
                 constraint others ~ ported
                 val portedFl : folder ported
                 val portedInj : $(map sql_injectable ported)
                 val port : $(map fst3 (key ++ otherIn)) -> $ported
                     
                 table tableOut : $(ported ++ map fst3 widgets ++ constants ++ others)
             end) = struct

    open M

    datatype formstate =
             Pending of $(map snd3 widgets)
           | Submitted

    type input = $(map fst3 key)
    type a = {InValues : $(map fst3 (key ++ otherIn)),
              Ids : $(map (fn _ => id) widgets),
              Formstate : source formstate}

    val inFl = @Folder.concat ! keyFl otherInFl
             
    fun create k =
        cfgIn <- @Monad.mapR _ [Widget.t'] [thd3]
                (fn [nm ::_] [p ::_] => @Widget.configure)
                inFl widgetsIn;
        rowo <- oneOrNoRows1 (SELECT *
                              FROM tableIn
                              WHERE {@@Sql.easy_where [#TableIn] [map fst3 key] [map fst3 otherIn] [[]] [_] [[]] ! ! keyInj (@Folder.mp keyFl) k});
        row <- (case rowo of
                    None => error <xml>Key not found.</xml>
                  | Some r => return r);
        
        cfg <- @Monad.mapR _ [Widget.t'] [thd3]
                (fn [nm ::_] [p ::_] => @Widget.configure)
                widgetsFl widgets;
        ids <- @Monad.mapR0 _ [fn _ => id]
                (fn [nm ::_] [p ::_] => fresh)
                widgetsFl;
        ws <- @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
               (fn [nm ::_] [p ::_] => @Widget.create)
               widgetsFl widgets cfg;
        fs <- source (Pending ws);

        return {InValues = row,
                Ids = ids,
                Formstate = fs}

    fun onload _ = return ()

    fun submit inv vs =
        ctx <- context (inv --- map fst3 otherIn);
        case ctx of
            None => error <xml>Access denied</xml>
          | Some ctx =>
            dml (insert tableOut
                        (@map2 [sql_injectable] [ident] [sql_exp [] [] []]
                          (fn [t] (_ : sql_injectable t) => sql_inject)
                          portedFl portedInj (port inv)
                          ++ @map2 [fn p => sql_injectable p.1] [fst3] [fn p => sql_exp [] [] [] p.1]
                          (fn [p] (_ : sql_injectable p.1) => sql_inject)
                          widgetsFl widgetsInj vs
                          ++ @map2 [sql_injectable] [ident] [sql_exp [] [] []]
                          (fn [t] (_ : sql_injectable t) => sql_inject)
                          constantsFl constantsInj constants
                          ++ others ctx))
                   
    fun render _ self = <xml>
      <dyn signal={sub <- signal self.Formstate;
                   return (case sub of
                               Submitted => <xml>
                                 <p>Thanks for submitting the form.</p>
                               </xml>
                             | Pending ws => <xml><div>
                                 {@mapX3 [fn _ => string] [Widget.t'] [fst3] [body]
                                     (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] lab w v => <xml>
                                       <div class="form-group">
                                         <label class="control-label">{[lab]}</label>
                                         {@Widget.asValue w v}
                                       </div>
                                     </xml>)
                                     inFl labelsIn widgetsIn self.InValues}
                                              
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
                                                     rpc (submit self.InValues vs);
                                                     set self.Formstate Submitted}/>
                               </div></xml>)}/>
    </xml>

    fun ui k = {Create = create k,
                Onload = onload,
                Render = render}
    
end
