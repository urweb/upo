open Bootstrap

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con fixed :: {Type}
                 con chosen :: {(Type * Type * Type)}
                 constraint const ~ given
                 constraint (const ++ given) ~ fixed
                 constraint (const ++ given ++ fixed) ~ chosen

                 val const : $const

                 table tab : (const ++ given ++ fixed ++ map fst3 chosen)

                 val chosenLabels : $(map (fn _ => string) chosen)

                 val widgets : $(map Widget.t' chosen)

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val givenEq : $(map eq given)
                 val chosenInj : $(map (fn p => sql_injectable p.1) chosen)

                 val textLabel : string

                 val amGiven : transaction (option $given)
             end) = struct

    open M

    val givenEq : eq $given = @Record.eq givenEq givenFl

    type input = _
    type a = _

    con chosen' = map fst3 chosen
    val chosenFl' = @Folder.mp chosenFl

    fun create gv =
        vs <- oneRow1 (SELECT tab.{{chosen'}}
                       FROM tab
                       WHERE {@@Sql.easy_where [#Tab] [const ++ given] [_] [_] [_] [_]
                         ! ! (constInj ++ givenInj) (@Folder.concat ! constFl givenFl) (const ++ gv)});
        vs <- @foldR2 [Widget.t'] [fst3] [fn r => transaction $(map (fn p => id * p.2) r)]
               (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] w v acc =>
                   cfg <- @Widget.configure w;
                   id <- fresh;
                   v <- @Widget.initialize w cfg v;
                   vs <- acc;
                   return ({nm = (id, v)} ++ vs))
               (return {}) chosenFl widgets vs;
        editing <- source False;
        return {Given = gv, Values = vs, Editing = editing}

    fun ensure gv =
        user <- amGiven;
        case user of
            None => error <xml>Must be authenticated to access this page</xml>
          | Some user =>
            if user = gv then
                return ()
            else
                error <xml>Wrong user to be accessing this page</xml>

    fun choose gv ch =
        ensure gv;
        @@Sql.easy_update'' [const ++ given] [chosen'] [_] [fixed]
          ! ! (constInj ++ givenInj) chosenInj
          (@Folder.concat ! constFl givenFl) chosenFl'
          tab (const ++ gv) ch

    fun render t = <xml>
      <h2>
        <button class="btn btn-secondary"
                onclick={fn _ =>
                            exp <- get t.Editing;
                            set t.Editing (not exp)}>
          <span dynClass={exp <- signal t.Editing;
                          return (if exp then
                                      CLASS "glyphicon glyphicon-trash"
                                  else
                                      CLASS "glyphicon glyphicon-pencil-alt")}/>
        </button>
        {[textLabel]}
      </h2>

      <dyn signal={exp <- signal t.Editing;
                   return (if exp then
                               <xml><div>
                                 {@mapX3 [Widget.t'] [fn _ => string] [fn p => _ * p.2] [body]
                                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] w lab (id, src) => <xml>
                                     <div class="form-group">
                                       <label class="control-label" for={id}>{[lab]}</label>
                                       {@Widget.asWidget w src (Some id)}
                                     </div>
                                   </xml>)
                                   chosenFl widgets chosenLabels t.Values}

                                 <button class="btn btn-primary"
                                         value="Save"
                                         onclick={fn _ =>
                                                     vs <- @foldR2 [Widget.t'] [fn p => _ * p.2]
                                                            [fn r => transaction $(map fst3 r)]
                                                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] w (_, src) acc =>
                                                                v <- current (@Widget.value w src);
                                                                vs <- acc;
                                                                return ({nm = v} ++ vs))
                                                            (return {}) chosenFl widgets t.Values;
                                                     set t.Editing False;
                                                     rpc (choose t.Given vs)}/>
                               </div></xml>
                           else
                               <xml><div>
                                 {@mapX3 [Widget.t'] [fn _ => string] [fn p => _ * p.2] [body]
                                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] w lab (id, src) => <xml>
                                     <div class="form-group">
                                       <label class="control-label" for={id}>{[lab]}</label>
                                       <div id={id}>
                                         <dyn signal={src <- @Widget.value w src;
                                                      return (@Widget.asValue w src)}/>
                                       </div>
                                     </div>
                                   </xml>)
                                   chosenFl widgets chosenLabels t.Values}
                               </div></xml>)}/>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui x = {Create = create x,
                Onload = fn _ => return (),
                Render = fn _ => render,
                Notification = notification,
                Buttons = buttons}

end
