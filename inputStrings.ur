open Bootstrap4

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con fixed :: {Type}
                 con chosen :: {Unit}
                 constraint const ~ given
                 constraint (const ++ given) ~ fixed
                 constraint (const ++ given ++ fixed) ~ chosen

                 val const : $const

                 table tab : (const ++ given ++ fixed ++ mapU string chosen)

                 val chosenLabels : $(mapU string chosen)

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val givenEq : $(map eq given)

                 val textLabel : string

                 (* Authentication *)
                 val amGiven : transaction (option $given)
             end) = struct

    open M

    val givenEq : eq $given = @Record.eq givenEq givenFl

    type input = _
    type a = _

    con chosen' = mapU string chosen
    val chosenFl' = @Folder.mp chosenFl
    val chosenInj = @map0 [fn _ => sql_injectable string] (fn [u ::_] => _) chosenFl

    fun create gv =
        vs <- oneRow1 (SELECT tab.{{chosen'}}
                       FROM tab
                       WHERE {@@Sql.easy_where [#Tab] [const ++ given] [_] [_] [_] [_]
                         ! ! (constInj ++ givenInj) (@Folder.concat ! constFl givenFl) (const ++ gv)});
        vs <- @foldR [fn _ => string] [fn r => transaction $(mapU (id * source string) r)]
               (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] v acc =>
                   id <- fresh;
                   v <- source v;
                   vs <- acc;
                   return ({nm = (id, v)} ++ vs))
               (return {}) chosenFl vs;
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
                                      CLASS "glyphicon glyphicon-remove"
                                  else
                                      CLASS "glyphicon glyphicon-pencil")}/>
        </button>
        {[textLabel]}
      </h2>

      <dyn signal={exp <- signal t.Editing;
                   return (if exp then
                               <xml><div>
                                 {@mapX2 [fn _ => string] [fn _ => _ * source string] [body]
                                   (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] lab (id, src) => <xml>
                                     <div class="form-group">
                                       <label class="control-label" for={id}>{[lab]}</label>
                                       <ctextbox id={id} class="form-control" source={src}/>
                                     </div>
                                   </xml>)
                                   chosenFl' chosenLabels t.Values}

                                 <button class="btn btn-primary"
                                         value="Save"
                                         onclick={fn _ =>
                                                     vs <- @foldR [fn _ => _ * source string]
                                                            [fn r => transaction $(mapU string r)]
                                                            (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] (_, src) acc =>
                                                                v <- get src;
                                                                vs <- acc;
                                                                return ({nm = v} ++ vs))
                                                            (return {}) chosenFl t.Values;
                                                     set t.Editing False;
                                                     rpc (choose t.Given vs)}/>
                               </div></xml>
                           else
                               <xml><div>
                                 {@mapX2 [fn _ => string] [fn _ => _ * source string] [body]
                                   (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] lab (id, src) => <xml>
                                     <div class="form-group">
                                       <label class="control-label" for={id}>{[lab]}</label>
                                       <div id={id}>
                                         <dyn signal={src <- signal src; return <xml>{[src]}</xml>}/>
                                       </div>
                                     </div>
                                   </xml>)
                                   chosenFl chosenLabels t.Values}
                               </div></xml>)}/>
    </xml>

    fun ui x = {Create = create x,
                Onload = fn _ => return (),
                Render = fn _ => render}

end
