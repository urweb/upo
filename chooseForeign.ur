open Bootstrap4

functor Make(M : sig
                 con const :: {Type}
                 con given :: {Type}
                 con chosen :: {Type}
                 constraint const ~ given
                 constraint (const ++ given) ~ chosen

                 val const : $const

                 table choices : (const ++ given ++ chosen)

                 con optionsConst :: {Type}
                 con others :: {Type}
                 constraint others ~ chosen
                 constraint (others ++ chosen) ~ optionsConst
                 table options : (optionsConst ++ chosen ++ others)
                 val optionsConst : $optionsConst

                 val constFl : folder const
                 val givenFl : folder given
                 val chosenFl : folder chosen
                 val optionsConstFl : folder optionsConst

                 val constInj : $(map sql_injectable const)
                 val givenInj : $(map sql_injectable given)
                 val chosenInj : $(map sql_injectable chosen)
                 val optionsConstInj : $(map sql_injectable optionsConst)

                 val chosenShow : show $chosen
                 val chosenRead : read $chosen
                 val chosenEq : eq $chosen

                 val givenEq : eq $given

                 val buttonLabel : string

                 (* Authentication *)
                 val amGiven : transaction (option $given)
             end) = struct

    open M

    type choiceSet = list $chosen
    type input = _
    type a = _

    fun create gv =
        opts <- queryL1 (SELECT options.{{chosen}}
                         FROM options
                         WHERE {@@Sql.easy_where [#Options] [optionsConst] [_] [_] [_] [_]
                              ! ! optionsConstInj optionsConstFl optionsConst}
                         ORDER BY {{{@Sql.order_by chosenFl
                            (@Sql.some_fields [#Options] [chosen] ! ! chosenFl)
                           sql_asc}}});
        prefs <- queryL1 (SELECT choices.{{chosen}}
                          FROM choices
                          WHERE {@@Sql.easy_where [#Choices] [const ++ given] [_] [_] [_] [_]
                              ! ! (constInj ++ givenInj) (@Folder.concat ! constFl givenFl) (const ++ gv)}
                          ORDER BY {{{@Sql.order_by chosenFl
                            (@Sql.some_fields [#Choices] [chosen] ! ! chosenFl)
                            sql_asc}}});
        prefs <- source prefs;
        toAdd <- source "";
        return {Given = gv, Options = opts, Prefs = prefs, ToAdd = toAdd}

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
        @@Sql.easy_insert [const ++ given ++ chosen] [_] (constInj ++ givenInj ++ chosenInj)
          (@Folder.concat ! constFl (@Folder.concat ! givenFl chosenFl))
          choices (const ++ gv ++ ch)

    fun unchoose gv ch =
        ensure gv;
        dml (DELETE FROM choices
             WHERE {@@Sql.easy_where [#T] [const ++ given ++ chosen] [_] [_] [_] [_]
               ! ! (constInj ++ givenInj ++ chosenInj) (@Folder.concat ! constFl (@Folder.concat ! givenFl chosenFl)) (const ++ gv ++ ch)})

    fun render t = <xml>
      <table class="bs-table">
        <dyn signal={chs <- signal t.Prefs;
                     return <xml>
                       {List.mapX (fn ch => <xml>
                         <tr><td>
                           {[ch]}
                           <button class="close"
                                   onclick={fn _ =>
                                               rpc (unchoose t.Given ch);
                                               set t.Prefs (List.filter (fn ch' => ch' <> ch) chs)}>
                             &times;
                           </button>
                         </td></tr>
                         </xml>) chs}

                       <tr><td/></tr>

                       <tr>
                         <td>
                           <cselect class="form-control" source={t.ToAdd}>
                             {List.mapX (fn ch =>
                                            if List.mem ch chs then
                                                <xml/>
                                            else
                                                <xml><coption>{[ch]}</coption></xml>) t.Options}
                           </cselect>

                           <button class="btn btn-primary"
                                   value={buttonLabel}
                                   onclick={fn _ =>
                                               ta <- get t.ToAdd;
                                               case ta of
                                                   "" => return ()
                                                 | _ =>
                                                   ch <- return (readError ta);
                                                   rpc (choose t.Given ch);
                                                   set t.Prefs (List.sort (fn x y => show x > show y) (ch :: chs))}/>
                         </td>
                       </tr>
                     </xml>}/>
      </table>
    </xml>

    fun ui x = {
        Create = create x,
        Onload = fn _ => return (),
        Render = fn _ => render
    }

end
