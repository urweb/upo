open Bootstrap4

functor Make(M : sig
                 con key :: {Type}
                 con rest :: {Type}
                 constraint key ~ rest

                 table tab : (key ++ rest)

                 val keyFl : folder key
                 val restFl : folder rest

                 val keyShow : $(map show key)
                 val keyRead : $(map read key)
                 val keyEq : $(map eq key)
                 val keyInj : $(map sql_injectable key)

                 val restShow : $(map show rest)
                 val restRead : $(map read rest)
                 val restInj : $(map sql_injectable rest)

                 val labels : $(map (fn _ => string) (key ++ rest))

                 val authorized : transaction bool
             end) = struct

    open M

    type a = _

    con all = key ++ rest
    val allFl = @Folder.concat ! keyFl restFl
    val allInj = keyInj ++ restInj

    val keyEq' : eq $key = @Record.eq keyEq keyFl

    val create =
        vs <- List.mapQueryM (SELECT *
                              FROM tab
                              ORDER BY {{{@Sql.order_by keyFl
                                (@Sql.some_fields [#Tab] [key] ! ! keyFl)
                                sql_asc}}})
                             (fn {Tab = r} =>
                                 curKey <- @foldR2 [show] [ident]
                                            [fn r => transaction $(map (fn _ => source string) r)]
                                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) v acc =>
                                                v <- source (show v);
                                                vs <- acc;
                                                return ({nm = v} ++ vs))
                                            (return {}) keyFl keyShow (r --- rest);
                                 rest <- @foldR2 [show] [ident]
                                          [fn r => transaction $(map (fn _ => source string) r)]
                                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) v acc =>
                                              v <- source (show v);
                                              vs <- acc;
                                              return ({nm = v} ++ vs))
                                          (return {}) restFl restShow (r --- key);
                                 return {OldKey = r --- rest,
                                         Cur = curKey ++ rest});
        vs <- source vs;
        new <- @fold [fn r => transaction $(map (fn _ => source string) r)]
                (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] acc =>
                    v <- source "";
                    vs <- acc;
                    return ({nm = v} ++ vs))
                (return {}) allFl;
        return {Rows = vs, New = new}

    val ensure =
        b <- authorized;
        if b then
            return ()
        else
            error <xml>Authorization failure</xml>

    fun del key =
        ensure;
        dml (DELETE FROM tab
             WHERE {@@Sql.easy_where [#T] [key] [_] [_] [_] [_]
               ! ! keyInj keyFl key})

    fun save rows =
        ensure;
        List.app (fn r =>
                     @@Sql.easy_update' [key] [rest] [_] ! keyInj restInj
                       keyFl restFl
                       tab r.OldKey r.New (WHERE TRUE)) rows

    fun add row =
        ensure;
        @@Sql.easy_insert [all] [_] allInj allFl tab row

    fun render ctx t = <xml>
      <button class="btn btn-primary"
              value="Save"
              onclick={fn _ =>
                          vs <- get t.Rows;
                          vs <- List.mapM (fn r =>
                                              new <- @foldR2 [read] [fn _ => source string] [fn r => transaction $r]
                                                      (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) src acc =>
                                                          v <- get src;
                                                          vs <- acc;
                                                          return ({nm = readError v} ++ vs))
                                                      (return {}) allFl (keyRead ++ restRead) r.Cur;
                                              return {OldKey = r.OldKey,
                                                      New = new}) vs;
                          rpc (save vs)}/>

      <table class="bs-table table-striped">
        <tr>
          <th/>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] lab =>
                <xml><th>{[lab]}</th></xml>)
            allFl labels}
        </tr>

        <dyn signal={vs <- signal t.Rows;
                     return (List.mapX (fn r => <xml>
                       <tr>
                         <td>
                           {Ui.modalButton ctx (CLASS "btn btn-secondary")
                                           <xml><span class="glyphicon glyphicon-remove"/></xml>
                                           (return (Ui.modal
                                                        (rpc (del r.OldKey);
                                                         set t.Rows (List.filter (fn r' => r'.OldKey <> r.OldKey) vs))
                                                        <xml>Are you sure you want to delete this row?</xml>
                                                        <xml/>
                                                        <xml>Yes!</xml>))}
                         </td>

                         {@mapX [fn _ => source string] [tr]
                           (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] src =>
                               <xml><td><ctextbox class="form-control" source={src}/></td></xml>)
                           allFl r.Cur}
                         </tr>
                     </xml>) vs)}/>

        <tr/>

        <tr>
          <td>
            <button class="btn btn-primary"
                    value="Add Row"
                    onclick={fn _ =>
                                (dups, new) <- @foldR2 [read] [fn _ => source string]
                                                [fn r => transaction ($(map (fn _ => source string) r) * $r)]
                                                (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) src acc =>
                                                    v <- get src;
                                                    dup <- source v;
                                                    set src "";
                                                    (dups, vs) <- acc;
                                                    return ({nm = dup} ++ dups,
                                                            {nm = readError v} ++ vs))
                                                (return ({}, {})) allFl (keyRead ++ restRead) t.New;
                                rpc (add new);
                                vs <- get t.Rows;
                                set t.Rows (List.append vs ({OldKey = new --- rest, Cur = dups} :: []))}/>
          </td>

          {@mapX [fn _ => source string] [tr]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] src =>
                <xml><td><ctextbox class="form-control" source={src}/></td></xml>)
            allFl t.New}
        </tr>
      </table>
    </xml>

    val ui = {Create = create,
              Render = render,
              Onload = fn _ => return ()}
end
