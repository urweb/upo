open Bootstrap3

type permission = {Add : bool,
                   Delete : bool,
                   Modify : bool}

functor Make(M : sig
                 con fs :: {(Type * Type * Type)}
                 val widgets : $(map Widget.t' fs)
                 table tab : $(map fst3 fs)
                 val fl : folder fs
                 val eqs : $(map eq (map fst3 fs))
                 val ords : $(map ord (map fst3 fs))
                 val injs : $(map sql_injectable (map fst3 fs))

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission

                 val onAdd : $(map fst3 fs) -> transaction unit
                 val onDelete : $(map fst3 fs) -> transaction unit
                 val onModify : {Old : $(map fst3 fs), New : $(map fst3 fs)} -> transaction unit
             end) = struct

    open M

    con data = $(map fst3 fs)
    con state = $(map snd3 fs)
    con config = $(map thd3 fs)

    val eq_data : eq data = @@Record.eq [map fst3 fs] eqs (@@Folder.mp [fst3] [_] fl)
    val ord_data : ord data = @@Record.ord [map fst3 fs] ords (@@Folder.mp [fst3] [_] fl)

    datatype action =
             ADD of data
           | DEL of data
           | MOD of { Old : data, New : data }

    table listeners : { Channel : channel action }

    type row = { Editing : source (option state),
                 Content : data }
    type a = { Config : config,
               Perm : permission,
               Rows : source (list row),
               ToAdd : state,
               Channel : channel action }

    val freshRow = @Monad.mapR2 _ [Widget.t'] [thd3] [snd3]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.create w) fl widgets

    val initRow = @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.initialize w)
                    fl widgets

    val rowOut = @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                  (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : snd3 p) =>
                      current (@Widget.value w v))
                  fl widgets

    val create =
        perm <- permission;
        config <- @Monad.mapR _ [Widget.t'] [thd3]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w) fl widgets;
        toAdd <- freshRow config;

        rows <- List.mapQueryM (SELECT *
                                FROM tab
                                ORDER BY {{{@Sql.order_by (@@Folder.mp [fst3] [_] fl)
                                  (@@Sql.some_fields [#Tab] [map fst3 fs] [[]] [[]] [[]] [[]] ! !
                                    (@@Folder.mp [fst3] [_] fl)) sql_asc}}})
                               (fn r =>
                                   editing <- source None;
                                   return {Editing = editing,
                                           Content = r.Tab});
        rows <- source rows;

        chan <- channel;
        dml (INSERT INTO listeners(Channel) VALUES ({[chan]}));

        return {Config = config,
                Perm = perm,
                Rows = rows,
                ToAdd = toAdd,
                Channel = chan}

    fun onload a =
        let
            fun loop () =
                msg <- recv a.Channel;
                (case msg of
                     ADD d =>
                     rows <- get a.Rows;
                     editing <- source None;
                     set a.Rows (List.sort (fn a b => a.Content > b.Content)
                                           ({Editing = editing,
                                             Content = d}
                                                :: rows))
                   | DEL d =>
                     rows <- get a.Rows;
                     set a.Rows (List.filter (fn a => a.Content <> d) rows)
                   | MOD d =>
                     rows <- get a.Rows;
                     editing <- source None;
                     set a.Rows (List.sort (fn a b => a.Content > b.Content)
                                           (List.mp (fn a =>
                                                        if a.Content = d.Old then
                                                            {Editing = editing,
                                                             Content = d.New}
                                                        else
                                                            a) rows)));
                loop ()
        in
            spawn (loop ())
        end

    fun add r =
        perm <- permission;
        (if perm.Add then
             return ()
         else
             error <xml>Don't have permission to add row</xml>);

        @@Sql.easy_insert [map fst3 fs] [_] injs (@@Folder.mp [fst3] [_] fl) tab r;

        queryI1 (SELECT * FROM listeners)
        (fn x => send x.Channel (ADD r));

        onAdd r

    fun del r =
        perm <- permission;
        (if perm.Delete then
             return ()
         else
             error <xml>Don't have permission to delete row</xml>);

        dml (DELETE FROM tab
             WHERE {@@Sql.easy_where [#T] [map fst3 fs] [[]] [[]] [[]] [[]] ! !
               injs (@@Folder.mp [fst3] [_] fl) r});

        queryI1 (SELECT * FROM listeners)
        (fn x => send x.Channel (DEL r));

        onDelete r

    fun mod r =
        perm <- permission;
        (if perm.Modify then
             return ()
         else
             error <xml>Don't have permission to delete row</xml>);

        dml (@@update [[]] [_] [map fst3 fs] !
              (@map2 [fn p => sql_injectable p.1] [fst3] [fn p => sql_exp _ _ _ p.1]
                (fn [p] => @sql_inject)
                fl injs r.New)
              tab
              (@@Sql.easy_where [#T] [map fst3 fs] [[]] [[]] [[]] [[]] ! !
                 injs (@@Folder.mp [fst3] [_] fl) r.Old));

        queryI1 (SELECT * FROM listeners)
        (fn x => send x.Channel (MOD r));

        onModify r

    fun render ctx a = <xml>
      <table class="bs3-table table-striped">
        <tr>
          <th/>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) => <xml><th>{[s]}</th></xml>)
            fl labels}
        </tr>

        <dyn signal={rs <- signal a.Rows;
                     return (List.mapX (fn r => <xml><tr>
                       <dyn signal={ed <- signal r.Editing;
                                    return (case ed of
                                                None => <xml>
                                                  <td>
                                                    {if a.Perm.Delete then
                                                         Ui.modalButton ctx (CLASS "btn glyphicon glyphicon-remove")
                                                                        <xml/>
                                                                        (return (Ui.modal
                                                                                     (rpc (del r.Content))
                                                                                     <xml>Are you sure you want to delete this row?</xml>
                                                                                     <xml/>
                                                                                     <xml>Yes!</xml>))
                                                     else
                                                         <xml/>}
                                                    {if a.Perm.Modify then <xml>
                                                         <button class="btn glyphicon glyphicon-pencil"
                                                                 onclick={fn _ =>
                                                                             fr <- initRow a.Config r.Content;
                                                                             set r.Editing (Some fr)}/>
                                                     </xml> else
                                                         <xml/>}
                                                  </td>
                                                  {@mapX2 [Widget.t'] [fst3] [_]
                                                    (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                                                 (w : Widget.t' p) (v : fst3 p) =>
                                                        <xml><td>{@Widget.asValue w v}</td></xml>)
                                                    fl widgets r.Content}
                                                </xml>
                                              | Some ws => <xml>
                                                <td>
                                                  <button class="btn glyphicon glyphicon-ok"
                                                          onclick={fn _ =>
                                                                      vs <- rowOut ws;
                                                                      set r.Editing None;
                                                                      rpc (mod {Old = r.Content,
                                                                                New = vs})}/>
                                                  <button class="btn glyphicon glyphicon-remove"
                                                          onclick={fn _ => set r.Editing None}/>
                                                </td>
                                                {@mapX2 [Widget.t'] [snd3] [_]
                                                  (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                                               (w : Widget.t' p) (v : snd3 p) =>
                                                      <xml><td>{@Widget.asWidget w v None}</td></xml>)
                                                  fl widgets ws}
                                              </xml>)}/>
                     </tr></xml>) rs)}/>
        {if a.Perm.Add then
             <xml>
               <tr/>

               <tr>
                 <th><button value="Add:"
                             class="btn btn-primary"
                             onclick={fn _ =>
                                         r <- rowOut a.ToAdd;
                                         rpc (add r)}/></th>

                 {@mapX2 [Widget.t'] [snd3] [_]
                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : snd3 p) =>
                       <xml><td>{@Widget.asWidget w v None}</td></xml>)
                   fl widgets a.ToAdd}
               </tr>
             </xml>
         else
             <xml/>}
      </table>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end
