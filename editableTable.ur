open Bootstrap3

type permission = {Add : bool,
                   Delete : bool,
                   Modify : bool}

functor Make(M : sig
                 con fs :: {(Type * Type)}
                 val widgets : $(map Widget.t' fs)
                 table tab : $(map fst fs)
                 val fl : folder fs
                 val eqs : $(map eq (map fst fs))
                 val ords : $(map ord (map fst fs))
                 val injs : $(map sql_injectable (map fst fs))

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission
             end) = struct

    open M

    con data = $(map fst fs)
    con state = $(map snd fs)

    val eq_data : eq data = @@Record.eq [map fst fs] eqs (@@Folder.mp [fst] [_] fl)
    val ord_data : ord data = @@Record.ord [map fst fs] ords (@@Folder.mp [fst] [_] fl)

    datatype action =
             ADD of data
           | DEL of data
           | MOD of { Old : data, New : data }

    table listeners : { Channel : channel action }

    type row = { Editing : source (option state),
                 Content : data }
    type a = { Perm : permission,
               Rows : source (list row),
               ToAdd : state,
               Channel : channel action }

    val freshRow = @Monad.mapR _ [Widget.t'] [snd]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.create w) fl widgets

    val initRow = @Monad.mapR2 _ [Widget.t'] [fst] [snd]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.initialize w)
                    fl widgets

    val create =
        perm <- permission;
        toAdd <- freshRow;

        rows <- List.mapQueryM (SELECT *
                                FROM tab
                                ORDER BY {{{@Sql.order_by (@@Folder.mp [fst] [_] fl)
                                  (@@Sql.some_fields [#Tab] [map fst fs] [[]] [[]] [[]] [[]] ! !
                                    (@@Folder.mp [fst] [_] fl)) sql_asc}}})
                               (fn r =>
                                   editing <- source None;
                                   return {Editing = editing,
                                           Content = r.Tab});
        rows <- source rows;

        chan <- channel;
        dml (INSERT INTO listeners(Channel) VALUES ({[chan]}));
                                   
        return {Perm = perm,
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
                     set a.Rows (List.mp (fn a =>
                                             if a.Content = d.Old then
                                                 {Editing = editing,
                                                  Content = d.New}
                                             else
                                                 a) rows));
                loop ()
        in
            loop ()
        end

    fun add r =
        perm <- permission;
        (if perm.Add then
             return ()
         else
             error <xml>Don't have permission to add row</xml>);

        @@Sql.easy_insert [map fst fs] [_] injs (@@Folder.mp [fst] [_] fl) tab r;

        queryI1 (SELECT * FROM listeners)
        (fn x => send x.Channel (ADD r))

    fun del r =
        perm <- permission;
        (if perm.Delete then
             return ()
         else
             error <xml>Don't have permission to delete row</xml>);

        dml (DELETE FROM tab
             WHERE {@@Sql.easy_where [#T] [map fst fs] [[]] [[]] [[]] [[]] ! !
               injs (@@Folder.mp [fst] [_] fl) r});

        queryI1 (SELECT * FROM listeners)
        (fn x => send x.Channel (DEL r))

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
                                                  </td>
                                                  {@mapX2 [Widget.t'] [fst] [_]
                                                    (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                                                 (w : Widget.t' p) (v : fst p) =>
                                                        <xml><td>{@Widget.asValue w v}</td></xml>)
                                                    fl widgets r.Content}
                                                </xml>
                                              | Some ws => <xml>
                                                <td/>
                                                {@mapX2 [Widget.t'] [snd] [_]
                                                  (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                                               (w : Widget.t' p) (v : snd p) =>
                                                      <xml><td>{@Widget.asWidget w v}</td></xml>)
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
                                         r <- @Monad.mapR2 _ [Widget.t'] [snd] [fst]
                                               (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : snd p) =>
                                                   current (@Widget.value w v))
                                               fl widgets a.ToAdd;
                                         rpc (add r)}/></th>

                 {@mapX2 [Widget.t'] [snd] [_]
                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : snd p) =>
                       <xml><td>{@Widget.asWidget w v}</td></xml>)
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
