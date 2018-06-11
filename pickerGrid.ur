open Bootstrap4

functor Make(M : sig
                 con hidden :: {Type}
                 con static :: {Type}
                 con dynamic :: {(Type * Type * Type)}
                 constraint hidden ~ static
                 constraint (hidden ++ static) ~ dynamic

                 table t : (hidden ++ static ++ map fst3 dynamic)
                 val shows : $(map show static)
                 val eqs : $(map eq static)
                 val widgets : $(map Widget.t' dynamic)
                 val labels : $(map (fn _ => string) static ++ map (fn _ => string) dynamic)

                 val fls : folder static
                 val fld : folder dynamic
                 val injs : $(map sql_injectable static)
                 val injd : $(map (fn p => sql_injectable p.1) dynamic)

                 val amAuthorized : transaction bool (* Is this user allowed to use this interface? *)
             end) = struct

    open M

    type input = _

    type a = {Config : $(map thd3 dynamic),
              Rows : list (source $(static ++ map snd3 dynamic)),
              Channel : channel (list $(static ++ map fst3 dynamic))}

    table listeners : { Channel : channel (list $(static ++ map fst3 dynamic)) }

    fun create filter =
        config <- @Monad.mapR _ [Widget.t'] [thd3]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.configure w) fld widgets;

        rows <- List.mapQueryM (SELECT t.{{static}}, t.{{map fst3 dynamic}}
                                FROM t
                                WHERE {filter}
                                ORDER BY {{{@Sql.order_by fls
                                  (@Sql.some_fields [#T] [static] ! ! fls)
                                  sql_asc}}})
                (fn r =>
                    dyns <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                            (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (x : p.1) =>
                                @Widget.initialize w cfg x)
                            fld widgets config (r.T --- static);
                    source (r.T --- (map fst3 dynamic) ++ dyns));

        ch <- channel;
        dml (INSERT INTO listeners(Channel) VALUES({[ch]}));

        return {Config = config,
                Rows = rows,
                Channel = ch}

    val eqs' = @Record.eq eqs fls

    fun onload a =
        let
            fun loop () =
                ls <- recv a.Channel;
                List.app (fn r =>
                             List.app (fn rs =>
                                          rv <- get rs;
                                          if rv --- (map snd3 dynamic) = r --- (map fst3 dynamic) then
                                              dyns <- @Monad.mapR3 _ [Widget.t'] [thd3] [fst3] [snd3]
                                                       (fn [nm ::_] [p ::_] (w : Widget.t' p) (cfg : p.3) (x : p.1) =>
                                                           @Widget.initialize w cfg x)
                                                       fld widgets a.Config (r --- static);
                                              set rs (rv --- (map snd3 dynamic) ++ dyns)
                                          else
                                              return ()) a.Rows) ls;
                loop ()
        in
            spawn (loop ())
        end

    fun save rs =
        b <- amAuthorized;
        if not b then
            error <xml>Access denied</xml>
        else
            List.app (fn r => @@Sql.easy_update'' [static] [map fst3 dynamic] [_] [_] ! !
                                injs injd fls (@Folder.mp fld)
                                t (r --- (map fst3 dynamic)) (r --- static)) rs;
            queryI1 (SELECT * FROM listeners)
            (fn r => send r.Channel rs)

    fun render _ a = <xml>
      <table class="bs-table table-striped">
        <tr>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) => <xml><th>{[s]}</th></xml>)
            fls (labels --- (map (fn _ => string) dynamic))}
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) => <xml><th>{[s]}</th></xml>)
            fld (labels --- (map (fn _ => string) static))}
        </tr>

        {List.mapX (fn rs => <xml>
          <dyn signal={r <- signal rs;
                       return <xml>
                         <tr>
                           {@mapX2 [show] [ident] [tr]
                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) =>
                                <xml><td>{[x]}</td></xml>)
                            fls shows (r --- (map snd3 dynamic))}

                           {@mapX2 [Widget.t'] [snd3] [_]
                            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (x : p.2) =>
                                <xml><td>{@Widget.asWidget w x None}</td></xml>)
                            fld widgets (r --- static)}
                         </tr>
                       </xml>}/>
         </xml>) a.Rows}
      </table>

      <button class="btn btn-primary"
              value="Save"
              onclick={fn _ =>
                          rs <- List.mapM (fn r =>
                                              rv <- get r;
                                              ds <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                                    (fn [nm ::_] [p ::_] (w : Widget.t' p) (x : p.2) =>
                                                        current (@Widget.value w x))
                                                    fld widgets (rv --- static);
                                              return (rv --- (map snd3 dynamic) ++ ds)) a.Rows;
                          rpc (save rs)}/>
    </xml>

    fun ui filter = {Create = create filter,
                     Onload = onload,
                     Render = render}
end
