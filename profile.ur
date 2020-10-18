open Bootstrap4

functor Make(M : sig
                 con key :: {Type}
                 con chosen :: {(Type * Type * Type)}
                 con rest :: {Type}
                 constraint key ~ chosen
                 constraint key ~ rest
                 constraint chosen ~ rest
                 table t : (key ++ map fst3 chosen ++ rest)

                 val widgets : $(map Widget.t' chosen)
                 val key_inj : $(map sql_injectable key)
                 val chosen_inj : $(map (fn p => sql_injectable p.1) chosen)
                 val kfl : folder key
                 val cfl : folder chosen
                 val labels : $(map (fn _ => string) chosen)

                 val whoami : transaction (option $key)
             end) = struct
    open M

    type input = _
    type a = $(map snd3 chosen)

    fun save r =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u =>
            @@Sql.easy_update'' [key] [map fst3 chosen] [_] [rest] ! !
              key_inj chosen_inj kfl (@Folder.mp cfl)
              t u r

    fun create u =
        fs <- oneRow1 (SELECT t.{{map fst3 chosen}}
                       FROM t
                       WHERE {@@Sql.easy_where [#T] [key] [map fst3 chosen ++ rest]
                         [[]] [_] [_] ! ! key_inj kfl u});
        @Monad.mapR2 _ [Widget.t'] [fst3] [snd3]
         (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.1) =>
             cfg <- @Widget.configure w;
             @Widget.initialize w cfg v)
         cfl widgets fs

    fun onload _ = return ()

    fun render _ ws = <xml>
      {@mapX3 [fn _ => string] [Widget.t'] [snd3] [body]
        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (lab : string) (w : Widget.t' p) (v : p.2) => <xml>
          <div class="form-group">
            <label class="control-label">{[lab]}</label>
            {@Widget.asWidget w v None}
          </div>
        </xml>)
        cfl labels widgets ws}
      <button class="btn btn-primary"
              onclick={fn _ =>
                          vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                 (fn [nm ::_] [p ::_]
                                     (w : Widget.t' p) (v : p.2) =>
                                     current (@Widget.value w v))
                                 cfl widgets ws;
                          rpc (save vs)}>
        Save
      </button>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui u = {Create = create u,
                Onload = onload,
                Render = render,
                Notification = notification,
                Buttons = buttons}
end
