open Bootstrap4

functor Make(M : sig
                 con custom :: {Type}
                 con ws :: {(Type * Type * Type)}
                 constraint custom ~ ws
                 val widgets : $(map Widget.t' ws)
                 val labels : $(map (fn _ => string) ws)
                 val cfl : folder custom
                 val wfl : folder ws
                 val injs : $(map sql_injectable (custom ++ map fst3 ws))

                 table tab : (map fst3 ws ++ custom)
                 val custom : $(map fst3 ws) -> transaction $custom
             end) = struct
    open M

    type a = $(map snd3 ws)

    val create =
        @Monad.mapR _ [Widget.t'] [snd3]
         (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
             cfg <- @Widget.configure w;
             @Widget.create w cfg) wfl widgets

    fun onload _ = return ()

    fun add r =
        c <- custom r;
        @@Sql.easy_insert [map fst3 ws ++ custom] [_] injs
          (@Folder.concat ! cfl (@@Folder.mp [fst3] [_] wfl))
          tab (r ++ c)

    fun render _ ws = <xml>
      {@mapX3 [Widget.t'] [fn _ => string] [snd3] [_]
        (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (w : Widget.t' p) (l : string) (v : snd3 p) =>
            <xml><div class="form-group">
              <label class="control-label">{[l]}</label>
              {@Widget.asWidget w v None}
            </div></xml>)
        wfl widgets labels ws}

      <button value="Add"
              class="btn btn-primary"
              onclick={fn _ =>
                          r <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : snd3 p) =>
                                    current (@Widget.value w v))
                                wfl widgets ws;
                          rpc (add r)}/>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification,
              Buttons = buttons}
end
