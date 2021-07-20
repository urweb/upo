open Bootstrap

functor Make(M : sig
                 type input
                 con fs :: {Type}
                 con tab :: Name
                 val query : input -> sql_query [] [] [tab = fs] []
                 val fl : folder fs
                 val show : $(map show fs)
                 val labels : $(map (fn _ => string) fs)
             end) = struct

    open M

    type a = list $fs

    fun create inp = queryL1 (query inp)

    fun onload _ = return ()

    fun render _ a = <xml>
      <table class="bs-table">
        <thead><tr>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] lab => <xml><th>{[lab]}</th></xml>)
            fl labels}
        </tr></thead>

        <tbody>
          {List.mapX (fn fs => <xml>
            <tr>
              {@mapX2 [show] [ident] [tr]
                (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (v : t) => <xml><td>{[v]}</td></xml>)
                fl show fs}
            </tr>
          </xml>) a}
        </tbody>
      </table>
    </xml>

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    fun ui inp = {Create = create inp,
                  Onload = onload,
                  Render = render,
                  Notification = notification,
                  Buttons = buttons}

end
