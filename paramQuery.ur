open Bootstrap3

functor Make(M : sig
                 type input
                 con fs :: {Type}
                 val query : input -> sql_query [] [] [] fs
                 val fl : folder fs
                 val show : $(map show fs)
                 val labels : $(map (fn _ => string) fs)
             end) = struct

    open M

    type a = list $fs

    fun create inp = queryL (query inp)

    fun onload _ = return ()

    fun render _ a = <xml>
      <table class="bs3-table table-striped">
        <tr>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] lab => <xml><th>{[lab]}</th></xml>)
            fl labels}
        </tr>

        {List.mapX (fn fs => <xml>
          <tr>
            {@mapX2 [show] [ident] [tr]
              (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (v : t) => <xml><td>{[v]}</td></xml>)
              fl show fs}
          </tr>
        </xml>) a}
      </table>
    </xml>

    fun ui inp = {Create = create inp,
                  Onload = onload,
                  Render = render}

end
