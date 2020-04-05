open Bootstrap4

functor Make(M : sig
                 con fs :: {Type}
                 con tab :: Name
                 val query : sql_query [] [] [tab = fs] []
                 val fl : folder fs
                 val show : $(map show fs)
                 val labels : $(map (fn _ => string) fs)
             end) = struct

    open M

    type a = list $fs

    val create = queryL1 query

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

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end
