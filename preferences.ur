open Bootstrap4

functor Make(M : sig
                 con choice :: Name
                 type choiceT
                 con choiceR :: {Type}
                 constraint [choice] ~ choiceR
                 table choice : ([choice = choiceT] ++ choiceR)
                 val show_choiceT : show choiceT
                 val read_choiceT : read choiceT
                 val inj_choiceT : sql_injectable_prim choiceT

                 con user :: Name
                 con slot :: Name
                 con available :: Name
                 con preferred :: Name
                 constraint [user] ~ [slot]
                 constraint [user, slot] ~ [available]
                 constraint [user, slot, available] ~ [preferred]
                 table pref : {user : string, slot : choiceT,
                               available : bool, preferred : bool}
                 
                 val whoami : transaction (option string)
             end) = struct
    open M

    type input = string
    type a = list (choiceT * source string)

    fun create u =
        List.mapQueryM (SELECT choice.{choice}, pref.{available}, pref.{preferred}
                        FROM choice LEFT JOIN pref
                          ON pref.{slot} = choice.{choice}
                            AND pref.{user} = {[u]}
                        ORDER BY choice.{choice})
                       (fn r =>
                           s <- (if r.Pref.preferred = Some True then
                                     source "Preferred"
                                 else if r.Pref.available = Some True then
                                     source "Available"
                                 else
                                     source "Unavailable");
                           return (r.Choice.choice, s))

    fun onload _ = return ()

    fun save cs =
        uo <- whoami;
        case uo of
            None => error <xml>You must be logged in to save your preferences.</xml>
          | Some u =>
            dml (DELETE FROM pref
                 WHERE T.{user} = {[u]});
            List.app (fn (c, a, p) =>
                         dml (INSERT INTO pref({user}, {slot}, {available}, {preferred})
                              VALUES ({[u]}, {[c]}, {[a]}, {[p]}))) cs

    fun render _ cs = <xml>
      <button class="btn btn-primary"
              onclick={fn _ =>
                          cs <- List.mapM (fn (c, s) =>
                                              s <- get s;
                                              return (c, s <> "Unavailable", s = "Preferred")) cs;
                          rpc (save cs)}>
        Save preferences
      </button>
        
      <table class="bs-table table-striped">
        {List.mapX (fn (c, s) => <xml><tr>
          <td>{[c]}</td>
          <td><cselect source={s}>
            <coption>Unavailable</coption>
            <coption>Available</coption>
            <coption>Preferred</coption>
          </cselect></td>
        </tr></xml>) cs}
      </table>
    </xml>

    fun ui u = {Create = create u,
                Onload = onload,
                Render = render}
end
