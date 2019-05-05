open Bootstrap4

signature S = sig
    con params :: {(Type * Type * Type)}
    val widgets : $(map Widget.t' params)
    val paramsFl : folder params
    val paramLabels : $(map (fn _ => string) params)

    con results :: {Type}
    val resultsFl : folder results
    val resultLabels : $(map (fn _ => string) results)
    val query : $(map fst3 params) -> sql_query [] [] [] results
    val shows : $(map show results)

    val authorized : transaction bool
end

functor Html(M : sig
                 include S
                 con buttons :: {Unit}
                 val buttonsFl : folder buttons
             end) = struct
    open M

    (* We put the button-function record in a source to ease server-to-client embedding. *)
    type a = {Buttons : source $(mapU ($results -> string (* label *) * url) buttons),
              Ids : $(map (fn _ => id) params),
              Widgets : $(map snd3 params),
              Results : source (list $results)}

    fun create bs =
        bs <- source bs;
        ids <- @Monad.mapR0 _ [fn _ => id]
                (fn [nm ::_] [p ::_] => fresh)
                paramsFl;
        ws <- @Monad.mapR _ [Widget.t'] [snd3]
               (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                   cfg <- @Widget.configure w;
                   @Widget.create w cfg)
               paramsFl widgets;
        rs <- (case @Row.isEmpty' [fn r => $(map fst3 r)] paramsFl of
                   None => return []
                 | Some cast => queryL (query (cast ())));
        rs <- source rs;
        return {Buttons = bs,
                Ids = ids,
                Widgets = ws,
                Results = rs}

    fun onload _ = return ()

    fun runQuery vs =
        b <- authorized;
        if b then
            queryL (query vs)
        else
            error <xml>Access denied</xml>

    fun render _ self = <xml>
      <table class="bs-table table-striped">
        {@mapX4 [fn _ => string] [Widget.t'] [fn _ => id] [snd3] [tabl]
          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) (w : Widget.t' p) (id : id) (c : p.2) =>
              <xml><tr class="form-group">
                <th><label class="control-label" for={id}>{[s]}</label></th>
                <td>{@Widget.asWidget w c (Some id)}</td>
              </tr></xml>)
            paramsFl paramLabels widgets self.Ids self.Widgets}
      </table>

      <button class="btn btn-primary"
              value="Search"
              onclick={fn _ =>
                          vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                (fn [nm ::_] [p ::_] (w : Widget.t' p) (c : p.2) =>
                                    current (@Widget.value w c))
                                paramsFl widgets self.Widgets;
                          rs <- rpc (runQuery vs);
                          set self.Results rs}/>

      <hr/>

      <table class="bs-table table-striped">
        <tr>
          <dyn signal={bs <- signal self.Buttons;
                       return (@mapX [fn _ => $results -> string * url] [tr]
                                (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] _ =>
                                    <xml><th/></xml>)
                                buttonsFl bs)}/>
          {@mapX [fn _ => string] [tr]
            (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) =>
                <xml><th>{[s]}</th></xml>)
            resultsFl resultLabels}
        </tr>

        <dyn signal={bs <- signal self.Buttons;
                     rs <- signal self.Results;
                     return (List.mapX (fn row => <xml><tr>
                       {@mapX [fn _ => $results -> string * url] [tr]
                         (fn [nm ::_] [u ::_] [r ::_] [[nm] ~ r] f =>
                             let
                                 val (label, link) = f row
                             in
                                 <xml><td class="col-sm-1"><a class="btn btn-info" href={link}>{[label]}</a></td></xml>
                             end)
                         buttonsFl bs}
                       {@mapX2 [show] [ident] [tr]
                         (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (v : t) =>
                             <xml><td>{[v]}</td></xml>)
                         resultsFl shows row}
                     </tr></xml>) rs)}/>
      </table>
    </xml>

    type input = _
    fun ui bs = {Create = create bs,
                 Onload = onload,
                 Render = render}
end

functor Csv(M : sig
                include S
                val filename : string
            end) = struct
    open M

    type a = {Ids : $(map (fn _ => id) params),
              Widgets : $(map snd3 params)}

    fun generate ps () : transaction page =
        csv <- @@Csv.buildComputed [results] resultsFl #"," shows resultLabels (query ps);
        setHeader (blessResponseHeader "Content-Disposition")
                  ("attachment; filename=" ^ filename);
        returnBlob (textBlob csv) (blessMime "text/csv")

    val create =
        ids <- @Monad.mapR0 _ [fn _ => id]
                (fn [nm ::_] [p ::_] => fresh)
                paramsFl;
        ws <- @Monad.mapR _ [Widget.t'] [snd3]
               (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                   cfg <- @Widget.configure w;
                   @Widget.create w cfg)
               paramsFl widgets;
        return {Ids = ids,
                Widgets = ws}

    fun onload _ = return ()

    fun runQuery vs =
        queryL (query vs)

    fun render _ self = <xml>
      <table class="bs-table table-striped">
        {@mapX4 [fn _ => string] [Widget.t'] [fn _ => id] [snd3] [tabl]
          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) (w : Widget.t' p) (id : id) (c : p.2) =>
              <xml><tr class="form-group">
                <th><label class="control-label" for={id}>{[s]}</label></th>
                <td>{@Widget.asWidget w c (Some id)}</td>
              </tr></xml>)
            paramsFl paramLabels widgets self.Ids self.Widgets}
      </table>

      <dyn signal={vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                          (fn [nm ::_] [p ::_] (w : Widget.t' p) (c : p.2) =>
                              @Widget.value w c)
                          paramsFl widgets self.Widgets;
                   return <xml><form>
                     <submit class="btn btn-primary"
                             value="Search"
                             action={generate vs}/>
                   </form></xml>}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end
