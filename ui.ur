open Bootstrap3

type context = {
     ModalId : id,
     ModalSpot : source xbody
}

type t a = {
     Create : transaction a,
     Onload : a -> transaction unit,
     Render : context -> a -> xbody
}

signature S0 = sig
    type a
    val ui : t a
end

signature S = sig
    type input
    type a
    val ui : input -> t a
end

type seq ts = $ts
fun seq [ts] (fl : folder ts) (ts : $(map t ts)) = {
    Create = @Monad.mapR _ [t] [ident] (fn [nm ::_] [t ::_] r => r.Create) fl ts,
    Onload = @Monad.appR2 _ [t] [ident] (fn [nm ::_] [t ::_] r => r.Onload) fl ts,
    Render = fn ctx =>
                @mapX2 [t] [ident] [body] (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] r =>
                                              r.Render ctx) fl ts
}

type const = unit
fun const bod = {
    Create = return (),
    Onload = fn () => return (),
    Render = fn _ () => bod
}

fun simple [a] titl (t : t a) =
    nid <- fresh;
    mid <- fresh;
    ms <- source <xml/>;
    state <- t.Create;
    return <xml>
      <head>
        <title>{[titl]}</title>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"/>
        <link rel="stylesheet" href="/style.css"/>
      </head>

      <body style="padding-top: 50px" onload={t.Onload state}>
        <div class="modal" id={mid}>
          <dyn signal={signal ms}/>
        </div>

        <nav class="navbar navbar-inverse navbar-fixed-top">
          <div class="container">
            <div class="navbar-header">
              <button class="navbar-toggle collapsed" data-toggle="collapse" data-target={"#" ^ show nid} aria-expanded="false" aria-controls="navbar">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
              </button>
              <a class="navbar-brand">{[titl]}</a>
            </div>
            <div id={nid} class="collapse navbar-collapse">
              <ul class="bs3-nav navbar-nav"/>
            </div>
          </div>
        </nav>

        <div class="container-fluid">
          {t.Render {ModalId = mid, ModalSpot = ms} state}
        </div>
      </body>
    </xml>

fun tabbed [ts] (fl : folder ts) titl (ts : $(map (fn a => string * t a) ts)) =
    nid <- fresh;
    mid <- fresh;
    ms <- source <xml/>;

    state <- @Monad.mapR _ [fn a => string * t a] [ident]
              (fn [nm ::_] [t ::_] (_, r) => r.Create) fl ts;

    curTab <- (case @fold [fn r => option (variant (map (fn _ => unit) r))]
                     (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] _ => Some (make [nm] ()))
                     None fl of
                   None => error <xml>Ui.tabbed: empty tab list</xml>
                 | Some v => source v);

    return <xml>
      <head>
        <title>{[titl]}</title>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"/>
        <link rel="stylesheet" href="/style.css"/>
      </head>

      <body style="padding-top: 50px" onload={@Monad.appR2 _ [fn a => string * t a] [ident]
                                               (fn [nm ::_] [t ::_] (_, r) => r.Onload)
                                               fl ts state}>
        <div class="modal" id={mid}>
          <dyn signal={signal ms}/>
        </div>

        <nav class="navbar navbar-inverse navbar-fixed-top">
          <div class="container">
            <div class="navbar-header">
              <button class="navbar-toggle collapsed" data-toggle="collapse" data-target={"#" ^ show nid} aria-expanded="false" aria-controls="navbar">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
              </button>
              <a class="navbar-brand">{[titl]}</a>
            </div>
            <div id={nid} class="collapse navbar-collapse">
              <ul class="bs3-nav navbar-nav">
                {@foldR2 [fn a => string * t a] [ident]
                  [fn r => rest :: {Type} -> [r ~ rest] =>
                      folder (rest ++ r)
                      -> source (variant (map (fn _ => unit) (rest ++ r)))
                      -> xbody]
                 (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (labl, r) st
                              (acc : rest :: {Type} -> [r ~ rest] =>
                               folder (rest ++ r)
                               -> source (variant (map (fn _ => unit) (rest ++ r)))
                               -> xbody) [rest ::_]
                              [([nm = t] ++ r) ~ rest] (fl : folder ([nm = t] ++ r ++ rest))
                              curTab => <xml>
                                <li dynClass={ct <- signal curTab;
                                              return (@@match [[nm = unit]
                                                                   ++ map (fn _ => unit)
                                                                              (rest ++ r)] [_] ct
                                                        (@map0 [fn _ => unit -> css_class]
                                                          (fn [t ::_] () => null)
                                                          fl
                                                          -- nm ++ {nm = fn () => bs3_active}))}
                                    onclick={fn _ => set curTab (make [nm] ())}><a>{[labl]}</a></li>
                                {@acc [[nm = t] ++ rest] ! fl curTab}
                              </xml>)
                 (fn [rest ::_] [[] ~ rest] _ _ => <xml/>)
                 fl ts state [[]] ! fl curTab}
              </ul>
            </div>
          </div>
        </nav>

        <div class="container-fluid">
          <dyn signal={ct <- signal curTab;
                       return (@@match [map (fn _ => unit) ts] [xbody] ct
                                 (@map2 [fn a => string * t a] [ident]
                                   [fn _ => unit -> xbody]
                                   (fn [t] (_, t) st () =>
                                       t.Render {ModalId = mid, ModalSpot = ms} st)
                                   fl ts state))}/>
        </div>
      </body>
    </xml>

fun modalButton ctx cls bod onclick = <xml>
  <button class={cls}
          data-toggle="modal"
          data-target={"#" ^ show ctx.ModalId}
          onclick={fn _ =>
                      ms <- onclick;
                      set ctx.ModalSpot ms}>
    {bod}
  </button>
</xml>

fun modal bcode titl bod blab = <xml>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h4 class="modal-title">{titl}</h4>
      </div>

      <div class="modal-body">
        {bod}
      </div>

      <div class="modal-footer">
        <button class="btn btn-primary"
                data-dismiss="modal"
                onclick={fn _ => bcode}>
          {blab}
        </button>
        <button class="btn btn-default"
                data-dismiss="modal"
                value="Cancel"/>
      </div>
    </div>
  </div>
</xml>

val p bod = const <xml><p>{bod}</p></xml>
val h1 bod = const <xml><h1>{bod}</h1></xml>
val h2 bod = const <xml><h2>{bod}</h2></xml>
val h3 bod = const <xml><h3>{bod}</h3></xml>
val h4 bod = const <xml><h4>{bod}</h4></xml>
