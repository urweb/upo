open Bootstrap

type context = {
     ModalId : id,
     ModalSpot : source xbody,
     Tab : option {Count : int, Current : source int}
}

type t a = {
     Create : transaction a,
     Onload : a -> transaction unit,
     Render : context -> a -> xbody,
     Notification : context -> a -> xbody,
     Buttons : context -> a -> xbody
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
                                              r.Render ctx) fl ts,
    Notification = fn ctx =>
                      @mapX2 [t] [ident] [body] (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] r =>
                                                    r.Notification ctx) fl ts,
    Buttons = fn ctx =>
                 @mapX2 [t] [ident] [body] (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] r =>
                                               r.Buttons ctx) fl ts
}

datatype moded a1 a2 = First of a1 | Second of a2
fun moded [a1] [a2] (which : bool) (t1 : t a1) (t2 : t a2) = {
    Create = if which then
                 x <- t1.Create;
                 return (First x)
             else
                 x <- t2.Create;
                 return (Second x),
    Onload = fn st => case st of
                          First x => t1.Onload x
                        | Second x => t2.Onload x,
    Render = fn ctx st => case st of
                              First x => t1.Render ctx x
                            | Second x => t2.Render ctx x,
    Notification = fn ctx st => case st of
                                    First x => t1.Notification ctx x
                                  | Second x => t2.Notification ctx x,
    Buttons = fn ctx st => case st of
                               First x => t1.Buttons ctx x
                             | Second x => t2.Buttons ctx x
}

type computed a b = a * b
fun computed [a] [b] (f : a -> t b) (x : transaction a) : t (computed a b) = {
    Create = v <- x; st <- (f v).Create; return (v, st),
    Onload = fn (v, st) => (f v).Onload st,
    Render = fn ctx (v, st) => (f v).Render ctx st,
    Notification = fn ctx (v, st) => (f v).Notification ctx st,
    Buttons = fn ctx (v, st) => (f v).Buttons ctx st
}

type const = unit
fun const bod = {
    Create = return (),
    Onload = fn () => return (),
    Render = fn _ () => bod,
    Notification = fn _ _ => <xml></xml>,
    Buttons = fn _ _ => <xml></xml>
}
fun constM bod = {
    Create = return (),
    Onload = fn () => return (),
    Render = fn ctx () => bod ctx,
    Notification = fn _ _ => <xml></xml>,
    Buttons = fn _ _ => <xml></xml>
}

signature THEME = sig
    con r :: {Unit}
    val fl : folder r
    val css : $(mapU url r)
    val icon : option url
    val wrap : xbody -> xbody
    val navclasses : css_class
    val titleInNavbar : bool
end



functor Make(M : THEME) = struct
    fun themed_head titl = <xml>
      <head>
        <title>{[titl]}</title>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        {@mapUX [url] [_]
          (fn [nm ::_] [rest ::_] [_~_] url =>
              <xml><link rel="stylesheet" href={url}/></xml>)
          M.fl M.css}
        {case M.icon of
             None => <xml></xml>
           | Some icon => <xml><link rel="shortcut icon" href={icon} type="image/vnd.microsoft.icon"></link></xml>}
      </head>
    </xml>

    fun themed_body url titl onl mid nid ms tbar tabs bod = <xml>
      <body onload={onl}>
        <div class="modal fade" id={mid}>
          <div class="modal-dialog">
            <div class="modal-content">
              <dyn signal={signal ms}/>
            </div>
          </div>
        </div>

        {M.wrap <xml>
          <header>
            <nav class={M.navclasses}>
              {if M.titleInNavbar then <xml><a class="navbar-brand" href={url}>{[titl]}</a></xml> else <xml></xml>}
              <button class="navbar-toggler" data-bs-toggle="collapse" data-bs-target={"#" ^ show nid} aria-controls="navbarCollapse" aria-expanded="false" aria-label="Toggle navigation">
                <span class="navbar-toggler-icon"/>
              </button>
              <div id={nid} class="collapse navbar-collapse">
                <ul class="bs-nav navbar-nav">
                  {tabs}
                </ul>
              </div>
              {tbar}
            </nav>
          </header>

          <main role="main" class="container">
            {bod}
          </main>
        </xml>}
      </body>
    </xml>

    fun themed url titl onl mid nid ms tbar tabs bod = <xml>
      {themed_head titl}
      {themed_body url titl onl mid nid ms tbar tabs bod}
    </xml>

    fun simple [a] titl (t : t a) =
        url <- currentUrl;
        nid <- fresh;
        mid <- fresh;
        ms <- source <xml/>;
        state <- t.Create;
        return (themed url
                       titl
                       (t.Onload state)
                       mid nid ms
                       <xml/>
                       <xml/>
                       (t.Render {ModalId = mid, ModalSpot = ms, Tab = None} state))

    fun embeddable [a] (t : t a) =
        mid <- fresh;
        ms <- source <xml/>;
        state <- t.Create;
        return <xml>
          <head>
            {@mapUX [url] [_]
              (fn [nm ::_] [rest ::_] [_~_] url =>
                  <xml><link rel="stylesheet" href={url}/></xml>)
              M.fl M.css}
          </head>

          <body onload={t.Onload state}>
            <div class="modal fade" id={mid}>
              <div class="modal-dialog">
                <div class="modal-content">
                  <dyn signal={signal ms}/>
                </div>
              </div>
            </div>

            {t.Render {ModalId = mid, ModalSpot = ms, Tab = None} state}
          </body>
        </xml>

    fun minimal [a] titl (t : t a) =
        mid <- fresh;
        ms <- source <xml/>;
        state <- t.Create;
        return <xml>
          <head>
            {@mapUX [url] [_]
              (fn [nm ::_] [rest ::_] [_~_] url =>
                  <xml><link rel="stylesheet" href={url}/></xml>)
              M.fl M.css}
            <title>{[titl]}</title>
          </head>

          <body onload={t.Onload state}>
            <div class="modal fade" id={mid}>
              <div class="modal-dialog">
                <div class="modal-content">
                  <dyn signal={signal ms}/>
                </div>
              </div>
            </div>

            {t.Render {ModalId = mid, ModalSpot = ms, Tab = None} state}
          </body>
        </xml>

    fun tabbedWithToolbar [ts] (fl : folder ts) (titl : string) (tbar : xbody) (ts : $(map (fn a => option string * t a) ts)) (below : context -> xbody) =
        url <- currentUrl;
        nid <- fresh;
        mid <- fresh;
        ms <- source <xml/>;

        state <- @Monad.mapR _ [fn a => option string * t a] [ident]
                  (fn [nm ::_] [t ::_] (_, r) => r.Create) fl ts;

        size <- return (@fold [fn _ => int]
                         (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] n => n + 1)
                         0 fl);
        (curTab : source int) <- source (@foldR [fn a => option string * t a] [fn _ => int * int]
                                          (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r] (opt, _) (cur, chosen) =>
                                              (cur - 1,
                                               case opt of
                                                   None => chosen
                                                 | Some _ => cur))
                                          (size-1, size) fl ts).2;
        ctx <- return {ModalId = mid, ModalSpot = ms,
                       Tab = Some {Count = size, Current = curTab}};

        return (themed url
                       titl
                       (@Monad.appR2 _ [fn a => option string * t a] [ident]
                         (fn [nm ::_] [t ::_] (_, r) => r.Onload)
                         fl ts state)
                       mid nid ms
                       tbar
                       ((@foldR2 [fn a => option string * t a] [ident]
                          [fn _ => xbody * int]
                         (fn [nm ::_] [a ::_] [r ::_] [[nm] ~ r] (labl : option string, r : t a) st (bod, n) =>
                             (case labl of
                                  None => bod
                                | Some labl => <xml>
                                  <li dynClass={ct <- signal curTab;
                                                      return (if ct = n then
                                                                  classes nav_item bs_active
                                                              else
                                                                  nav_item)}
                                      onclick={fn _ => set curTab n}><a class="nav-link">{[labl]} {r.Notification ctx st}</a></li>
                                      {bod}
                                </xml>,
                              n-1))
                         (<xml/>, size-1)
                         fl ts state).1)
                         <xml>
                           <dyn signal={ct <- signal curTab;
                                        return (@foldR2 [fn a => option string * t a] [ident] [fn _ => xbody * int]
                                                 (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_, t) st (acc, n) =>
                                                     (if ct = n then
                                                          t.Render ctx st
                                                      else
                                                          acc, n-1))
                                                 (<xml/>, size-1) fl ts state).1}/>
                           {below ctx}
                         </xml>)

    fun tabbed [ts] (fl : folder ts) titl (ts : $(map (fn a => option string * t a) ts)) =
        @tabbedWithToolbar fl titl <xml></xml> ts (fn _ => <xml></xml>)

    fun tabbedStatic [ts] (fl : folder ts) titl (ts : $(mapU (string * bool * url) ts)) bod =
        url <- currentUrl;
        nid <- fresh;
        mid <- fresh;
        ms <- source <xml/>;
        bod <- bod {ModalId = mid, ModalSpot = ms, Tab = None};

        return (themed url
                       titl
                       (return ())
                       mid nid ms
                       <xml/>
                       (@mapUX_rev [string * bool * url] [body]
                         (fn [nm ::_] [r ::_] [[nm] ~ r] (labl, ct, url) => <xml>
                           <li class={if ct then
                                          classes nav_item bs_active
                                      else
                                          nav_item}><a class="nav-link" href={url}>{[labl]}</a></li>
                         </xml>)
                       fl ts)
                       bod)

    fun printPages [data ::: Type] [ui ::: Type] (f : data -> t ui) (ls : list data) (titl : string) =
        ts <- List.mapM (fn x => t <- (f x).Create; return (x, t)) ls;
        mid <- fresh;
        ms <- source <xml/>;

        return <xml>
          <head>
            <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
            <title>{[titl]}</title>
            {@mapUX [url] [_]
              (fn [nm ::_] [rest ::_] [_~_] url =>
                  <xml><link rel="stylesheet" href={url}/></xml>)
              M.fl M.css}
            {case M.icon of
                 None => <xml></xml>
               | Some icon => <xml><link rel="shortcut icon" href={icon} type="image/vnd.microsoft.icon"></link></xml>}
          </head>

          <body onload={List.app (fn (x, t) => (f x).Onload t) ts}>
            <div class="modal fade" id={mid}>
              <div class="modal-dialog">
                <div class="modal-content">
                  <dyn signal={signal ms}/>
                </div>
              </div>
            </div>

            {List.mapX (fn (x, t) => <xml>
              <div style="page-break-after: right">
                {(f x).Render {ModalId = mid, ModalSpot = ms, Tab = None} t}
              </div>
            </xml>) ts}
          </body>
        </xml>
end

fun modalButton ctx cls bod onclick = <xml>
  <button class={cls}
          data-bs-toggle="modal"
          data-bs-target={"#" ^ show ctx.ModalId}
          onclick={fn _ =>
                      ms <- onclick;
                      set ctx.ModalSpot ms}>
    {bod}
  </button>
</xml>

fun modalIcon ctx cls onclick = <xml>
  <i class={cls} style="cursor: pointer"
     data-bs-toggle="modal"
     data-bs-target={"#" ^ show ctx.ModalId}
     onclick={fn _ =>
                 ms <- onclick;
                 set ctx.ModalSpot ms}/>
</xml>

fun modalAnchor ctx cls bod onclick = <xml>
  <a class={cls}
     data-bs-toggle="modal"
     data-bs-target={"#" ^ show ctx.ModalId}
     href="#"
     onclick={fn _ =>
                 ms <- onclick;
                 set ctx.ModalSpot ms}>
    {bod}
  </a>
</xml>

fun activateModal ctx bod =
    set ctx.ModalSpot bod;
    UpoFfi.activateModal ctx.ModalId

fun deactivateModal ctx =
    UpoFfi.deactivateModal ctx.ModalId

fun modal bcode titl bod blab = <xml>
  <div class="modal-header">
    <h4 class="modal-title">{titl}</h4>
  </div>

  <div class="modal-body">
    {bod}
  </div>

  <div class="modal-footer">
    <button class="btn btn-primary"
            data-bs-dismiss="modal"
            onclick={fn _ => bcode}>
      {blab}
    </button>
    <button class="btn btn-default"
            data-bs-dismiss="modal"
            value="Cancel"/>
  </div>
</xml>

fun simpleModal bod blab = <xml>
  <div class="modal-body">
    {bod}
  </div>

  <div class="modal-footer">
    <button class="btn btn-primary"
            data-bs-dismiss="modal">
      {blab}
    </button>
  </div>
</xml>


val p bod = const <xml><p>{bod}</p></xml>
val h1 bod = const <xml><h1>{bod}</h1></xml>
val h2 bod = const <xml><h2>{bod}</h2></xml>
val h3 bod = const <xml><h3>{bod}</h3></xml>
val h4 bod = const <xml><h4>{bod}</h4></xml>
val hr = const <xml><hr/></xml>

fun when b lab = if b then Some lab else None

fun nextTab ctx =
    case ctx.Tab of
        None => return ()
      | Some r =>
        if r.Count <= 0 then
            return ()
        else
            cur <- get r.Current;
            set r.Current ((cur + 1) % r.Count)

fun inFinalTab ctx =
    case ctx.Tab of
        None => return True
      | Some r =>
        cur <- signal r.Current;
        return (r.Count = 0 || cur = r.Count - 1)

fun context x = x

val tooltip = UpoFfi.tooltip
