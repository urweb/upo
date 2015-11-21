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

datatype moded a1 a2 = First of a1 | Second of a2
fun moded [a1] [a2] (t1 : t a1) (t2 : t a2) (which : bool) = {
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
                            | Second x => t2.Render ctx x
}

type const = unit
fun const bod = {
    Create = return (),
    Onload = fn () => return (),
    Render = fn _ () => bod
}
fun constM bod = {
    Create = return (),
    Onload = fn () => return (),
    Render = fn ctx () => bod ctx
}

signature THEME = sig
    val bootstrap : url
    val custom : url
end

functor Make(M : THEME) = struct
    fun simple [a] titl (t : t a) =
        nid <- fresh;
        mid <- fresh;
        ms <- source <xml/>;
        state <- t.Create;
        return <xml>
          <head>
            <title>{[titl]}</title>
            <link rel="stylesheet" href={M.bootstrap}/>
            <link rel="stylesheet" href={M.custom}/>
          </head>

          <body onload={t.Onload state}>
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

    fun tabbed [ts] (fl : folder ts) titl (ts : $(map (fn a => option string * t a) ts)) =
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

        return <xml>
          <head>
            <title>{[titl]}</title>
            <link rel="stylesheet" href={M.bootstrap}/>
            <link rel="stylesheet" href={M.custom}/>
          </head>

          <body onload={@Monad.appR2 _ [fn a => option string * t a] [ident]
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
                    {(@foldR2 [fn a => option string * t a] [ident]
                      [fn _ => xbody * int]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (labl : option string, r) st (bod, n) =>
                         (case labl of
                              None => bod
                            | Some labl => <xml>
                              <li dynClass={ct <- signal curTab;
                                                  return (if ct = n then
                                                              bs3_active
                                                          else
                                                              null)}
                                  onclick={fn _ => set curTab n}><a>{[labl]}</a></li>
                                  {bod}
                            </xml>,
                          n-1))
                     (<xml/>, size-1)
                     fl ts state).1}
                  </ul>
                </div>
              </div>
            </nav>

            <div class="container-fluid">
              <dyn signal={ct <- signal curTab;
                           return (@foldR2 [fn a => option string * t a] [ident] [fn _ => xbody * int]
                                    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_, t) st (acc, n) =>
                                        (if ct = n then
                                             t.Render {ModalId = mid, ModalSpot = ms} st
                                         else
                                             acc, n-1))
                                    (<xml/>, size-1) fl ts state).1}/>
            </div>
          </body>
        </xml>
end

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

fun simpleModal bod blab = <xml>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-body">
        {bod}
      </div>

      <div class="modal-footer">
        <button class="btn btn-primary"
                data-dismiss="modal">
          {blab}
        </button>
      </div>
    </div>
  </div>
</xml>


val p bod = const <xml><p>{bod}</p></xml>
val h1 bod = const <xml><h1>{bod}</h1></xml>
val h2 bod = const <xml><h2>{bod}</h2></xml>
val h3 bod = const <xml><h3>{bod}</h3></xml>
val h4 bod = const <xml><h4>{bod}</h4></xml>
val hr = const <xml><hr/></xml>
