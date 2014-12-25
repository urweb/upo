open Bootstrap3

fun page onload brand menuOptions titl bod =
    nid <- fresh;
    return <xml>
      <head>
        <title>{[titl]}</title>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"/>
        <link rel="stylesheet" href="/style.css"/>
      </head>

      <body style="padding-top: 50px" onload={onload}>
        <nav class="navbar navbar-inverse navbar-fixed-top">
          <div class="container">
            <div class="navbar-header">
              <button class="navbar-toggle collapsed" data-toggle="collapse" data-target={"#" ^ show nid} aria-expanded="false" aria-controls="navbar">
<span class="sr-only">Toggle navigation</span>
<span class="icon-bar"></span>
<span class="icon-bar"></span>
<span class="icon-bar"></span>
              </button>
              <a class="navbar-brand">{[brand]}</a>
            </div>
            <div id={nid} class="collapse navbar-collapse">
              <ul class="bs3-nav navbar-nav">
                {menuOptions}
              </ul>
            </div>
          </div>
        </nav>

        <div class="container-fluid">
          <h1>{[titl]}</h1>

          {bod}
        </div>
      </body>
    </xml>

fun makeModal bcode titl bod blab = <xml>
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
                value={blab}
                onclick={fn _ => bcode}/>
        <button class="btn btn-default"
                data-dismiss="modal"
                value="Cancel"/>
      </div>
    </div>
  </div>
</xml>
