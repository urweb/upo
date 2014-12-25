open Bootstrap3

fun page onload titl bod = return <xml>
  <head>
    <title>{[titl]}</title>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"/>
  </head>

  <body onload={onload}><div class="container-fluid">
    <h1>{[titl]}</h1>

    {bod}
  </div></body>
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
