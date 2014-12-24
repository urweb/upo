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
