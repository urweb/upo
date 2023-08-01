open ChartistFfi

fun graph gr = <xml>
  <active code={id <- fresh;
                return <xml>
                  <span id={id}/>
                  <active code={ChartistFfi.replace id gr;
                                return <xml></xml>}/>
                </xml>}/>
</xml>
