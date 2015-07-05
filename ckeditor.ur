open CkeditorFfi

fun show ed = <xml>
  <active code={id <- fresh;
                return <xml>
                  <span id={id}/>
                  <active code={CkeditorFfi.replace ed id; return <xml/>}/>
                </xml>}/>
</xml>
