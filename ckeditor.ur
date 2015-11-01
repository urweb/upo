open CkeditorFfi

type editor = {Id : id,
               Source : source string,
               Show : xbody}

fun editor r =
    id <- fresh;
    s <- source r.InitialText;
    return {Id = id,
            Source = s,
            Show = <xml>
              <span id={id}/>
              <active code={CkeditorFfi.replace (r -- #InitialText ++ {Id = id, Source = s});
                            return <xml></xml>}/>
            </xml>}

fun show ed = ed.Show

fun content ed = signal ed.Source

fun setContent ed s =
    set ed.Source s;
    CkeditorFfi.setContent ed.Id s
