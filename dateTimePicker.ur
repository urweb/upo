open DateTimePickerFfi

type t = {Id : id,
          Source : source time,
          Show : xbody}

fun create tmo =
    tm <- (case tmo of
               None => now
             | Some tm => return tm);
    id <- fresh;
    s <- source tm;
    return {Id = id,
            Source = s,
            Show = <xml>
              <ctextbox id={id}/>
              <active code={DateTimePickerFfi.replace {Id = id, Source = s};
                            return <xml></xml>}/>
            </xml>}

fun render ed = ed.Show

fun content ed = signal ed.Source

fun reset ed = tm <- now; set ed.Source tm
fun set ed tm = Basis.set ed.Source tm
