con t (value :: Type) (state :: Type) =
      { Create : transaction state,
        Initialize : value -> transaction state,
        AsWidget : state -> xbody,
        Value : state -> signal value,
        AsValue : value -> xbody }

con t' (value :: Type, state :: Type) = t value state

fun create [value] [state] (t : t value state) = t.Create
fun initialize [value] [state] (t : t value state) = t.Initialize
fun asWidget [value] [state] (t : t value state) = t.AsWidget
fun value [value] [state] (t : t value state) = t.Value
fun asValue [value] [state] (t : t value state) = t.AsValue

fun make [value] [state] r = r


val textbox = { Create = source "",
                Initialize = source,
                AsWidget = fn s => <xml><ctextbox source={s}/></xml>,
                Value = signal,
                AsValue = txt }

val checkbox = { Create = source False,
                 Initialize = source,
                 AsWidget = fn s => <xml><ccheckbox source={s}/></xml>,
                 Value = signal,
                 AsValue = txt }

val intbox = { Create = source "",
               Initialize = fn n => source (show n),
               AsWidget = fn s => <xml><ctextbox source={s}/></xml>,
               Value = fn s => v <- signal s; return (Option.get 0 (read v)),
               AsValue = txt }

val timebox = { Create = source "",
                Initialize = fn n => source (show n),
                AsWidget = fn s => <xml><ctextbox source={s}/></xml>,
                Value = fn s => v <- signal s; return (Option.get minTime (read v)),
                AsValue = fn t => if t = minTime then <xml><b>INVALID</b></xml> else txt t }

val urlbox = { Create = source "",
               Initialize = source,
               AsWidget = fn s => <xml><ctextbox source={s}/></xml>,
               Value = signal,
               AsValue = fn s =>
                           case checkUrl s of
                               None => <xml><b>[BLOCKED URL]</b></xml>
                             | Some url => <xml><a href={url}><tt>{[url]}</tt></a></xml> }
