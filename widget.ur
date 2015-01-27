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
                AsValue = cdata }

val checkbox = { Create = source False,
                 Initialize = source,
                 AsWidget = fn s => <xml><ccheckbox source={s}/></xml>,
                 Value = signal,
                 AsValue = txt }
