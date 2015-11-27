con t (value :: Type) (state :: Type) (config :: Type) =
      { Configure : transaction config,
        Create : config -> transaction state,
        Initialize : config -> value -> transaction state,
        Reset : state -> transaction unit,
        AsWidget : state -> option id -> xbody,
        Value : state -> signal value,
        AsValue : value -> xbody }

con t' (value :: Type, state :: Type, config :: Type) = t value state config

fun configure [value] [state] [config] (t : t value state config) = t.Configure
fun create [value] [state] [config] (t : t value state config) = t.Create
fun initialize [value] [state] [config] (t : t value state config) = t.Initialize
fun reset [value] [state] [config] (t : t value state config) = t.Reset
fun asWidget [value] [state] [config] (t : t value state config) = t.AsWidget
fun value [value] [state] [config] (t : t value state config) = t.Value
fun asValue [value] [state] [config] (t : t value state config) = t.AsValue

fun make [value] [state] [config] r = r


val textbox = { Configure = return (),
                Create = fn () => source "",
                Initialize = fn () => source,
                Reset = fn s => set s "",
                AsWidget = fn s ido =>
                              case ido of
                                  None => <xml><ctextbox class={Bootstrap3.form_control} source={s}/></xml>
                                | Some id => <xml><ctextbox class={Bootstrap3.form_control} source={s} id={id}/></xml>,
                Value = signal,
                AsValue = txt }

val checkbox = { Configure = return (),
                 Create = fn () => source False,
                 Initialize = fn () => source,
                 Reset = fn s => set s False,
                 AsWidget = fn s ido =>
                               case ido of
                                   None => <xml><ccheckbox class={Bootstrap3.form_control} source={s}/></xml>
                                 | Some id => <xml><ccheckbox class={Bootstrap3.form_control} source={s} id={id}/></xml>,
                 Value = signal,
                 AsValue = txt }

val intbox = { Configure = return (),
               Create = fn () => source "",
               Initialize = fn () n => source (show n),
               Reset = fn s => set s "",
               AsWidget = fn s ido =>
                             case ido of
                                 None => <xml><ctextbox class={Bootstrap3.form_control} source={s}/></xml>
                               | Some id => <xml><ctextbox class={Bootstrap3.form_control} source={s} id={id}/></xml>,
               Value = fn s => v <- signal s; return (Option.get 0 (read v)),
               AsValue = txt }

val timebox = { Configure = return (),
                Create = fn () => source "",
                Initialize = fn () n => source (show n),
                Reset = fn s => set s "",
                AsWidget = fn s ido =>
                              case ido of
                                  None => <xml><ctextbox class={Bootstrap3.form_control} source={s}/></xml>
                                | Some id => <xml><ctextbox class={Bootstrap3.form_control} source={s} id={id}/></xml>,
                Value = fn s => v <- signal s; return (Option.get minTime (read v)),
                AsValue = fn t => if t = minTime then <xml><b>INVALID</b></xml> else txt t }

val urlbox = { Configure = return (),
               Create = fn () => source "",
               Initialize = fn () => source,
               Reset = fn s => set s "",
               AsWidget = fn s ido =>
                             case ido of
                                 None => <xml><ctextbox class={Bootstrap3.form_control} source={s}/></xml>
                               | Some id => <xml><ctextbox class={Bootstrap3.form_control} source={s} id={id}/></xml>,
               Value = signal,
               AsValue = fn s =>
                           case checkUrl s of
                               None => <xml><b>[BLOCKED URL]</b></xml>
                             | Some url => <xml><a href={url}><tt>{[url]}</tt></a></xml> }

val undoEtc = Ckeditor.Bar {Nam = Some "Undo, etc.",
                            Buttons = Ckeditor.Cut
                                          :: Ckeditor.Copy
                                          :: Ckeditor.Paste
                                          :: Ckeditor.PasteText
                                          :: Ckeditor.PasteFromWord
                                          :: Ckeditor.Undo
                                          :: Ckeditor.Redo
                                          :: []}

val find = Ckeditor.Bar {Nam = Some "Find",
                         Buttons = Ckeditor.Find
                                       :: Ckeditor.Replace
                                       :: Ckeditor.SelectAll
                                       :: Ckeditor.Scayt
                                       :: []}

val basic = Ckeditor.Bar {Nam = Some "Basic Formatting",
                          Buttons = Ckeditor.Bold
                                        :: Ckeditor.Italic
                                        :: Ckeditor.Underline
                                        :: Ckeditor.RemoveFormat
                                        :: []}

val links = Ckeditor.Bar {Nam = Some "Links",
                          Buttons = Ckeditor.Link
                                        :: Ckeditor.Unlink
                                        :: []}

fun ed s = Ckeditor.editor {Width = Ckeditor.DefaultSize,
                            Height = Ckeditor.DefaultSize,
                            ToolbarSet = Ckeditor.Custom (undoEtc :: find :: basic :: links :: []),
                            InitialText = s}

fun html s =
    case Html.format (Html.b, Html.i, Html.a, Html.strong, Html.em, Html.p) s of
        Html.Failure msg => <xml><b>HTML error: {[msg]}</b></xml>
      | Html.Success xm => xm

val htmlbox = { Configure = return (),
                Create = fn () => ed "",
                Initialize = fn () => ed,
                Reset = fn me => Ckeditor.setContent me "",
                AsWidget = fn me _ => Ckeditor.show me,
                Value = Ckeditor.content,
                AsValue = html }

type foreignbox (a :: Type) =
    { Choices : list a,
      Source : source string }

type foreignbox_config (a :: Type) = list a

fun foreignbox [a ::: Type] [f ::: Name] (_ : show a) (_ : read a) (q : sql_query [] [] [] [f = a]) =
    { Configure = List.mapQuery q (fn r => r.f),
      Create = fn ls =>
                  s <- source "";
                  return {Choices = ls, Source = s},
      Initialize = fn ls v =>
                      s <- source (show v);
                      return {Choices = ls, Source = s},
      Reset = fn me => set me.Source "",
      AsWidget = fn me id =>
                    let
                        val inner = <xml>
                          <coption></coption>
                          {List.mapX (fn v => <xml><coption>{[v]}</coption></xml>) me.Choices}
                        </xml>
                    in
                        case id of
                            None => <xml><cselect class={Bootstrap3.form_control} source={me.Source}>{inner}</cselect></xml>
                          | Some id => <xml><cselect class={Bootstrap3.form_control} id={id} source={me.Source}>{inner}</cselect></xml>
                    end,
      Value = fn me =>
                 v <- signal me.Source;
                 return (case v of
                             "" => None
                           | _ => read v),
      AsValue = txt }
