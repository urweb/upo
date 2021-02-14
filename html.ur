datatype parse a =
         Success of a
       | Failure of string

con attribute = fn t => {Nam : string,
                         Parse : string -> option t}

con tag = fn ts => {Nam : string,
                    Attributes : $(map attribute ts),
                    Folder : folder ts,
                    Construct : ctx ::: {Unit} -> [[Body] ~ ctx] => $ts
                                -> xml ([Body] ++ ctx) [] [] -> xml ([Body] ++ ctx) [] [],
                    ConstructPlainText : $ts -> string -> string,
                    ForceNoChildren : bool}

fun tag [use] [ignore] [use ~ ignore] (fl : folder use) (name : string) (attrs : $(map attribute use))
        (construct : ctx ::: {Unit} -> [[Body] ~ ctx] => Basis.tag (use ++ ignore) ([Body] ++ ctx) ([Body] ++ ctx) [] [])
        (constructPlainText : $use -> string -> string) =
    {Nam = name,
     Attributes = attrs,
     Folder = fl,
     Construct = fn [ctx] [[Body] ~ ctx] (ats : $use) (inner : xml ([Body] ++ ctx) [] []) =>
                    Basis.tag null None noStyle None ats construct inner,
     ConstructPlainText = constructPlainText,
     ForceNoChildren = False}

fun simpleTag [ignore] fmt name (bt : bodyTag ignore) : tag [] =
    @@tag [[]] [ignore] ! _ name {} (fn [ctx] [[Body] ~ ctx] => bt ()) (fn {} => fmt)

fun simpleTag' [use] [ignore] [use ~ ignore] (fl : folder use)
               (fmt : $use -> string -> string) name
               (bt : bodyTag (use ++ ignore)) (ats : $(map attribute use)) : tag use =
    @@tag [use] [ignore] ! fl name ats (fn [ctx] [[Body] ~ ctx] => bt ()) fmt

fun url name = {Nam = name,
                Parse = checkUrl}

datatype error a =
         Good of a
       | Bad of string

fun format [tags] (fl : folder tags) (tags : $(map tag tags)) [ctx] [[Body] ~ ctx] s =
    let
        fun loop s : error (xml ([Body] ++ ctx) [] [] * string) =
            case String.msplit {Haystack = s, Needle = "&<"} of
                None => Good (cdata s, "")
              | Some (pre, ch, post) =>
                case ch of
                    #"&" =>
                    (case String.split post #";" of
                         None => Bad "No ';' after '&'"
                       | Some (code, post) =>
                         let
                             val xml =
                                 case code of
                                     "lt" => <xml>&lt;</xml>
                                   | "gt" => <xml>&gt;</xml>
                                   | "amp" => <xml>&amp;</xml>
                                   | "nbsp" => <xml>&nbsp;</xml>
                                   | "quot" => <xml>&quot;</xml>
                                   | _ =>
                                     if String.length code > 0 && String.sub code 0 = #"#" then
                                         case read (String.suffix code 1) of
                                             None => <xml/>
                                           | Some 0 => <xml/>
                                           | Some n => cdataChar (Char.fromInt n)
                                     else
                                         <xml/>
                         in
                             case loop post of
                                 Good (after, post) => Good (<xml>{[pre]}{xml}{after}</xml>, post)
                               | x => x
                         end)
                  | _ =>
                    if String.length post > 0 && String.sub post 0 = #"/" then
                        case String.split post #"\x3E" of
                            None => Bad "No '>' after '</'"
                          | Some (_, post) => Good (<xml>{[pre]}</xml>, post)
                    else
                        case String.msplit {Haystack = post, Needle = " >/"} of
                            None => Bad "No '>' after '<'"
                          | Some (tname, ch, post) =>
                            @foldR [tag] [fn _ => unit -> error (xml ([Body] ++ ctx) [] [] * string)]
                            (fn [nm :: Name] [ts :: {Type}] [r :: {{Type}}] [[nm] ~ r] (meta : tag ts) acc () =>
                                if meta.Nam = tname then
                                    let
                                        fun doAttrs (ch, post, ats : $(map option ts), isClosed) =
                                            case ch of
                                                #"\x3E" => (Good (ats, post), isClosed)
                                              | #"/" =>
                                                if String.length post > 0 then
                                                    doAttrs (String.sub post 0, String.suffix post 1, ats, True)
                                                else
                                                    (Bad "Tag-ending '/' followed by nothing", True)
                                              | _ =>
                                                if ch = #" " && String.length post > 0 && String.sub post 0 = #"/" then
                                                    doAttrs (String.sub post 0, String.suffix post 1, ats, isClosed)
                                                else if String.length post > 0 && Char.isSpace (String.sub post 0) then
                                                    doAttrs (ch, String.substring post {Start = 1,
                                                                                        Len = String.length post - 1},
                                                             ats, isClosed)
                                                else 
                                                    case String.split post #"=" of
                                                        None =>
                                                        (case String.split post #"\x3E" of
                                                             None => (Bad "No tag ender '\x3E'", isClosed)
                                                           | Some (_, post) => (Good (ats, post), isClosed))
                                                      | Some (aname, post) =>
                                                        if String.length post >= 1 && String.sub post 0 = #"\"" then
                                                            case String.split (String.substring post
                                                                                                {Start = 1,
                                                                                                 Len = String.length post
                                                                                                       - 1})
                                                                              #"\"" of
                                                                None => (Bad "No '\"' to end attribute value", isClosed)
                                                              | Some (aval, post) =>
                                                                let
                                                                    val ats =
                                                                        @map2 [attribute] [option] [option]
                                                                         (fn [t] meta v =>
                                                                             if aname = meta.Nam then
                                                                                 meta.Parse aval
                                                                             else
                                                                                 v)
                                                                         meta.Folder meta.Attributes ats

                                                                    val (ch, post) =
                                                                        if String.length post > 0 && String.sub post 0 <> #" " then
                                                                            (String.sub post 0, String.suffix post 1)
                                                                        else
                                                                            (#" ", post)
                                                                in
                                                                    doAttrs (ch, post, ats, isClosed)
                                                                end
                                                        else
                                                            (Bad "Attribute value doesn't begin with quote", isClosed)
                                    in
                                        case doAttrs (ch, post, @map0 [option] (fn [t :: Type] => None)
                                                                 meta.Folder, False) of
                                            (Good (ats, post), isClosed) =>
                                            let
                                                val isClosed = isClosed || meta.ForceNoChildren
                                                val ats =
                                                    @map2 [attribute] [option] [ident]
                                                     (fn [t] meta v =>
                                                         case v of
                                                             None => error <xml>Missing attribute {[meta.Nam]}
                                                               for {[tname]}</xml>
                                                           | Some v => v)
                                                     meta.Folder meta.Attributes ats
                                            in
                                                case (if isClosed then Good (<xml></xml>, post) else loop post) of
                                                    Good (inner, post) =>
                                                    (case loop post of
                                                         Good (after, post) =>
                                                         Good (<xml>{[pre]}{meta.Construct [ctx] !
                                                                                           ats inner}{if meta.Nam = "p" then txt "\n" else <xml></xml>}{after}</xml>, post)
                                                       | x => x)
                                                  | x => x
                                            end
                                          | (Bad s, _) => Bad s
                                    end
                                else
                                    acc ())
                            (fn () => Bad ("Unknown HTML tag " ^ tname)) fl tags ()
    in
        case loop s of
            Bad msg => Failure msg
          | Good (xml, _) => Success xml
    end

fun eatClosingNewline s =
    if String.length s > 0 && String.sub s (String.length s - 1) = #"\n" then
        String.substring s {Start = 0, Len = String.length s - 1}
    else
        s

fun formatPlainText [tags] (fl : folder tags) (tags : $(map tag tags)) s =
    let
        fun loop s : error (string * string) =
            case String.msplit {Haystack = s, Needle = "&<"} of
                None => Good (s, "")
              | Some (pre, ch, post) =>
                case ch of
                    #"&" =>
                    (case String.split post #";" of
                         None => Bad "No ';' after '&'"
                       | Some (code, post) =>
                         let
                             val raw =
                                 case code of
                                     "lt" => "<"
                                   | "gt" => ">"
                                   | "amp" => "&"
                                   | "nbsp" => " "
                                   | "quot" => "\""
                                   | _ =>
                                     if String.length code > 0 && String.sub code 0 = #"#" then
                                         case read (String.suffix code 1) of
                                             None => ""
                                           | Some 0 => ""
                                           | Some n => String.str (Char.fromInt n)
                                     else
                                         ""
                         in
                             case loop post of
                                 Good (after, post) => Good (eatClosingNewline pre ^ raw ^ after, post)
                               | x => x
                         end)
                  | _ =>
                    if String.length post > 0 && String.sub post 0 = #"/" then
                        case String.split post #"\x3E" of
                            None => Bad "No '>' after '</'"
                          | Some (_, post) => Good (pre, post)
                    else
                        case String.msplit {Haystack = post, Needle = " >/"} of
                            None => Bad "No '>' after '<'"
                          | Some (tname, ch, post) =>
                            @foldR [tag] [fn _ => unit -> error (string * string)]
                            (fn [nm :: Name] [ts :: {Type}] [r :: {{Type}}] [[nm] ~ r] (meta : tag ts) acc () =>
                                if meta.Nam = tname then
                                    let
                                        fun doAttrs (ch, post, ats : $(map option ts), isClosed) =
                                            case ch of
                                                #"\x3E" => (Good (ats, post), isClosed)
                                              | #"/" =>
                                                if String.length post > 0 then
                                                    doAttrs (String.sub post 0, String.suffix post 1, ats, True)
                                                else
                                                    (Bad "Tag-ending '/' followed by nothing", True)
                                              | _ =>
                                                if ch = #" " && String.length post > 0 && String.sub post 0 = #"/" then
                                                    doAttrs (String.sub post 0, String.suffix post 1, ats, isClosed)
                                                else if String.length post > 0 && Char.isSpace (String.sub post 0) then
                                                    doAttrs (ch, String.substring post {Start = 1,
                                                                                        Len = String.length post - 1},
                                                             ats, isClosed)
                                                else 
                                                    case String.split post #"=" of
                                                        None =>
                                                        (case String.split post #"\x3E" of
                                                             None => (Bad "No tag ender '\x3E'", isClosed)
                                                           | Some (_, post) => (Good (ats, post), isClosed))
                                                      | Some (aname, post) =>
                                                        if String.length post >= 1 && String.sub post 0 = #"\"" then
                                                            case String.split (String.substring post
                                                                                                {Start = 1,
                                                                                                 Len = String.length post
                                                                                                       - 1})
                                                                              #"\"" of
                                                                None => (Bad "No '\"' to end attribute value", isClosed)
                                                              | Some (aval, post) =>
                                                                let
                                                                    val ats =
                                                                        @map2 [attribute] [option] [option]
                                                                         (fn [t] meta v =>
                                                                             if aname = meta.Nam then
                                                                                 meta.Parse aval
                                                                             else
                                                                                 v)
                                                                         meta.Folder meta.Attributes ats

                                                                    val (ch, post) =
                                                                        if String.length post > 0 && String.sub post 0 <> #" " then
                                                                            (String.sub post 0, String.suffix post 1)
                                                                        else
                                                                            (#" ", post)
                                                                in
                                                                    doAttrs (ch, post, ats, isClosed)
                                                                end
                                                        else
                                                            (Bad "Attribute value doesn't begin with quote", isClosed)
                                    in
                                        case doAttrs (ch, post, @map0 [option] (fn [t :: Type] => None)
                                                                 meta.Folder, False) of
                                            (Good (ats, post), isClosed) =>
                                            let
                                                val isClosed = isClosed || meta.ForceNoChildren
                                                val ats =
                                                    @map2 [attribute] [option] [ident]
                                                     (fn [t] meta v =>
                                                         case v of
                                                             None => error <xml>Missing attribute {[meta.Nam]}
                                                               for {[tname]}</xml>
                                                           | Some v => v)
                                                     meta.Folder meta.Attributes ats
                                            in
                                                case (if isClosed then Good ("", post) else loop post) of
                                                    Good (inner, post) =>
                                                    (case loop post of
                                                         Good (after, post) =>
                                                         Good (eatClosingNewline pre ^ meta.ConstructPlainText ats inner ^ after, post)
                                                       | x => x)
                                                  | x => x
                                            end
                                          | (Bad s, _) => Bad s
                                    end
                                else
                                    acc ())
                            (fn () => Bad ("Unknown HTML tag " ^ tname)) fl tags ()
    in
        case loop s of
            Bad msg => Failure msg
          | Good (xml, _) => Success xml
    end

fun addUrl u s =
    if u = s || u = "mailto:" ^ s then
        s
    else
        s ^ " <" ^ u ^ ">"

val b = simpleTag (fn s => "*" ^ s ^ "*") "b" @@b
val i = simpleTag (fn s => "_" ^ s ^ "_") "i" @@i
val a = simpleTag' (fn r s => addUrl (show r.Href) s) "a" @@a {Href = url "href"}
val strong = simpleTag (fn s => "*" ^ s ^ "*") "strong" @@strong
val em = simpleTag (fn s => "_" ^ s ^ "_") "em" @@em
val code = simpleTag (fn s => s) "code" @@tt
val tt = simpleTag (fn s => s) "tt" @@tt
val p = simpleTag (fn s => s ^ "\n\n") "p" @@p
val ol = simpleTag (fn s => s) "ol" @@ol
val ul = simpleTag (fn s => s) "ul" @@ul
val li = simpleTag (fn s => " - " ^ s ^ "\n") "li" @@li

val br = {Nam = "br",
          Attributes = {},
          Folder = _,
          Construct = fn [ctx ::: {Unit}] [[Body] ~ ctx] () _ => <xml><br/></xml>,
          ConstructPlainText = fn {} _ => "\n",
          ForceNoChildren = True}

fun unhtml s =
    case String.msplit {Haystack = s, Needle = "&<"} of
        None => s
      | Some (pre, ch, post) =>
        case ch of
            #"&" =>
            (case String.split post #";" of
                 None => s
               | Some (code, post) =>
                 let
                     val ch =
                         case code of
                             "lt" => #"<"
                           | "gt" => #">"
                           | "amp" => #"&"
                           | _ =>
                             if String.length code > 0 && String.sub code 0 = #"#" then
                                 case read (String.suffix code 1) of
                                     None => #" "
                                   | Some 0 => #" "
                                   | Some n => Char.fromInt n
                             else
                                 #" "
                 in
                     pre ^ String.str ch ^ unhtml post
                 end)
          | _ =>
            if String.length post > 0 && String.sub post 0 = #"/" then
                case String.split post #">" of
                    None => s
                  | Some (tag, post) => (if tag = "/p" then pre ^ "\r\n\r\n" else pre) ^ unhtml post
            else
                let
                    fun passHtmlStuff post =
                        case String.msplit {Haystack = post, Needle = "\">"} of
                            None => s
                          | Some (_, ch, post) =>
                            case ch of
                                #">" => pre ^ unhtml post
                              | _ =>
                                case String.split post #"\"" of
                                    None => s
                                  | Some (_, post) => passHtmlStuff post
                in
                    passHtmlStuff post
                end
