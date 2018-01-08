(** Safe HTML parsing *)

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
                    ConstructPlainText : $ts -> string (* inner text *) -> string}

val tag : use ::: {Type} -> ignore ::: {Type} -> [use ~ ignore] => folder use -> string
          -> $(map attribute use)
          -> (ctx ::: {Unit} -> [[Body] ~ ctx] => Basis.tag (use ++ ignore) ([Body] ++ ctx) ([Body] ++ ctx) [] [])
          -> ($use -> string -> string)
          -> tag use

val simpleTag : ignore ::: {Type} -> (string -> string) (* wrap with plain-text formatting *)
                -> string (* tag name *) -> bodyTag ignore -> tag []
val simpleTag' : use ::: {Type} -> ignore ::: {Type} -> [use ~ ignore] => folder use
    -> ($use -> string -> string) -> string -> bodyTag (use ++ ignore) -> $(map attribute use) -> tag use

val url : string -> attribute url

val format : tags ::: {{Type}} -> folder tags -> $(map tag tags)
             -> ctx ::: {Unit} -> [[Body] ~ ctx] => string
             -> parse (xml ([Body] ++ ctx) [] [])

val formatPlainText : tags ::: {{Type}} -> folder tags -> $(map tag tags)
                      -> string
                      -> parse string
(* This operation is to take HTML and format it in a quasi-Markdown style, say
 * for the plain-text alternative part of an e-mail message. *)

val b : tag []
val i : tag []
val a : tag [Href = url]
val strong : tag []
val em : tag []
val p : tag []
val br : tag []
val code : tag []
val tt : tag []
val ol : tag []
val ul : tag []
val li : tag []

val unhtml : string -> string
(* Remove all HTML formatting. *)
