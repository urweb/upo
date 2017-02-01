(* Every page rendering has its own unique value of this type. *)
type context

(* A general type of GUI units, which can be composed and dropped into pages.
 * The parameter is the unit's state, which should be generated on the server. *)
type t a = {
     Create : transaction a,
     Onload : a -> transaction unit,
     Render : context -> a -> xbody
}

(* Some signatures for packaging units with their abstract state types *)
signature S0 = sig
    type a
    val ui : t a
end

signature S = sig
    type input
    type a
    val ui : input -> t a
end

(* Compose some units in sequence
 * (whether that means horizontally or vertically on the page depends on the
 * surrounding DOM context!). *)
con seq :: {Type} -> Type
val seq : ts ::: {Type} -> folder ts -> $(map t ts) -> t (seq ts)

(* A widget that can be in one of two modes, indicated by a [bool] *)
con moded :: Type -> Type -> Type
val moded : a1 ::: Type -> a2 ::: Type -> bool (* use first one? *)
            -> t a1 -> t a2 -> t (moded a1 a2)

(* Compute a UI's input (on the server). *)
con computed :: Type -> Type -> Type
val computed : a ::: Type -> b ::: Type -> (a -> t b) -> transaction a -> t (computed a b)


(* Boring "static" units *)
type const
val const : xbody -> t const
val constM : (context -> xbody) -> t const

(* Specialized versions that wrap with particular tags *)
val p : xbody -> t const
val h1 : xbody -> t const
val h2 : xbody -> t const
val h3 : xbody -> t const
val h4 : xbody -> t const
val hr : t const

(* Themes control where to get CSS and JavaScript from, etc. *)

signature THEME = sig
    (* CSS URLs *)
    con r :: {Unit}
    val fl : folder r
    val css : $(mapU url r)

    (* Shortcut icon *)
    val icon : option url

    (* Wrap body contents with this function, to introduce extra fanciness *)
    val wrap : xbody -> xbody

    (* Styling for top navbar *)
    val navclasses : css_class

    (* Should page titles be rendered in navbars, or should we leave that to [wrap]? *)
    val titleInNavbar : bool
end

functor Make(M : THEME) : sig
    (* Render a page with just one simple UI worth of content. *)
    val simple : a ::: Type
                 -> string (* title *)
                 -> t a    (* content *)
                 -> transaction page

    (* Render a page with several named tabs, each with its own UI.
     * Switch between tabs using the top navbar. *)
    val tabbed : ts ::: {Type} -> folder ts
                 -> string                                     (* title *)
                 -> $(map (fn a => option string * t a) ts)    (* content *)
                 -> transaction page
    (* Tabs labeled with [None] are hidden in this rendering. *)

    (* Less dynamic version of [tabbed] *)
    val tabbedStatic : ts ::: {Unit} -> folder ts
                       -> string
                       -> $(mapU (string * bool (* selected? *) * url) ts)
                       -> (context -> transaction xbody)
                       -> transaction page

    (* Generating multiple pages of printed content, with no extra visual style wrapped around each page (so include everything you want in the UI itself)  *)
    val printPages : data ::: Type
                     -> ui ::: Type
                     -> (data -> t ui)
                     -> list data
                     -> string (* page title *)
                     -> transaction page
end

(* Create an HTML button that opens a modal dialog with the content
 * that the callback function returns. *)
val modalButton : context
                  -> css_class         (* additional styling *)
                  -> xml [Body] [] []  (* text label *)
                  -> transaction xbody (* content of modal dialog *)
                  -> xbody

(* The same, but uses <a> instead of <button>. Useful for button dropdowns. *)
val modalAnchor : context
                  -> css_class         (* additional styling *)
                  -> xbody             (* text label *)
                  -> transaction xbody (* content of modal dialog *)
                  -> xbody

(* A standard template for creating the modal form *)
val modal : transaction unit     (* callback on clicking main button *)
            -> xbody             (* main prompt *)
            -> xbody             (* main GUI widgets *)
            -> xml [Body] [] []  (* label on main "OK" button *)
            -> xbody

(* An even simpler version, meant for displaying some information that goes away upon a button click *)
val simpleModal : xbody                (* main content *)
                  -> xml [Body] [] []  (* label on dismissal button *)
                  -> xbody

(* Useful with tabbed pages: generate an optional text label based on a Boolean condition. *)
val when : bool -> string -> option string
