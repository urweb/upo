(* General client-side GUI widgets *)

class t :: Type (* final value generated *)
      -> Type (* internal state *)
      -> Type (* global configuration for this kind of widget, computed on the server *)
      -> Type

con t' (value :: Type, state :: Type, config :: Type) = t value state config

val configure : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> transaction config
val create : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> config -> transaction state
val initialize : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> config -> value -> transaction state
val reset : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> transaction unit
val setValue : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> value -> transaction unit
val reconfigure : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> config -> transaction unit
val asWidget : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> option id (* Use this ID if you can, to help group with labels. *) -> xbody
val asWidget_simple : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> option id -> xbody (* Alternative version, currently only used to show a textbox instead of a dropdown *)
val value : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> state -> signal value
val asValue : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> value -> xbody
val optional : value ::: Type -> state ::: Type -> config ::: Type -> t value state config -> bool
(* Does this widget represent an [option] type, where the default "null" value is a perfectly sensible final value? *)

val make : value ::: Type -> state ::: Type -> config ::: Type
           -> { Configure : transaction config,
                Create : config -> transaction state,
                Initialize : config -> value -> transaction state,
                Reset : state -> transaction unit,
                Set : state -> value -> transaction unit,
                Reconfigure : state -> config -> transaction unit,
                AsWidget : state -> option id -> xbody,
                AsWidgetSimple : state -> option id -> xbody,
                Value : state -> signal value,
                AsValue : value -> xbody,
                Optional : bool }
           -> t value state config

(* Some default widgets *)

type urlbox
type urlbox_config
val urlbox : t string urlbox urlbox_config
(* This one is earlier in the list so that [textbox] overrides it by default! *)

type htmlbox
type htmlbox_config = {}
val htmlbox : t string htmlbox htmlbox_config
val html : string -> xbody (* Use this one to parse result of [htmlbox] into real HTML. *)
val textFromHtml : string -> string (* Use this one to parse result of [htmlbox] into Markdown-style plain text. *)

type textbox
type textbox_config
val textbox : t string textbox textbox_config

type opt_textbox
type opt_textbox_config
val opt_textbox : t (option string) opt_textbox opt_textbox_config

type checkbox
type checkbox_config
val checkbox : t bool checkbox checkbox_config

type opt_checkbox
type opt_checkbox_config
val opt_checkbox : t (option bool) opt_checkbox opt_checkbox_config

type intbox
type intbox_config
val intbox : t int intbox intbox_config

type opt_intbox
type opt_intbox_config
val opt_intbox : t (option int) opt_intbox opt_intbox_config

type timebox
type timebox_config
val timebox : t time timebox timebox_config

type opt_timebox
type opt_timebox_config
val opt_timebox : t (option time) opt_timebox opt_timebox_config

con choicebox :: Type -> Type
con choicebox_config :: Type -> Type
val choicebox : a ::: Type ->
                 show a
                 -> read a
                 -> a -> list a
                 -> t a (choicebox a) (choicebox_config a)

(* A widget that only allows selection from a finite list, computed via an SQL query *)
con foreignbox :: Type -> Type
con foreignbox_config :: Type -> Type
val foreignbox : a ::: Type -> f ::: Name ->
                 show a
                 -> read a
                 -> sql_query [] [] [] [f = a]
                 -> t (option a) (foreignbox a) (foreignbox_config a)

con foreignbox_default :: Type -> Type
con foreignbox_default_config :: Type -> Type
val foreignbox_default : a ::: Type -> f ::: Name ->
                           show a
                           -> read a
                           -> sql_query [] [] [] [f = a]
                           -> a (* default value *)
                           -> t a (foreignbox_default a) (foreignbox_default_config a)

(* We might also want foreignboxes sorted by similarity to initial value
 * (even if it doesn't appear in the set of choices). *)
functor Fuzzybox(M : sig
                     con f :: Name
                     con fs :: {Type}
                     constraint [f] ~ fs
                     table t : ([f = string] ++ fs)

                     val top_n : int
                     (* Truncate similarity-sorted list to this length. *)
                 end) : sig
    type state
    type config
    val w : unit -> t string state config
end
