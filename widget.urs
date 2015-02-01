(* General client-side GUI widgets *)

class t :: Type (* final value generated *)
      -> Type (* internal state *)
      -> Type

con t' (value :: Type, state :: Type) = t value state

val create : value ::: Type -> state ::: Type -> t value state -> transaction state
val initialize : value ::: Type -> state ::: Type -> t value state -> value -> transaction state
val asWidget : value ::: Type -> state ::: Type -> t value state -> state -> xbody
val value : value ::: Type -> state ::: Type -> t value state -> state -> signal value
val asValue : value ::: Type -> state ::: Type -> t value state -> value -> xbody

val make : value ::: Type -> state ::: Type
           -> { Create : transaction state,
                Initialize : value -> transaction state,
                AsWidget : state -> xbody,
                Value : state -> signal value,
                AsValue : value -> xbody }
           -> t value state

(* Some default widgets *)

type urlbox
val urlbox : t string urlbox
(* This one is earlier in the list so that [textbox] overrides it by default! *)

type textbox
val textbox : t string textbox

type checkbox
val checkbox : t bool checkbox

type intbox
val intbox : t int intbox
