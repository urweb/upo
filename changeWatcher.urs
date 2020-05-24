(* Here lies a shared database for subscribing to notification of changes to
 * pieces of state.  We just use arbitrary strings as names of state categories,
 * and any given notification only reports that some part of that piece changed.
 * We recommend using table names for this purpose, and maybe some day Ur/Web
 * will provide a native way to coerce a table reference into a first-class
 * identifier of this kind, though that sounds suspiciously like a nasty way to
 * break parametricity, alpha-equivalence, or some other cherished semantic
 * property. *)

val changed : string -> transaction unit
type t
val listen : string -> transaction t                     (* Call this server-side. *)
val onChange : t -> transaction unit -> transaction unit (* Call this client-side. *)
