(* Here lies a shared database for subscribing to notification of changes to
 * pieces of state.  We just use arbitrary strings as names of state categories,
 * and any given notification only reports that some part of that piece changed.
 * We recommend using table names for this purpose, and maybe some day Ur/Web
 * will provide a native way to coerce a table reference into a first-class
 * identifier of this kind, though that sounds suspiciously like a nasty way to
 * break parametricity, alpha-equivalence, or some other cherished semantic
 * property. *)

type client_part
type server_part

(* Server-side *)
val listen : string -> transaction client_part
val changed : string -> transaction unit
val changedBy : server_part -> string -> transaction unit (* Don't send notification to self. *)
val retire : server_part -> transaction unit              (* No more notifications here. *)

(* Client-side *)
val onChange : client_part -> transaction unit -> transaction unit
val server : client_part -> server_part
