type t
val create : option time -> transaction t
val render : t -> xbody
val content : t -> signal time
val reset : t -> transaction unit
val set : t -> time -> transaction unit
