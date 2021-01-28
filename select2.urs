type t
val create : (* options: *) xml [Cselect, Body] [] [] -> transaction t
val createSingle (* only select one option *) : xml [Cselect, Body] [] [] -> transaction t
val render : t -> xbody
val selected : t -> signal (list string)
