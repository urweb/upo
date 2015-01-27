(* Metaprogramming helpers for records in general *)

val eq : ts ::: {Type} -> $(map eq ts) -> folder ts -> eq $ts
val ord : ts ::: {Type} -> $(map ord ts) -> folder ts -> ord $ts
