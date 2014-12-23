(* Metaprogramming helpers for records in general *)

val equal : ts ::: {Type} -> $(map eq ts) -> folder ts -> eq $ts
