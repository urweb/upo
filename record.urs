(* Metaprogramming helpers for records in general *)

val eq : ts ::: {Type} -> $(map eq ts) -> folder ts -> eq $ts
val ord : ts ::: {Type} -> $(map ord ts) -> folder ts -> ord $ts

val select : K --> tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> r ::: {K} -> folder r -> out ::: Type
             -> (t ::: K -> tf1 t -> tf2 t -> out)
             -> $(map tf1 r) -> variant (map tf2 r) -> out
