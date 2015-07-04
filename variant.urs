(* Metaprogramming helpers for variants in general *)

val eq : ts ::: {Type} -> $(map eq ts) -> folder ts -> eq (variant ts)

val withAll : K --> r ::: {K} -> folder r
              -> (variant (map (fn _ => unit) r) -> transaction unit) -> transaction unit

val erase : r ::: {Type} -> folder r
            -> variant r -> variant (map (fn _ => unit) r)

val test : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => folder ([nm = t] ++ ts)
                                                                    -> variant ([nm = t] ++ ts) -> option t

val weaken : r1 ::: {Type} -> r2 ::: {Type} -> [r1 ~ r2] => folder r1
             -> variant r1 -> variant (r1 ++ r2)
