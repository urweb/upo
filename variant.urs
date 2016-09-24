(* Metaprogramming helpers for variants in general *)

val eq : ts ::: {Type} -> $(map eq ts) -> folder ts -> eq (variant ts)

val withAll : K --> r ::: {K} -> folder r
              -> (variant (map (fn _ => unit) r) -> transaction unit) -> transaction unit

val withAllX : K --> r ::: {K} -> ctx ::: {Unit} -> inp ::: {Type} -> folder r
              -> (variant (map (fn _ => unit) r) -> xml ctx inp []) -> xml ctx inp []

val erase : r ::: {Type} -> folder r
            -> variant r -> variant (map (fn _ => unit) r)

val test : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => folder ([nm = t] ++ ts)
                                                                    -> variant ([nm = t] ++ ts) -> option t

val weaken : r1 ::: {Type} -> r2 ::: {Type} -> [r1 ~ r2] => folder r1
             -> variant r1 -> variant (r1 ++ r2)

val fromString : r ::: {Unit} -> folder r -> $(mapU string r) -> string -> option (variant (mapU unit r))

val mp : r ::: {Unit} -> t ::: Type -> folder r -> (variant (mapU {} r) -> t) -> $(mapU t r)

val destrR : K --> f :: (K -> Type) -> fr :: (K -> Type) -> t ::: Type
             -> (p :: K -> f p -> fr p -> t)
             -> r ::: {K} -> folder r -> variant (map f r) -> $(map fr r) -> t

val eqU : ts ::: {Unit} -> folder ts -> eq (variant (map (fn _ => unit) ts))
