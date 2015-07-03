val eq [ts] (eqs : $(map eq ts)) (fl : folder ts) =
    mkEq (@foldR3 [eq] [ident] [ident] [fn _ => bool]
           (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] e x0 y0 b =>
               b && @eq e x0 y0)
           True fl eqs)

datatype result = Yes | No | Maybe

fun ord [ts ::: {Type}] (r : $(map ord ts)) (fl : folder ts) =
    let
        val lte = @foldR3 [ord] [ident] [ident] [fn _ => result]
                   (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (o : ord t) (a : t) (b : t) acc =>
                       if a < b then
                           Yes
                       else if a > b then
                           No
                       else
                           acc)
                   Maybe fl r
    in
        mkOrd {Lt = fn a b =>
                       case lte a b of
                           Yes => True
                         | _ => False,
               Le = fn a b =>
                       case lte a b of
                           No => False
                         | _ => True}
    end

fun select [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [r ::: {K}] (fl : folder r) [out ::: Type]
           (f : t ::: K -> tf1 t -> tf2 t -> out)
           (r : $(map tf1 r)) (v : variant (map tf2 r)) : out =
    match v (@mp [tf1] [fn t => tf2 t -> out] @@f fl r)
