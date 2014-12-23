fun equal [ts] (eqs : $(map eq ts)) (fl : folder ts) =
    mkEq (@foldR3 [eq] [ident] [ident] [fn _ => bool]
           (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] e x0 y0 b =>
               b && @eq e x0 y0)
           True fl eqs)
