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

fun project 
    (* Get value from record by field name 
    
    Usage:
    (project [#Key] rec)
    
    *)
    [nm :: Name] [t ::: Type] [ts ::: {Type}] [[nm] ~ ts]
    (r : $([nm = t] ++ ts)) : t = r.nm

fun select [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [r ::: {K}] (fl : folder r) [out ::: Type]
           (f : t ::: K -> tf1 t -> tf2 t -> out)
           (r : $(map tf1 r)) (v : variant (map tf2 r)) : out =
    match v (@mp [tf1] [fn t => tf2 t -> out] @@f fl r)

fun select2 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [r ::: {K}] (fl : folder r) [out ::: Type]
           (f : t ::: K -> tf1 t -> tf2 t -> tf3 t -> out)
           (r1 : $(map tf1 r)) (r2 : $(map tf2 r)) (v : variant (map tf3 r)) : out =
    match v (@map2 [tf1] [tf2] [fn t => tf3 t -> out] @@f fl r1 r2)

fun select' [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [r ::: {K}] (fl : folder r) [out ::: Type]
            (f : t ::: K -> tf1 t -> (tf2 t -> variant (map tf2 r)) -> tf3 t -> out)
            (r : $(map tf1 r)) (v : variant (map tf3 r)) : out =
    match v
    (@fold [fn r => o :: {K} -> [o ~ r] =>
               (t ::: K -> tf1 t -> (tf2 t -> variant (map tf2 (r ++ o))) -> tf3 t -> out)
               -> $(map tf1 r)
               -> $(map (fn t => tf3 t -> out) r)]
      (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                   (acc : o :: {K} -> [o ~ r] =>
                    (t ::: K -> tf1 t -> (tf2 t -> variant (map tf2 (r ++ o))) -> tf3 t -> out)
                    -> $(map tf1 r)
                    -> $(map (fn t => tf3 t -> out) r))
                   [o ::_] [o ~ [nm = t] ++ r]
                   (f : t' ::: K -> tf1 t' -> (tf2 t' -> variant (map tf2 ([nm = t] ++ r ++ o))) -> tf3 t' -> out)
                   (r : $(map tf1 ([nm = t] ++ r))) =>
          {nm = f r.nm (make [nm])}
              ++ acc [[nm = t] ++ o] @@f (r -- nm))
      (fn [o ::_] [o ~ []] _ _ => ())
      fl [[]] ! @@f r)

fun select2' [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tf4 :: K -> Type] [r ::: {K}] (fl : folder r) [out ::: Type]
            (f : t ::: K -> tf1 t -> tf2 t -> (tf3 t -> variant (map tf3 r)) -> tf4 t -> out)
            (r1 : $(map tf1 r)) (r2 : $(map tf2 r)) (v : variant (map tf4 r)) : out =
    match v
    (@fold [fn r => o :: {K} -> [o ~ r] =>
               (t ::: K -> tf1 t -> tf2 t -> (tf3 t -> variant (map tf3 (r ++ o))) -> tf4 t -> out)
               -> $(map tf1 r)
               -> $(map tf2 r)
               -> $(map (fn t => tf4 t -> out) r)]
      (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                   (acc : o :: {K} -> [o ~ r] =>
                    (t ::: K -> tf1 t -> tf2 t -> (tf3 t -> variant (map tf3 (r ++ o))) -> tf4 t -> out)
                    -> $(map tf1 r)
                    -> $(map tf2 r)
                    -> $(map (fn t => tf4 t -> out) r))
                   [o ::_] [o ~ [nm = t] ++ r]
                   (f : t' ::: K -> tf1 t' -> tf2 t' -> (tf3 t' -> variant (map tf3 ([nm = t] ++ r ++ o))) -> tf4 t' -> out)
                   (r1 : $(map tf1 ([nm = t] ++ r)))
                   (r2 : $(map tf2 ([nm = t] ++ r))) =>
          {nm = f r1.nm r2.nm (make [nm])}
              ++ acc [[nm = t] ++ o] @@f (r1 -- nm) (r2 -- nm))
      (fn [o ::_] [o ~ []] _ _ _ => ())
      fl [[]] ! @@f r1 r2)
