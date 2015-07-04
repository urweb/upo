fun test [nm :: Name] [t ::: Type] [ts ::: {Type}] [[nm] ~ ts] (fl : folder ([nm = t] ++ ts))
          (v : variant ([nm = t] ++ ts)) : option t =
    match v ({nm = Some}
                 ++ (@map0 [fn t' => t' -> option t] (fn [t' :: Type] _ => None) fl -- nm))

fun eq [r] (eqs : $(map eq r)) (fl : folder r) =
    mkEq (fn (v1 : variant r) (v2 : variant r) =>
             match v1
                   (@fold [fn r => r' :: {Type} -> [r ~ r'] => $(map eq r) -> folder (r ++ r') -> variant (r ++ r') -> $(map (fn t => t -> bool) r)]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                                  (acc : r' :: {Type} -> [r ~ r'] => $(map eq r) -> folder (r ++ r') -> variant (r ++ r') -> $(map (fn t => t -> bool) r))
                                  [r' ::_] [[nm = t] ++ r ~ r'] (eqs : $(map eq ([nm = t] ++ r)))
                                  (fl' : folder ([nm = t] ++ r ++ r')) v =>
                         {nm = fn v1 => case @@test [nm] [t] [r ++ r'] ! fl' v of
                                            None => False
                                          | Some v2 => @@Basis.eq [t] eqs.nm v1 v2}
                             ++ @acc [[nm = t] ++ r'] ! (eqs -- nm) fl' v)
                     (fn [r' ::_] [[] ~ r'] _ _ _ => {}) fl [[]] ! eqs fl v2))

fun withAll [K] [r ::: {K}] (fl : folder r) (f : variant (map (fn _ => unit) r) -> transaction unit) =
    @fold [fn r => o :: {K} -> [o ~ r] => (variant (map (fn _ => unit) (r ++ o)) -> transaction unit)
                                          -> transaction unit]
    (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r]
                 (acc : o :: {K} -> [o ~ r] => (variant (map (fn _ => unit) (r ++ o)) -> transaction unit)
                                               -> transaction unit)
                 [o ::_] [o ~ [nm = v] ++ r] f =>
        f (make [nm] ());
        acc [[nm = v] ++ o] f)
    (fn [o :: {K}] [o ~ []] _ => return ())
    fl [[]] ! f

fun erase [r ::: {Type}] (fl : folder r) (v : variant r) =
    match v
    (@fold [fn r => o :: {Type} -> [o ~ r] => $(map (fn t => t -> variant (map (fn _ => unit) (r ++ o))) r)]
      (fn [nm ::_] [v ::_] [r ::_] [[nm] ~ r]
                   (acc : o :: {Type} -> [o ~ r] => $(map (fn t => t -> variant (map (fn _ => unit) (r ++ o))) r))
                   [o ::_] [o ~ [nm = v] ++ r] =>
          {nm = fn _ => make [nm] ()}
              ++ acc [[nm = v] ++ o])
      (fn [o ::_] [o ~ []] => ())
      fl [[]] !)

fun weaken [r1 ::: {Type}] [r2 ::: {Type}] [r1 ~ r2] (fl : folder r1) (v : variant r1) : variant (r1 ++ r2) =
    match v
    (@fold [fn r => r' :: {Type} -> [r ~ r'] => $(map (fn t => t -> variant (r ++ r')) r)]
      (fn [nm :: Name] [t ::_] [r ::_] [[nm] ~ r] (acc : r' :: {Type} -> [r ~ r'] => $(map (fn t => t -> variant (r ++ r')) r)) [r'::_] [[nm = t] ++ r ~ r'] =>
          {nm = make [nm]} ++ acc [[nm = t] ++ r'])
      (fn [r'::_] [[] ~ r'] => {}) fl [r2] !)
