type numeric t = t -> float
val numeric_int = float
fun numeric_float n = n
fun numeric_option [t] (f : numeric t) (x : option t) =
    case x of
        None => 0.0
      | Some n => f n

functor Make(M : sig
                 con xName :: Name
                 type xType
                 con y :: {Type}
                 constraint [xName] ~ y
                 val query : sql_query [] [] [] ([xName = xType] ++ y)
                 val fl : folder y
                 val label : show xType
                 val numerics : $(map numeric y)
             end) = struct
    open M

    type a = list (xType * $y)

    val create =
        List.mapQuery query
                      (fn r => (r.xName, r -- xName))

    fun onload _ = return ()

    fun render _ a =
        let
            val x = List.mp (fn (x, _) => show x) a

            val y = List.foldl (fn (_, y) acc =>
                                   @map3 [numeric] [ident] [fn _ => list float] [fn _ => list float]
                                    (fn [t] (f : numeric t) (v : t) (acc : list float) => f v :: acc)
                                    fl numerics y acc)
                               (@map0 [fn _ => list float] (fn [t ::_] => []) fl) a

            val y = @foldR [fn _ => list float] [fn _ => list (list float)]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] lf acc => lf :: acc)
                     [] fl y
        in
            Chartist.graph (Chartist.Bar (x, y))
        end

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification,
              Buttons = buttons}
end
