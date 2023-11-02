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
                 val labels : $(map (fn _ => string) ([xName = xType] ++ y))
             end) = struct
    open M

    type a = source Chartjs.graph * Chartjs.graph

    val create =
        q <- List.mapQuery query (fn r => (r.xName, r -- xName));
        let
            val dataLabels = List.mp (fn (x, _) => show x) q

            val dataseries =
                List.foldr (fn (_, y) acc =>
                                   @map3 [numeric] [ident] [fn _ => list float] [fn _ => list float]
                                    (fn [t] (f : numeric t) (v : t) (acc : list float) => f v :: acc)
                                    fl numerics y acc)
                               (@map0 [fn _ => list float] (fn [t ::_] => []) fl) q

            val dataseries =
                @map2 [fn _ => string] [fn _ => list float] [fn _ => Chartjs.dataset]
                (fn [t] label values => {Label = label, Values = values}) fl (labels -- xName) dataseries

            val dataseries =
                @foldR [fn _ => Chartjs.dataset] [fn _ => list Chartjs.dataset]
                    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] lf acc => lf :: acc)
                    [] fl dataseries
            val gr = Chartjs.Bar (dataLabels, dataseries)
        in src <- source gr; return (src, gr) end

    fun onload (src, gr) = set src gr

    fun render _ (src, _) = Chartjs.graph src

    fun notification _ _ = <xml></xml>
    fun buttons _ _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification,
              Buttons = buttons}
end
