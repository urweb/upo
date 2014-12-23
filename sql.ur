fun order_by [tables] [exps] [dummy] (fl : folder dummy)
             (es : $(map (sql_exp tables [] exps) dummy))
             (dir : sql_direction) =
    @foldR [sql_exp tables [] exps] [fn _ => sql_order_by tables exps]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] e ob =>
        sql_order_by_Cons e dir ob)
    (sql_order_by_Nil [_]) fl es

fun some_fields [tab :: Name] [keep :: {Type}] [drop] [others] [agg] [exps] [keep ~ drop] [[tab] ~ others] (fl : folder keep) =
    @fold [fn r => rest :: {Type} -> [drop ~ rest] => [drop ~ r] => [rest ~ r] => $(map (sql_exp ([tab = r ++ rest ++ drop] ++ others) agg exps) r)]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                 (acc : rest :: {Type} -> [drop ~ rest] => [drop ~ r] => [rest ~ r] => $(map (sql_exp ([tab = r ++ rest ++ drop] ++ others) agg exps) r))
                 [rest ::_] [drop ~ rest] [drop ~ ([nm = t] ++ r)] [rest ~ ([nm = t] ++ r)] =>
        {nm = (SQL {{tab}}.{nm})} ++ acc [[nm = t] ++ rest])
    (fn [rest ::_] [drop ~ rest] [drop ~ []] [rest ~ []] => {}) fl [[]] ! ! !
