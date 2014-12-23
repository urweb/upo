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

fun easy_matching [fs] (fl : folder fs) =
    @fold [fn r => matching r r]
     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] => mat_cons [nm] [nm])
    mat_nil fl

fun easy_foreign [nm] [fnm] [ft] [fs] [munused] [funused] [uniques]
                 [[fnm] ~ fs] [[fnm] ~ munused] [fs ~ munused] [[fnm] ~ funused] [fs ~ funused] [[nm] ~ uniques]
                 (fl : folder ([fnm = ft] ++ fs))
                 (tab : sql_table ([fnm = ft] ++ fs ++ funused) ([nm = map (fn _ => ()) ([fnm = ft] ++ fs)] ++ uniques)) =
    foreign_key (@easy_matching fl) tab {OnDelete = restrict, OnUpdate = restrict}

fun easy_insert [fields] [uniques] (injs : $(map sql_injectable fields)) (fl : folder fields)
    (tab : sql_table fields uniques) (fs : $fields) =
    dml (insert tab (@Top.map2 [sql_injectable] [ident] [sql_exp [] [] []]
                     (fn [t] => @sql_inject)
                     fl injs fs))
