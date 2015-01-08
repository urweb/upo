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
    foreign_key (@easy_matching fl) tab {OnDelete = cascade, OnUpdate = cascade}

fun easy_insert [fields] [uniques] (injs : $(map sql_injectable fields)) (fl : folder fields)
    (tab : sql_table fields uniques) (fs : $fields) =
    dml (insert tab (@Top.map2 [sql_injectable] [ident] [sql_exp [] [] []]
                     (fn [t] => @sql_inject)
                     fl injs fs))

fun easy_where [tab :: Name] [using] [notUsing] [otherTables] [agg] [exps] [using ~ notUsing] [[tab] ~ otherTables]
    (injs : $(map sql_injectable using)) (fl : folder using) (r : $using) =
    @foldR2 [sql_injectable] [ident] [fn r => rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (inj : sql_injectable t) x
                 (acc : rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool)
                 [rest ::_] [notUsing ~ rest] [notUsing ~ ([nm = t] ++ r)] [rest ~ ([nm = t] ++ r)] =>
        (WHERE {{tab}}.{nm} = {[x]} AND {acc [[nm = t] ++ rest]}))
    (fn [rest ::_] [notUsing ~ rest] [notUsing ~ []] [rest ~ []] => (WHERE TRUE)) fl injs r [[]] ! ! !

fun easy_update [key] [fields] [uniques] [key ~ fields]
    (keyInj : $(map sql_injectable key)) (fieldsInj : $(map sql_injectable fields))
    (keyFl : folder key) (fieldsFl : folder fields)
    (tab : sql_table (key ++ fields) uniques)
    (key : $key) (fields : $fields) =
    dml (update [fields]
                (@Top.map2 [sql_injectable] [ident] [sql_exp _ _ _]
                  (fn [t] => @sql_inject)
                  fieldsFl fieldsInj fields)
                tab
                (@easy_where [#T] ! ! keyInj keyFl key))

fun easy_insertOrUpdate [keys ::_] [fields] [uniques] [keys ~ fields]
                        (kinjs : $(map sql_injectable keys)) (finjs : $(map sql_injectable fields))
                        (kfl : folder keys) (ffl : folder fields)
                        (tab : sql_table (keys ++ fields) uniques) (fs : $(keys ++ fields)) =
    b <- oneRowE1 (SELECT COUNT( * ) > 0
                   FROM tab
                   WHERE {@easy_where [#Tab] ! ! kinjs kfl (fs --- fields)});
    if b then
        (* Row already exists.  Update it. *)
        @easy_update ! kinjs finjs kfl ffl tab (fs --- fields) (fs --- keys)
    else
        (* Doesn't exist yet.  Insert it. *)
        @@easy_insert [keys ++ fields] [_] (kinjs ++ finjs) (@Folder.concat ! kfl ffl) tab fs

fun easy_insertOrSkip [keys ::_] [fields] [uniques] [keys ~ fields]
                      (kinjs : $(map sql_injectable keys)) (finjs : $(map sql_injectable fields))
                      (kfl : folder keys) (ffl : folder fields)
                      (tab : sql_table (keys ++ fields) uniques) (fs : $(keys ++ fields)) =
    b <- oneRowE1 (SELECT COUNT( * ) > 0
                   FROM tab
                   WHERE {@easy_where [#Tab] ! ! kinjs kfl (fs --- fields)});
    if b then
        (* Row already exists.  Ignore it. *)
        return ()
    else
        (* Doesn't exist yet.  Insert it. *)
        @@easy_insert [keys ++ fields] [_] (kinjs ++ finjs) (@Folder.concat ! kfl ffl) tab fs

fun easy_update' [key] [fields] [uniques] [key ~ fields]
    (keyInj : $(map sql_injectable key)) (fieldsInj : $(map sql_injectable fields))
    (keyFl : folder key) (fieldsFl : folder fields)
    (tab : sql_table (key ++ fields) uniques)
    (key : $key) (fields : $(key ++ fields)) =
    dml (update [key ++ fields]
                (@Top.map2 [sql_injectable] [ident] [sql_exp _ _ _]
                  (fn [t] => @sql_inject)
                  (@Folder.concat ! keyFl fieldsFl) (keyInj ++ fieldsInj) fields)
                tab
                (@easy_where [#T] ! ! keyInj keyFl key))

fun easy_update'' [key] [fields] [uniques] [leftAlone]
    [key ~ fields] [key ++ fields ~ leftAlone]
    (keyInj : $(map sql_injectable key)) (fieldsInj : $(map sql_injectable fields))
    (keyFl : folder key) (fieldsFl : folder fields)
    (tab : sql_table (key ++ fields ++ leftAlone) uniques)
    (key : $key) (fields : $fields) =
    dml (update [fields]
                (@Top.map2 [sql_injectable] [ident] [sql_exp _ _ _]
                  (fn [t] => @sql_inject)
                  fieldsFl fieldsInj fields)
                tab
                (@easy_where [#T] ! ! keyInj keyFl key))

fun easy_join [tab1 :: Name] [tab2 :: Name] [using] [notUsing1] [notUsing2] [otherTables] [agg] [exps]
              [[tab1] ~ [tab2]] [using ~ notUsing1] [using ~ notUsing2] [[tab1, tab2] ~ otherTables]
              (fl : folder using) =
    @fold [fn r => rest :: {Type} -> [notUsing1 ~ rest] => [notUsing2 ~ rest] =>
              [notUsing1 ~ r] => [notUsing2 ~ r] =>
              [rest ~ r] => sql_exp ([tab1 = r ++ rest ++ notUsing1, tab2 = r ++ rest ++ notUsing2]
                                         ++ otherTables) agg exps bool]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r]
                 (acc : rest :: {Type} -> [notUsing1 ~ rest] => [notUsing2 ~ rest] =>
                  [notUsing1 ~ r] => [notUsing2 ~ r] =>
                  [rest ~ r] => sql_exp ([tab1 = r ++ rest ++ notUsing1, tab2 = r ++ rest ++ notUsing2]
                                             ++ otherTables) agg exps bool)
                 [rest ::_] [notUsing1 ~ rest] [notUsing2 ~ rest]
                 [notUsing1 ~ ([nm = t] ++ r)] [notUsing2 ~ ([nm = t] ++ r)]  [rest ~ ([nm = t] ++ r)] =>
        (WHERE {{tab1}}.{nm} = {{tab2}}.{nm} AND {acc [[nm = t] ++ rest]}))
    (fn [rest ::_] [notUsing1 ~ rest] [notUsing2 ~ rest] [notUsing1 ~ []] [notUsing2 ~ []] [rest ~ []] =>
        (WHERE TRUE)) fl [[]] ! ! ! ! !
