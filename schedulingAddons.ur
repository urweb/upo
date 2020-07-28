type t (key :: Type) = {
     Times : option (sql_query [] [] [] [Time = time]),
     Unavailable : tabs :: {{Type}} -> tname1 :: Name -> tname2 :: Name
                   -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                   => sql_exp tabs [] [] key
                   -> sql_exp tabs [] [] time
                   -> sql_exp tabs [] [] int,
     Unpreferred : tabs :: {{Type}} -> tname1 :: Name -> tname2 :: Name
                   -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                   => sql_exp tabs [] [] key
                   -> sql_exp tabs [] [] time
                   -> sql_exp tabs [] [] int,
     Preferred : tabs :: {{Type}} -> tname1 :: Name -> tname2 :: Name
                 -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                 => sql_exp tabs [] [] key
                 -> sql_exp tabs [] [] time
                 -> sql_exp tabs [] [] int
}

fun times [key ::: Type] (t : t key) = t.Times
fun unavailable [key ::: Type] [tabs ::: {{Type}}] [tname1 :: Name] [tname2 :: Name]
    [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs]
    (t : t key) = t.Unavailable [tabs] [tname1] [tname2] ! !
fun unpreferred [key ::: Type] [tabs ::: {{Type}}] [tname1 :: Name] [tname2 :: Name]
    [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs]
    (t : t key) = t.Unpreferred [tabs] [tname1] [tname2] ! !
fun preferred [key ::: Type] [tabs ::: {{Type}}] [tname1 :: Name] [tname2 :: Name]
    [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs]
    (t : t key) = t.Preferred [tabs] [tname1] [tname2] ! !

val empty [key ::: Type] = {
    Times = None,
    Unavailable = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] _ _ =>
                     (SQL 0),
    Unpreferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] _ _ =>
                     (SQL 0),
    Preferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                   [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] _ _ =>
                   (SQL 0)
}

fun compose [key ::: Type] (t1 : t key) (t2 : t key) = {
    Times =
      case t1.Times of
          None => t2.Times
        | Some tms1 =>
          case t2.Times of
              None => None
            | Some tms2 => Some (SELECT Times.Time AS Time FROM ({{tms1}}) AS Times
                                 UNION SELECT Times.Time AS Time FROM ({{tms2}}) AS Times),
    Unavailable = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL {t1.Unavailable [tabs] [tname1] [tname2] ! ! k t}
                        + {t2.Unavailable [tabs] [tname1] [tname2] ! ! k t}),
    Unpreferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL {t1.Unpreferred [tabs] [tname1] [tname2] ! ! k t}
                        + {t2.Unpreferred [tabs] [tname1] [tname2] ! ! k t}),
    Preferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                   [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                   (SQL {t1.Preferred [tabs] [tname1] [tname2] ! ! k t}
                      + {t2.Preferred [tabs] [tname1] [tname2] ! ! k t})
}

fun like [key ::: Type] [lkey :: Name] [luser :: Name] [lks ::: {{Unit}}]
    [auser :: Name] [atime :: Name] [apreferred :: Name] [aks ::: {{Unit}}]
    [[lkey] ~ [luser]] [[auser] ~ [atime]] [[auser, atime] ~ [apreferred]]
    (like : sql_table [lkey = key, luser = string] lks)
    (avail : sql_table [auser = string, atime = time, apreferred = bool] aks) = {
    Times = Some (SELECT DISTINCT avail.{atime} AS Time
                  FROM avail),
    Unavailable = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL COALESCE((SELECT COUNT( * )
                                    FROM like AS {tname1} LEFT JOIN avail AS {tname2}
                                      ON {{tname1}}.{luser} = {{tname2}}.{auser}
                                        AND {{tname2}}.{atime} = {sql_exp_weaken t}
                                    WHERE {{tname1}}.{lkey} = {sql_exp_weaken k}
                                      AND {{tname2}}.{apreferred} IS NULL), 0)),
    Unpreferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL COALESCE((SELECT COUNT( * )
                                    FROM like AS {tname1}, avail AS {tname2}
                                    WHERE NOT {{tname2}}.{apreferred}
                                      AND {{tname1}}.{luser} = {{tname2}}.{auser}
                                      AND {{tname1}}.{lkey} = {sql_exp_weaken k}
                                      AND {{tname2}}.{atime} = {sql_exp_weaken t}), 0)),
    Preferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                   [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                   (SQL COALESCE((SELECT COUNT( * )
                                  FROM like AS {tname1}, avail AS {tname2}
                                  WHERE {{tname2}}.{apreferred}
                                    AND {{tname1}}.{luser} = {{tname2}}.{auser}
                                    AND {{tname1}}.{lkey} = {sql_exp_weaken k}
                                    AND {{tname2}}.{atime} = {sql_exp_weaken t}), 0))
}

fun assigned [key ::: Type] [lkey :: Name] [luser :: Name] [others ::: {Type}] [lks ::: {{Unit}}]
    [auser :: Name] [atime :: Name] [apreferred :: Name] [aks ::: {{Unit}}]
    [[lkey] ~ [luser]] [[lkey, luser] ~ others] [[auser] ~ [atime]] [[auser, atime] ~ [apreferred]]
    (like : sql_table ([lkey = key, luser = option string] ++ others) lks)
    (avail : sql_table [auser = string, atime = time, apreferred = bool] aks) = {
    Times = Some (SELECT DISTINCT avail.{atime} AS Time
                  FROM avail),
    Unavailable = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL COALESCE((SELECT COUNT( * )
                                    FROM like AS {tname1} LEFT JOIN avail AS {tname2}
                                      ON {{tname1}}.{luser} = {sql_nullable (SQL {{tname2}}.{auser})}
                                        AND {{tname2}}.{atime} = {sql_exp_weaken t}
                                    WHERE {{tname1}}.{lkey} = {sql_exp_weaken k}
                                      AND NOT ({{tname1}}.{luser} IS NULL)
                                      AND {{tname2}}.{apreferred} IS NULL), 0)),
    Unpreferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                     [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                     (SQL COALESCE((SELECT COUNT( * )
                                    FROM like AS {tname1}, avail AS {tname2}
                                    WHERE NOT {{tname2}}.{apreferred}
                                      AND {{tname1}}.{luser} = {sql_nullable (SQL {{tname2}}.{auser})}
                                      AND {{tname1}}.{lkey} = {sql_exp_weaken k}
                                      AND {{tname2}}.{atime} = {sql_exp_weaken t}), 0)),
    Preferred = fn [tabs :: {{Type}}] [tname1 :: Name] [tname2 :: Name]
                   [[tname1] ~ [tname2]] [[tname1, tname2] ~ tabs] k t =>
                   (SQL COALESCE((SELECT COUNT( * )
                                  FROM like AS {tname1}, avail AS {tname2}
                                  WHERE {{tname2}}.{apreferred}
                                    AND {{tname1}}.{luser} = {sql_nullable (SQL {{tname2}}.{auser})}
                                    AND {{tname1}}.{lkey} = {sql_exp_weaken k}
                                    AND {{tname2}}.{atime} = {sql_exp_weaken t}), 0))
}
