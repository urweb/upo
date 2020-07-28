(* Combinators to describe how to measure folks' availability for different times *)

con t :: Type (* key of table whose rows we are scheduling *) -> Type

val times : key ::: Type -> t key -> option (sql_query [] [] [] [Time = time])
val unavailable : key ::: Type -> tabs ::: {{Type}} -> tname1 :: Name -> tname2 :: Name
                  -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                  => t key
                  -> sql_exp tabs [] [] key
                  -> sql_exp tabs [] [] time
                  -> sql_exp tabs [] [] int
val unpreferred : key ::: Type -> tabs ::: {{Type}} -> tname1 :: Name -> tname2 :: Name
                  -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                  => t key
                  -> sql_exp tabs [] [] key
                  -> sql_exp tabs [] [] time
                  -> sql_exp tabs [] [] int
val preferred : key ::: Type -> tabs ::: {{Type}} -> tname1 :: Name -> tname2 :: Name
                -> [[tname1] ~ [tname2]] => [[tname1, tname2] ~ tabs]
                => t key
                -> sql_exp tabs [] [] key
                -> sql_exp tabs [] [] time
                -> sql_exp tabs [] [] int

val empty : key ::: Type -> t key
val compose : key ::: Type -> t key -> t key -> t key

val like : key ::: Type -> lkey :: Name -> luser :: Name -> lks ::: {{Unit}}
           -> auser :: Name -> atime :: Name -> apreferred :: Name -> aks ::: {{Unit}}
           -> [[lkey] ~ [luser]] => [[auser] ~ [atime]] => [[auser, atime] ~ [apreferred]]
           => sql_table [lkey = key, luser = string] lks
           -> sql_table [auser = string, atime = time, apreferred = bool] aks
           -> t key
