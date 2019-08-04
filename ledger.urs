(* An extensible ledger: accumulate debits and credits from different data sources *)

(* Generators of ledger entries *)
type t

val fromTable : when :: Name -> fs ::: {Type} -> ks ::: {{Unit}}
                -> [[when] ~ fs]
                => sql_table ([when = time] ++ fs) ks
                -> ($([when = time] ++ fs) -> int)
                   (* This function generates the delta to the ledger balance.
                    * It could be positive, negative, or even zero,
                    * and the sign could vary across calls. *)
                -> ($([when = time] ++ fs) -> xbody)
                   (* This function generates the display description,
                    * to put in a table of credits/debits. *)
                -> t

(* Each month, we own some number of each resource.
 * Timestamped table entries inform us of:
 * (1) changes in count of this resource;
 * (2) cost per unit of the resource.
 * Thus, charge us [current count] * [unit cost] at the start of each month. *)
val deltaAndMultiplier : kt ::: Type -> k1 :: Name
                         -> when1 :: Name -> change :: Name -> rest1 ::: {Type}
                         -> k2 :: Name
                         -> when2 :: Name -> multiplier :: Name -> rest2 ::: {Type}
                         -> ks1 ::: {{Unit}} -> ks2 ::: {{Unit}}
                         -> [[when1] ~ [change]]
                         => [[when1, change] ~ [k1]]
                         => [[when1, change] ~ rest1]
                         => [[k1] ~ rest1]
                         => [[when2] ~ [multiplier]]
                         => [[when2, multiplier] ~ [k2]]
                         => [[when2, multiplier] ~ rest2]
                         => [[k2] ~ rest2]
                         => eq kt -> show kt
                         -> sql_table ([k1 = kt, when1 = time, change = int] ++ rest1) ks1
                         -> sql_table ([k2 = kt, when2 = time, multiplier = int] ++ rest2) ks2
                         -> t

val compose : t -> t -> t

functor Make(M : sig
                 val t : t
                 val authorized : transaction bool
             end) : Ui.S0
