(* An extensible ledger: accumulate debits and credits from different data sources,
 * combining ground-truth data for the past with projections for the future.
 * For now, balance changes are grouped at the level of months. *)

(* Generators of ledger entries, with private-state types *)
con t :: (Type * Type) -> Type

(* Include one ground-truth table with no distinction between categories of expenses. *)
con fromFlatTable :: (Type * Type)
val fromFlatTable : when :: Name -> fs ::: {Type} -> ks ::: {{Unit}}
                    -> [[when] ~ fs]
                    => sql_table ([when = time] ++ fs) ks
                    -> xbody
                       (* Description of this whole category, for when it
                        * appears in a month's transactions *)
                    -> ($([when = time] ++ fs) -> int)
                       (* This function generates the delta to the ledger balance.
                        * It could be positive, negative, or even zero,
                        * and the sign could vary across calls. *)
                    -> t fromFlatTable

(* Include one ground-truth table, using some columns to key into categories of expenses. *)
con fromGroupedTable :: (Type * Type)
val fromGroupedTable : when :: Name -> k ::: {Type} -> fs ::: {Type} -> ks ::: {{Unit}}
                    -> [k ~ fs] => [[when] ~ k ++ fs]
                    => sql_table ([when = time] ++ k ++ fs) ks
                    -> $(map eq k)
                    -> folder k
                    -> ($k -> xbody)
                       (* Description of a category *)
                    -> ($([when = time] ++ k ++ fs) -> int)
                       (* Delta to the ledger balance *)
                    -> t fromGroupedTable

(* Project future months' transactions by starting with the count & average
 * magnitude of all entries in a flat table.  Apply simple rules to change count
 * and magnitude month-by-month. *)
con flatAveragedEstimate :: (Type * Type)
val flatAveragedEstimate :
    (* First, let's take care of the three forecasting parameters,
     * whose default values should be found in a single-row table. *)
    monthsToAverage :: Name -> addToCountMonthly :: Name -> maxCount :: Name -> monthlyPercentChangeInMultiplier :: Name -> ks ::: {{Unit}}
    -> [[monthsToAverage] ~ [addToCountMonthly]] => [[monthsToAverage, addToCountMonthly] ~ [maxCount]]
    => [[monthsToAverage, addToCountMonthly, maxCount] ~ [monthlyPercentChangeInMultiplier]]
    => sql_table [monthsToAverage = int, addToCountMonthly = int, maxCount = int, monthlyPercentChangeInMultiplier = int] ks

    (* Now for the ground-truth table that we use to compute the initial count & multiplier. *)
    -> when :: Name -> fs ::: {Type} -> gks ::: {{Unit}}
    -> [[when] ~ fs]
    => sql_table ([when = time] ++ fs) gks
    -> xbody (* category description *)
    -> ($([when = time] ++ fs) -> int) (* extracting transaction value *)

    -> t flatAveragedEstimate

con compose :: (Type * Type) -> (Type * Type) -> (Type * Type)
val compose : a ::: (Type * Type) -> b ::: (Type * Type) -> t a -> t b -> t (compose a b)

functor Make(M : sig
                 con ps :: (Type * Type)
                 val t : t ps
                 val authorized : transaction bool
             end) : Ui.S0
