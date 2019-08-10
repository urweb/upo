(* An extensible ledger: accumulate debits and credits from different data sources,
 * combining ground-truth data for the past with projections for the future.
 * For now, balance changes are grouped at the level of months. *)

(* Generators of ledger entries *)
type t

(* Include one ground-truth table with no distinction between categories of expenses. *)
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
                    -> t

(* Include one ground-truth table, using some columns to key into categories of expenses. *)
val fromGroupedTable : when :: Name -> k ::: {Type} -> fs ::: {Type} -> ks ::: {{Unit}}
                    -> [k ~ fs] => [[when] ~ k ++ fs]
                    => sql_table ([when = time] ++ k ++ fs) ks
                    -> $(map eq k)
                    -> folder k
                    -> ($k -> xbody)
                       (* Description of a category *)
                    -> ($([when = time] ++ k ++ fs) -> int)
                       (* Delta to the ledger balance *)
                    -> t

val compose : t -> t -> t

functor Make(M : sig
                 val t : t
                 val authorized : transaction bool
             end) : Ui.S0
