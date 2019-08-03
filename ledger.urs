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

val compose : t -> t -> t

functor Make(M : sig
                 val t : t
                 val authorized : transaction bool
             end) : Ui.S0
