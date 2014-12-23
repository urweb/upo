(* Scheduling meetings between two groups of users ("home" and "away").
 * The set of legal meeting times (represented with a table) is another parameter. *)

functor Make(M : sig
                 con homeKey :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)

                 con awayKey :: {Type}
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)

                 con timeKey :: {Type}
                 con timeRest :: {Type}
                 constraint timeKey ~ timeRest
                 con timeKeyName :: Name
                 con timeOtherConstraints :: {{Unit}}
                 constraint [timeKeyName] ~ timeOtherConstraints
                 val time : sql_table (timeKey ++ timeRest) ([timeKeyName = map (fn _ => ()) timeKey] ++ timeOtherConstraints)
                 val timeKeyFl : folder timeKey
                 val timeKeyShow : show $timeKey

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
             end) : sig

    structure FullGrid : sig
        type t
        val create : transaction t
        val render : t -> xbody
    end

end
