(* Scheduling meetings between two groups of users ("home" and "away").
 * The set of legal meeting times (represented with a table) is another parameter. *)

functor Make(M : sig
                 con homeKey1 :: Name
                 con homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyEq : eq $homeKey

                 con awayKey1 :: Name
                 con awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyRead : read $awayKey
                 val awayKeyEq : eq $awayKey

                 con timeKey1 :: Name
                 con timeKeyT
                 con timeKeyR :: {Type}
                 constraint [timeKey1] ~ timeKeyR
                 con timeKey = [timeKey1 = timeKeyT] ++ timeKeyR
                 con timeRest :: {Type}
                 constraint timeKey ~ timeRest
                 con timeKeyName :: Name
                 con timeOtherConstraints :: {{Unit}}
                 constraint [timeKeyName] ~ timeOtherConstraints
                 val time : sql_table (timeKey ++ timeRest) ([timeKeyName = map (fn _ => ()) timeKey] ++ timeOtherConstraints)
                 val timeInj : $(map sql_injectable_prim timeKey)
                 val timeKeyFl : folder timeKey
                 val timeKeyShow : show $timeKey
                 val timeKeyEq : eq $timeKey

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
                 constraint awayKey ~ [Channel]
             end) : sig

    val addMeeting : $(M.homeKey ++ M.awayKey ++ M.timeKey) -> transaction unit

    structure FullGrid : sig
        type t
        val create : transaction t
        val onload : t -> transaction unit
        val render : t -> xbody
    end

    structure OneAway : sig
        type t
        val create : $M.awayKey -> transaction t
        val onload : t -> transaction unit
        val render : t -> xbody
    end

end
