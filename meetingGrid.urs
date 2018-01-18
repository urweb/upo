(* Scheduling meetings between two groups of users ("home" and "away").
 * The set of legal meeting times (represented with a table) is another parameter. *)

functor Make(M : sig
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeOffice :: {Type}
                 con homeHardConst :: {Type}
                 con homeSoftConst :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeOffice
                 constraint (homeKey ++ homeRest ++ homeOffice) ~ homeSoftConst
                 constraint (homeKey ++ homeRest ++ homeOffice ++ homeSoftConst) ~ homeHardConst
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeOffice ++ homeSoftConst ++ homeHardConst ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyRead : read $homeKey
                 val homeKeyEq : $(map eq homeKey)
                 val homeKeyOrd : $(map ord homeKey)
                 val officeFl : folder homeOffice
                 val officeShow : show $homeOffice
                 val homeSoftConstFl : folder homeSoftConst
                 val homeSoftConstInj : $(map sql_injectable homeSoftConst)
                 val homeSoftConst : $homeSoftConst
                 val homeHardConstFl : folder homeHardConst
                 val homeHardConstInj : $(map sql_injectable homeHardConst)
                 val homeHardConst : $homeHardConst

                 con awayKey1 :: Name
                 type awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayConst :: {Type}
                 con awayFilter :: {Type}
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 constraint (awayKey ++ awayRest) ~ awayConst
                 constraint (awayKey ++ awayRest ++ awayConst) ~ awayFilter
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayConst ++ awayFilter ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyRead : read $awayKey
                 val awayKeyEq : $(map eq awayKey)
                 val awayKeyOrd : $(map ord awayKey)
                 val awayConstFl : folder awayConst
                 val awayConstInj : $(map sql_injectable awayConst)
                 val awayConst : $awayConst
                 val awayFilterFl : folder awayFilter

                 con timeKey1 :: Name
                 type timeKeyT
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
                 val timeKeyRead : read $timeKey
                 val timeKeyEq : $(map eq timeKey)

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
                 constraint homeOffice ~ timeKey
                 constraint (homeKey ++ awayKey) ~ [ByHome, Channel]

                 val amHome : transaction (option $homeKey)
                 val amAway : transaction (option $awayKey)

                 (* We also allow for interleaving fixed events in individual schedules, calling this function to get the list. *)
                 val fixed : transaction (list {When : $timeKey, Descr : string})
                 val timeOrd : ord $timeKey
             end) : sig

    (* Two nested modules provide views centered on the home and away perspectives, respectively. *)

    structure Home : sig
        (* Display a full, editable grid of all meetings (rows: homes, columns: times). *)
        structure FullGrid : Ui.S0

        (* Display a read-only record of all records for a home. *)
        structure One : Ui.S where type input = $M.homeKey

        (* Inputing meeting preferences for a home *)
        structure Prefs : Ui.S where type input = $M.homeKey

        (* Inputing schedule constraints for a home *)
        structure Unavail : Ui.S where type input = $M.homeKey

        (* Delete all of this person's meetings. *)
        val deleteFor : $M.homeKey -> transaction unit
    end

    structure Away : sig
        (* Display a full, editable grid of all meetings (rows: aways, columns: times). *)
        structure FullGrid : Ui.S where type input = $(M.awayKey ++ M.awayFilter) -> signal bool

        (* Display a read-only record of all records for an away. *)
        structure One : Ui.S where type input = $M.awayKey

        (* Inputing meeting preferences for an away *)
        structure Prefs : Ui.S where type input = $M.awayKey

        (* Inputing schedule constraints for an away *)
        structure Unavail : Ui.S where type input = $M.awayKey

        (* Delete all of this person's meetings. *)
        val deleteFor : $M.awayKey -> transaction unit
    end

    (* Using preferences from both sides, try to schedule more meetings heuristically. *)
    val scheduleSome : transaction unit

    (* And if you really want an SQL view of the current meeting state, use this one. *)
    view meetings : (M.homeKey ++ M.timeKey ++ M.awayKey)

end
