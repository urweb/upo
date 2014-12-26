(* Scheduling meetings between two groups of users ("home" and "away").
 * The set of legal meeting times (represented with a table) is another parameter. *)

functor Make(M : sig
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeOffice :: {Type}
                 con homeConst :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeOffice
                 constraint (homeKey ++ homeRest ++ homeOffice) ~ homeConst
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeOffice ++ homeConst ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyRead : read $homeKey
                 val homeKeyEq : eq $homeKey
                 val officeFl : folder homeOffice
                 val officeShow : show $homeOffice
                 val constFl : folder homeConst
                 val constInj : $(map sql_injectable homeConst)
                 val const : $homeConst

                 con awayKey1 :: Name
                 type awayKeyT
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
                 val timeKeyEq : eq $timeKey

                 constraint homeKey ~ awayKey
                 constraint (homeKey ++ awayKey) ~ timeKey
                 constraint homeOffice ~ timeKey
                 constraint (homeKey ++ awayKey) ~ [ByHome, Channel]

                 val amHome : transaction (option $homeKey)
                 val amAway : transaction (option $awayKey)
             end) : sig

    (* Two nested modules provide views centered on the home and away perspectives, respectively. *)

    structure Home : sig
        (* Display a full, editable grid of all meetings (rows: homes, columns: times). *)
        structure FullGrid : sig
            type t
            val create : transaction t
            val onload : t -> transaction unit
            val render : t -> xbody
        end

        (* Display a read-only record of all records for a home. *)
        structure One : sig
            type t
            val create : $M.homeKey -> transaction t
            val onload : t -> transaction unit
            val render : t -> xbody
        end

        (* Inputing meeting preferences for a home *)
        structure Prefs : sig
            type t
            val create : $M.homeKey -> transaction t
            val render : t -> xbody
        end

        (* Inputing schedule constraints for a home *)
        structure Unavail : sig
            type t
            val create : $M.homeKey -> transaction t
            val render : t -> xbody
        end
    end

    structure Away : sig
        (* Display a full, editable grid of all meetings (rows: aways, columns: times). *)
        structure FullGrid : sig
            type t
            val create : transaction t
            val onload : t -> transaction unit
            val render : t -> xbody
        end

        (* Display a read-only record of all records for an away. *)
        structure One : sig
            type t
            val create : $M.awayKey -> transaction t
            val onload : t -> transaction unit
            val render : t -> xbody
        end

        (* Inputing meeting preferences for an away *)
        structure Prefs : sig
            type t
            val create : $M.awayKey -> transaction t
            val render : t -> xbody
        end

        (* Inputing schedule constraints for an away *)
        structure Unavail : sig
            type t
            val create : $M.awayKey -> transaction t
            val render : t -> xbody
        end
    end

    (* Using preferences from both sides, try to schedule more meetings heuristically. *)
    val scheduleSome : transaction unit

end
