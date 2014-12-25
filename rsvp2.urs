(* Managing RSVP's to sets of events, with 2 different categories of participants *)

functor Make(M : sig
                 (* First category of users, thought of as the local event hosts *)
                 val homeLabel : string (* how to describe this category in text? *)
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeData :: {Type} (* Per-guest information to display *)
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeData
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeData ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyEq : eq $homeKey
                 val homeDataFl : folder homeData
                 val homeDataShow : $(map show homeData)
                 val homeDataLabels : $(map (fn _ => string) homeData)

                 (* Second category of users, thought of as visitors *)
                 val awayLabel : string (* how to describe this category in text? *)
                 con awayKey1 :: Name
                 type awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayData :: {Type} (* Per-guest information to display *)
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 constraint (awayKey ++ awayRest) ~ awayData
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayData ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyEq : eq $awayKey
                 val awayDataFl : folder awayData
                 val awayDataShow : $(map show awayData)
                 val awayDataLabels : $(map (fn _ => string) awayData)

                 (* Events, to which users may RSVP *)
                 con eventKey1 :: Name
                 type eventKeyT
                 con eventKeyR :: {Type}
                 constraint [eventKey1] ~ eventKeyR
                 con eventKey = [eventKey1 = eventKeyT] ++ eventKeyR
                 con eventData :: {Type} (* Per-guest information to display *)
                 con eventRest :: {Type}
                 constraint eventKey ~ eventRest
                 constraint (eventKey ++ eventRest) ~ eventData
                 con eventKeyName :: Name
                 con eventOtherConstraints :: {{Unit}}
                 constraint [eventKeyName] ~ eventOtherConstraints
                 val event : sql_table (eventKey ++ eventData ++ eventRest) ([eventKeyName = map (fn _ => ()) eventKey] ++ eventOtherConstraints)
                 val eventInj : $(map sql_injectable_prim eventKey)
                 val eventKeyFl : folder eventKey
                 val eventKeyShow : show $eventKey
                 val eventDataShow : show $eventData
                 val eventKeyEq : eq $eventKey

                 constraint homeKey ~ eventKey
                 constraint awayKey ~ eventKey
             end) : sig

    (* Home's view: a list of all events, with attendee lists and buttons to change your own RSVP's *)
    structure Home : sig
        type t
        val create : $M.homeKey -> transaction t
        val render : t -> xbody
        val onload : t -> transaction unit
    end

    (* Away's view: a list of all events, with buttons to change your own RSVP's *)
    structure Away : sig
        type t
        val create : $M.awayKey -> transaction t
        val render : t -> xbody
    end

end
