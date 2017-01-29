(* Managing RSVP's to sets of events, with 2 different categories of participants *)

functor Make(M : sig
                 val homeLabel : string
                 con homeKey1 :: Name
                 type homeKeyT
                 con homeKeyR :: {Type}
                 constraint [homeKey1] ~ homeKeyR
                 con homeKey = [homeKey1 = homeKeyT] ++ homeKeyR
                 con homeData :: {Type}
                 con homeSensitiveData :: {Type}
                 con homeRest :: {Type}
                 constraint homeKey ~ homeRest
                 constraint (homeKey ++ homeRest) ~ homeData
                 constraint (homeKey ++ homeRest ++ homeData) ~ homeSensitiveData
                 con homeKeyName :: Name
                 con homeOtherConstraints :: {{Unit}}
                 constraint [homeKeyName] ~ homeOtherConstraints
                 val home : sql_table (homeKey ++ homeData ++ homeSensitiveData ++ homeRest) ([homeKeyName = map (fn _ => ()) homeKey] ++ homeOtherConstraints)
                 val homeInj : $(map sql_injectable_prim homeKey)
                 val homeKeyFl : folder homeKey
                 val homeKeyShow : show $homeKey
                 val homeKeyEq : $(map eq homeKey)
                 val homeDataFl : folder homeData
                 val homeDataShow : $(map show homeData)
                 val homeDataLabels : $(map (fn _ => string) homeData)
                 val homeSensitiveDataFl : folder homeSensitiveData
                 val homeSensitiveDataShow : $(map show homeSensitiveData)
                 val homeSensitiveDataLabels : $(map (fn _ => string) homeSensitiveData)

                 val awayLabel : string
                 con awayKey1 :: Name
                 type awayKeyT
                 con awayKeyR :: {Type}
                 constraint [awayKey1] ~ awayKeyR
                 con awayKey = [awayKey1 = awayKeyT] ++ awayKeyR
                 con awayData :: {Type}
                 con awaySensitiveData :: {Type}
                 con awayRest :: {Type}
                 constraint awayKey ~ awayRest
                 constraint (awayKey ++ awayRest) ~ awayData
                 constraint (awayKey ++ awayRest ++ awayData) ~ awaySensitiveData
                 con awayKeyName :: Name
                 con awayOtherConstraints :: {{Unit}}
                 constraint [awayKeyName] ~ awayOtherConstraints
                 val away : sql_table (awayKey ++ awayData ++ awaySensitiveData ++ awayRest) ([awayKeyName = map (fn _ => ()) awayKey] ++ awayOtherConstraints)
                 val awayInj : $(map sql_injectable_prim awayKey)
                 val awayKeyFl : folder awayKey
                 val awayKeyShow : show $awayKey
                 val awayKeyEq : $(map eq awayKey)
                 val awayDataFl : folder awayData
                 val awayDataShow : $(map show awayData)
                 val awayDataLabels : $(map (fn _ => string) awayData)
                 val awaySensitiveDataFl : folder awaySensitiveData
                 val awaySensitiveDataShow : $(map show awaySensitiveData)
                 val awaySensitiveDataLabels : $(map (fn _ => string) awaySensitiveData)

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
                 val render : $eventData -> xbody
                 val eventInj : $(map sql_injectable_prim eventKey)
                 val eventKeyFl : folder eventKey
                 val eventKeyShow : show $eventKey
                 val eventKeyEq : $(map eq eventKey)

                 constraint homeKey ~ eventKey
                 constraint awayKey ~ eventKey

                 val amHome : transaction (option $homeKey)
                 val amPrivilegedHome : transaction (option $homeKey)
                 val amAway : transaction (option $awayKey)

                 (* Access control *)
                 val homeMayRsvpTo : $homeKey -> transaction (list $eventKey)
                 val awayMayRsvpTo : $awayKey -> transaction (list $eventKey)
             end) : sig

    (* Home's view: a list of all events, with attendee lists and buttons to change your own RSVP's *)
    structure Home : Ui.S where type input = $M.homeKey

    (* Expanded Home view with sensitive data *)
    structure HomePrivileged : Ui.S where type input = $M.homeKey

    (* Away's view: a list of all events, with buttons to change your own RSVP's *)
    structure Away : Ui.S where type input = $M.awayKey

end
