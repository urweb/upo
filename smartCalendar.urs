(* The dumbest way of using a calendar widget: show rows from tables *)

functor Make(M : sig
                 val addon : CalendarAddons.t []
                 val slotDuration : option string
                 val whoami : transaction (option string)
             end) : Ui.S0
