(* UI-less concept that just makes sure a table is populated with all times in some range *)

functor Make(M : sig
                 table slot : { Begin : time, End : time }
                 val initial : time
                 val final : time
                 val duration : int (* in seconds *)
             end) : sig
end
