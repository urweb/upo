(* For scheduling events happening in different sessions *)

functor Make(M: sig
                 con sessionKey1 :: Name
                 con sessionKeyT :: Type
                 con sessionKeyR :: {Type}
                 con sessionOther :: {Type}
                 constraint [sessionKey1] ~ sessionKeyR
                 con sessionKey = [sessionKey1 = sessionKeyT] ++ sessionKeyR
                 constraint sessionKey ~ sessionOther
                 table session : (sessionKey ++ sessionOther)
                 val sessionFl : folder sessionKey
                 val session_inj : $(map sql_injectable_prim sessionKey)
                 val session_show : show $sessionKey
                 val session_read : read $sessionKey
                 val session_eq : $(map eq sessionKey)
                 val session_ord : $(map ord sessionKey)

                 con speakerKey1 :: Name
                 con speakerKeyT :: Type
                 con speakerKeyR :: {Type}
                 con speakerOther :: {Type}
                 constraint [speakerKey1] ~ speakerKeyR
                 con speakerKey = [speakerKey1 = speakerKeyT] ++ speakerKeyR
                 constraint speakerKey ~ speakerOther
                 constraint sessionKey ~ speakerKey
                 table speaker : (speakerKey ++ speakerOther)
                 val speakerFl : folder speakerKey
                 val speaker_inj : $(map sql_injectable_prim speakerKey)
                 val speaker_show : show $speakerKey
                 val speaker_read : read $speakerKey

                 con time :: {Type}
                 constraint time ~ sessionKey
                 constraint time ~ speakerKey
                 val timeFl : folder time
                 val time_inj : $(map sql_injectable_prim time)
                 val time_show : show $time
                 val time_read : read $time
                 val time_eq : $(map eq time)
                 val time_ord : $(map ord time)

                 table talk : (sessionKey ++ time ++ map option speakerKey)
             end) : sig
    structure AllSessions : Ui.S0
end
