(* Attaching comments to the results of queries, in a generic way *)

functor Make(M : sig
                 con key1 :: Name
                 type keyT
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 con keyRest :: {Type}
                 constraint key ~ keyRest
                 con keyName :: Name
                 con keyOtherConstraints :: {{Unit}}
                 constraint [keyName] ~ keyOtherConstraints
                 val key : sql_table (key ++ keyRest) ([keyName = map (fn _ => ()) key] ++ keyOtherConstraints)
                 val keyInj : $(map sql_injectable_prim key)
                 val keyFl : folder key
                 val keyEq : $(map eq key)
                 val keyOrd : $(map ord key)

                 con user1 :: Name
                 type userT
                 con userR :: {Type}
                 constraint [user1] ~ userR
                 con user = [user1 = userT] ++ userR
                 con userRest :: {Type}
                 constraint user ~ userRest
                 con userKeyName :: Name
                 con userOtherConstraints :: {{Unit}}
                 constraint [userKeyName] ~ userOtherConstraints
                 val user : sql_table (user ++ userRest) ([userKeyName = map (fn _ => ()) user] ++ userOtherConstraints)
                 val userInj : $(map sql_injectable_prim user)
                 val userFl : folder user
                 val userEq : $(map eq user)
                 val userOrd : $(map ord user)
                 val userShow : show $user

                 con rest :: {Type}
                 constraint key ~ user
                 constraint (key ++ user) ~ rest
                 constraint (key ++ user ++ rest) ~ [Comment]
                 val query : sql_query [] [] [] (key ++ rest)
                 val restFl : folder rest

                 val render : $(key ++ rest) -> xbody
                 val amUser : transaction (option $user)
             end) : sig
    include Ui.S where type input = option $M.user

    (* Call these when changing the [key] table. *)
    val add : $M.key -> transaction unit
    val delete : $M.key -> transaction unit
    val modify : {Old : $M.key, New : $M.key} -> transaction unit
end
