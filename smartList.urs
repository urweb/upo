(* A configurable way of listing table rows *)

con t :: {Type} (* available columns of table we are listing *)
    -> Type     (* configuration, to prepare once on server *)
    -> Type     (* internal type for server-generated data to render locally *)
    -> Type

val compose : r ::: {Type} -> cfga ::: Type -> cfgb ::: Type
              -> sta ::: Type -> stb ::: Type
              -> t r cfga sta -> t r cfgb stb -> t r (cfga * cfgb) (sta * stb)

con columnInHeader_cfg :: Type -> Type
con columnInHeader_st :: Type -> Type
val columnInHeader : col :: Name -> colT ::: Type -> r ::: {Type} -> [[col] ~ r]
                     => show colT
                     -> t ([col = colT] ++ r) (columnInHeader_cfg colT) (columnInHeader_st colT)

con columnInBody_cfg :: Type -> Type
con columnInBody_st :: Type -> Type
val columnInBody : col :: Name -> colT ::: Type -> r ::: {Type} -> [[col] ~ r]
                   => show colT -> string (* label *)
                   -> t ([col = colT] ++ r) (columnInBody_cfg colT) (columnInBody_st colT)

con iconButtonInHeader_cfg :: {Type} -> Type
con iconButtonInHeader_st :: {Type} -> Type
val iconButtonInHeader : cols ::: {Type} -> r ::: {Type} -> [cols ~ r]
                         => transaction (option string) (* get username, if any *)
                         -> (option string (* username, if any *)
                             -> time       (* very recent timestamp *)
                             -> $cols      (* values of selected columns *)
                             -> option (css_class (* choose a Font Awesome icon *)
                                        * url     (* ...and where clicking should take you *)))
                         -> t (cols ++ r) (iconButtonInHeader_cfg cols) (iconButtonInHeader_st cols)

con orderedLinked_cfg :: Type -> Type
con orderedLinked_st :: Type -> Type
val orderedLinked : this :: Name -> fthis :: Name -> thisT ::: Type
                    -> fthat :: Name -> thatT ::: Type
                    -> r ::: {Type} -> fr ::: {Type} -> ks ::: {{Unit}}
                    -> [[this] ~ r] => [[fthis] ~ [fthat]] => [[fthis, fthat] ~ [SeqNum]] => [[fthis, fthat, SeqNum] ~ fr]
                    => show thatT -> sql_injectable thisT
                    -> sql_table ([fthis = thisT, fthat = thatT, SeqNum = int] ++ fr) ks
                    -> string (* label *)
                    -> t ([this = thisT] ++ r) (orderedLinked_cfg thatT) (orderedLinked_st thatT)

functor Make(M : sig
                 con sortBy :: Name
                 type sortByT
                 con r :: {Type}
                 constraint [sortBy] ~ r
                 table tab : ([sortBy = sortByT] ++ r)
                 val wher : sql_exp [Tab = [sortBy = sortByT] ++ r] [] [] bool

                 type cfg
                 type st
                 val t : t ([sortBy = sortByT] ++ r) cfg st
             end) : Ui.S0
