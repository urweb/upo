(* A configurable way of listing table rows as Bootstrap cards with fairly freeform formatting *)

con t :: Type   (* input provided before we grab the items to list *)
    -> {Type}   (* available columns of table we are listing *)
    -> Type     (* configuration, to prepare once on server *)
    -> Type     (* internal type for server-generated data to render locally *)
    -> Type

val empty : inp ::: Type -> r ::: {Type} -> t inp r unit unit
val compose : inp ::: Type -> r ::: {Type} -> cfga ::: Type -> cfgb ::: Type
              -> sta ::: Type -> stb ::: Type
              -> t inp r cfga sta -> t inp r cfgb stb -> t inp r (cfga * cfgb) (sta * stb)

type inputIs_cfg
type inputIs_st
val inputIs : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
              => sql_injectable inp
              -> t inp ([col = inp] ++ r) inputIs_cfg inputIs_st

type inputIsOpt_cfg
type inputIsOpt_st
val inputIsOpt : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
                 => sql_injectable_prim inp
                 -> t inp ([col = option inp] ++ r) inputIs_cfg inputIs_st

type inputConnected_cfg
type inputConnected_st
val inputConnected : inp ::: Type -> key :: Name -> keyT ::: Type -> r ::: {Type} -> ckey :: Name -> inpCol :: Name -> cothers ::: {Type} -> ckeys ::: {{Unit}}
                     -> [[key] ~ r] => [[ckey] ~ [inpCol]] => [[ckey, inpCol] ~ cothers]
                     => sql_injectable inp
                     -> sql_table ([ckey = keyT, inpCol = inp] ++ cothers) ckeys
                     -> t inp ([key = keyT] ++ r) inputConnected_cfg inputConnected_st

con columnInHeader_cfg :: Type -> Type
con columnInHeader_st :: Type -> Type
val columnInHeader : inp ::: Type -> col :: Name -> colT ::: Type -> r ::: {Type} -> [[col] ~ r]
                     => show colT
                     -> t inp ([col = colT] ++ r) (columnInHeader_cfg colT) (columnInHeader_st colT)

con columnInBody_cfg :: Type -> Type
con columnInBody_st :: Type -> Type
val columnInBody : inp ::: Type -> col :: Name -> colT ::: Type -> r ::: {Type} -> [[col] ~ r]
                   => show colT -> string (* label *)
                   -> t inp ([col = colT] ++ r) (columnInBody_cfg colT) (columnInBody_st colT)

type htmlInBody_cfg
type htmlInBody_st
val htmlInBody : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
                 => t inp ([col = string] ++ r) htmlInBody_cfg htmlInBody_st

con iconButtonInHeader_cfg :: {Type} -> Type
con iconButtonInHeader_st :: {Type} -> Type
val iconButtonInHeader : inp ::: Type -> cols ::: {Type} -> r ::: {Type} -> [cols ~ r]
                         => transaction (option string) (* get username, if any *)
                         -> (option string (* username, if any *)
                             -> time       (* very recent timestamp *)
                             -> $cols      (* values of selected columns *)
                             -> option (css_class (* choose a Font Awesome icon *)
                                        * url     (* ...and where clicking should take you *)))
                         -> t inp (cols ++ r) (iconButtonInHeader_cfg cols) (iconButtonInHeader_st cols)

con linked_cfg :: Type -> Type
con linked_st :: Type -> Type
val linked : inp ::: Type -> this :: Name -> fthis :: Name -> thisT ::: Type
             -> fthat :: Name -> thatT ::: Type
             -> r ::: {Type} -> fr ::: {Type} -> ks ::: {{Unit}}
             -> [[this] ~ r] => [[fthis] ~ [fthat]] => [[fthis, fthat] ~ fr]
             => show thatT -> sql_injectable thisT
             -> sql_table ([fthis = thisT, fthat = thatT] ++ fr) ks
             -> string (* label *)
             -> t inp ([this = thisT] ++ r) (linked_cfg thatT) (linked_st thatT)

con orderedLinked_cfg :: Type -> Type
con orderedLinked_st :: Type -> Type
val orderedLinked : inp ::: Type -> this :: Name -> fthis :: Name -> thisT ::: Type
                    -> fthat :: Name -> thatT ::: Type
                    -> r ::: {Type} -> fr ::: {Type} -> ks ::: {{Unit}}
                    -> [[this] ~ r] => [[fthis] ~ [fthat]] => [[fthis, fthat] ~ [SeqNum]] => [[fthis, fthat, SeqNum] ~ fr]
                    => show thatT -> sql_injectable thisT
                    -> sql_table ([fthis = thisT, fthat = thatT, SeqNum = int] ++ fr) ks
                    -> string (* label *)
                    -> t inp ([this = thisT] ++ r) (orderedLinked_cfg thatT) (orderedLinked_st thatT)

functor LinkedWithFollow(M : sig
                             type inp
                             con this :: Name
                             con fthis :: Name
                             con thisT :: Type
                             con fthat :: Name
                             con thatT :: Type
                             con r :: {Type}
                             constraint [this] ~ r
                             constraint [fthis] ~ [fthat]
                             val show_that : show thatT
                             val inj_this : sql_injectable thisT
                             val inj_that : sql_injectable thatT
                             table from : {fthis : thisT, fthat : thatT}

                             con user :: Name
                             con cthat :: Name
                             constraint [user] ~ [cthat]
                             table to : {user : string, cthat : thatT}

                             val label : string
                             val whoami : transaction (option string)
                         end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT] ++ M.r) cfg internal
end

type nonnull_cfg
type nonnull_st
val nonnull : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
              => t inp ([col = option ct] ++ r) nonnull_cfg nonnull_st

type isnull_cfg
type isnull_st
val isnull : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
    => t inp ([col = option ct] ++ r) isnull_cfg isnull_st

type isTrue_cfg
type isTrue_st
val isTrue : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
             => t inp ([col = bool] ++ r) isTrue_cfg isTrue_st

type past_cfg
type past_st
val past : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
           => t inp ([col = time] ++ r) past_cfg past_st
val pastOpt : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
              => t inp ([col = option time] ++ r) past_cfg past_st

type future_cfg
type future_st
val future : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
             => t inp ([col = time] ++ r) future_cfg future_st
val futureOpt : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
                => t inp ([col = option time] ++ r) future_cfg future_st

type interval_cfg
type interval_st
val interval : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
               => string -> string (* FullCalendar durations: before and after *)
               -> t inp ([col = time] ++ r) interval_cfg interval_st

type taggedWithUser_cfg
type taggedWithUser_st
val taggedWithUser : inp ::: Type -> user :: Name -> r ::: {Type} -> [[user] ~ r]
                   => transaction (option string) (* get username, if any *)
                   -> t inp ([user = string] ++ r) taggedWithUser_cfg taggedWithUser_st
val taggedWithUserOpt : inp ::: Type -> user :: Name -> r ::: {Type} -> [[user] ~ r]
                        => transaction (option string)
                        -> t inp ([user = option string] ++ r) taggedWithUser_cfg taggedWithUser_st

type linkedToUser_cfg
type linkedToUser_st
val linkedToUser : inp ::: Type -> key :: Name -> keyT ::: Type -> r ::: {Type} -> [[key] ~ r]
                   => ckey :: Name -> user :: Name -> cr ::: {Type} -> ks ::: {{Unit}} -> [[ckey] ~ [user]] => [[ckey, user] ~ cr]
                   => sql_table ([ckey = keyT, user = string] ++ cr) ks (* connector that must link current user to row *)
                   -> transaction (option string) (* get username, if any *)
                   -> t inp ([key = keyT] ++ r) linkedToUser_cfg linkedToUser_st

type doubleLinkedToUser_cfg
type doubleLinkedToUser_st
val doubleLinkedToUser : inp ::: Type -> key :: Name -> keyT ::: Type -> r ::: {Type} -> [[key] ~ r]
                         => ckey :: Name -> ikey :: Name -> ikeyT ::: Type -> cr1 ::: {Type} -> ks1 ::: {{Unit}} -> [[ckey] ~ [ikey]] => [[ckey, ikey] ~ cr1]
                         => ikey2 :: Name -> user :: Name -> cr2 ::: {Type} -> ks2 ::: {{Unit}} -> [[ikey2] ~ [user]] => [[ikey2, user] ~ cr2]
                         => sql_table ([ckey = keyT, ikey = ikeyT] ++ cr1) ks1
                         -> sql_table ([ikey2 = ikeyT, user = string] ++ cr2) ks2
                         -> transaction (option string) (* get username, if any *)
                         -> t inp ([key = keyT] ++ r) doubleLinkedToUser_cfg doubleLinkedToUser_st

type sortby_cfg
type sortby_st
val sortby : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
             => t inp ([col = ct] ++ r) sortby_cfg sortby_st
val sortbyDesc : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
                 => t inp ([col = ct] ++ r) sortby_cfg sortby_st

type periodicRefresh_cfg
type periodicRefresh_st
val periodicRefresh : inp ::: Type -> r ::: {Type}
                      -> string (* frequency, as FullCalendar duration *)
                      -> t inp r periodicRefresh_cfg periodicRefresh_st

functor Make(M : sig
                 con r :: {Type}
                 table tab : r
                 val title : string

                 type cfg
                 type st
                 val t : t unit r cfg st
                 val notifyOnNonempty : bool
                 val authorized : transaction bool
             end) : Ui.S0

(* This version expects an input of the same type as one of the columns. *)
functor Make1(M : sig
                  type inp
                  con r :: {Type}
                  table tab : r
                  val title : string

                  type cfg
                  type st
                  val t : t inp r cfg st
                  val notifyOnNonempty : bool
                  val authorized : transaction bool
              end) : Ui.S where type input = M.inp
