(* A configurable way of listing table rows as HTML tables *)

con t :: Type (* arbitrary input value, to use for filtering *)
    -> {Type} (* available columns of table we are listing *)
    -> Type   (* configuration, to prepare once on server *)
    -> Type   (* internal type for server-generated data to render locally *)
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

type inputConnects_cfg
type inputConnects_st
val inputConnects : inp ::: Type -> key :: Name -> keyT ::: Type -> r ::: {Type} -> ckey :: Name -> inpCol :: Name -> ckeys ::: {{Unit}}
                    -> [[key] ~ r] => [[ckey] ~ [inpCol]]
                    => sql_injectable keyT -> sql_injectable inp
                    -> sql_table [ckey = keyT, inpCol = inp] ckeys
                    -> t inp ([key = keyT] ++ r) inputConnected_cfg inputConnected_st

con column_cfg :: Type -> Type
con column_st :: Type -> Type
val column : inp ::: Type -> col :: Name -> colT ::: Type -> r ::: {Type} -> [[col] ~ r]
             => show colT
             -> string (* label *)
             -> t inp ([col = colT] ++ r) (column_cfg colT) (column_st colT)

type html_cfg
type html_st
val html : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
           => string (* label *)
           -> t inp ([col = string] ++ r) html_cfg html_st

con iconButton_cfg :: {Type} -> Type
con iconButton_st :: {Type} -> Type
val iconButton : inp ::: Type -> cols ::: {Type} -> r ::: {Type} -> [cols ~ r]
                 => transaction (option string) (* get username, if any *)
                 -> (option string (* username, if any *)
                     -> time       (* very recent timestamp *)
                     -> $cols      (* values of selected columns *)
                     -> option (css_class (* choose a Font Awesome icon *)
                                * url     (* ...and where clicking should take you *)))
                 -> string (* label *)
                 -> t inp (cols ++ r) (iconButton_cfg cols) (iconButton_st cols)

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

con doubleLinked_cfg :: Type -> Type
con doubleLinked_st :: Type -> Type
val doubleLinked : inp ::: Type -> this :: Name -> fthis :: Name -> thisT ::: Type
                   -> finterm1 :: Name -> finterm2 :: Name -> intermT ::: Type
                   -> fthat :: Name -> thatT ::: Type
                   -> r ::: {Type} -> fr1 ::: {Type} -> fr2 ::: {Type}
                   -> ks1 ::: {{Unit}} -> ks2 ::: {{Unit}}
                   -> [[this] ~ r] => [[fthis] ~ [finterm1]] => [[fthis, finterm1] ~ fr1]
                   => [[finterm2] ~ [fthat]] => [[finterm2, fthat] ~ fr2]
                   => show thatT -> sql_injectable thisT
                   -> sql_table ([fthis = thisT, finterm1 = intermT] ++ fr1) ks1
                   -> sql_table ([finterm2 = intermT, fthat = thatT] ++ fr2) ks2
                   -> string (* label *)
                   -> t inp ([this = thisT] ++ r) (doubleLinked_cfg thatT) (doubleLinked_st thatT)

con tripleLinked_cfg :: Type -> Type
con tripleLinked_st :: Type -> Type
val tripleLinked : inp ::: Type -> this :: Name -> fthis :: Name -> thisT ::: Type
                   -> finterm1 :: Name -> finterm2 :: Name -> finterm3 :: Name -> finterm4 :: Name
                   -> intermT1 ::: Type -> intermT2 ::: Type
                   -> fthat :: Name -> thatT ::: Type
                   -> r ::: {Type} -> fr1 ::: {Type} -> fr2 ::: {Type} -> fr3 ::: {Type}
                   -> ks1 ::: {{Unit}} -> ks2 ::: {{Unit}} -> ks3 ::: {{Unit}}
                   -> [[this] ~ r] => [[fthis] ~ [finterm1]] => [[fthis, finterm1] ~ fr1]
                   => [[finterm2] ~ [finterm3]] => [[finterm2, finterm3] ~ fr2]
                   => [[finterm4] ~ [fthat]] => [[finterm4, fthat] ~ fr3]
                   => show thatT -> sql_injectable thisT
                   -> sql_table ([fthis = thisT, finterm1 = intermT1] ++ fr1) ks1
                   -> sql_table ([finterm2 = intermT1, finterm3 = intermT2] ++ fr2) ks2
                   -> sql_table ([finterm4 = intermT2, fthat = thatT] ++ fr3) ks3
                   -> string (* label *)
                   -> t inp ([this = thisT] ++ r) (tripleLinked_cfg thatT) (tripleLinked_st thatT)

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

functor LinkedWithEdit(M : sig
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
                           val read_that : read thatT
                           val eq_that : eq thatT
                           val inj_this : sql_injectable thisT
                           val inj_that : sql_injectable thatT
                           table link : {fthis : thisT, fthat : thatT}
                           val title : string

                           con tkey :: Name
                           con tr :: {Type}
                           constraint [tkey] ~ tr
                           table that : ([tkey = thatT] ++ tr)
                           val thatTitle : string

                           val label : string
                           val authorized : transaction bool
                       end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT] ++ M.r) cfg internal
end

functor LinkedWithEditForOwner(M : sig
                                   type inp
                                   con this :: Name
                                   con owner :: Name
                                   con fthis :: Name
                                   con thisT :: Type
                                   con fthat :: Name
                                   con thatT :: Type
                                   con r :: {Type}
                                   constraint [this] ~ [owner]
                                   constraint [this, owner] ~ r
                                   constraint [fthis] ~ [fthat]
                                   table tab : ([this = thisT, owner = option string] ++ r)
                                   val show_that : show thatT
                                   val read_that : read thatT
                                   val eq_that : eq thatT
                                   val inj_this : sql_injectable thisT
                                   val inj_that : sql_injectable thatT
                                   table link : {fthis : thisT, fthat : thatT}
                                   val title : string

                                   con tkey :: Name
                                   con tr :: {Type}
                                   constraint [tkey] ~ tr
                                   table that : ([tkey = thatT] ++ tr)
                                   val thatTitle : string

                                   val label : string
                                   val whoami : transaction (option string)
                               end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT, M.owner = option string] ++ M.r) cfg internal
end

functor LinkedWithEditAndDefault(M : sig
                                     con this :: Name
                                     con fthis :: Name
                                     con thisT :: Type
                                     con fthat :: Name
                                     con thatT :: Type
                                     con r :: {Type}
                                     constraint [this] ~ r
                                     constraint [fthis] ~ [fthat]
                                     val show_that : show thatT
                                     val read_that : read thatT
                                     val eq_that : eq thatT
                                     val inj_this : sql_injectable thisT
                                     val inj_that : sql_injectable thatT
                                     table link : {fthis : thisT, fthat : thatT}
                                     val title : string

                                     con tkey :: Name
                                     con tr :: {Type}
                                     constraint [tkey] ~ tr
                                     table that : ([tkey = thatT] ++ tr)
                                     val thatTitle : string

                                     val label : string
                                     val authorized : transaction bool
                                 end) : sig
    type cfg
    type internal
    val t : t M.thatT ([M.this = M.thisT] ++ M.r) cfg internal
end

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

(* Indicate interest in the current item. *)
functor Like(M : sig
                 type inp
                 con this :: Name
                 con fthis :: Name
                 con thisT :: Type
                 con user :: Name
                 con r :: {Type}
                 constraint [this] ~ r
                 constraint [fthis] ~ [user]
                 val inj_this : sql_injectable thisT
                 table like : {fthis : thisT, user : string}

                 val label : string
                 val whoami : transaction (option string)
             end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT] ++ M.r) cfg internal
end

(* Like [Like], but with an additional option to indicate preferred choices *)
functor Bid(M : sig
                type inp
                con this :: Name
                con fthis :: Name
                con thisT :: Type
                con user :: Name
                con preferred :: Name
                con r :: {Type}
                constraint [this] ~ r
                constraint [fthis] ~ [user]
                constraint [fthis, user] ~ [preferred]
                val inj_this : sql_injectable thisT
                table bid : {fthis : thisT, user : string, preferred : bool}
                val title : string

                val label : string
                val whoami : transaction (option string)
                val response : option string (* Optionally, say this in a popup after user makes a positive selection. *)
            end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT] ++ M.r) cfg internal
end

(* Now an admin can use those bids to assign users to items. *)
functor AssignFromBids(M : sig
                           type inp
                           con this :: Name
                           con assignee :: Name
                           con fthis :: Name
                           con thisT :: Type
                           con user :: Name
                           con preferred :: Name
                           con r :: {Type}
                           constraint [this] ~ [assignee]
                           constraint [this, assignee] ~ r
                           constraint [fthis] ~ [user]
                           constraint [fthis, user] ~ [preferred]
                           val inj_this : sql_injectable thisT
                           table bid : {fthis : thisT, user : string, preferred : bool}
                           val bidTitle : string

                           val label : string
                           val whoami : transaction (option string)

                           table tab : ([this = thisT, assignee = option string] ++ r)
                           val tabTitle : string
                       end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT, M.assignee = option string] ++ M.r) cfg internal
end

(* Variant: we have assigned some users, who gave their preferences for another aspect (e.g. times).
 * Now assign a value for that aspect, based on preferences of assigned users. *)
functor AssignFromBids2(M : sig
                            type inp
                            con fthat :: Name
                            con thatT :: Type
                            con user :: Name
                            con preferred :: Name
                            constraint [fthat] ~ [user]
                            constraint [fthat, user] ~ [preferred]
                            table bid : {fthat : thatT, user : string, preferred : bool}

                            con this :: Name
                            con thisT :: Type
                            con that :: Name
                            con assignees :: {Unit}
                            con r :: {Type}
                            constraint [this] ~ [that]
                            constraint [this, that] ~ assignees
                            constraint [this, that] ~ r
                            constraint assignees ~ r
                            table tab : ([this = thisT, that = option thatT] ++ mapU (option string) assignees ++ r)

                            val fl : folder assignees
                            val show_that : show thatT
                            val read_that : read thatT
                            val eq_that : eq thatT
                            val inj_that : sql_injectable_prim thatT
                            val inj_this : sql_injectable thisT

                            val label : string
                            val whoami : transaction (option string)
                        end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT, M.that = option M.thatT] ++ mapU (option string) M.assignees ++ M.r) cfg internal
end

type nonnull_cfg
type nonnull_st
val nonnull : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
              => t inp ([col = option ct] ++ r) nonnull_cfg nonnull_st
type isnull_cfg
type isnull_st
val isnull : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
              => t inp ([col = option ct] ++ r) isnull_cfg isnull_st

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

type taggedWithUser_cfg
type taggedWithUser_st
val taggedWithUser : inp ::: Type -> user :: Name -> r ::: {Type} -> [[user] ~ r]
                   => transaction (option string) (* get username, if any *)
                   -> t inp ([user = string] ++ r) taggedWithUser_cfg taggedWithUser_st

type taggedWithUserOpt_cfg
type taggedWithUserOpt_st
val taggedWithUserOpt : inp ::: Type -> user :: Name -> r ::: {Type} -> [[user] ~ r]
                        => transaction (option string) (* get username, if any *)
                        -> t inp ([user = option string] ++ r) taggedWithUser_cfg taggedWithUser_st

type linkedToUser_cfg
type linkedToUser_st
val linkedToUser : inp ::: Type -> key :: Name -> keyT ::: Type -> r ::: {Type} -> [[key] ~ r]
                   => ckey :: Name -> user :: Name -> cr ::: {Type} -> ks ::: {{Unit}} -> [[ckey] ~ [user]] => [[ckey, user] ~ cr]
                   => sql_table ([ckey = keyT, user = string] ++ cr) ks (* connector that must link current user to row *)
                   -> transaction (option string) (* get username, if any *)
                   -> string (* table title *)
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

type owner_cfg
type owner_st
val owner : inp ::: Type -> owner :: Name -> r ::: {Type} -> [[owner] ~ r]
            => transaction (option string) (* get user *)
            -> string
            -> t inp ([owner = option string] ++ r) owner_cfg owner_st

type sortby_cfg
type sortby_st
val sortby : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
             => t inp ([col = ct] ++ r) sortby_cfg sortby_st
val sortbyDesc : inp ::: Type -> col :: Name -> ct ::: Type -> r ::: {Type} -> [[col] ~ r]
                 => t inp ([col = ct] ++ r) sortby_cfg sortby_st

type isTrue_cfg
type isTrue_st
val isTrue : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
             => t inp ([col = bool] ++ r) isTrue_cfg isTrue_st

type isTrueOpt_cfg
type isTrueOpt_st
val isTrueOpt : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
                => t inp ([col = option bool] ++ r) isTrueOpt_cfg isTrueOpt_st

type isNotTrueOpt_cfg
type isNotTrueOpt_st
val isNotTrueOpt : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
                   => t inp ([col = option bool] ++ r) isNotTrueOpt_cfg isNotTrueOpt_st

functor Moderate(M : sig
                     type inp
                     con key :: Name
                     type keyT
                     con hide :: Name
                     con r :: {Type}
                     constraint [key] ~ [hide]
                     constraint [key, hide] ~ r
                     val show_key : show keyT
                     val inj_key : sql_injectable keyT

                     table tab : ([key = keyT, hide = option bool] ++ r)
                     val title : string

                     val authorized : transaction bool
                     val label : string
                 end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.key = M.keyT, M.hide = option bool] ++ M.r) cfg internal
end

functor Upvote(M : sig
                type inp
                con this :: Name
                con fthis :: Name
                con thisT :: Type
                con user :: Name
                con r :: {Type}
                constraint [this] ~ r
                constraint [fthis] ~ [user]
                val inj_this : sql_injectable thisT
                table vote : {fthis : thisT, user : string}
                val title : string

                val label : string
                val whoami : transaction (option string)
            end) : sig
    type cfg
    type internal
    val t : t M.inp ([M.this = M.thisT] ++ M.r) cfg internal
end

functor Make(M : sig
                 con r :: {(Type * Type * Type)}
                 table tab : (map fst3 r)
                 val title : string

                 type cfg
                 type st
                 val t : t unit (map fst3 r) cfg st
                 val widgets : $(map Widget.t' r)
                 val fl : folder r
                 val labels : $(map (fn _ => string) r)
                 val injs : $(map (fn p => sql_injectable p.1) r)

                 val authorized : transaction bool
                 val allowCreate : bool
                 (* Note: creation is streamlined and
                  * omits showing widgets that are marked optional. *)
             end) : Ui.S0

(* This version expects an explicit input. *)
functor Make1(M : sig
                  type inp
                  con key :: Name
                  type keyT
                  type key2
                  type key3
                  con r :: {(Type * Type * Type)}
                  constraint [key] ~ r
                  con keyName :: Name
                  con otherKeys :: {{Unit}}
                  constraint [keyName] ~ otherKeys
                  val tab : sql_table ([key = keyT] ++ map fst3 r) ([keyName = [key]] ++ otherKeys)
                  val title : string

                  type cfg
                  type st
                  val t : t inp ([key = keyT] ++ map fst3 r) cfg st
                  val widgets : $(map Widget.t' ([key = (keyT, key2, key3)] ++ r))
                  val fl : folder ([key = (keyT, key2, key3)] ++ r)
                  val labels : $(map (fn _ => string) ([key = (keyT, key2, key3)] ++ r))
                  val injs : $(map (fn p => sql_injectable p.1) ([key = (keyT, key2, key3)] ++ r))

                  val authorized : transaction bool
                  val allowCreate : bool
                  val notifyWhenEmpty : bool
                  val notifyWhenNonempty : bool

                  con buttons :: {Unit}
                  val buttonsFl : folder buttons
              end) : Ui.S where type input = M.inp
                                             * $(mapU (unit -> {M.key : M.keyT} -> string (* label *) * url) M.buttons)
