(* An extensible calendar: different program modules may contribute different sorts of events *)

(* Generators of calendar entries *)
con t :: {Type}    (* Dictionary of all key fields used across all sources of events *)
         -> {Type} (* Mapping user-meaningful tags (for event kinds) to associated data *)
         -> Type

val create : tag :: Name
             -> key ::: {Type}
             -> [[When] ~ key]
             => folder key
             -> (otherKeys :: {Type}
                 -> [([When = time] ++ key) ~ otherKeys]
                 => folder otherKeys
                 -> $(map sql_injectable_prim otherKeys)
                 -> sql_query1 [] [] [] [] ([When = time] ++ map option (key ++ otherKeys)))
             -> t key [tag = $([When = time] ++ key)]

val fromTable : tag :: Name
                -> key :: {Type}
                -> when :: Name
                -> other ::: {Type}
                -> us ::: {{Unit}}
                -> [key ~ other] => [[when] ~ (key ++ other)] => [[When] ~ (key ++ other)]
                => folder key -> $(map sql_injectable_prim key)
                -> sql_table (key ++ other ++ [when = time]) us
                -> t key [tag = $([When = time] ++ key)]

val compose : keys1 ::: {Type} -> keys2 ::: {Type}
              -> tags1 ::: {Type} -> tags2 ::: {Type}
              -> [keys1 ~ keys2] => [tags1 ~ tags2]
              => folder keys1 -> folder keys2
              -> $(map sql_injectable_prim keys1)
              -> $(map sql_injectable_prim keys2)
              -> t keys1 tags1 -> t keys2 tags2 -> t (keys1 ++ keys2) (tags1 ++ tags2)

val items : keys ::: {Type} -> tags ::: {Type}
            -> [[When] ~ keys]
            => t keys tags
            -> transaction (list (variant tags))

con calendar :: {Type} -> Type
val calendar : keys ::: {Type} -> tags ::: {Type}
               -> [[When] ~ keys]
               => t keys tags
               -> $(map show tags)
               -> folder tags
               -> {FromDay : time,
                   ToDay : time} (* inclusive *)
               -> Ui.t (calendar tags)
