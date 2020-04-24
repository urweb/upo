type richtext
val show_richtext : show richtext
val read_richtext : read richtext
val eq_richtext : eq richtext
val ord_richtext : ord richtext
val inj_richtext : sql_injectable_prim richtext
val widget_richtext : Widget.t richtext Widget.htmlbox Widget.htmlbox_config

val explorer_richtext : full ::: {Type}
                        -> tname :: Name -> key ::: Type -> col :: Name
                        -> cols ::: {Type} -> colsDone ::: {Type} -> cstrs ::: {{Unit}}
                        -> impl1 ::: Type -> impl2 ::: Type -> impl3 ::: Type -> old ::: {(Type * {Type} * {Type} * {{Unit}} * Type * Type * Type)}
                        -> [[col] ~ cols] => [[col] ~ colsDone] => [[tname] ~ old]
                        => string
                           -> Explorer.t full ([tname = (key, [col = richtext] ++ cols, colsDone, cstrs, impl1, impl2, impl3)] ++ old)
                           -> Explorer.t full ([tname = (key, [col = richtext] ++ cols, [col = richtext] ++ colsDone, cstrs, Explorer.htmlbox1 impl1, Explorer.htmlbox2 impl2, Explorer.htmlbox3 impl3)] ++ old)

type richtextInBody_cfg
type richtextInBody_st
val richtextInBody : col :: Name -> r ::: {Type} -> [[col] ~ r]
                     => SmartList.t ([col = richtext] ++ r) richtextInBody_cfg richtextInBody_st

type richtext_cfg
type richtext_st
val richtext : inp ::: Type -> col :: Name -> r ::: {Type} -> [[col] ~ r]
               => string -> SmartTable.t inp ([col = richtext] ++ r) richtext_cfg richtext_st
