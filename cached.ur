open Bootstrap4

con r = _
val fl = _

val css =
    {Bootstrap = bless "/bootstrap.min.css",
     FontAwesome = bless "/font-awesome.min.css",
     Upo = bless "/style.css"}

val navclasses = CLASS "navbar navbar-inverse navbar-fixed-top"
val icon = None
fun wrap b = b
val titleInNavbar = True
