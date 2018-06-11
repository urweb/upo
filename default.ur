open Bootstrap4

con r = _
val fl = _

val css =
    {Bootstrap = bless "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css",
     FontAwesome = bless "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
     Upo = bless "/style.css"}

val navclasses = CLASS "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
val icon = None
fun wrap b = b
val titleInNavbar = True
