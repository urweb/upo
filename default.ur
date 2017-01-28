open Bootstrap3

con r = _
val fl = _

val css =
    {Bootstrap = bless "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
     Upo = bless "/style.css"}

val navclasses = CLASS "navbar navbar-inverse navbar-fixed-top"
val icon = None
fun wrap b = b
