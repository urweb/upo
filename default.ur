open Bootstrap4

con r = _
val fl = _

val css =
    {Bootstrap = bless "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css",
     FontAwesome = bless "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
     Select2 = bless "https://cdn.jsdelivr.net/npm/select2@4.0.13/dist/css/select2.min.css",
     Fullcalendar = bless "https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/4.2.0/core/main.min.css",
     FullcalendarDaygrid = bless "https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/4.2.0/daygrid/main.min.css",
     FullcalendarTimegrid = bless "https://cdnjs.cloudflare.com/ajax/libs/fullcalendar/4.2.0/timegrid/main.min.css",
     Upo = bless "/style.css"}

val navclasses = CLASS "navbar navbar-expand-md navbar-dark fixed-top bg-dark"
val icon = None
fun wrap b = b
val titleInNavbar = True
