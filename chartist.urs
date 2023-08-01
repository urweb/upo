datatype graph =
         Bar of list string (* labels *) * list (list float) (* series *)

val graph : graph -> xbody
