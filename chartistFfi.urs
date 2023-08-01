datatype graph =
         Bar of list string (* labels *) * list (list float) (* series *)

val replace : id -> graph -> transaction unit
