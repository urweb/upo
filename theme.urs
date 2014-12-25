val page : transaction unit (* onload *)
           -> string (* title *)
           -> xbody (* body *)
           -> transaction page

(* Format a Bootstrap modal dialog *)
val makeModal : transaction unit (* Run if user clicks the main action button *)
                -> xbody (* title *)
                -> xbody (* body (widgets) *)
                -> string (* label on main action button *)
                -> xbody
