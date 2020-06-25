(* Everything will compile if you leave the settings below blank, but related operations will fail at runtime. *)

(* Default admin account *)
val admin_name = "Adam Chlipala"
val admin_email = "adam.chlipala@gmail.com"

(* Zoom *)
(* Register an "account-level JWT app" with Zoom and enter its details here. *)
val api_key = ""
val api_secret = ""

(* Slack *)
(* Set up an app/bot and put its authentication token here. *)
val token = ""

(* Google *)
(* Register a Google API OAuth app and put its details here. *)
val client_id = ""
val client_secret = ""

(* Talk scheduling *)
val start = readError "2020-3-27 14:00:00"
