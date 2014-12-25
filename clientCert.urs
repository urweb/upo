(* Authenticate users via SSL client certificates, after a server like Apache has set the right environment variable *)

val user : transaction {Email : string,
                        CommonName : string}
