val user =
    s <- getenv (blessEnvVar "SSL_CLIENT_S_DN");
    case s of
        None => error <xml>No client certificate information found</xml>
      | Some s =>
        let
            val delimiter =
                case String.sindex {Haystack = s, Needle = ",emailAddress"} of
                    Some _ => #","
                  | None => case String.sindex {Haystack = s, Needle = ",CN"} of
                                Some _ => #","
                              | None => #"/"

            fun loop s email cname =
                case s of
                    "" =>
                    (case (email, cname) of
                         (Some email, Some cname) => {Email = email,
                                                      CommonName = cname}
                       | _ => error <xml>Client certificate missing either emailAddress or CN</xml>)
                  | _ =>
                    let
                        val (this, rest) =
                            case String.split s delimiter of
                                None => (s, "")
                              | Some p => p
                    in
                        case String.split this #"=" of
                            None => loop rest email cname
                          | Some (key, value) =>
                            let
                                val email = case key of
                                                "emailAddress" => Some value
                                              | _ => email

                                val cname = case key of
                                                "CN" => Some value
                                              | _ => cname
                            in
                                loop rest email cname
                            end
                    end
        in
            if naughtyDebug ("s = " ^ s) = 0 then
                return (loop s None None)
            else
                return (loop s None None)
        end
