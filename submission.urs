(* Basic abstraction of submitting files associated with some database key *)

functor Make(M : sig
                 (* Table that provides the key we submit to *)
                 con key1 :: Name
                 con keyT :: Type
                 con keyR :: {Type}
                 constraint [key1] ~ keyR
                 con key = [key1 = keyT] ++ keyR
                 constraint [Filename, Content, MimeType, When, Channel] ~ key
                 con other :: {Type}
                 constraint other ~ key
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint [keyName] ~ otherConstraints
                 val tab : sql_table (key ++ other) ([keyName = map (fn _ => ()) key] ++ otherConstraints)
                 val keyInj : $(map sql_injectable_prim key)
                 val keyFl : folder key
                 val keyShow : show $key

                 (* Table of users *)
                 con ukey :: Name
                 con uother :: {Type}
                 con ukeyName :: Name
                 con uotherConstraints :: {{Unit}}
                 constraint [ukeyName] ~ uotherConstraints
                 constraint [ukey] ~ [Filename, Content, MimeType, When, Channel]
                 constraint [ukey] ~ uother
                 constraint [ukey] ~ key
                 val user : sql_table ([ukey = string] ++ uother) ([ukeyName = [ukey]] ++ uotherConstraints)
                 val whoami : transaction (option string)

                 (* Metadata to collect with submissions *)
                 con fs :: {(Type * Type * Type)}
                 constraint [ukey, Filename, Content, MimeType, When] ~ fs
                 constraint key ~ fs
                 val widgets : $(map Widget.t' fs)
                 val fl : folder fs
                 val injs : $(map sql_injectable (map fst3 fs))
                 val labels : $(map (fn _ => string) fs)

                 val makeFilename : $key -> string (* username *) -> string
                 val mayInspect : option string (* username, or [None] for all users *) -> transaction bool
             end) : sig
    val newUpload : $M.key -> transaction xbody
    (* Form to upload a new submission for a key *)

    val latests : (string -> xbody) -> $M.key -> transaction xbody
    (* List latest upload for each user who has made at least one, with link to view it.
     * Passed a function to generate some extra content for each row (username). *)

    structure AllFiles : Ui.S where type input = {Key : $M.key, User : string}
    structure AllFilesAllUsers : Ui.S where type input = $M.key
    structure AllFilesAllKeys : Ui.S0

    con others :: {Type}
    con ukey = M.ukey
    constraint others ~ (M.key ++ [ukey = string, When = time] ++ map fst3 M.fs)
    table submission : (M.key ++ [ukey = string, When = time] ++ map fst3 M.fs ++ others)
end
