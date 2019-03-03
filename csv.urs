(* Support for parsing comma-separated text files *)

val importTable : fs ::: {Type} -> cs ::: {{Unit}}
                  -> $(map sql_injectable fs) -> $(map read fs) -> folder fs
                  -> sql_table fs cs
                  -> int (* the number of header lines to skip *)
                  -> string
                  -> transaction unit

val parse : fs ::: {Type}
            -> $(map sql_injectable fs) -> $(map read fs) -> folder fs
            -> int (* the number of header lines to skip *)
            -> string
            -> list $fs

val importTableWithHeader : fs ::: {Type} -> fsC ::: {Type} -> cs ::: {{Unit}}
                            -> [fs ~ fsC]
                            => $(map sql_injectable (fs ++ fsC)) -> $(map read fs)
                            -> folder fs -> folder fsC
                            -> $(map (fn _ => string) fs)
                               (* for each column, the name of its header in the CSV file *)
                            -> $fsC
                            -> sql_table (fs ++ fsC) cs
                            -> string
                            -> transaction unit

val splitLine : string -> list string
(* A handy exported helper: split a single line into fields at commas (taking quoting into account). *)

val nextLine : string -> option (string * string)
(* Split at first line of a CSV text, if there is a first line. *)

(* Let's expose all that as a UI. *)

functor Import1(M : sig
                    con fs :: {Type}
                    con cs :: {{Unit}}
                    val tab : sql_table fs cs

                    val injs : $(map sql_injectable fs)
                    val reads : $(map read fs)
                    val fl : folder fs
                    val labels : $(map (fn _ => string) fs)

                    val skipHeaderLines : int
                    val mayAccess : transaction bool
                end) : Ui.S0

functor ImportWithHeader1(M : sig
                              con fs :: {Type}
                              con fsC :: {Type}
                              constraint fs ~ fsC
                              con cs :: {{Unit}}
                              val tab : sql_table (fs ++ fsC) cs

                              val injs : $(map sql_injectable (fs ++ fsC))
                              val reads : $(map read fs)
                              val fl : folder fs
                              val flC : folder fsC
                              val headers : $(map (fn _ => string) fs)
                              (* This record says which CSV-file header corresponds to each field.
                               * Note that there may be fields of the CSV file that are not used to import data. *)
                              val constants : $fsC

                              val mayAccess : transaction bool
                          end) : Ui.S0

(* And we can also generate CSV data: *)
functor Generate1(M : sig
                      con fs :: {Type}
                      con tab :: Name
                      val query : sql_query [] [] [tab = fs] []
                      val fl : folder fs
                      val shows : $(map show fs)
                      val labels : $(map (fn _ => string) fs)

                      val mayAccess : transaction bool
                      val filename : string (* Tell browsers this is the name of the file being downloaded. *)
                  end) : Ui.S0
