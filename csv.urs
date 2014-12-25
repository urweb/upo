(* Support for parsing comma-separated text files *)

val importTable : fs ::: {Type} -> cs ::: {{Unit}}
                  -> $(map sql_injectable fs) -> $(map read fs) -> folder fs
                  -> sql_table fs cs
                  -> string
                  -> transaction unit
