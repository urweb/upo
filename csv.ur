fun csvFold [fs] [acc] (f : $fs -> acc -> acc)
            (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) =
    let
        fun doLine line acc =
            let
                val (commas, total, line, acc') =
                    @foldR [read] [fn r => string -> int * int * string * $r]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) acc line =>
                         case String.split line #"," of
                             None =>
                             let
                                 val (commas, total, line, r) = acc line
                             in
                                 (commas, total+1, "", {nm = readError (String.trim line)} ++ r)
                             end
                           | Some (token, line) =>
                             let
                                 val (commas, total, line, r) = acc line
                             in
                                 (commas+1, total+1, line, {nm = readError (String.trim token)} ++ r)
                             end)
                     (fn line => (0, 0, line, {})) fl reads line
            in
                if commas <> total-1 || line <> "" then
                    error <xml>Wrong number of commas in CSV input ({[commas]}, {[total]}, {[line]})</xml>
                else
                    f acc' acc
            end

        fun loop input acc =
            case String.split input #"\n" of
                None =>
                (case input of
                     "" => acc
                   | _ => doLine input acc)
              | Some (line, input) =>
                loop input (doLine line acc)
    in
        loop
    end


fun parse [fs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
          (input : string) =
    @csvFold (fn r acc => r :: acc) injs reads fl input []

fun importTable [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
                (tab : sql_table fs cs) (input : string) =
    List.app (@Sql.easy_insert injs fl tab) (@parse injs reads fl input)
