fun importTable [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
                (tab : sql_table fs cs) (input : string) =
    let
        val addOne = @Sql.easy_insert injs fl tab

        fun doLine line =
            let
                val (commas, total, line, acc) =
                    @foldR [read] [fn r => string -> int * int * string * $r]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) acc line =>
                         case String.split line #"," of
                             None =>
                             let
                                 val (commas, total, line, r) = acc line
                             in
                                 (commas, total+1, "", {nm = readError line} ++ r)
                             end
                           | Some (token, line) =>
                             let
                                 val (commas, total, line, r) = acc line
                             in
                                 (commas+1, total+1, line, {nm = readError token} ++ r)
                             end)
                     (fn line => (0, 0, line, {})) fl reads line
            in
                if commas <> total-1 || line <> "" then
                    error <xml>Wrong number of commas in CSV input ({[commas]}, {[total]}, {[line]})</xml>
                else
                    addOne acc
            end

        fun loop input =
            case String.split input #"\n" of
                None =>
                (case input of
                     "" => return ()
                   | _ => doLine input)
              | Some (line, input) =>
                doLine line;
                loop input
    in
        loop input
    end
