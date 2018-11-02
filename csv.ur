fun csvFold [fs] [acc] (f : $fs -> acc -> acc)
            (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) =
    let
        fun doLine line acc =
            let
                fun readStringLiteral line acc =
                    case String.split line #"\"" of
                        None => error <xml>Quoted CSV field value is missing a closing double quote.</xml>
                      | Some (chars, line') =>
                        if String.length line' > 0 && String.sub line' 0 = #"\"" then
                            readStringLiteral (String.suffix line' 1) (acc ^ "\"")
                        else
                            (acc ^ chars, line')
                
                fun fields line justReadQuoted acc =
                    case String.msplit {Haystack = line, Needle = ",\""} of
                        None =>
                        if line = "" then
                            acc
                        else
                            line :: acc
                      | Some (field, #",", line') =>
                        let
                            val acc =
                                if justReadQuoted then
                                    if String.all Char.isSpace field then
                                        acc
                                    else
                                        error <xml>There are extra characters after a quoted CSV field value.</xml>
                                else
                                    field :: acc
                        in
                            fields line' False acc
                        end
                      | Some (betterBeWhitespace, #"\"", line') =>
                        if not (String.all Char.isSpace betterBeWhitespace) then
                            error <xml>CSV file contains other nonspace characters before first double quote of a field value.</xml>
                        else
                            let
                                val (lit, line') = readStringLiteral line' ""
                            in
                                fields line' True (lit :: acc)
                            end
                      | Some _ => error <xml>CSV: impossible return from <tt>String.msplit</tt>!</xml>

                val fields = fields line False []

                val (fields', acc') =
                    @foldR [read] [fn r => list string * $r]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t)
                                  (fields, r) =>
                         case fields of
                             [] => error <xml>Not enough fields in CSV line</xml>
                           | token :: fields' =>
                             (fields', {nm = readError (String.trim token)} ++ r))
                     (fields, {}) fl reads
            in
                if List.length fields' <> 0 then
                    error <xml>Too many commas in CSV input ({[List.length fields']} unmatched) ({[fields]})</xml>
                else
                    f acc' acc
            end

        fun loop (header : int) input acc =
            case String.split input #"\n" of
                None =>
                (case input of
                     "" => acc
                   | _ => if header = 0
                          then doLine input acc
                          else acc)
              | Some (line, input) =>
                if header = 0
                then loop 0 input (doLine line acc)
                else loop (header-1) input acc
    in
        loop
    end


fun parse [fs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
          (header : int) (input : string) =
    @csvFold (fn r acc => r :: acc) injs reads fl header input []

fun importTable [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
                (tab : sql_table fs cs) (header : int) (input : string) =
    List.app (@Sql.easy_insert injs fl tab) (@parse injs reads fl header input)

open Bootstrap4

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
                end) = struct

    open M

    type a = source string

    val create = source ""

    fun onload _ = return ()

    fun import s =
        ma <- mayAccess;
        if not ma then
            error <xml>Access denied</xml>
        else
           @importTable injs reads fl tab skipHeaderLines s
                 
    fun render _ s = <xml>
      <p>Please copy and paste the CSV data here, with each line in the format:
        {case @foldR [fn _ => string] [fn _ => option xbody]
               (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (label : string) (ob : option xbody) =>
                   Some (case ob of
                             None => <xml><i>{[label]}</i></xml>
                           | Some ob => <xml><i>{[label]}</i>, {ob}</xml>))
               None fl labels of
             None => <xml></xml>
           | Some ob => ob}</p>


        <ctextarea source={s} cols={20} class="form-control"/>
        
        <button value="Import"
                class="btn btn-primary"
                onclick={fn _ =>
                            csv <- get s;
                            rpc (import csv);
                            set s ""}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end
     
functor Generate1(M : sig
                      con fs :: {Type}
                      con tab :: Name
                      val query : sql_query [] [] [tab = fs] []
                      val fl : folder fs
                      val shows : $(map show fs)
                      val labels : $(map (fn _ => string) fs)

                      val mayAccess : transaction bool
                      val filename : string
                  end) = struct

    open M

    type a = unit

    fun escape' s =
        case s of
            "" => ""
          | _ =>
            let
                val ch = String.sub s 0
                val rest = String.suffix s 1
            in
                case ch of
                    #"\"" => "\"\"" ^ escape' rest
                  | _ => String.str ch ^ escape' rest
            end

    fun escape s =
        if String.all (fn ch => ch <> #"," && ch <> #"\"" && ch <> #"\n" && ch <> #"\r") s then
            s
        else
            if String.all (fn ch => ch <> #"\"") s then
                "\"" ^ s ^ "\""
            else
                "\"" ^ escape' s ^ "\""

    val build =
        Basis.query query (fn r acc =>
                              return
                                  (acc ^ @foldR2 [show] [ident] [fn _ => string]
                                          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) acc =>
                                              case acc of
                                                  "" => escape (show x)
                                                | _ => escape (show x) ^ "," ^ acc)
                                          "" fl shows r.tab ^ "\n"))
        (@foldR [fn _ => string] [fn _ => string]
          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] s acc =>
              case acc of
                  "" => escape s
                | _ => escape s ^ "," ^ acc)
          "" fl labels ^ "\n")

    fun generate () : transaction page =
        ma <- mayAccess;
        if not ma then
            error <xml>Access denied</xml>
        else
            csv <- build;
            setHeader (blessResponseHeader "Content-Disposition")
                      ("attachment; filename=" ^ filename);
            returnBlob (textBlob csv) (blessMime "text/csv")

    val create = return ()

    fun onload () = return ()

    fun render _ () = <xml>
      <form>
        <submit value="Generate" class="btn btn-primary" action={generate}/>
      </form>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end
