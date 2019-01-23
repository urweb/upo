fun csvFold [m] (_ : monad m) [acc] (processHeaderLine : string -> acc -> m acc)
            (processRow : list string -> acc -> m acc) =
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
            in
                processRow fields acc
            end

        fun loop (header : int) input acc =
            case String.split input #"\n" of
                None =>
                (case input of
                     "" => return acc
                   | _ => if header = 0
                          then doLine input acc
                          else processHeaderLine input acc)
              | Some (line, input) =>
                if header = 0
                then acc <- doLine line acc; loop 0 input acc
                else acc <- processHeaderLine line acc; loop (header-1) input acc
    in
        loop
    end

fun parseLine_simple [fs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) (fields : list string) : $fs =
    let
        val (fields', acc) =
            @foldR [read] [fn r => list string * $r]
             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) (fields, r) =>
                 case fields of
                     [] => error <xml>Not enough fields in CSV line</xml>
                   | token :: fields' =>
                     (fields', {nm = readError (String.trim token)} ++ r))
             (fields, {}) fl reads
    in
        if List.length fields' <> 0 then
            error <xml>Too many commas in CSV input ({[List.length fields']} unmatched) ({[fields]})</xml>
        else
            acc
    end
    
fun parse [fs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
          (header : int) (input : string) =
    IdentityMonad.run (@csvFold _
                        (fn _ acc => return acc)
                        (fn fs acc => return (@parseLine_simple injs reads fl fs :: acc)) header input [])

fun importTable [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
                (tab : sql_table fs cs) (header : int) (input : string) =
    @csvFold _
     (fn _ () => return ())
     (fn fs () => @Sql.easy_insert injs fl tab (@parseLine_simple injs reads fl fs)) header input ()

fun positionInList [a] (_ : eq a) (x : a) (ls : list a) : option int =
    case ls of
        [] => None
      | y :: ls' =>
        if x = y then
            Some 0
        else
            case positionInList x ls' of
                None => None
              | Some n => Some (n + 1)
    
fun importTableWithHeader [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs)
    (headers : $(map (fn _ => string) fs))
    (tab : sql_table fs cs) (input : string) =
    Monad.ignore (@csvFold _
                   (fn line _ =>
                       let
                           fun processHeader posn text positions =
                               @map2 [fn _ => string] [fn _ => option int] [fn _ => option int]
                                (fn [t] (text' : string) (pos : option int) =>
                                    if text' = text then
                                        Some posn
                                    else
                                        pos) fl headers positions

                           fun splitHeader posn line positions =
                               case String.split line #"," of
                                   None => processHeader posn line positions
                                 | Some (header, rest) => splitHeader (posn+1) rest (processHeader posn header positions)

                           val positions = splitHeader 0 line (@map0 [fn _ => option int]
                                                                (fn [t::_] => None) fl)
                           val positions = @map2 [fn _ => string] [fn _ => option int] [fn _ => int]
                                            (fn [t] (header : string) (posno : option int) =>
                                                case posno of
                                                    None => error <xml>Header {[header]} is missing from the input CSV file.</xml>
                                                  | Some posn => posn)
                                           fl headers positions
                       in
                           return (Some positions)
                       end)
                   (fn (fs : list string) (hso : option $(map (fn _ => int) fs)) =>
                       case hso of
                           None => error <xml>Somehow started parsing CSV data rows before parsing header.</xml>
                         | Some hs =>
                           @Sql.easy_insert injs fl tab (@map2 [read] [fn _ => int] [ident]
                                                          (fn [t] (_ : read t) (posn : int) =>
                                                              case List.nth fs posn of
                                                                  None => error <xml>CSV line is too short.</xml>
                                                                | Some token => case read token of
                                                                                    None => error <xml>Malformed CSV token "{[token]}"</xml>
                                                                                  | Some v => v)
                                                          fl reads hs);
                           return hso)
                    1 input None)

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

    datatype uploadStatus =
             AtRest
           | Uploading
           | UploadFailed
           | Uploaded
         
    type a = {UploadStatus : source uploadStatus,
              PasteHere : source string}

    val create =
        us <- source AtRest;
        ph <- source "";
        return {UploadStatus = us,
                PasteHere = ph}

    fun onload _ = return ()

    fun import s =
        ma <- mayAccess;
        if not ma then
            error <xml>Access denied</xml>
        else
           @importTable injs reads fl tab skipHeaderLines s

    fun claimUpload h =
        res <- AjaxUpload.claim h;
        case res of
            AjaxUpload.NotFound => error <xml>Upload not found.</xml>
          | AjaxUpload.Found r =>
            case textOfBlob r.Content of
                None => error <xml>Uploaded file is not text.</xml>
              | Some s => import s
                   
    fun render _ a = <xml>
      Upload CSV file:
      <active code={AjaxUpload.render {SubmitLabel = Some "Submit",
                                       OnBegin = set a.UploadStatus Uploading,
                                       OnSuccess = fn h =>
                                                      rpc (claimUpload h);
                                                      set a.UploadStatus Uploaded,
                                       OnError = set a.UploadStatus UploadFailed}}/><br/>
      <dyn signal={us <- signal a.UploadStatus;
                   return (case us of
                               AtRest => <xml></xml>
                             | Uploading => <xml>Uploading...</xml>
                             | Uploaded => <xml>Import complete.</xml>
                             | UploadFailed => <xml>Import failed!</xml>)}/>
      <hr/>
                                
      <p><i>Or</i> copy and paste the CSV data here, with each line in the format:
        {case @foldR [fn _ => string] [fn _ => option xbody]
               (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (label : string) (ob : option xbody) =>
                   Some (case ob of
                             None => <xml><i>{[label]}</i></xml>
                           | Some ob => <xml><i>{[label]}</i>, {ob}</xml>))
               None fl labels of
             None => <xml></xml>
           | Some ob => ob}</p>

        <ctextarea source={a.PasteHere} cols={20} class="form-control"/>
        
        <button value="Import"
                class="btn btn-primary"
                onclick={fn _ =>
                            csv <- get a.PasteHere;
                            rpc (import csv);
                            set a.PasteHere ""}/>
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
