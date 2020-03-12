fun splitLine sep line =
    let
        fun readStringLiteral line acc =
            case String.split line #"\"" of
                None => error <xml>Quoted CSV field value is missing a closing double quote.</xml>
              | Some (chars, line') =>
                if String.length line' > 0 && String.sub line' 0 = #"\"" then
                    readStringLiteral (String.suffix line' 1) (acc ^ chars ^ "\"")
                else
                    (acc ^ chars, line')
    
        fun fields line justReadSeparator justReadQuoted acc =
            case String.msplit {Haystack = line, Needle = String.str sep ^ "\""} of
                None =>
                if String.all Char.isSpace line && not justReadSeparator then
                    acc
                else
                    line :: acc
              | Some (field, sep', line') =>
                if sep' = sep then
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
                        fields line' True False acc
                    end
                else
                    if not (String.all Char.isSpace field) then
                        error <xml>CSV file contains other nonspace characters ("{[field]}") before first double quote of a field value.</xml>
                    else
                        let
                            val (lit, line') = readStringLiteral line' ""
                        in
                            fields line' False True (lit :: acc)
                        end
              | Some _ => error <xml>CSV: impossible return from <tt>String.msplit</tt>!</xml>
    in
        fields line False False []
    end

fun nextLine lines =
    let
        fun findIt pos lines =
            case lines of
                "" => None
              | _ =>
                if String.sub lines 0 = #"\n" then
                    Some pos
                else if String.sub lines 0 = #"\"" then
                    skipQuoted (pos+1) (String.suffix lines 1)
                else
                    findIt (pos+1) (String.suffix lines 1)

        and skipQuoted pos lines =
            case lines of
                "" => None
              | _ =>
                if String.sub lines 0 = #"\"" then
                    let
                        val lines = String.suffix lines 1
                    in
                        case lines of
                            "" => None
                          | _ =>
                            if String.sub lines 0 = #"\"" then
                                skipQuoted (pos+2) (String.suffix lines 1)
                            else
                                findIt (pos+1) lines
                    end
                else
                    skipQuoted (pos+1) (String.suffix lines 1)
    in
        case findIt 0 lines of
            None =>
            if String.all Char.isSpace lines then
                None
            else
                Some (lines, "")
          | Some pos =>
            Some (String.substring lines {Start = 0, Len = pos},
                  String.suffix lines (pos+1))
    end
    
fun csvFold [m] (_ : monad m) [acc] (processHeaderLine : string -> acc -> m acc)
            (processRow : list string -> acc -> m acc) sep =
    let
        fun loop (header : int) input acc =
            case nextLine input of
                None => return acc
              | Some (line, input) =>
                if header = 0
                then acc <- processRow (splitLine sep line) acc; loop 0 input acc
                else acc <- processHeaderLine line acc; loop (header-1) input acc
    in
        loop
    end

fun parseLine_simple [fs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) (fields : list string) : $fs =
    let
        val (fields', acc) =
            @foldR [read] [fn r => list string * $r]
             (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t) (fields', r) =>
                 case fields' of
                     [] => error <xml>Not enough fields in CSV line ({[fields]})</xml>
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
          sep (header : int) (input : string) =
    IdentityMonad.run (@csvFold _
                        (fn _ acc => return acc)
                        (fn fs acc => return (@parseLine_simple injs reads fl fs :: acc)) sep header input [])

fun importTable [fs] [cs] (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) sep
                (tab : sql_table fs cs) (header : int) (input : string) =
    @csvFold _
     (fn _ () => return ())
     (fn fs () => @Sql.easy_insert injs fl tab (@parseLine_simple injs reads fl fs)) sep header input ()

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
    
fun importTableWithHeader [fs] [fsC] [cs] [fs ~ fsC]
    (injs : $(map sql_injectable (fs ++ fsC))) (reads : $(map read fs)) (fl : folder fs) (flC : folder fsC)
    sep (headers : $(map (fn _ => string) fs)) (constants : $fsC)
    (tab : sql_table (fs ++ fsC) cs) (input : string) =
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

                           val positions = List.foldli processHeader
                                                       (@map0 [fn _ => option int]
                                                         (fn [t::_] => None) fl)
                                                       (List.rev (splitLine sep line))
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
                           (fs : list string) <- return (List.rev fs);
                           @@Sql.easy_insert [fs ++ fsC] [_] injs (@Folder.concat ! fl flC) tab
                            (@map2 [read] [fn _ => int] [ident]
                              (fn [t] (_ : read t) (posn : int) =>
                                  case List.nth fs posn of
                                      None => error <xml>CSV line is too short.</xml>
                                    | Some token => case read token of
                                                        None => error <xml>Malformed CSV token "{[token]}"</xml>
                                                      | Some v => v)
                              fl reads hs ++ constants);
                           return hso)
                    sep 1 input None)

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

                    val separator : char

                    type refreshed
                    val refreshed : Ui.t refreshed
                end) = struct

    open M

    datatype uploadStatus =
             AtRest
           | Uploading
           | UploadFailed
           | Uploaded
         
    type a = {UploadStatus : source uploadStatus,
              PasteHere : source string,
              Subwidget : source refreshed}

    val create =
        us <- source AtRest;
        ph <- source "";
        sub <- refreshed.Create;
        sub <- source sub;
        return {UploadStatus = us,
                PasteHere = ph,
                Subwidget = sub}

    fun onload a = sub <- get a.Subwidget; refreshed.Onload sub

    fun import s =
        ma <- mayAccess;
        if not ma then
            error <xml>Access denied</xml>
        else
           @importTable injs reads fl separator tab skipHeaderLines s

    fun claimUpload h =
        res <- AjaxUpload.claim h;
        case res of
            AjaxUpload.NotFound => error <xml>Upload not found.</xml>
          | AjaxUpload.Found r =>
            case textOfBlob r.Content of
                None => error <xml>Uploaded file is not text.</xml>
              | Some s => import s
                   
    val subwidget = refreshed.Create

    fun refresh a =
        sub <- rpc subwidget;
        set a.Subwidget sub;
        refreshed.Onload sub

    fun render ctx a = <xml>
      Upload CSV file:
      <active code={AjaxUpload.render {SubmitLabel = Some "Submit",
                                       OnBegin = set a.UploadStatus Uploading,
                                       OnSuccess = fn h =>
                                                      rpc (claimUpload h);
                                                      refresh a;
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
                            refresh a;
                            set a.PasteHere ""}/>

      <hr/>

      <dyn signal={sub <- signal a.Subwidget;
                   return (refreshed.Render ctx sub)}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}

end

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
                              val constants : $fsC

                              val mayAccess : transaction bool

                              val separator : char

                              type refreshed
                              val refreshed : Ui.t refreshed

                              val label : option string
                              val showTextarea : bool
                          end) = struct

    open M

    datatype uploadStatus =
             AtRest
           | Uploading
           | UploadFailed
           | Uploaded
         
    type a = {UploadStatus : source uploadStatus,
              PasteHere : source string,
              Subwidget : source refreshed}

    val create =
        us <- source AtRest;
        ph <- source "";
        sub <- refreshed.Create;
        sub <- source sub;
        return {UploadStatus = us,
                PasteHere = ph,
                Subwidget = sub}

    fun onload a = sub <- get a.Subwidget; refreshed.Onload sub

    fun import s =
        ma <- mayAccess;
        if not ma then
            error <xml>Access denied</xml>
        else
           @importTableWithHeader ! injs reads fl flC separator headers constants tab s

    fun claimUpload h =
        res <- AjaxUpload.claim h;
        case res of
            AjaxUpload.NotFound => error <xml>Upload not found.</xml>
          | AjaxUpload.Found r =>
            case textOfBlob r.Content of
                None => error <xml>Uploaded file is not text.</xml>
              | Some s => import s
                   
    val subwidget = refreshed.Create

    fun refresh a =
        sub <- rpc subwidget;
        set a.Subwidget sub;
        refreshed.Onload sub

    fun render ctx a = <xml>
      Upload {[Option.get "CSV file" label]}:
      <active code={AjaxUpload.render {SubmitLabel = Some "Submit",
                                       OnBegin = set a.UploadStatus Uploading,
                                       OnSuccess = fn h =>
                                                      rpc (claimUpload h);
                                                      refresh a;
                                                      set a.UploadStatus Uploaded,
                                       OnError = set a.UploadStatus UploadFailed}}/><br/>
      <dyn signal={us <- signal a.UploadStatus;
                   return (case us of
                               AtRest => <xml></xml>
                             | Uploading => <xml>Uploading...</xml>
                             | Uploaded => <xml>Import complete.</xml>
                             | UploadFailed => <xml>Import failed!</xml>)}/>
      <hr/>

      {if showTextarea then <xml>
        <p><i>Or</i> copy and paste the CSV data here, with a header line that includes at least the following field names:
          {case @foldR [fn _ => string] [fn _ => option xbody]
                 (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (label : string) (ob : option xbody) =>
                     Some (case ob of
                               None => <xml><i>{[label]}</i></xml>
                             | Some ob => <xml><i>{[label]}</i>, {ob}</xml>))
                 None fl headers of
               None => <xml></xml>
             | Some ob => ob}</p>

          <ctextarea source={a.PasteHere} cols={20} class="form-control"/>

          <button value="Import"
                  class="btn btn-primary"
                  onclick={fn _ =>
                              csv <- get a.PasteHere;
                              rpc (import csv);
                              refresh a;                            
                              set a.PasteHere ""}/>

        <hr/>
      </xml> else <xml></xml>}
        
      <dyn signal={sub <- signal a.Subwidget;
                   return (refreshed.Render ctx sub)}/>
    </xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render}
end

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

fun escape sep s =
    if String.all (fn ch => ch <> sep && ch <> #"\"" && ch <> #"\n" && ch <> #"\r") s then
        s
    else
        if String.all (fn ch => ch <> #"\"") s then
            "\"" ^ s ^ "\""
        else
            "\"" ^ escape' s ^ "\""

fun build [fs] [tab] (fl : folder fs) sep (shows : $(map show fs)) (labels : $(map (fn _ => string) fs)) (q : sql_query [] [] [tab = fs] []) =
    query q (fn r acc =>
                return
                    (acc ^ @foldR2 [show] [ident] [fn _ => string]
                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) acc =>
                                case acc of
                                    "" => escape sep (show x)
                                  | _ => escape sep (show x) ^ String.str sep ^ acc)
                            "" fl shows r.tab ^ "\n"))
          (@foldR [fn _ => string] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] s acc =>
                case acc of
                    "" => escape sep s
                  | _ => escape sep s ^ String.str sep ^ acc)
            "" fl labels ^ "\n")

fun buildComputed [fs] (fl : folder fs) sep (shows : $(map show fs)) (labels : $(map (fn _ => string) fs)) (q : sql_query [] [] [] fs) =
    query q (fn r acc =>
                return
                    (acc ^ @foldR2 [show] [ident] [fn _ => string]
                            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : show t) (x : t) acc =>
                                case acc of
                                    "" => escape sep (show x)
                                  | _ => escape sep (show x) ^ "," ^ acc)
                            "" fl shows r ^ "\n"))
          (@foldR [fn _ => string] [fn _ => string]
            (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] s acc =>
                case acc of
                    "" => escape sep s
                  | _ => escape sep s ^ "," ^ acc)
            "" fl labels ^ "\n")

functor Generate1(M : sig
                      con fs :: {Type}
                      con tab :: Name
                      val query : sql_query [] [] [tab = fs] []
                      val fl : folder fs
                      val shows : $(map show fs)
                      val labels : $(map (fn _ => string) fs)

                      val mayAccess : transaction bool
                      val filename : string

                      val separator : char
                  end) = struct

    open M

    type a = unit

    val build = @build fl separator shows labels query

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
