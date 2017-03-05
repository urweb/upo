fun csvFold [fs] [acc] (f : $fs -> acc -> acc)
            (injs : $(map sql_injectable fs)) (reads : $(map read fs)) (fl : folder fs) =
    let
        fun doLine line acc =
            let
                fun fields (line, acc) =
                    case String.split line #"," of
                        None =>
                        if line = "" then
                            acc
                        else
                            line :: acc
                      | Some (field, line') => fields (line', field :: acc)

                val (fields, acc') =
                    @foldR [read] [fn r => list string * $r]
                     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (_ : read t)
                                  (fields, r) =>
                         case fields of
                             [] => error <xml>Not enough fields in CSV line</xml>
                           | token :: fields' =>
                             (fields', {nm = readError (String.trim token)} ++ r))
                     (fields (line, []), {}) fl reads
            in
                if List.length fields <> 0 then
                    error <xml>Too many commas in CSV input ({[List.length fields]} unmatched)</xml>
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

open Bootstrap3

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
                                                | _ => acc ^ "," ^ escape (show x))
                                          "" fl shows r.tab ^ "\n"))
        (@foldR [fn _ => string] [fn _ => string]
          (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] s acc =>
              case acc of
                  "" => escape s
                | _ => acc ^ "," ^ escape s)
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
