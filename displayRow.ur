open Bootstrap4

functor Make(M : sig
                 con key :: {(Type * Type * Type)}
                 con r :: {(Type * Type * Type)}
                 con keyName :: Name
                 con otherConstraints :: {{Unit}}
                 constraint key ~ r
                 constraint [keyName] ~ otherConstraints

                 val t : sql_table (map fst3 (key ++ r)) ([keyName = map (fn _ => ()) key] ++ otherConstraints)

                 val labels : $(map (fn _ => string) (key ++ r))
                 val ws : $(map Widget.t' (key ++ r))
                 val kfl : folder key
                 val rfl : folder r
                 val injs : $(map sql_injectable (map fst3 key))
             end) = struct
    open M
    type input = $(map fst3 key)
    type a = $(map fst3 (key ++ r))

    fun create k =
        r <- oneRow1 (SELECT t.{{map fst3 r}}
                      FROM t
                      WHERE {@@Sql.easy_where [#T] [map fst3 key] [map fst3 r] [[]] [_] [[]] ! ! injs (@Folder.mp kfl) k});
        return (k ++ r)

    fun onload _ = return ()

    fun render _ r = <xml>
      <table class="bs-table">
        {@mapX3 [Widget.t'] [fn _ => string] [fst3] [_]
          (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                       (w : Widget.t' p) (label : string) (v : p.1) =>
              <xml><tr>
                <th>{[label]}</th>
                <td>{@Widget.asValue w v}</td>
              </tr></xml>)
          (@Folder.concat ! kfl rfl) ws labels r}
      </table>
    </xml>

    fun notification _ _ = <xml></xml>

    fun ui k = {
        Create = create k,
        Onload = onload,
        Render = render,
        Notification = notification
    }
end
