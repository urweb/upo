type t = {Multi : bool,
          Options : xml [Cselect, Body] [] [],
          Selected : source (list string)}

fun create options =
    s <- source [];
    return {Multi = True, Options = options, Selected = s}

fun createSingle options =
    s <- source [];
    return {Multi = False, Options = options, Selected = s}

fun render self = <xml>
  <active code={id <- fresh;
                return <xml>
                  <cselect id={id} multiple={self.Multi}>
                    {self.Options}
                  </cselect>
                  <active code={Select2Ffi.replace id (set self.Selected);
                                return <xml></xml>}/>
                </xml>}/>
</xml>

fun selected self = signal self.Selected
