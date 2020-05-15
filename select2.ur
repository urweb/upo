type t = {Options : xml [Cselect, Body] [] [],
          Selected : source (list string)}

fun create options =
    s <- source [];
    return {Options = options, Selected = s}

fun render self = <xml>
  <active code={id <- fresh;
                return <xml>
                  <cselect id={id} multiple={True}>
                    {self.Options}
                  </cselect>
                  <active code={Select2Ffi.replace id (set self.Selected);
                                return <xml></xml>}/>
                </xml>}/>
</xml>

fun selected self = signal self.Selected
