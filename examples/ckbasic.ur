fun main () : transaction page =
    return <xml><body>
      <active code={ck <- Ckeditor.editor {Width = Ckeditor.DefaultSize,
                                           Height = Ckeditor.DefaultSize,
                                           ToolbarSet = Ckeditor.DefaultToolbarSet};
                    return (Ckeditor.show ck)}/>
    </body></xml>
