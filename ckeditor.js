window.CKEDITOR_BASEPATH = 'http://ckeditor.com/apps/ckeditor/4.3/';

function toolbar_set(toolbars) {
    var toolbarsOut = [];

    for (; toolbars != null; toolbars = toolbars._2) {
        var toolbar = toolbars._1;

        if (toolbar == null)
            toolbarsOut.push('/');
        else if (toolbar.n == "Bar") {
            var buttonsOut = [], r = toolbar.v;
            var name = r._Nam;

            for (var buttons = r._Buttons; buttons != null; buttons = buttons._2) {
                var button = buttons._1;
                buttonsOut.push(button == "Separator" ? "-" : button);
            }

            if (name == null)
                toolbarsOut.push(buttonsOut);
            else
                toolbarsOut.push({name: name, items: buttonsOut});
        } else
            throw ("Invalid Ckeditor toolbar " + toolbar);
    }

    return toolbarsOut;
}

function sizeOut(v) {
    if (v == "DefaultSize")
        return null;
    else if (v.n == "Pixels")
        return (ts(v.v));
    else if (v.n == "Percent")
        return (ts(v.v) + "%");
    else
        throw ("Invalid Ckeditor.size " + v);
}

function uw_ckeditor_editor(r) {
    var config = {};

    var width = sizeOut(r._Width);
    if (width != null)
        config.width = width;

    var height = sizeOut(r._Height);
    if (height != null)
        config.height = height;

    var toolbarSet = r._ToolbarSet;

    if (toolbarSet != null)
        config.toolbar = toolbar_set(toolbarSet.v);
    
    return {config: config, name: fresh(), source: sc("")};
}

function uw_ckeditor_replace(t, id) {
    t.editor = CKEDITOR.replace(id, t.config);
    t.editor.setData(sg(t.source));
    t.editor.on('saveSnapshot', function(e) { sv(t.source, t.editor.getData()); });
}

function uw_ckeditor_content(t) {
    return ss(t.source);
}

function uw_ckeditor_setContent(t, s) {
    sv(t.source, s);
    if (t.editor != undefined)
        t.editor.setData(s);
}
