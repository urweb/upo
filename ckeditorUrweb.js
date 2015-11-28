window.CKEDITOR_BASEPATH = 'https://cdn.ckeditor.com/4.5.5/';

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

function uw_ckeditor_replace(r) {
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

    var ed = CKEDITOR.replace(r._Id, config);
    var src = r._Source;
    ed.setData(sg(src));
    ed.on('change', function(e) { sv(src, ed.getData()); });
}

function uw_ckeditor_setContent(id, s) {
    var ed = CKEDITOR.instances[id];
    if (ed) ed.setData(s);
}
