function arrayToUrweb(arr) {
    var ls = null;
    for (var i = arr.length - 1; i >= 0; i--)
        ls = {_1: arr[i].text, _2: ls};
    return ls;
}

function uw_select2_replace(id, onChange) {
    $('#' + id).select2({width: '100%'}).change(function() {
        execF(execF(onChange, arrayToUrweb($('#' + id).select2('data'))), null);
    });
}
