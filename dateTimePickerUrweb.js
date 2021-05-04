function uw_dateTimePicker_replace(r) {
    var start = sg(r._Source);
    var dtp = $('#' + r._Id);
    dtp.datetimepicker({
        defaultDate: new Date(start / 1000),
        onChangeDateTime: function(v) {
            sv(r._Source, v.getTime() * 1000);
        }
    });
    listen(ss(r._Source),
           function(v) {
               dtp.Value = new Date(v);
           });
}
