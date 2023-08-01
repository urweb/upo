function list_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push(l._1);

    return out;
}

function list_in2(l) {
    var out = [];

    for (; l; l = l._2)
        out.push(list_in(l._1));

    return out;
}

function uw_chartist_replace(id, gr) {
    switch (gr.n) {
    case 'Bar':
        new Chartist.Bar('#' + id,
                         {labels: list_in(gr.v._1),
                          series: list_in2(gr.v._2)});
        break;
    default:
        throw 'Unknown chart type';
    }
}
