function list_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push(l._1);

    return out;
}

function dataset_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push({label: l._1._Label, data: list_in(l._1._Values)});

    return out;
}

function list_xy_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push({x: l._1._x, y: l._1._y});

    return out;
}

function scatter_dataset_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push({label: l._1._Label, data: list_xy_in(l._1._Values)});

    return out;
}

function list_xyr_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push({x: l._1._x, y: l._1._y, r: l._1._r});

    return out;
}

function bubble_dataset_in(l) {
    var out = [];

    for (; l; l = l._2)
        out.push({label: l._1._Label, data: list_xyr_in(l._1._Values)});

    return out;
}

function uw_set_chartjs(id, gr) {
    var parentDiv = document.getElementById(id);
    var canvas = document.createElement('canvas');
    parentDiv.appendChild(canvas);
    switch (gr.n) {
    case 'Pie':
    case 'Doughnut':
    case 'PolarArea':
    case 'Radar':
        parentDiv.style["aspect-ratio"] = "1 / 1";
        break;
    default:
        parentDiv.style["aspect-ratio"] = "2 / 1";
    }

    switch (gr.n) {
    case 'Bar':
    case 'StackedBar':
    case 'Line':
    case 'Pie':
    case 'Doughnut':
    case 'PolarArea':
    case 'Radar':
        var gtype = (gr.n == 'StackedBar') ? 'bar' : gr.n.toLowerCase();
        var options = (gr.n == 'StackedBar') ? {scales: {x: {stacked: true }, y: { stacked: true } } } : {};
        var labels = list_in(gr.v._1);
        var datasets = dataset_in(gr.v._2);
        return new Chart(canvas,
                  {type: gtype,
                   options: options,
                   data: {
                    labels: labels,
                    datasets: datasets}});
    case 'Scatter':
        return new Chart(canvas,
                  {type: 'scatter',
                   data: {datasets: scatter_dataset_in(gr.v)}});
    case 'Bubble':
        return new Chart(canvas,
                  {type: 'scatter',
                   data: {datasets: bubble_dataset_in(gr.v)}});
    default:
        throw 'Unknown chart type';
    }
}

function uw_update_chartjs(ch) {
    ch.update();
}