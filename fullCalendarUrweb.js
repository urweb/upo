function uw_fullcalendar_refresh(cal) {
    t = cal.view.type;
    if (cal.view.type == 'timeGridWeek')
        cal.changeView('timeGridDay');
    else
        cal.changeView('timeGridWeek');
    cal.changeView(t);
}

function fctimein(tm) {
    return Math.round(tm / 1000);
}

function fctimeout(tm) {
    return tm.getTime() * 1000;
}

function uw_fullcalendar_replace(id, settings) {
    var node = document.getElementById(id);
    if (node) {
        var props = {
            plugins: [ 'interaction', 'dayGrid', 'timeGrid' ],
            header: {
                left: 'prev,next today',
                center: 'title',
                right: 'dayGridMonth,timeGridWeek,timeGridDay'
            },
            editable: settings["_OnDrop"] != null,
            defaultView: 'timeGridWeek',
            navLinks: true,
            selectable: settings["_OnSelect"] != null,
            selectMirror: settings["_OnSelect"] != null,
//          minTime: "06:00:00",
            allDaySlot: settings["_AllDaySlot"]
        };

        if (settings["_SlotDuration"])
            props.slotDuration = settings["_SlotDuration"];
        if (settings["_SnapDuration"])
            props.snapDuration = settings["_SnapDuration"];

        if (settings["_DefaultDate"])
            props.defaultDate = fctimein(settings["_DefaultDate"]);

        if (settings["_OnSelect"])
            props.select = function(arg) {
                execF(execF(execF(settings["_OnSelect"],
                                  fctimeout(arg.start)),
                            fctimeout(arg.end)),
                      null);
            };

        if (settings["_OnDrop"])
            props.eventDrop = function(arg) {
                execF(execF(execF(settings["_OnDrop"],
                                  arg.oldEvent),
                            arg.event),
                      null);
            };

        if (settings["_Content"])
            props.eventRender = function(arg) {
                if (arg.isMirror)
                    return;

                if (arg.event.rendering && arg.event.title)
                    arg.el.innerText = arg.event.title;

                var r = execF(execF(settings["_Content"], arg.event), null);
                
                var cls = {v : null};
                var html = flatten(cls, r["_Header"]);
                var span = document.createElement("span");
                span.closures = cls.v;
                span.innerHTML = html;
                var node = arg.el.querySelector('.fc-time');
                if (node) {
                    node.appendChild(span);
                    runScripts(span);
                }

                cls = {v : null};
                html = flatten(cls, r["_Body"]);
                span = document.createElement("span");
                span.closures = cls.v;
                span.innerHTML = html;
                node = arg.el.querySelector('.fc-content');
                if (node) {
                    node.appendChild(span);
                    runScripts(span);
                }
            };
        else
             props.eventRender = function(arg) {
                if (arg.event.rendering && arg.event.title)
                    arg.el.innerText = arg.event.title;
             }
        
        var cal = new FullCalendar.Calendar(node, props);

        cal.render();

        var setSize = function() {
            if ($(node).is(':visible'))
                uw_fullcalendar_refresh(cal);
            else
                window.setTimeout(setSize, 500);
        };
        setSize();

        return cal;
    } else er("FullCalendar: couldn't find location of calendar");
}

function fceventin(data) {
    var edata = {allDay: data["_AllDay"],
                 start: fctimein(data["_Start"]),
                 title : data["_Title"]};
    if (data["_Id"]) edata.id = data["_Id"];
    if (data["_End"]) edata.end = fctimein(data["_End"]);
    if (data["_Rendering"] != "Normal") {
        if (data["_Rendering"] == "Background")
            edata.rendering = "background";
        else
            edata.rendering = "inverse-background";
    }

    return edata;
}

var fctextcolor_cache_valid = false;
var fctextcolor_cache = null;

function fctextcolor() {
    if (fctextcolor_cache_valid)
        return fctextcolor_cache;

    fctextcolor_cache_valid = true;
    var styleSheets = window.document.styleSheets;
    for (var i = 0; i < styleSheets.length; i++) {
        var classes = null;
        try {
            classes = styleSheets[i].rules || styleSheets[i].cssRules;
        } catch (e) { }
        if (!classes)
            continue;
        for (var x = 0; x < classes.length; x++) {
            if (classes[x].selectorText == '.fc-text-color') {
                fctextcolor_cache = classes[x].style.color;
                return fctextcolor_cache;
            }
        }
    }
}

function fceventpost(data, ev) {
    if (data["_TextColor"])
        listen(data["_TextColor"],
               function(v) { ev.setProp('textColor', v || fctextcolor()); });
    else
        ev.setProp('textColor', fctextcolor());
    if (data["_BackgroundColor"]) listen(data["_BackgroundColor"],
                                         function(v) { ev.setProp('backgroundColor', v); });
}

function fceventpostCopout(data, dataOut) {
    if (data["_TextColor"])
        dataOut['textColor'] = scur(data["_TextColor"]) || fctextcolor();
    else
        dataOut['textColor'] = fctextcolor();
    if (data["_BackgroundColor"])
        dataOut['backgroundColor'] = scur(data["_BackgroundColor"]);
}
    
function uw_fullcalendar_events(cal) {
    var out = null, evs = cal.getEvents();
    for (var i = evs.length - 1; i >= 0; --i)
        out = {"_1": evs[i], "_2": out};
    return out;
}

function uw_fullcalendar_addEvent(cal, data) {
    var ev = cal.addEvent(fceventin(data));
    fceventpost(data, ev);
    return ev;
}

function uw_fullcalendar_addEvents(cal, datas) {
    var evs = [];
    
    for (; datas; datas = datas["_2"]) {
        var data = datas["_1"];
        var dataOut = fceventin(data);
        fceventpostCopout(data, dataOut);
        evs.push(dataOut);
    }
    cal.addEventSource(evs);
    return null;
}

function uw_fullcalendar_removeEvent(ev) {
    ev.remove();
}

function uw_fullcalendar_eventStart(ev) {
    return fctimeout(ev.start);
}

function uw_fullcalendar_eventTitle(ev) {
    return ev.title;
}

function uw_fullcalendar_eventRendering(ev) {
    return (ev.rendering == "background" ? "Background"
            : ev.rendering == "inverse-background" ? "InverseBackground"
            : "Normal");
}

function uw_fullcalendar_eventId(ev) {
    return ev.id;
}

function uw_fullcalendar_getEventById(cal, id) {
    return cal.getEventById(id);
}

function uw_fullcalendar_unselect(cal) {
    cal.unselect();
}
