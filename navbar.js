$(window).resize(function () { 
   $('body').css('padding-top', parseInt($('.navbar').css("height"))+10);
});

$(function () { 
   $('body').css('padding-top', parseInt($('.navbar').css("height"))+10);
});

function activateModal(id) {
    $('#' + id).modal({});
}
