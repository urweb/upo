function startBelowNavbar() {
    var height = parseInt($('.navbar').css("height"));
    if (height > 0)
        $('body').css('padding-top', height+10);
    else
        // Oh, we must be in an embeddable widget.
        $('body').css('padding-top', 0);
}

$(window).resize(startBelowNavbar);
$(startBelowNavbar);

function activateModal(id) {
    $('#' + id).modal('show');
}

function deactivateModal(id) {
    $('#' + id).modal('hide');
}
