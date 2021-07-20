function startBelowNavbar() {
    var navbar = $('.navbar');
    var height = parseInt(navbar.css("height"))
        - parseInt(navbar.css("padding-top"));
        - parseInt(navbar.css("padding-bottom"));
    if (height > 0)
        $('body').css('padding-top', height + 5);
    else
        // Oh, we must be in an embeddable widget.
        $('body').css('padding-top', 0);
}

$(window).resize(startBelowNavbar);
$(startBelowNavbar);
