$(window).resize(function () { 
   $('body').css('padding-top', parseInt($('.navbar').css("height"))+10);
});

$(window).load(function () { 
   $('body').css('padding-top', parseInt($('.navbar').css("height"))+10);         
});
