module("uce.adminbar", {
    setup: function() {
        $('#adminbar').adminbar({ucemeeting: 'test_meeting',
                                 widgets: {
                                    'widget1': {
                                        title: 'Widget1 title',
                                        description: 'Widget1 description'
                                    },
                                    'widget2': {
                                        title: 'Widget2 title',
                                        description: 'Widget2 description'
                                    },
                                    'widget3': {
                                        title: 'Widget3 title',
                                        description: 'Widget3 description'
                                    }
                                }});
    },
    teardown: function() {
        $('#adminbar').adminbar('destroy');
    }});

test("add widget1 by click on carousel", function () {
   $('a.uce-adminbar-widget-widget1-link').click();
   equals($('#adminbar').adminbar('getSelectedWidgets').length, 1); 
});

test("add widget1 by click on carousel and cancel", function () {
   $('a.uce-adminbar-widget-widget1-link').click();
   equals($('#adminbar').adminbar('getSelectedWidgets').length, 1); 
   $('a.uce-adminbar-cancel-button').click();
   equals($('#adminbar').adminbar('getSelectedWidgets').length, 0);
});

test("create basic structure", function () {
    ok($('#adminbar').hasClass('ui-widget'));
    ok($('#adminbar').hasClass('uce-adminbar'));
    equals($('#adminbar .uce-adminbar-header').children().size(), 2, "The header have children");
    equals($('#adminbar .uce-adminbar-header .uce-adminbar-buttons').children().size(), 3, "The header have buttons");
    equals($('#adminbar .uce-adminbar-header .uce-adminbar-title').text(), "Owner's features", "The header have the good label");
    equals($('#adminbar').find('ul.jcarousel-list').children().size(), 3);
});

test("can toggle buttons", function() {
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(0)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(1)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(2)').hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(0)').click();

    ok($('#adminbar .uce-adminbar-buttons li:eq(0)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(1)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar uce-adminbar-buttons li:eq(2)').hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(2)').click();

    ok(!$('#adminbar .uce-adminbar-buttons li:eq(0)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(1)').hasClass('uce-adminbar-active'));
    ok($('#adminbar .uce-adminbar-buttons li:eq(2)').hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(2)').click();

    ok(!$('#adminbar .uce-adminbar-buttons li:eq(0)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(1)').hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(2)').hasClass('uce-adminbar-active'));
});

test("can toggle content", function() {
    ok(!$($('#adminbar .uce-adminbar-content')[0]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[1]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[2]).hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(0)').click();

    ok($($('#adminbar .uce-adminbar-content')[0]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[1]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[2]).hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(2)').click();

    ok(!$($('#adminbar .uce-adminbar-content')[0]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[1]).hasClass('uce-adminbar-active'));
    ok($($('#adminbar .uce-adminbar-content')[2]).hasClass('uce-adminbar-active'));

    $('#adminbar .uce-adminbar-buttons li:eq(2)').click();

    ok(!$($('#adminbar .uce-adminbar-content')[0]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[1]).hasClass('uce-adminbar-active'));
    ok(!$($('#adminbar .uce-adminbar-content')[2]).hasClass('uce-adminbar-active'));
});
