module("uce.adminbar", {
    setup: function() {
        $('#adminbar').adminbar();
    },
    teardown: function() {
        $('#adminbar').adminbar('destroy');
    }});

test("create basic structure", function () {
    ok($('#adminbar').hasClass('ui-widget'));
    ok($('#adminbar').hasClass('uce-adminbar'));
    equals($('#adminbar .uce-adminbar-header').children().size(), 2);
    equals($('#adminbar .uce-adminbar-header .uce-adminbar-buttons').children().size(), 3);
    equals($('#adminbar .uce-adminbar-header .uce-adminbar-buttons').children().size(), 3);
    equals($('#adminbar .uce-adminbar-header .uce-adminbar-title').text(), "Owner's features");
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
