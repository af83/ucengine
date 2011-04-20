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

test("create 'close meeting' tab", function() {
    ok($('#adminbar .uce-adminbar-meeting p').length == 2);
    ok($('#adminbar .uce-adminbar-meeting .uce-adminbar-button').length == 2);
    ok($($('#adminbar .uce-adminbar-meeting .uce-adminbar-button')[0]).text() == "Close the meeting");
    ok($($('#adminbar .uce-adminbar-meeting .uce-adminbar-button')[1]).text() == "Continue the meeting");
});

jackTest("update and close the meeting when clicking on 'Close the meeting'", function() {
    var ucemeeting = jack.create("ucemeeting", ['get', 'update', 'push']);
    jack.expect('ucemeeting.get')
        .exactly('1 time')
        .mock(function(callback) {
            callback(undefined, {start_date: 21,
                                 end_date: 42,
                                 metadata: {test: 'value'}}, undefined);
        });
    jack.expect('ucemeeting.update')
        .exactly('1 time')
        .mock(function(start, end, metadata, callback) {
            ok(start == 21, "check start");
            ok(end == 4567, "check end");
            ok(metadata.test == 'value', "check metadata");
            callback(undefined, {result: 'ok'}, undefined);
        });
    jack.expect('ucemeeting.push')
        .exactly('1 time')
        .mock(function(type, metadata, callback) {
            ok(type == 'admin.meeting.close', 'check type');
        });

    var time = jack.create('time', ['get']);
    jack.expect('time.get')
        .exactly('1 time')
        .mock(function(callback) {
            callback(undefined, 4567, undefined);
        });

    $('#adminbar').adminbar({
        ucemeeting: ucemeeting,
        uceclient: {time: time}
    });

    $($('#adminbar .uce-adminbar-meeting .uce-adminbar-button')[0]).click();
});

test("hide the 'close meeting' tag when clicking on  'Continue the meeting'", function() {
    $($('#adminbar .uce-adminbar-meeting .uce-adminbar-button')[1]).click();
    ok(!$($('#adminbar .uce-adminbar-content')[1]).hasClass('uce-adminbar-active'));
    ok(!$('#adminbar .uce-adminbar-buttons li:eq(1)').hasClass('uce-adminbar-active'));
});
