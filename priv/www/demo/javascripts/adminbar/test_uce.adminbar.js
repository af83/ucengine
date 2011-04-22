module("uce.adminbar", {
    setup: function() {
        var that = this;
        this.ucemeeting = {
            name: "ucemeeting",
            on: function(eventName, callback) {
                if (eventName == "admin.meeting.widgets.add") {
                    that.callback_widgets_add = callback;
                } 
            }
        };
        $('#adminbar').adminbar({ucemeeting: this.ucemeeting,
                                 widgets: {
                                    'fileupload': {
                                        title: 'Widget1 title',
                                        description: 'Widget1 description'
                                    },
                                    'filesharing': {
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
        $('#fileupload').fileupload('destroy');
        $('#filesharing').filesharing('destroy');
    }});

test("add widget3 by click on carousel", function () {
   $('.uce-adminbar-widget-widget3-link').click();
   equals($('#adminbar').adminbar('getSelectedWidgets').length, 1);
});

test("add widget3 by click on carousel and cancel", function () {
   $('.uce-adminbar-widget-widget3-link').click();
   equals($('#adminbar').adminbar('getSelectedWidgets').length, 1);
   $('.uce-adminbar-cancel-button').click();
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

jackTest("valid selected widgets by dispatching 'admin.meeting.widgets.add'", function() {
    var ucemeeting = jack.create("ucemeeting", ['push']);

    jack.expect('ucemeeting.push')
        .exactly('1 time')
        .mock(function(type, metadata, callback) {
            ok(type == 'admin.meeting.widgets.add', 'check type');
            ok(metadata.widgets == 'filesharing', 'check selected widgets');
        });

    $('#fileupload').fileupload({ucemeeting: ucemeeting,
                                 hidden: true});

    $('#filesharing').filesharing({ucemeeting: ucemeeting,
                                   hidden: true});

    $('#fileupload').hide();
    $('#filesharing').hide();

    $('#adminbar').adminbar({
        ucemeeting: ucemeeting
    });

    ok($('#fileupload').is(':visible') == false, 'fileupload widget is hidden');
    ok($('#filesharing').is(':visible') == false, 'filesharing widget is hidden');

    $('#adminbar').adminbar('addSelectedWidget', 'fileupload');
    $('#adminbar').adminbar('addSelectedWidget', 'filesharing');

    ok($('#adminbar').adminbar('getSelectedWidgets').length == 2, '2 widgets selected');

    $('#adminbar').find('span.uce-adminbar-cancel-button').click();

    ok($('#adminbar').adminbar('getSelectedWidgets').length == 0, 'no more widget selected after clik on cancel button');

    $('#adminbar').adminbar('addSelectedWidget', 'filesharing');

    ok($('#adminbar').adminbar('getSelectedWidgets').length == 1, '1 widget selected');

    $('#adminbar').find('span.uce-adminbar-valid-button').click();

    ok($('#adminbar').adminbar('getSelectedWidgets').length == 0, 'no more widget selected after clik on valid button');
});
