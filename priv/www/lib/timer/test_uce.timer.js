module("uce.timer", {
    teardown: function() {
        $('#timer').timer('destroy');
    }
});

jackTest("create basic structure", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);
    $('#timer').timer({ucemeeting: ucemeeting});
    ok($('#timer').hasClass('ui-widget'), 'class ui-widget');
    ok($('#timer').hasClass('ui-timer'), 'class ui-timer');
    ok($('#timer > div').hasClass('ui-widget-content'), 'class ui-widget-content');
});

jackTest("the widget retrieves the meeting's informations", function () {
    stop();
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);
   jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never"), undefined);
            start();
        });

    $('#timer').timer({ucemeeting: ucemeeting});
});

jackTest("test widget default state", function () {
    stop();
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);

    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, 10000), undefined);
            start();
            equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:00 / 00:00:10 / 00:00:10");
        });

    $('#timer').timer({ucemeeting: ucemeeting});
});

jackTest("test widget increment every second", function () {
    expect(4);
    stop();
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);

    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, 10000), undefined);

            equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:00 / 00:00:10 / 00:00:10");
            setTimeout(function() {
                equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:01 / 00:00:09 / 00:00:10");
                setTimeout(function() {
                    start();
                    equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:02 / 00:00:08 / 00:00:10");
                }, 1200);
            }, 1200);
        });

    $('#timer').timer({ucemeeting: ucemeeting});
});

jackTest("test remaining time before open", function () {
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);

    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(30000, 50000), undefined);
            equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "-00:00:20 / 00:00:40 / 00:00:20");
        });

    $('#timer').timer({ucemeeting: ucemeeting, start: 10000});
});

jackTest("test time elapsed after close", function () {
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);

    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(10000, 20000), undefined);
            equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:10 / 00:00:00 / 00:00:10");
        });

    $('#timer').timer({ucemeeting: ucemeeting, start: 30000});
});

jackTest("test meeting infinity", function () {
    var ucemeeting = jack.create("ucemeeting", ['on', 'get']);

    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(10000, "never"), undefined);
            equals($('#timer > .ui-widget-content > .ui-timer-value').text(), "00:00:20 / \u221E");
        });

    $('#timer').timer({ucemeeting: ucemeeting, start: 30000});
});
