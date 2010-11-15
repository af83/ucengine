module("uce.replay",
       {
           teardown: function() {
               //$("#replay").replay("destroy");
           }
       }
);

test("create element play and slider", function() {
    $("#replay").replay();
    ok($("#replay").hasClass('ui-widget'), 'has class ui-widget');
    ok($("#replay").hasClass('ui-replay'), 'has class ui-replay');
    equal($("#replay .ui-button").size(), 1);
    equal($("#replay .ui-button").text(), 'Play');
    equal($("#replay .ui-slider").size(), 1);
});

test("destroy remove all element", function() {
    $("#replay").replay();
    ok($("#replay").children().size() > 0);
    $("#replay").replay("destroy");
    equal($("#replay").children().size(), 0);
    ok(!$("#replay").hasClass('ui-widget'), 'has class ui-widget');
    ok(!$("#replay").hasClass('ui-replay'), 'has class ui-replay');
});

test("on init, compute total time", function() {
    $("#replay").replay({
        date_start : 1000,
        date_end   : 1000 + (60 * 66 * 1000)
    });
    equal($("#replay .ui-replay-time").text(), '00:00:00 / 01:06:00');
});

test("on init, compute real time", function() {
    $("#replay").replay({
        date_start : 1000,
        date_end   : 1000 + (60  * 74 * 1000) + (30 * 1000)
    });
    equal($("#replay .ui-replay-time").text(), '00:00:00 / 01:14:30');
});

test("click on play, trigger start event, toggle button and time updated", function() {
    stop();
    expect(3);

    $("#replay").replay({
        date_start : 0,
        date_end   : 3000,
        start: function(event, ui) {
            equal($("#replay .ui-button").text(), 'Stop');
        }
    });
    $("#replay .ui-button").click();
    setTimeout(function() {
        start();
        ok($("#replay .ui-slider").slider( "option", "value") != 0, "slider");
        ok($("#replay .time").text() != '00:00:00 / 00:00:03', "time should be updated");
    }, 1500);
});

test("click on stop, trigger stop event and toggle button", function() {
    stop();
    $("#replay").replay({
        date_start : 0,
        date_end   : 3000,
        stop: function(event, ui) {
            equal($("#replay .ui-button").text(), 'Play');
            var text = $("#replay .ui-replay-time").text();
            setTimeout(function() {
                start();
                equal($("#replay .ui-replay-time").text(), text, "time ellapsed should not be updated");
            }, 1500);
        }
    });
    $("#replay .ui-button").click().click();
});

test("trigger stop event at the end", function() {
    stop();
    $("#replay").replay({
        date_start : 0,
        date_end   : 3000,
        stop: function(event, ui) {
            equal($("#replay .ui-button").text(), 'Play');
            setTimeout(function() {
                start();
                equal($("#replay .ui-replay-time").text(), "00:00:03 / 00:00:03");
            }, 1500);
        }
    });
    $("#replay .ui-button").click();
});

test("trigger jump on click slide", function() {
    expect(2);
    stop();
    $("#replay").replay({
        date_start : 100,
        date_end   : 5100,
        jump: function(event, ui) {
            start();
            equal(ui.value, 50);
            equal(ui.timecode, 2600);
            $("#replay").replay("destroy");
        }
    });
    $("#replay .ui-button").click();
    var offset = $("#replay .ui-slider").offset();
    Syn.click({clientX: offset.left + ($("#replay .ui-slider").width() / 2),
               clientY: offset.top},
              $("#replay .ui-slider"));
});


test("can jump to a specified time", function() {
    expect(2);
    stop();
    $("#replay").replay({
        date_start : 100,
        date_end   : 5100,
        jump: function(event, ui) {
            start();
            equal(ui.value, 50);
            equal(ui.timecode, 2600);
            $("#replay").replay("destroy");
        }
    });
    $("#replay .ui-button").click();
    $("#replay").replay("jump", 2600);
});

test("jump after stop", function() {
    stop();
    $("#replay").replay({
        date_start : 0,
        date_end   : 1000,
        jump       : function() {
            start();
            equal($("#replay .ui-button").text(), 'Stop');
        }
    });
    $("#replay .ui-button").click();
    setTimeout(function() {
        $("#replay").replay("jump", 100);
    }, 2000);
});
