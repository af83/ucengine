module("uce.whiteboard", {teardown: function() {
    $('#whiteboard').whiteboard('destroy');
}});

test("create some elements, and destroy", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard .ui-widget-header-title').text(), 'Whiteboard');
    equals($('#whiteboard .ui-widget-content .ui-whiteboard-drawing > canvas').size(), 1);
    equals($('#whiteboard .ui-whiteboard-toolbar > *').size(), 8);
    ok($('#whiteboard').hasClass('ui-widget'), 'should have class ui-widget');
    ok($('#whiteboard').hasClass('ui-whiteboard'), 'should have class ui-whiteboard');
    $('#whiteboard').whiteboard('destroy');
    equals($('#whiteboard > *').size(), 0);
    ok(!$('#whiteboard').hasClass('ui-widget'), 'should have class ui-widget');
    ok(!$('#whiteboard').hasClass('ui-whiteboard'), 'should have class ui-whiteboard');
});

test("can set width/height of main canvas on contruct", function() {
     $('#whiteboard').whiteboard({width   : 200,
                                  height  : 150});
    equals($('#whiteboard .ui-whiteboard-drawing > canvas').attr("height"), 150);
    equals($('#whiteboard .ui-whiteboard-drawing > canvas').attr("width"), 200);
});

test("hide/show controls after init", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard .ui-whiteboard-toolbar').css('display'), 'block');
    $('#whiteboard').whiteboard('hideControls');
    equals($('#whiteboard .ui-whiteboard-toolbar').css('display'), 'none');
    $('#whiteboard').whiteboard('showControls');
    equals($('#whiteboard .ui-whiteboard-toolbar').css('display'), 'block');
});

jackTest("bind event handler", function() {
    var ucemeeting = jack.create("ucemeeting", ['on']);
    jack.expect("ucemeeting.on")
        .exactly("2 times");
    $('#whiteboard').whiteboard({ucemeeting: ucemeeting});
});

function emptyCanvas() {
    var canvas = document.createElement("canvas");
    canvas.setAttribute('height', 240);
    canvas.setAttribute('width', 300);
    return canvas;
}

jackTest("when performing action, send event to ucengine", function() {
    expect(9);
    var ucemeeting = jack.create("ucemeeting", ['push', 'on']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "whiteboard.shape.draw");
            ok(metadata.tool);
            ok(metadata.color);
            ok(metadata.x1);
            ok(metadata.y1);
            ok(metadata.x2);
            ok(metadata.y2);
            ok($('#whiteboard canvas').get(0).toDataURL("image/png") ==
               emptyCanvas().toDataURL("image/png"), "should be a empty canvas");
        });
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting,
        width : 300,
        height: 240
    });
    $('#whiteboard canvas').triggerSyn("mousedown", {});
    $('#whiteboard canvas').triggerSyn("mousemove", {});
});

jackTest("on clear, send special event to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'on']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "whiteboard.drawing.clear");
        });
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting
    });
    $('#whiteboard .ui-icon-trash').click();
});

test("handle draw/clear events", function() {
    expect(3);
    $('#whiteboard').whiteboard({width: 300, height: 240});
    ok(($('#whiteboard canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard.shape.draw",
                                                    metadata: {'tool': 'pencil',
                                                               'color': '#000000',
                                                               'x1': '1',
                                                               'y1': '1',
                                                               'x2': '2',
                                                               'y2': '2'}});
    ok(!($('#whiteboard canvas').get(0).toDataURL("image/png") ==
         emptyCanvas().toDataURL("image/png")), "should not be a empty canvas");
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard.drawing.clear", metadata: {}});
    ok(($('#whiteboard canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
});

test("clear method", function() {
    $('#whiteboard').whiteboard({width: 300, height: 240});

    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard.shape.draw",
                                                    metadata: {'tool': 'pencil',
                                                               'color': '#000000',
                                                               'x1': '1',
                                                               'y1': '1',
                                                               'x2': '2',
                                                               'y2': '2'}});

    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard.drawing.clear", metadata: {}});
    ok(($('#whiteboard canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");

    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard.shape.draw",
                                                    metadata: {'tool': 'pencil',
                                                               'color': '#000000',
                                                               'x1': '1',
                                                               'y1': '1',
                                                               'x2': '2',
                                                               'y2': '2'}});

    $('#whiteboard').whiteboard("clear");
    ok(($('#whiteboard canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
});

jackTest("when disabled, no event are send to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'on']);
    jack.expect("ucemeeting.push")
        .exactly("0 time");
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting,
        disabled   : true
    });
    $('#whiteboard .ui-icon-trash').click();
    $('#whiteboard canvas').triggerSyn("mousedown", {});
    $('#whiteboard canvas').triggerSyn("mousemove", {});
});

jackTest("dynanic disabled, no event are send to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'on']);
    jack.expect("ucemeeting.push")
        .exactly("0 time");
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting
    });
    $('#whiteboard').whiteboard("option", "disabled", true);
    $('#whiteboard .ui-icon-trash').click();
    $('#whiteboard canvas').triggerSyn("mousedown", {});
    $('#whiteboard canvas').triggerSyn("mousemove", {});
});

