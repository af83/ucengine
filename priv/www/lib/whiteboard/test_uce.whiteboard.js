module("uce.whiteboard", {teardown: function() {
    $('#whiteboard').whiteboard('destroy');
}});

test("create some elements, and destroy", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard .ui-widget-header').text(), 'Whiteboard');
    equals($('#whiteboard .ui-widget-content > canvas').size(), 2);
    equals($('#whiteboard #controls > *').size(), 6);
    equals($('#whiteboard #chooserWidgets > canvas').size(), 3);
    ok($('#whiteboard').hasClass('ui-widget'), 'should have class ui-widget');
    ok($('#whiteboard').hasClass('ui-whiteboard'), 'should have class ui-whiteboard');
    $('#whiteboard').whiteboard('destroy');
    equals($('#whiteboard > *').size(), 0);
    ok(!$('#whiteboard').hasClass('ui-widget'), 'should have class ui-widget');
    ok(!$('#whiteboard').hasClass('ui-whiteboard'), 'should have class ui-whiteboard');
});

test("create some element with custom ids", function() {
    $('#whiteboard').whiteboard({canvas_id_bottom   : "canvas_chuck",
                                 canvas_id_interface: "canvas_norris",
                                 controls_id        : "mycontrols",
                                 choosers_id        : "myChooserWidgets"});
    equals($('#whiteboard .ui-widget-content > canvas:eq(0)').attr("id"), "canvas_chuck");
    equals($('#whiteboard .ui-widget-content > canvas:eq(1)').attr("id"), "canvas_norris");
    equals($('#whiteboard #mycontrols > *').size(), 6);
    equals($('#whiteboard #myChooserWidgets > canvas').size(), 3);
});

test("can set width/height of main canvas on contruct", function() {
     $('#whiteboard').whiteboard({width   : 200,
                                  height  : 150});
    equals($('#whiteboard .ui-widget-content > canvas:eq(0)').attr("height"), 150);
    equals($('#whiteboard .ui-widget-content > canvas:eq(0)').attr("width"), 200);
    equals($('#whiteboard .ui-widget-content > canvas:eq(1)').attr("height"), 150);
    equals($('#whiteboard .ui-widget-content > canvas:eq(1)').attr("width"), 200);
});

test("can customize widgets", function() {
    $('#whiteboard').whiteboard({widget_color: false});
    equals($('#whiteboard #chooserWidgets > canvas').size(), 2);
    $('#whiteboard').whiteboard('destroy');
    $('#whiteboard').whiteboard({widget_color: false, widget_linewidth: false});
    equals($('#whiteboard #chooserWidgets > canvas').size(), 1);
    $('#whiteboard').whiteboard('destroy');
    $('#whiteboard').whiteboard({widget_color: false, widget_linewidth: false, widget_transport: false});
    equals($('#whiteboard #chooserWidgets > canvas').size(), 0);
});

test("hide/show controls after init", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard #controls').css('display'), 'block');
    $('#whiteboard').whiteboard('hideControls');
    equals($('#whiteboard #controls').css('display'), 'none');
    $('#whiteboard').whiteboard('showControls');
    equals($('#whiteboard #controls').css('display'), 'block');
});

test("hide/show color widget", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard #colorChooser').css('display'), 'inline');
    $('#whiteboard').whiteboard('option', 'widget_color', false);
    equals($('#whiteboard #colorChooser').css('display'), 'none');
    $('#whiteboard').whiteboard('option', 'widget_color', true);
    equals($('#whiteboard #colorChooser').css('display'), 'inline');
});

test("hide/show linewidth widget", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard #lineWidthChooser').css('display'), 'inline');
    $('#whiteboard').whiteboard('option', 'widget_linewidth', false);
    equals($('#whiteboard #lineWidthChooser').css('display'), 'none');
    $('#whiteboard').whiteboard('option', 'widget_linewidth', true);
    equals($('#whiteboard #lineWidthChooser').css('display'), 'inline');
});

test("hide/show transport widget", function() {
    $('#whiteboard').whiteboard();
    equals($('#whiteboard #transportWidget').css('display'), 'inline');
    $('#whiteboard').whiteboard('option', 'widget_transport', false);
    equals($('#whiteboard #transportWidget').css('display'), 'none');
    $('#whiteboard').whiteboard('option', 'widget_transport', true);
    equals($('#whiteboard #transportWidget').css('display'), 'inline');
});

jackTest("bind event handler", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    jack.expect("ucemeeting.bind")
        .exactly("2 times");
    $('#whiteboard').whiteboard({ucemeeting: ucemeeting});
});

function emptyCanvas() {
    var canvas = document.createElement("canvas");
    canvas.setAttribute('height', 400);
    canvas.setAttribute('width', 400);
    return canvas;
}

jackTest("when performing action, send event to ucengin", function() {
    expect(4);
    var ucemeeting = jack.create("ucemeeting", ['push', 'bind']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "whiteboard_draw_event");
            ok(metadata.wevent);
            ok($('#canvas').get(0).toDataURL("image/png") ==
               emptyCanvas().toDataURL("image/png"), "should be a empty canvas");
        });
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting
    });
    $('#whiteboard #btn_0').click();
    $('#canvasInterface').triggerSyn("mousedown", {});
    $('#canvasInterface').triggerSyn("mousemove", {});
});

jackTest("on clear, send special event to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'bind']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "whiteboard_clear_event");
        });
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting
    });
    $('#whiteboard #btn_5').click();
});

test("handle draw/clear events", function() {
    expect(3);
    $('#whiteboard').whiteboard();
    ok(($('#canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard_draw_event", metadata: {wevent: '{"event":"drawBrush","args":{"startPos":{"x":-8,"y":-145.39999389648438},"curPos":{"x":0,"y":145.39999389648438},"drawColor":"rgb(0,0,0)","lineWidth":10}}'}});
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard_draw_event", metadata: {wevent: '{"event":"drawBrush","args":{"startPos":{"x":-8,"y":-145.39999389648438},"curPos":{"x":9,"y":145.39999389648438},"drawColor":"rgb(0,0,0)","lineWidth":10}}'}});
    ok(!($('#canvas').get(0).toDataURL("image/png") ==
         emptyCanvas().toDataURL("image/png")), "should not be a empty canvas");
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard_clear_event", metadata: {wevent: '{"event":"clearCanvas","args":{}}'}});
    ok(($('#canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
});

test("clear method", function() {
    $('#whiteboard').whiteboard();
    $('#whiteboard').whiteboard("triggerUceEvent", {type: "whiteboard_clear_event", metadata: {wevent: '{"event":"drawBrush","args":{"startPos":{"x":-8,"y":-145.39999389648438},"curPos":{"x":0,"y":145.39999389648438},"drawColor":"rgb(0,0,0)","lineWidth":10}}'}});
    ok(!($('#canvas').get(0).toDataURL("image/png") ==
         emptyCanvas().toDataURL("image/png")), "should not be a empty canvas");
    $('#whiteboard').whiteboard("clear");
    ok(($('#canvas').get(0).toDataURL("image/png") ==
        emptyCanvas().toDataURL("image/png")), "should be a empty canvas");
});

test("toggle class active on button", function() {
    $('#whiteboard').whiteboard();
    $('#whiteboard #btn_0').click();
    ok($('#whiteboard #btn_0').hasClass('ui-state-active'), "btn 0 should have class active");
    $('#whiteboard #btn_1').click();
    ok(!$('#whiteboard #btn_0').hasClass('ui-state-active'), "btn 0 should not have class active");
    ok($('#whiteboard #btn_1').hasClass('ui-state-active'), "btn 1 should have class active");
});

jackTest("when disabled, no event are send to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'bind']);
    jack.expect("ucemeeting.push")
        .exactly("0 time");
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting,
        disabled   : true
    });
    $('#whiteboard #btn_0').click();
    $('#canvasInterface').triggerSyn("mousedown", {});
    $('#canvasInterface').triggerSyn("mousemove", {});
});

jackTest("dynanic disabled, no event are send to ucengine", function() {
    var ucemeeting = jack.create("ucemeeting", ['push', 'bind']);
    jack.expect("ucemeeting.push")
        .exactly("0 time");
    $('#whiteboard').whiteboard({
        ucemeeting : ucemeeting
    });
    $('#whiteboard').whiteboard("option", "disabled", true);
    $('#whiteboard #btn_0').click();
    $('#canvasInterface').triggerSyn("mousedown", {});
    $('#canvasInterface').triggerSyn("mousemove", {});
});

