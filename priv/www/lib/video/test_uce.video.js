module("uce.video", {teardown: function() {
    $("#video").video("destroy");
}});

test("create basic html, and destroy", function() {
    $("#video").video();
    equals($("#video .ui-widget-header .ui-video-title").text(), 'Video', 'the widget has a title');
    equals($("#video .ui-widget-header .ui-button").size(), 1, 'the widget has a publish button');
    equals($("#video .ui-widget-content embed").size(), 1, 'has embed');
    ok($("#video").hasClass('ui-widget'), 'has class ui-widget');
    ok($("#video").hasClass('ui-video'), 'has class ui-video');
    $("#video").video("destroy");
    ok(!$("#video").hasClass('ui-widget'), 'has not class ui-widget');
    ok(!$("#video").hasClass('ui-video'), 'has not class ui-video');
    equals($("#video > *").size(), 0, 'the widget has no content');
});

test("customize with domain, stream and width/height", function() {
    $("#video").video({width  : 400,
                       height : 200,
                       domain : 'example.com/ucengine',
                       stream : 'plop'});
    equals(/server=([^&]+)/.exec($("#video embed").attr('flashvars'))[1], encodeURIComponent('rtmp://example.com/ucengine'), "server param");
    equals(/stream=(\w+)/.exec($("#video embed").attr('flashvars'))[1], 'plop');
    equals(/width=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 400);
    equals(/height=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 200);
    equals($("#video .ui-widget-content embed").attr('width'), 400);
    equals($("#video .ui-widget-content embed").attr('height'), 200);
});

test("dynamic updated options", function() {
    $("#video").video();
    $("#video").video("option", {"width" : "100",
                                 "height": "150",
                                 "domain": "example.org",
                                 "stream": "plop2"});
    equals(/server=([^&]+)/.exec($("#video embed").attr('flashvars'))[1], encodeURIComponent('rtmp://example.org'), "server param");
    equals(/stream=(\w+)/.exec($("#video embed").attr('flashvars'))[1], 'plop2');
    equals(/width=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 100);
    equals(/height=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 150);
    equals($("#video .ui-widget-content embed").attr('width'), 100);
    equals($("#video .ui-widget-content embed").attr('height'), 150);
});

test("publish video", function() {
    $("#video").video();
    $("#video").video("publish");
    equals($("#video .ui-widget-content embed").attr('src'), '/lib/video/publish_video.swf');
});

test("publish video and set option", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("option", "width", "100");
    equals($("#video embed").attr('width'), 100);
    equals($("#video .ui-widget-content embed").attr('src'), '/lib/video/publish_video.swf');
});

test("receive video", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("receive");
    equals($("#video .ui-widget-content embed").attr('src'), '/lib/video/receive_video.swf');
});

jackTest("on receive, handle video.stream.new", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    $("#video").video({ucemeeting: ucemeeting,
                       "domain": "example.org"});
    equals($("#video .ui-widget-content embed").size(), 0);
    $("#video").video("triggerUceEvent", Factories.createStreamNew());
    equals($("#video").video("option", "stream"), "channel_1", "channel option should be updated");
    equals($("#video").video("option", "token"), "123456", "token option should updated");
});

test("publish then stop a video stream", function() {
    $("#video").video();
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Stop publish", "label has changed");
    equals($("#video .ui-widget-content embed").attr('src'), '/lib/video/publish_video.swf', 'src is publish_video.swf');
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Publish", "label has changed");
    equals($("#video .ui-widget-content embed").attr('src'), '/lib/video/receive_video.swf', 'src is receive_video.swf');
});

jackTest("desactivate/activate the publish button when a stream start and stop", function () {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.uid = 'john';
    $("#video").video({ucemeeting: ucemeeting});
    $("#video").video("triggerUceEvent", Factories.createStreamStart("root"));
    ok($("#video .ui-button").button("option", "disabled"), "button should be disabled");
    // test click on disabled button
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Publish", "label should not be updated");
    // trigger stream.stop
    $("#video").video("triggerUceEvent", Factories.createStreamStop("root"));
    ok(!$("#video .ui-button").button("option", "disabled"), "button should be enabled");
});

jackTest("doesn't desactivate the publish button when a stream started by the same uid", function () {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.uid = 'root';
    $("#video").video({ucemeeting: ucemeeting});
    $("#video").video("triggerUceEvent", Factories.createStreamStart("root"));
    ok(!$("#video .ui-button").button("option", "disabled"), "button should be enabled");
});

jackTest("show message when no stream is available", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.uid = 'root';
    $("#video").video({ucemeeting: ucemeeting});
    $("#video").video("triggerUceEvent", Factories.createStreamNew());
    $("#video").video("triggerUceEvent", Factories.createStreamStart("root"));
    $("#video").video("triggerUceEvent", Factories.createStreamStop("root"));
    equals($("#video .ui-widget-content embed").size(), 0, "no embed");
    equals($("#video .ui-widget-content p").text(), "No stream is available", "show text");
});
