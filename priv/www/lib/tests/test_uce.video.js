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
    equals(/rtmp:\/\/([\w\.]+)/.exec($("#video embed").attr('flashvars'))[1], 'example.com');
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
    equals(/rtmp:\/\/([\w\.]+)/.exec($("#video embed").attr('flashvars'))[1], 'example.org');
    equals(/stream=(\w+)/.exec($("#video embed").attr('flashvars'))[1], 'plop2');
    equals(/width=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 100);
    equals(/height=(\d+)/.exec($("#video embed").attr('flashvars'))[1], 150);
    equals($("#video .ui-widget-content embed").attr('width'), 100);
    equals($("#video .ui-widget-content embed").attr('height'), 150);
});

test("publish video", function() {
    $("#video").video();
    $("#video").video("publish");
    equals($("#video .ui-widget-content embed").attr('src'), '/publish_video.swf');
});

test("publish video and set option", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("option", "width", "100");
    equals($("#video embed").attr('width'), 100);
    equals($("#video .ui-widget-content embed").attr('src'), '/publish_video.swf');
});

test("receive video", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("receive");
    equals($("#video .ui-widget-content embed").attr('src'), '/receive_video.swf');
});

jackTest("on receive, handle video.stream.new", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    $("#video").video({ucemeeting: ucemeeting,
                       "domain": "example.org"});
    equals($("#video .ui-widget-content embed").size(), 0);
    $("#video").video("triggerUceEvent", {type: "video.stream.new",
                                          metadata : {token : "123456",
                                                      channel : "channel_1"}});
    equals($("#video embed").size(), 1);
    equals(/stream=([\w]+)/.exec($("#video .ui-widget-content embed").attr('flashvars'))[1], 'channel_1');
    equals(/token=([\w]+)/.exec($("#video .ui-widget-content embed").attr('flashvars'))[1], '123456');
});

jackTest("on publish, handle video.stream.new", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    $("#video").video({ucemeeting: ucemeeting,
                       "domain": "example.org"});
    $("#video").video("triggerUceEvent", {type: "video.stream.new",
                                          metadata : {token : "123456",
                                                      channel : "channel_1"}});
    equals($("#video .ui-widget-content embed").size(), 1);
    $("#video").video("publish");
    equals(/stream=([\w]+)/.exec($("#video .ui-widget-content embed").attr('flashvars'))[1], 'channel_1');
    equals(/token=([\w]+)/.exec($("#video .ui-widget-content embed").attr('flashvars'))[1], '123456');
});

test("publish then stop a video stream", function() {
    $("#video").video();
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Stop publish", "label has changed");
    equals($("#video .ui-widget-content embed").attr('src'), '/publish_video.swf', 'src is publish_video.swf');
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Publish", "label has changed");
    equals($("#video .ui-widget-content embed").attr('src'), '/receive_video.swf', 'src is receive_video.swf');
});

jackTest("desactivate/activate the publish button when a stream start and stop", function () {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.uid = 'john';
    $("#video").video({ucemeeting: ucemeeting});
    $("#video").video("triggerUceEvent",{type: "video.stream.start",
                                         metadata: {broadcaster: "root"}});
    ok($("#video .ui-button").button("option", "disabled"), "button should be disabled");
    // test click on disabled button
    $("#video .ui-button").click();
    equals($("#video .ui-button").text(), "Publish", "label should not be updated");
    // trigger stream.stop
    $("#video").video("triggerUceEvent",{type: "video.stream.stop",
                                         metadata: {broadcaster: "root"}});
    ok(!$("#video .ui-button").button("option", "disabled"), "button should be enabled");
});

jackTest("doesn't desactivate the publish button when a stream started by the same uid", function () {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.uid = 'root';
    $("#video").video({ucemeeting: ucemeeting});
    $("#video").video("triggerUceEvent",{type: "video.stream.start",
                                         metadata: {broadcaster: "root"}});
    ok(!$("#video .ui-button").button("option", "disabled"), "button should be enabled");
});
