module("uce.video", {teardown: function() {
    $("#video").video("destroy");
}});

test("create embed tag, and destroy", function() {
    $("#video").video();
    equals($("#video embed").size(), 1);
    ok($("#video").hasClass('ui-widget'), 'has class ui-widget');
    ok($("#video").hasClass('ui-video'), 'has class ui-video');
    $("#video").video("destroy");
    ok(!$("#video").hasClass('ui-widget'), 'has class ui-widget');
    ok(!$("#video").hasClass('ui-video'), 'has class ui-video');
    equals($("#video embed").size(), 0);
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
    equals($("#video embed").attr('width'), 400);
    equals($("#video embed").attr('height'), 200);
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
    equals($("#video embed").attr('width'), 100);
    equals($("#video embed").attr('height'), 150);
});

test("publish video", function() {
    $("#video").video();
    $("#video").video("publish");
    equals(/(publish_video.swf)/.exec($("#video embed").attr('src'))[1], 'publish_video.swf');
});

test("publish video and set option", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("option", "width", "100");
    equals($("#video embed").attr('width'), 100);
    equals(/(publish_video.swf)/.exec($("#video embed").attr('src'))[1], 'publish_video.swf');
});

test("receive video", function() {
    $("#video").video();
    $("#video").video("publish");
    $("#video").video("receive");
    equals(/(receive_video.swf)/.exec($("#video embed").attr('src'))[1], 'receive_video.swf');
});

jackTest("handle video.stream.new", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
     jack.expect("ucemeeting.bind")
        .exactly("1 time").mock(function(eventName) {
            equals(eventName, 'video.stream.new');
        });
    $("#video").video({ucemeeting: ucemeeting,
                       "domain": "example.org"});
    equals($("#video embed").size(), 0);
    $("#video").video("handleEvent", {"metadata" : {"token" : "123456",
                                                    "channel" : "channel_1"}});
    equals($("#video embed").size(), 1);
    equals(/stream=(\w+)/.exec($("#video embed").attr('flashvars'))[1], 'channel_1');
    equals(/token=(\w+)/.exec($("#video embed").attr('flashvars'))[1], '123456');
});
