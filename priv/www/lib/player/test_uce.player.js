module("uce.player", {
    setup: function() {
        $("#player").bind('error', function(e) {
            if (e.target.localName == 'source')
                e.stopPropagation();
        });
    },
    teardown: function() {
        $("#player video").unbind();
        $("#player").unbind();
        $("#player").player("destroy");
    }});

var videourl = "http://encre.2metz.fr/windowsill";

test("create html, and destroy", function() {
    $("#player").player({src: videourl});
    ok($("#player").hasClass("ui-widget ui-player"), "has class ui-widget ui-player");
    equals($("#player .ui-widget-header").text(), "Video");
    equals($("#player .ui-widget-content video").size(), 1);
    equals($("#player video source[name=mp4]").size(), 1);
    equals($("#player video source[name=webm]").size(), 1);
    $("#player").player("destroy");
    ok(!$("#player").hasClass("ui-widget ui-player"), "should not have class ui-widget ui-player");
    equals($("#player > *").size(), 0);
});

test("customize with src and width/height", function() {
    $("#player").player({width  : 400,
                         height : 200,
                         src    : videourl});
    equals($("#player video source[name=mp4]").attr('src'), videourl+'.mp4');
    equals($("#player video source[name=webm]").attr('src'), videourl +'.webm');
    equals($("#player video").attr('width'), 400);
    equals($("#player video").attr('height'), 200);
});

test("dynamic updated options", function() {
    $("#player").player({src:'http://content.bitsontherun.com/videos/nPripu9l-60830'});
    $("#player").player("option", {"width" : 100,
                                  "height" : 150,
                                  "src"    : videourl});
    equals($("#player video source[name=mp4]").attr('src'), videourl+'.mp4');
    equals($("#player video source[name=webm]").attr('src'), videourl+'.webm');
    equals($("#player video").attr('width'), 100);
    equals($("#player video").attr('height'), 150);
});

test("can play video", function() {
    $("#player").player({width : 400,
                        height : 200,
                        src    : videourl});
    equals($("#player video").get(0).paused, true);
    $('#player').player("play");
    equals($("#player video").get(0).paused, false);
});

test("can pause video", function() {
    $("#player").player({width : 400,
                        height : 200,
                        src    : videourl});
    equals($("#player video")[0].paused, true);
    $('#player').player("play");
    equals($("#player video")[0].paused, false);
    $('#player').player("pause");
    equals($("#player video")[0].paused, true);
});

test("can stop video", function() {
    stop();
    $("#player").player({src : videourl});
    $("#player video").bind("canplay", function() {
        start();
        $("#player video").unbind("canplay");
        equals($("#player video")[0].paused, true);
        $('#player').player("play");
        equals($("#player video")[0].paused, false);
        $('#player').player("stop");
        equals($("#player video")[0].paused, true);
        equals($("#player video")[0].currentTime, 0);
    });
});

test("can seek video", function() {
    expect(1);
    stop();
    $("#player").player({width  : 400,
                         height : 200,
                         src    : videourl});
    $("#player video").bind("canplay", function() {
        $("#player video").unbind("canplay");
        $('#player').player("play");
        $('#player').player("seek", 12000);
        $('#player video').bind('timeupdate', function() {
            $('#player video').unbind('timeupdate');
            equals($("#player video").get(0).currentTime, 12);
            start();
        });
    });
});
