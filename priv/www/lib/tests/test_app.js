module('app', {
    setup: function() {
        this.app = $.sammy('#sammy_anchor', sammyapp);
        $.mockjax({
            url : '/api/' + uce.version + '/presence/',
            responseText: { "result": "409832095702309473209" }
        });
        $.mockjax({
            url : '/api/' + uce.version + '/infos/',
            responseText: {"result": {"domain": "localhost",
                                      "metadata": {"description":"af83m\u00e9dia specializes in digital communication. Our mission is to design and manage online content and communities.",
                                                   "logo":"af83.png",
                                                   "htags":"af83"}}}
        });
        $.mockjax({
            url: '/api/' + uce.version + '/meeting/opened',
            responseText: {"result":[{"name":"demo","start_date":1287493374119,"end_date":"never","roster":["root"],"metadata":{"description":"U.C.Engine demo meetup"}},{"name":"demo2","start_date":1287493374120,"end_date":"never","roster":[],"metadata":{"description":"Meeting R&D"}}]}
        });
        $.mockjax({
            url : '/api/' + uce.version + '/meeting/closed',
            responseText: {"result":[{"name":"agoroom","start_date":1287493374120,"end_date":"1287493374120","roster":["root"],"metadata":{"description":"Meeting agoroom"}}]}
        });
        $.mockjax({
            url: '/api/' + uce.version + '/meeting/upcoming',
            responseText: {"result":[]}
        });
    },

    teardown: function() {
        $.mockjaxClear();
        $('#sammy_anchor').unbind('DOMSubtreeModified');
        this.app.unload();
        window.location.hash = '#/';
        $('#sammy_anchor').find('*').remove();
    }
});

function whenNotEmpty(el, callback) {
    setTimeout(function() {
        if (el.children().size() != 0)
            callback()
        else
            whenNotEmpty(el, callback);
    }, 50);
}

test("should load home page", function() {
    stop();
    whenNotEmpty($('#sammy_anchor'), function(e) {
        start();
        equals(document.title, 'Home - U.C.Engine', "document.title");
        ok($('nav .page li:eq(0)').hasClass('on'), "hav class 'on' on first li");
        ok(!$('nav .page li:eq(1)').hasClass('on'), "no class 'on' on second li");
        $(this).unbind();
    });
    this.app.run('#/');
});

test("should load about page", function() {
    stop();
    expect(3);
    window.location.hash = '#/about';
    whenNotEmpty($('#sammy_anchor'), function(e) {
        start();
        equals(document.title, 'About - U.C.Engine', "document.title");
        ok(!$('nav .page li:eq(0)').hasClass('on'), "not class 'on' on first li");
        ok($('nav .page li:eq(1)').hasClass('on'), "has class 'on' on second li");
        $(this).unbind();
    });
    this.app.run('#/about');
});
