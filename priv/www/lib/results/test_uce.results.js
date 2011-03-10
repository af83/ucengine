module("uce.results", {
    teardown: function() {
        $("#results").results("destroy");
    }
});

test("create and destroy", function() {
    var ucemeeting = jack.create("ucemeeting", ['on']);
    $("#results").results({ucemeeting: ucemeeting});
    ok($("#results").hasClass('ui-widget'), 'has class ui-widget');
    ok($("#results").hasClass('ui-results'), 'has class ui-results');
    $("#results").results("destroy");
    ok(!$("#results").hasClass('ui-widget'), 'has class ui-widget');
    ok(!$("#results").hasClass('ui-results'), 'has class ui-results');
});

test("show 5 events", function() {
    var ucemeeting = jack.create("ucemeeting", ['on']);
    $("#results").results({ucemeeting: ucemeeting});
    $("#results").results("triggerUceEvent", {type: "internal.search.result",
                                              metadata: {
                                                  events : [{"type": "terminator",
                                                             "datetime": 0,
                                                             "metadata": {"text": "I'll be back",
                                                                          "from": "terminator"}},
                                                            {"type": "terminator",
                                                             "datetime": 0,
                                                             "metadata": {"text": "I'll be back",
                                                                          "from": "terminator"}},
                                                            {"type": "terminator",
                                                             "datetime": 0,
                                                             "metadata": {"text": "I'll be back",
                                                                          "from": "terminator"}},
                                                            {"type": "terminator",
                                                             "datetime": 0,
                                                             "metadata": {"text": "I'll be back",
                                                                          "from": "terminator"}},
                                                            {"type": "terminator",
                                                             "datetime": 0,
                                                             "metadata": {"text": "I'll be back",
                                                                          "from": "terminator"}}]}});

    equals($("#results div ul li").size(), 5);
    equals($("#results div ul > li:eq(0)").text(), "terminator at 00:00:00:I'll be back");
});
