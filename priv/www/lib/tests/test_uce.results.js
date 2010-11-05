module("uce.results", {
    teardown: function() {
        $("#results").results("destroy");
    }
});

test("show 5 events", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    $("#results").results({ucemeeting: ucemeeting});
    $("#results").results("handleSearchResultsEvent", [{"type": "terminator",
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
								     "from": "terminator"}}]);

    equals($("#results div ul li").size(), 5);
    equals($("#results div ul > li:eq(0)").text(), "terminator at 00:00:00:I'll be back");
});
