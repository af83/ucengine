module("uce.activity", {
    teardown: function() {
        $("#activity").activity("destroy");
    }
});

test("create element", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.start_date = 0;
    $("#activity").activity({ucemeeting: ucemeeting});
    $("#activity").activity("handleSearchResultsEvent", []);
    equals($("#activity > div").size(), 2);
});

test("show 4 bars", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind']);
    ucemeeting.start_date = 0;
    $("#activity").activity({ucemeeting: ucemeeting});
    $("#activity").activity("handleSearchResultsEvent", [{"type": "chuck_norris", "datetime":"61000"},    // 1 min 1 sec
                                                         {"type": "jacky_chan",   "datetime":"62000"},    // 1 min 2 sec
                                                         {"type": "jacky_chan",   "datetime":"63000"},    // 1 min 3 sec
                                                         {"type": "jacky_chan",   "datetime":"121000"},   // 2 min 1 sec
                                                         {"type": "bruce_lee",    "datetime":"181000"}]); // 3 min 1 sec
    // The graph should have 3 bars
    equals($("#activity svg rect").size(), 3);
});
