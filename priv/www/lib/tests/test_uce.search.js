module("uce.search", {teardown: function() {
    $('#search').search('destroy');
}});

test("create basic structure", function() {
    $('#search').search();
    equals($('#search form').size(), 1);
    equals($('#search form input').size(), 2);
    ok($('#search').hasClass('ui-widget'), 'has class ui-widget');
    ok($('#search').hasClass('ui-search'), 'has class ui-search');
    $('#search').search('destroy');
    equals($('#search form').size(), 0);
    ok(!$('#search').hasClass('ui-widget'), 'has class ui-widget');
    ok(!$('#search').hasClass('ui-search'), 'has class ui-search');
});

jackTest("submit keywords and call the UCE API", function() {
    var ucemeeting = jack.create("ucemeeting", ['getEvents', 'trigger']);
    jack.expect("ucemeeting.getEvents")
	.exactly("1 time")
	.mock(function(params, callback) {
	    callback([{'type': 'coucou'}]);
	});

    jack.expect("ucemeeting.trigger")
	.exactly("1 time");

    $('#search').search({ucemeeting: ucemeeting});
    $('#search form input[name=keywords]').val('toto');
    $('#search form').submit();
    start();
});
