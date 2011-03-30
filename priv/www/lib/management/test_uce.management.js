module("uce.management", {teardown: function() {
    $('#management').management('destroy');
}});

Factories.addRosterEvent = function(from) {
    return {
        type: "internal.roster.add",
        from: from
    };
}

Factories.deleteRosterEvent = function(from) {
    return {
        type: "internal.roster.delete",
        from: from
    };
}

test("create some elements", function() {
    $('#management').management();
    ok($('#management').hasClass("ui-management"), "should have class ui-management");
    ok($('#management').hasClass("ui-widget"), "should have class ui-widget");
    equals($('#management').children().size(), 2);
    equals($("#management .ui-widget-content").children().size(), 1);
});

test("destroy delete all elements", function() {
    $('#management').management();
    $('#management').management("destroy");
    ok(!$('#management').hasClass("ui-management"), "should not have class ui-management");
    ok(!$('#management').hasClass("ui-widget"), "should not have class ui-widget");
    equals($('#management > *').size(), 0);
});

module("uce.management", {
    setup: function() {
        var that = this;
        this.ucemeeting = {
            on: function(eventName, callback) {
                if (eventName == "internal.roster.add") {
                    that.callback_roster_add = callback;
                } else if (eventName == "internal.roster.delete") {
                    that.callback_roster_delete = callback;
                }
            }
        };
        $('#management').management({
            ucemeeting: this.ucemeeting,
            dock: '#management-dock'
        });
    },
    teardown: function() {
        $('#management').management('destroy');
    }});

test("clear the widget", function() {
    $('#management').management('clear');
    equals($("#management .ui-management-roster").children().size(), 0);
});

test("handle join", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'chuck');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'chuck');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'chuck');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 0);
});

jackTest("send a chat.private.start event when clicking on a user", function() {
    expect(3);
    var ucemeeting = jack.create("ucemeeting", ['trigger']);
    jack.expect("ucemeeting.trigger")
        .exactly("1 time")
        .mock(function(event) {
            equals(event.type, "chat.private.start");
            equals(event.metadata.interlocutor, "brucelee");
        });
    ucemeeting.on = this.ucemeeting.on;

    $('#management').management({
        ucemeeting: ucemeeting,
        me: 'chuck'
    });

    this.callback_roster_add(Factories.addRosterEvent('brucelee'));
    $("#management .ui-management-roster li:eq(0)").click();    
});
