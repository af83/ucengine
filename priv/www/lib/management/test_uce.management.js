module("uce.management", {teardown: function() {
    $('#management').management('destroy');
}});

Factories.addRosterEvent = function(from) {
    return {
        type: "internal.roster.add",
        from: from
    };
}

Factories.updateNicknameEvent = function(from, nickname) {
    return {
        type: "roster.nickname.update",
        from: from,
        metadata: {nickname: nickname}
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
                } else if (eventName == "roster.nickname.update") {
                    that.callback_nickname_update = callback;
                }
            }
        };
        $('#management').management({
            ucemeeting: this.ucemeeting,
            uceclient: {uid: 'chuck'},
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
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Unnamed 1');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Unnamed 1');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Unnamed 1');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 0);
});

test("handle roster.nickame.update event", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_nickname_update(Factories.updateNicknameEvent('chuck', 'Chuck Norris'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Chuck Norris');
});

jackTest("push a roster.nickname.update event after changing our nickname", function() {
    var ucemeeting = jack.create("ucemeeting", ['push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata) {
            equals(type, "roster.nickname.update");
            equals(metadata.nickname, "Chuck Norris");
        });
    ucemeeting.on = this.ucemeeting.on;

    $('#management').management({
        ucemeeting: ucemeeting,
        uceclient: {uid: 'chuck'}
    });

    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Unnamed 1');
    $("#management .ui-management-roster li:eq(0)").click();
    $("#management .ui-management-roster li:eq(0) input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) input").trigger("blur");
});

jackTest("don't push an event if setting the same nickname", function() {
    var ucemeeting = jack.create("ucemeeting", ['push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time");
    ucemeeting.on = this.ucemeeting.on;

    $('#management').management({
        ucemeeting: ucemeeting,
        uceclient: {uid: 'chuck'}
    });

    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster li:eq(0)").text(), 'Unnamed 1');
    $("#management .ui-management-roster li:eq(0)").click();
    $("#management .ui-management-roster li:eq(0) input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) input").trigger("blur");

    $("#management .ui-management-roster li:eq(0)").click();
    $("#management .ui-management-roster li:eq(0) input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) input").trigger("blur");
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
        uceclient: {uid: 'chuck'}
    });

    this.callback_roster_add(Factories.addRosterEvent('brucelee'));
    $("#management .ui-management-roster li:eq(0)").click();    
});
