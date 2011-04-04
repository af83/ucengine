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

Factories.addUserRoleEvent = function(from, user, role) {
    return {
        type: "internal.user.role.add",
        from: from,
        metadata: {user: user,
                   role: role}
    };
}

Factories.deleteUserRoleEvent = function(from, user, role) {
    return {
        type: "internal.user.role.delete",
        from: from,
        metadata: {user: user,
                   role: role}
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
                } else if (eventName == "internal.user.role.add") {
                    that.callback_role_add = callback;
                } else if (eventName == "internal.user.role.delete") {
                    that.callback_role_delete = callback;
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
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 0);
});

test("handle internal.user.role.add event", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    equals($("#management .ui-management-roster li:eq(0) .ui-management-role").text(), 'You');

    this.callback_role_add(Factories.addUserRoleEvent('god', 'chuck', 'speaker'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    equals($("#management .ui-management-roster li:eq(0) .ui-management-role").text(), 'Speaker');
});

test("handle internal.user.role.delete event", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_role_add(Factories.addUserRoleEvent('god', 'chuck', 'speaker'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    equals($("#management .ui-management-roster li:eq(0) .ui-management-role").text(), 'Speaker');
    this.callback_role_delete(Factories.deleteUserRoleEvent('god', 'chuck', 'speaker'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    equals($("#management .ui-management-roster li:eq(0) .ui-management-role").text(), 'You');
});

test("sort roster correctly", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('speaker'));
    this.callback_roster_add(Factories.addRosterEvent('participant1'));
    this.callback_roster_add(Factories.addRosterEvent('participant2'));
    this.callback_roster_add(Factories.addRosterEvent('owner'));

    this.callback_role_add(Factories.addUserRoleEvent('god', 'owner', 'owner'));
    this.callback_role_add(Factories.addUserRoleEvent('god', 'speaker', 'speaker'));

    this.callback_nickname_update(Factories.updateNicknameEvent('chuck', 'Z'));
    this.callback_nickname_update(Factories.updateNicknameEvent('speaker', 'Y'));
    this.callback_nickname_update(Factories.updateNicknameEvent('participant1', 'B'));
    this.callback_nickname_update(Factories.updateNicknameEvent('participant2', 'A'));
    this.callback_nickname_update(Factories.updateNicknameEvent('owner', 'X'));

    equals($("#management .ui-management-roster").children().size(), 5);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Z');
    equals($("#management .ui-management-roster li:eq(0) .ui-management-role").text(), 'You');
    equals($("#management .ui-management-roster li:eq(1) .ui-management-user").text(), 'X');
    equals($("#management .ui-management-roster li:eq(1) .ui-management-role").text(), 'Owner');
    equals($("#management .ui-management-roster li:eq(2) .ui-management-user").text(), 'Y');
    equals($("#management .ui-management-roster li:eq(2) .ui-management-role").text(), 'Speaker');
    equals($("#management .ui-management-roster li:eq(3) .ui-management-user").text(), 'A');
    equals($("#management .ui-management-roster li:eq(3) .ui-management-role").text(), '');
    equals($("#management .ui-management-roster li:eq(4) .ui-management-user").text(), 'B');
    equals($("#management .ui-management-roster li:eq(4) .ui-management-role").text(), '');
});

test("handle roster.nickame.update event", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_nickname_update(Factories.updateNicknameEvent('chuck', 'Chuck Norris'));
    equals($("#management .ui-management-roster").children().size(), 1);
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Chuck Norris');
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
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    $("#management .ui-management-roster li:eq(0) .ui-management-user").click();
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").trigger("blur");
});

jackTest("don't push an event if setting the same nickname or an empty nickname", function() {
    var ucemeeting = jack.create("ucemeeting", ['push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time");
    ucemeeting.on = this.ucemeeting.on;

    $('#management').management({
        ucemeeting: ucemeeting,
        uceclient: {uid: 'chuck'}
    });

    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Unnamed 1');
    $("#management .ui-management-roster li:eq(0) .ui-management-user").click();
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").trigger("blur");

    $("#management .ui-management-roster li:eq(0) .ui-management-user").click();
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").val("Chuck Norris");
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").trigger("blur");

    $("#management .ui-management-roster li:eq(0) .ui-management-user").click();
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").val("");
    $("#management .ui-management-roster li:eq(0) .ui-management-user input").trigger("blur");
    equals($("#management .ui-management-roster li:eq(0) .ui-management-user").text(), 'Chuck Norris');
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
    $("#management .ui-management-roster li:eq(0) .ui-management-user").click();
});
