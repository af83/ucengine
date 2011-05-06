Factories.updateMeetingEvent = function() {
    return {
        type: "internal.meeting.update"
    };
}

module("uce.information", {
    setup: function() {
        var that = this;
        this.ucemeeting = {
            name: "testmeeting",
            on: function(eventName, callback) {
                if (eventName == "internal.meeting.update") {
                    that.callback_meeting_update = callback;
                }
            },
            get: function() {}
        };
        this.uceclient = {
            user: {
                can: function(uid, action, object, conditions, location, callback) {
                    callback(undefined, true, undefined);
                }
            }
        }
        $('#information').information({
            ucemeeting: this.ucemeeting,
            uceclient: this.uceclient
        });
    },
    teardown: function() {
        $('#information').information('destroy');
    }});

jackTest("the widget check the user right to update meeting", function () {
    var user = jack.create("user", ['can']);
    jack.expect("user.can")
        .exactly("1 time")
        .mock(function(uid, action, object, conditions, location, callback) {
            equals(uid, "chuck", "User id"),
            equals(action, "update", "Action"),
            equals(object, "meeting", "Object"),
            equals(location, "testmeeting", "Location");
            callback(undefined, true, undefined);
        });

    var uceclient = {uid: 'chuck'};
    uceclient.user = user;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: this.ucemeeting,
                                   uceclient: uceclient});
});

jackTest("the widget retrieves the meeting's informations", function () {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting, uceclient: this.uceclient});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-description .ui-information-value').text(), 'default description', "Meeting description");
});

jackTest("display only filled fields if not owner", function () {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;

    var user = jack.create("user", ['can']);
    jack.expect("user.can")
        .exactly("1 time")
        .mock(function(uid, action, object, conditions, location, callback) {
            callback(undefined, false, undefined);
        });

    var uceclient = {};
    uceclient.user = user;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: uceclient,
                                   fields: {'name': {title: "Meeting Name",
                                                     placeholder: ""},
                                            'unfilled_field': {title: "Test Field",
                                                               placeholder: "Test placeholder"}}});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-unfilled_field .ui-information-value').size(), 0, "The unfilled field has no value");
    equals($('#information .ui-information-unfilled_field .ui-information-placeholder').size(), 0, "The unfilled field has no placeholder");
});

jackTest("display all editable fields when owner", function() {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;

    var user = jack.create("user", ['can']);
    jack.expect("user.can")
        .exactly("1 time")
        .mock(function(uid, action, object, conditions, location, callback) {
            callback(undefined, true, undefined);
        });

    var uceclient = {};
    uceclient.user = user;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: uceclient,
                                   fields: {'name': {title: "Meeting Name",
                                                     placeholder: ""},
                                            'unfilled_field': {title: "Test Field",
                                                               placeholder: "Test placeholder"}}});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-unfilled_field .ui-information-value').size(), 0, "The unfilled field has no value");
    equals($('#information .ui-information-unfilled_field .ui-information-placeholder').text(), "Test placeholder", "The unfilled field have a placeholder");
});

jackTest("update the widget after receiving internal.meeting.update", function () {
    var ucemeeting = jack.create("ucemeeting", ['get']);

    var that = this;
    that._description = "default description";
    jack.expect("ucemeeting.get")
        .exactly("2 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", that._description), undefined);
            that._description = "new description";
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting, uceclient: this.uceclient});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-description .ui-information-value').text(), 'default description', "Meeting description");

    this.callback_meeting_update(Factories.updateMeetingEvent());

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-description .ui-information-value').text(), 'new description', "Meeting description");
});

jackTest("send an internal.meeting.update after updating the meeting informations", function() {
    var ucemeeting = jack.create("ucemeeting", ['get', 'update']);

    jack.expect("ucemeeting.get")
        .exactly("2 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description", undefined));
        });
    jack.expect("ucemeeting.update")
        .exactly("1 time")
        .mock(function(start, end, metadata, callback) {
            equals(start, 0, "Meeting start date");
            equals(end, 'never', "Meeting end date");
            equals(metadata.description, "new description", "Metadata event description");
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting, uceclient: this.uceclient});

    equals($('#information .ui-information-description .ui-information-value').text(), "default description", "Meeting description");
    $('#information .ui-information-description .ui-information-value').click();
    $('#information .ui-information-description .ui-information-value input').val("new description");
    $('#information .ui-information-description .ui-information-value input').trigger("blur");
})

test("test that long text are reduced", function() {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: this.uceclient,
                                   maxlength: 6});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemee', "Meeting name");
    equals($('#information .ui-information-name .ui-information-size-toggle').text(), 'More', "The toggle button");
    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting description");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");
});

test("test that the 'More' button display all the text", function() {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: this.uceclient,
                                   maxlength: 6});

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemee', "Meeting name");
    equals($('#information .ui-information-name .ui-information-size-toggle').text(), 'More', "The toggle button");
    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting description");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");

    $('#information .ui-information-description .ui-information-size-toggle').click();

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemee', "Meeting name");
    equals($('#information .ui-information-name .ui-information-size-toggle').text(), 'More', "The toggle button");
    equals($('#information .ui-information-description .ui-information-value').text(), 'default description', "Meeting description");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'Less', "The toggle button");

    $('#information .ui-information-name .ui-information-size-toggle').click();

    equals($('#information .ui-information-name .ui-information-value').text(), 'ucemeeting', "Meeting name");
    equals($('#information .ui-information-name .ui-information-size-toggle').text(), 'Less', "The toggle button");
    equals($('#information .ui-information-description .ui-information-value').text(), 'default description', "Meeting description");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'Less', "The toggle button");
});

test("test that the 'Less' button collapses the text", function() {
    var ucemeeting = jack.create("ucemeeting", ['get']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: this.uceclient,
                                   maxlength: 6});

    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting name");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");

    $('#information .ui-information-description .ui-information-size-toggle').click();

    equals($('#information .ui-information-description .ui-information-value').text(), 'default description', "Meeting description");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'Less', "The toggle button");

    $('#information .ui-information-description .ui-information-size-toggle').click();

    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting name");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");
});

test("test that the editable text is the full text", function() {
    var ucemeeting = jack.create("ucemeeting", ['get', 'update']);
    jack.expect("ucemeeting.get")
        .exactly("1 time")
        .mock(function(callback) {
            callback(undefined, Factories.createMeeting(0, "never", "default description"), undefined);
        });

    ucemeeting.on = this.ucemeeting.on;
    $('#information').information('destroy');
    $('#information').information({ucemeeting: ucemeeting,
                                   uceclient: this.uceclient,
                                   maxlength: 6});

    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting name");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");

    $('#information .ui-information-description .ui-information-value').click();
    equals($('#information .ui-information-description .ui-information-value input').val(), 'default description', "Meeting description");
    $('#information .ui-information-description .ui-information-value input').trigger("blur");

    equals($('#information .ui-information-description .ui-information-value').text(), 'defaul', "Meeting name");
    equals($('#information .ui-information-description .ui-information-size-toggle').text(), 'More', "The toggle button");
});
