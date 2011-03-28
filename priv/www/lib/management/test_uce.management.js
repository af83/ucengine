module("uce.management", {teardown: function() {
    $('#management').management('destroy');
}});

Factories.newHashTagEvent = function(params) {
    return {
        type: "twitter.hashtag.add",
        metadata: {
            hashtag: params.hashtag
        }
    } ;
}

Factories.newTweetEvent = function(p) {
    var params = $.extend({hastags: '#chuck,#plop',
                           text   : 'plop',
                           from   : 'Chuck norris'}, p);
    return {
        type: "twitter.tweet.new",
        metadata: {
            hashtags: params.hashtags,
            from: params.from,
            text: params.text
        }
    } ;
}

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

Factories.newManagementEvent = function(from, text) {
    return {
        type: "management.message.new",
        from: from,
        datetime: 42424242424242,
        metadata: {
            text: text,
            lang: 'fr'
        }
    };
}

Factories.newTranslationEvent = function(from, text, lang) {
    return {
        type: "management.translation.new",
        from: from,
        metadata: {
            text: text,
            lang: lang,
            from: "chuck"
        }
    };
}

test("create some elements", function() {
    $('#management').management();
    ok($('#management').hasClass("ui-management"), "should have class ui-management");
    ok($('#management').hasClass("ui-widget"), "should have class ui-widget");
    equals($('#management').children().size(), 2);
    equals($("#management .ui-widget-content").children().size(), 2);
});

test("destroy delete all elements", function() {
    $('#management').management();
    $('#management').management("destroy");
    ok(!$('#management').hasClass("ui-management"), "should not have class ui-management");
    ok(!$('#management').hasClass("ui-widget"), "should not have class ui-widget");
    equals($('#management > *').size(), 0);
});

test("handle minus mode", function() {
    $('#management').management().management('reduce');
    ok($('#management').hasClass("ui-management-reduced"), "should have class ui-management-reduced");
});

test("toggle to big mode", function() {
    $('#management').management().management("expand");
    ok($('#management').hasClass("ui-management-expanded"), "should have class ui-management-expanded");
});

jackTest("add hashtag", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(eventname, metadata) {
            equals(eventname, "twitter.hashtag.add");
            equals(metadata.hashtag, "#pouet");
            equals($('#management').find('form.ui-management-hashtag-form input[type="text"]').val(), "");
        });
    $('#management').management({
        ucemeeting: ucemeeting
    });
    $('#management').find('form.ui-management-hashtag-form input[type="text"]').val("pouet");
    $('#management').find('form.ui-management-hashtag-form').submit();
});

module("uce.management", {
    setup: function() {
        var that = this;
        var ucemeeting = {
            on: function(eventName, callback) {
                if (eventName == "twitter.hashtag.add") {
                    that.callback_hashtag = callback;
                } else if (eventName == "twitter.tweet.new") {
                    that.callback_tweet = callback;
                } else if (eventName == "internal.roster.add") {
                    that.callback_roster_add = callback;
                } else if (eventName == "internal.roster.delete") {
                    that.callback_roster_delete = callback;
                } else if (eventName == "management.message.new") {
                    that.callback_management = callback;
                } else if (eventName == "management.translation.new") {
                    that.callback_translation = callback;
                }
            }
        };
        $('#management').management({
            ucemeeting: ucemeeting,
            dock: '#management-dock'
        });
    },
    teardown: function() {
        $('#management').management('destroy');
    }});

test("handle new hash event", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:last").text(), '#chuck (0)');
});

test("handle duplicate hashtag", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:last").text(), '#chuck (0)');
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:last").text(), '#chuck (0)');
});

test("sort hashtag by alphabetical order", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#atomic'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#TED'}));
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems").children().size(), 3);
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:eq(0)").text(), '#atomic (0)');
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:last").text(), '#ted (0)');
});

test("handle tweets events", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck,#norris'}));

    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:eq(0)").text(), '#chuck (2)');
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems li:last").text(), '#norris (1)');
});

test("can show all tweets", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#management .ui-management-selector-hashtags.ui-management-selector-all").click();
    equals($("#management .ui-management-container[name='hashtag:all']").children().size(), 2);
    ok(!$('#management .ui-management-container[name="hashtag:all"]').hasClass("ui-management-current"), "should have class ui-management-current");
    equals($("#management .ui-management-container[name='hashtag:all'] .ui-management-list li:eq(0) .ui-management-message-text")
           .text(), "plop");
});

test("can show hastag tweet and go back", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#management .ui-management-selector-hashtags.ui-management-selector-elems li:eq(0)").click();
    ok($('#management .ui-management-container[name="hashtag:#chuck"]').hasClass("ui-management-current"), 
       "should have class ui-management-current");

    ok(!$('#management .ui-management-container[name="hashtag:#norris"]').hasClass("ui-management-current"),
       "should not have class ui-management-current");

    equals($('#management .ui-management-container[name="hashtag:#chuck"] .ui-management-list').children().size(), 1);

    $('#management .ui-management-container .ui-management-container-back').click();

    ok(!$('#management .ui-management-container[name="hashtag:#chuck"]').hasClass("ui-management-current"),
       "should not have class ui-management-current");
});

test("can change language", function() {
    $("#management .ui-management-flag.ui-management-lang-fr").click();
    ok($('#management .ui-management-flag.ui-management-lang-fr').hasClass("ui-state-highlight"), 
           "should have class ui-state-highlight");
    ok($('#management .ui-management-container[name=conversation:all:fr]').hasClass("ui-management-current"), 
           "should have class ui-state-current");

    $("#management .ui-management-flag.ui-management-lang-en").click();
    ok($('#management .ui-management-flag.ui-management-lang-en').hasClass("ui-state-highlight"), 
           "should have class ui-state-highlight");
    ok(!$('#management .ui-management-flag.ui-management-lang-fr').hasClass("ui-state-highlight"), 
           "should not have class ui-state-highlight");
    ok($('#management .ui-management-container[name=conversation:all:en]').hasClass("ui-management-current"), 
           "should have class ui-state-current");
});

test("clear management", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));

    $('#management').management('clear');
    equals($("#management .ui-management-containers").children().size(), 0);
    equals($("#management .ui-management-selector-hashtags.ui-management-selector-elems").children().size(), 0);
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems").children().size(), 0);
});

test("handle join", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems li:eq(0)").text(), 'chuck');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems li:eq(0)").text(), 'chuck');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems").children().size(), 1);
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems li:eq(0)").text(), 'chuck');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#management .ui-management-selector-conversations.ui-management-selector-elems").children().size(), 0);
});

test("test messages", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_management(Factories.newManagementEvent('chuck', 'hello'));

    equals($('#management .ui-management-container[name="conversation:all:fr"] .ui-management-list')
           .children().size(), 1);
    equals($('#management .ui-management-container[name="conversation:all:fr"] .ui-management-list li:eq(0) .ui-management-message-from')
           .text(), "chuck");
    equals($('#management .ui-management-container[name="conversation:all:fr"] .ui-management-list li:eq(0) .ui-management-message-text')
           .text(), "hello");
});

test("test message notification", function() {
    this.callback_management(Factories.newManagementEvent('chuck', 'hello'));
    equals($("#management-dock .ui-widget-dock-notification").text(), "1");
});

test("test translation", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello', 'en'));
    equals($('#management .ui-management-container[name="conversation:all:en"] .ui-management-list')
           .children().size(), 1);
    equals($('#management .ui-management-container[name="conversation:all:en"] .ui-management-list li:eq(0) .ui-management-message-from')
           .text(), "chuck");
    equals($('#management .ui-management-container[name="conversation:all:en"] .ui-management-list li:eq(0) .ui-management-message-text')
           .text(), "hello");
});

test("can show managementroom and go back", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_management(Factories.newManagementEvent('chuck', 'hello'));

    $("#management .ui-management-selector-conversations.ui-management-selector-all li:eq(0)").click();
    ok($('#management .ui-management-container[name="conversation:all:fr"]').hasClass("ui-management-current"), 
       "should have class ui-management-current");

    equals($('#management .ui-management-container[name="conversation:all:fr"] .ui-management-list').children().size(), 1);

    $('#management .ui-management-container .ui-management-container-back').click();

    ok(!$('#management .ui-management-container[name="conversation:all:fr"]').hasClass("ui-management-current"), 
       "should not have class ui-management-current");
});
