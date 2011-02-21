module("uce.chat", {teardown: function() {
    $('#chat').chat('destroy');
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

Factories.newChatEvent = function(from, text) {
    return {
        type: "chat.message.new",
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
        type: "chat.translation.new",
        from: from,
        metadata: {
            text: text,
            lang: lang,
            from: "chuck"
        }
    };
}

test("create some elements", function() {
    $('#chat').chat();
    ok($('#chat').hasClass("ui-chat"), "should have class ui-chat");
    ok($('#chat').hasClass("ui-widget"), "should have class ui-widget");
    equals($('#chat').children().size(), 2);
    equals($("#chat .ui-widget-content").children().size(), 2);
});

test("destroy delete all elements", function() {
    $('#chat').chat();
    $('#chat').chat("destroy");
    ok(!$('#chat').hasClass("ui-chat"), "should not have class ui-chat");
    ok(!$('#chat').hasClass("ui-widget"), "should not have class ui-widget");
    equals($('#chat > *').size(), 0);
});

test("handle minus mode", function() {
    $('#chat').chat().chat('reduce');
    ok($('#chat').hasClass("ui-chat-reduced"), "should have class ui-chat-reduced");
});

test("toggle to big mode", function() {
    $('#chat').chat().chat("expand");
    ok($('#chat').hasClass("ui-chat-expanded"), "should have class ui-chat-expanded");
});

jackTest("add hashtag", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(eventname, metadata) {
            equals(eventname, "twitter.hashtag.add");
            equals(metadata.hashtag, "#pouet");
            equals($('#chat').find('form.ui-chat-hashtag-form input[type="text"]').val(), "");
        });
    $('#chat').chat({
        ucemeeting: ucemeeting
    });
    $('#chat').find('form.ui-chat-hashtag-form input[type="text"]').val("pouet");
    $('#chat').find('form.ui-chat-hashtag-form').submit();
});

module("uce.chat", {
    setup: function() {
        var that = this;
        var ucemeeting = {
            bind: function(eventName, callback) {
                if (eventName == "twitter.hashtag.add") {
                    that.callback_hashtag = callback;
                } else if (eventName == "twitter.tweet.new") {
                    that.callback_tweet = callback;
                } else if (eventName == "internal.roster.add") {
                    that.callback_roster_add = callback;
                } else if (eventName == "internal.roster.delete") {
                    that.callback_roster_delete = callback;
                } else if (eventName == "chat.message.new") {
                    that.callback_chat = callback;
                } else if (eventName == "chat.translation.new") {
                    that.callback_translation = callback;
                }
            }
        };
        $('#chat').chat({
            ucemeeting: ucemeeting,
            dock: '#chat-dock'
        });
    },
    teardown: function() {
        $('#chat').chat('destroy');
    }});

test("handle new hash event", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:last").text(), '#chuck (0)');
});

test("handle duplicate hashtag", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:last").text(), '#chuck (0)');
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:last").text(), '#chuck (0)');
});

test("sort hashtag by alphabetical order", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#atomic'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#TED'}));
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems").children().size(), 3);
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:eq(0)").text(), '#atomic (0)');
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:last").text(), '#ted (0)');
});

test("handle tweets events", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck,#norris'}));

    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:eq(0)").text(), '#chuck (2)');
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:last").text(), '#norris (1)');
});

test("can show all tweets", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-selector-hashtags.ui-chat-selector-all").click();
    equals($("#chat .ui-chat-container[name='hashtag:all']").children().size(), 2);
    ok(!$('#chat .ui-chat-container[name="hashtag:all"]').hasClass("ui-chat-current"), "should have class ui-chat-current");
    equals($("#chat .ui-chat-container[name='hashtag:all'] .ui-chat-list li:eq(0) .ui-chat-message-text")
           .text(), "plop");
});

test("can show hastag tweet and go back", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems li:eq(0)").click();
    ok($('#chat .ui-chat-container[name="hashtag:#chuck"]').hasClass("ui-chat-current"), 
       "should have class ui-chat-current");

    ok(!$('#chat .ui-chat-container[name="hashtag:#norris"]').hasClass("ui-chat-current"),
       "should not have class ui-chat-current");

    equals($('#chat .ui-chat-container[name="hashtag:#chuck"] .ui-chat-list').children().size(), 1);

    $('#chat .ui-chat-container .ui-chat-container-back').click();

    ok(!$('#chat .ui-chat-container[name="hashtag:#chuck"]').hasClass("ui-chat-current"),
       "should not have class ui-chat-current");
});

test("can change language", function() {
    $("#chat .ui-chat-flag.ui-chat-lang-fr").click();
    ok($('#chat .ui-chat-flag.ui-chat-lang-fr').hasClass("ui-state-highlight"), 
           "should have class ui-state-highlight");
    ok($('#chat .ui-chat-container[name=conversation:all:fr]').hasClass("ui-chat-current"), 
           "should have class ui-state-current");

    $("#chat .ui-chat-flag.ui-chat-lang-en").click();
    ok($('#chat .ui-chat-flag.ui-chat-lang-en').hasClass("ui-state-highlight"), 
           "should have class ui-state-highlight");
    ok(!$('#chat .ui-chat-flag.ui-chat-lang-fr').hasClass("ui-state-highlight"), 
           "should not have class ui-state-highlight");
    ok($('#chat .ui-chat-container[name=conversation:all:en]').hasClass("ui-chat-current"), 
           "should have class ui-state-current");
});

test("clear chat", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));

    $('#chat').chat('clear');
    equals($("#chat .ui-chat-containers").children().size(), 0);
    equals($("#chat .ui-chat-selector-hashtags.ui-chat-selector-elems").children().size(), 0);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 0);
});

test("handle join", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'chuck');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'chuck');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'chuck');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 0);
});

test("test messages", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));

    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list')
           .children().size(), 1);
    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list li:eq(0) .ui-chat-message-from')
           .text(), "chuck");
    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list li:eq(0) .ui-chat-message-text')
           .text(), "hello");
});

test("test message notification", function() {
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));
    equals($("#chat-dock .ui-widget-dock-notification").text(), "1");
});

test("test translation", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello', 'en'));
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list')
           .children().size(), 1);
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list li:eq(0) .ui-chat-message-from')
           .text(), "chuck");
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list li:eq(0) .ui-chat-message-text')
           .text(), "hello");
});

test("can show chatroom and go back", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));

    $("#chat .ui-chat-selector-conversations.ui-chat-selector-all li:eq(0)").click();
    ok($('#chat .ui-chat-container[name="conversation:all:fr"]').hasClass("ui-chat-current"), 
       "should have class ui-chat-current");

    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list').children().size(), 1);

    $('#chat .ui-chat-container .ui-chat-container-back').click();

    ok(!$('#chat .ui-chat-container[name="conversation:all:fr"]').hasClass("ui-chat-current"), 
       "should not have class ui-chat-current");
});
