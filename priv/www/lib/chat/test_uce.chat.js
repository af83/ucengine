module("uce.chat", {
    teardown: function() {
        $('#chat').chat('destroy');
    }
});

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

Factories.newChatEvent = function(from, text, to) {
    var event = {
        type: "chat.message.new",
        from: from,
        datetime: 42424242424242,
        metadata: {
            text: text,
            lang: 'fr'
        }
    };

    if (to) {
        event.to = to;
    }
    return event
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

Factories.addChatStartEvent = function(interlocutor) {
    return {
        type: "chat.private.start",
        from: "internal",
        metadata: {
            interlocutor: interlocutor
        }
    };
},

Factories.updateNicknameEvent = function(from, nickname) {
    return {
        type: "roster.nickname.update",
        from: from,
        metadata: {nickname: nickname}
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
    var ucemeeting = jack.create("ucemeeting", ['on', 'push']);
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
        this.ucemeeting = {
            on: function(eventName, callback) {
                if (eventName == "twitter.hashtag.add") {
                    that.callback_hashtag = callback;
                } else if (eventName == "twitter.tweet.new") {
                    that.callback_tweet = callback;
                } else if (eventName == "internal.roster.add") {
                    that.callback_roster_add = callback;
                } else if (eventName == "internal.roster.delete") {
                    that.callback_roster_delete = callback;
                } else if (eventName == "chat.private.start") {
                    that.callback_chat_start = callback;
                } else if (eventName == "chat.message.new") {
                    that.callback_chat = callback;
                } else if (eventName == "chat.translation.new") {
                    that.callback_translation = callback;
                } else if (eventName == "roster.nickname.update") {
                    that.callback_nickname_update = callback;
                }
            }
        };
        $('#chat').chat({
            ucemeeting: this.ucemeeting,
            uceclient: {uid: 'chuck'},
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

test("handle chat.private.start", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'Unnamed 1');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'Unnamed 1');
});

test("close the one2one chat", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'Unnamed 1');

    var closeButton = $("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0) .ui-button-close");
    closeButton.click();
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 0);
});

test("test messages", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));

    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list')
           .children().size(), 1);
    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list li:eq(0) .ui-chat-message-from')
           .text(), "Unnamed 1");
    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list li:eq(0) .ui-chat-message-text')
           .text(), "hello");
});

test("test private message", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('brucelee'));
    this.callback_chat(Factories.newChatEvent('brucelee', 'hello', 'chuck'));

    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'Unnamed 2');

    equals($('#chat .ui-chat-container[name="conversation:brucelee:fr"] .ui-chat-list')
           .children().size(), 1);
    equals($('#chat .ui-chat-container[name="conversation:brucelee:fr"] .ui-chat-list li:eq(0) .ui-chat-message-from')
           .text(), "Unnamed 2");
    equals($('#chat .ui-chat-container[name="conversation:brucelee:fr"] .ui-chat-list li:eq(0) .ui-chat-message-text')
           .text(), "hello");
});

test("test message notification", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));
    equals($("#chat-dock .ui-widget-dock-notification").text(), "1");
});

test("test translation", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello', 'en'));
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list')
           .children().size(), 1);
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list li:eq(0) .ui-chat-message-from')
           .text(), "Unnamed 1");
    equals($('#chat .ui-chat-container[name="conversation:all:en"] .ui-chat-list li:eq(0) .ui-chat-message-text')
           .text(), "hello");
});

test("can show chatroom and go back", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));

    $("#chat .ui-chat-selector-conversations.ui-chat-selector-all li:eq(0)").click();
    ok($('#chat .ui-chat-container[name="conversation:all:fr"]').hasClass("ui-chat-current"), 
       "should have class ui-chat-current");

    equals($('#chat .ui-chat-container[name="conversation:all:fr"] .ui-chat-list').children().size(), 1);

    $('#chat .ui-chat-container .ui-chat-container-back').click();

    ok(!$('#chat .ui-chat-container[name="conversation:all:fr"]').hasClass("ui-chat-current"), 
       "should not have class ui-chat-current");
});

jackTest("post a message in the global chatroom", function() {
    expect(4);
    var ucemeeting = jack.create("ucemeeting", ['push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata) {
            equals(type, "chat.message.new");
            equals(metadata.text, "hello world");
            equals(metadata.lang, "fr");
        });
    ucemeeting.on = this.ucemeeting.on;

    $('#chat').chat({
        ucemeeting: ucemeeting,
        uceclient: {uid: 'chuck'}
    });

    $("#chat .ui-chat-container[name='conversation:all:fr'] .ui-chat-message-form input[type='text']").val("hello world");
    $("#chat .ui-chat-container[name='conversation:all:fr'] .ui-chat-message-form").submit();
});

jackTest("post a message in the private chatroom", function() {
    expect(5);
    var ucemeeting = jack.create("ucemeeting", ['pushTo']);
    jack.expect("ucemeeting.pushTo")
        .exactly("1 time")
        .mock(function(to, type, metadata) {
            equals(to, "brucelee");
            equals(type, "chat.message.new");
            equals(metadata.text, "hello world");
            equals(metadata.lang, "fr");
        });
    ucemeeting.on = this.ucemeeting.on;

    $('#chat').chat({
        ucemeeting: ucemeeting,
        uceclient: {uid: 'chuck'}
    });

    this.callback_roster_add(Factories.addRosterEvent('brucelee'));
    this.callback_chat_start(Factories.addChatStartEvent('brucelee'));
    $("#chat .ui-chat-container[name='conversation:brucelee:fr'] .ui-chat-message-form input[type='text']").val("hello world");
    $("#chat .ui-chat-container[name='conversation:brucelee:fr'] .ui-chat-message-form").submit();
});

test("handle roster.nickame.update event", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat_start(Factories.addChatStartEvent('chuck'));
    this.callback_nickname_update(Factories.updateNicknameEvent('chuck', 'Chuck Norris'));

    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems").children().size(), 1);
    equals($("#chat .ui-chat-selector-conversations.ui-chat-selector-elems li:eq(0)").text(), 'Chuck Norris');
});
