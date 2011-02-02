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
            lang: lang
        }
    };
}

test("create some elements", function() {
    $('#chat').chat();
    ok($('#chat').hasClass("ui-chat"), "should have class ui-chat");
    ok($('#chat').hasClass("ui-widget"), "should have class ui-widget");
    equals($('#chat').children().size(), 3);
    equals($("#chat .ui-chat-minus > div").children().size(), 7);
    equals($("#chat .ui-chat-big > div").children().size(), 2);
});

test("destroy delete all elements", function() {
    $('#chat').chat();
    $('#chat').chat("destroy");
    ok(!$('#chat').hasClass("ui-chat"), "should not have class ui-chat");
    ok(!$('#chat').hasClass("ui-widget"), "should not have class ui-widget");
    equals($('#chat > *').size(), 0);
});

test("handle default minus mode", function() {
    $('#chat').chat();
    equals($("#chat .ui-chat-big").css('display'), 'none');
    equals($("#chat .ui-chat-minus").css('display'), 'block');
    equals($("#chat .ui-chat-minus .block-content[name='main']").css('display'), 'block');
});

test("toggle to big mode", function() {
    $('#chat').chat().chat("expand");
    equals($("#chat .ui-chat-big").css('display'), 'block');
    equals($("#chat .ui-chat-minus").css('display'), 'none');
});

jackTest("add hashtag", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'push']);
    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(eventname, metadata) {
            equals(eventname, "twitter.hashtag.add");
            equals(metadata.hashtag, "#pouet");
            equals($('#chat').find('.column input[name="new-hashtag"]').val(), "");
        });
    $('#chat').chat({
        ucemeeting: ucemeeting
    });
    $('#chat').find('.column input[name="new-hashtag"]').val("pouet");
    $('#chat').find('.column form').submit();
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
    // minus mode
    equals($("#chat .ui-chat-minus .block-content.hashtags ul").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content.hashtags li:last").text(), '#chuck (0)');
    // big mode
    equals($("#chat .ui-chat-big .block.tweets dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.tweets dl dt:eq(0)").text(), '#chuck (0)');
});

test("handle duplicate hashtag", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-minus .block-content.hashtags").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content.hashtags li:last").text(), '#chuck (0)');
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-minus .block-content.hashtags").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content.hashtags li:last").text(), '#chuck (0)');
});

test("sort hashtag by alphabetical order", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#atomic'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#TED'}));
    equals($("#chat .ui-chat-minus .block-content.hashtags ul").children().size(), 3);
    equals($("#chat .ui-chat-minus .block-content.hashtags li:eq(0)").text(), '#atomic (0)');
    equals($("#chat .ui-chat-minus .block-content.hashtags li:last").text(), '#ted (0)');
});

test("handle tweets events", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck,#norris'}));

    equal($("#chat .ui-chat-minus .block-content.hashtags li:eq(0)").css('display'), 'list-item', 'show nb total of tweets');
    equals($("#chat .ui-chat-minus .block-content.hashtags li:eq(0)").text(), '#chuck (2)');
    equals($("#chat .ui-chat-minus .block-content.hashtags li:last").text(), '#norris (1)');
    // big mode
    equals($("#chat .ui-chat-big .block.tweets dl dt:eq(0)").text(), '#chuck (2)');
    equals($("#chat .ui-chat-big .block.tweets dl dt:last").text(), '#norris (1)');
});

test("can show all tweets in reduced mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .ui-chat-minus-header.tweets h3").click();
    equals($("#chat .ui-chat-minus .block-content[name='main']").css('display'), 'none');
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:all']").css('display'), 'block');
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:all'] .ui-chat-title").text(), 'All tweets');
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:all'] ul").children().size(), 2);
});

test("can show all tweets in expanded mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat").chat("expand");
    $("#chat .ui-chat-big .block.tweets .block-header h3").click();
    equals($("#chat .ui-chat-big .block.msg .block-content[name='hashtag:all']").children().size(), 1);
});

test("can show hastag tweet and go back", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .block-content.hashtags ul li:eq(0)").click();
    equals($("#chat .ui-chat-minus .ui-chat-title").text(), '#chuck');
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:#chuck'] ul").children().size(), 1);

    $('#chat .ui-chat-minus .block-content[name="hashtag:#chuck"] .ui-chat-back').click();
    equals($("#chat .ui-chat-minus .block-content[name='main']").css('display'), 'block');
});

test("automatic update tweets in minus mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .block-content.hashtags ul li:eq(0)").click();
    equals($("#chat .ui-chat-minus .ui-chat-title").text(), '#chuck');
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:#chuck'] ul").children().size(), 1);

    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    equals($("#chat .ui-chat-minus .block-content[name='hashtag:#chuck'] ul").children().size(), 2);
});

test("automatic update tweets in big mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));
    
    $("#chat").chat("toggleMode", "big");
    $("#chat .ui-chat-big .block.tweets dl dt:eq(0)").click();
    equals($("#chat .ui-chat-big .block-content[name='hashtag:#chuck'] ul").children().size(), 1);
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    equals($("#chat .ui-chat-big .block-content[name='hashtag:#chuck'] ul").children().size(), 2);
});

test("clear chat", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));

    $('#chat').chat('clear');
    equals($("#chat .ui-chat-minus .block.tweets .block-content ul").children().size(), 0);
    equals($("#chat .ui-chat-big .block.tweets dl").children().size(), 0);
});

test("handle join", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');
});

test("handle duplicate participant", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');
});

test("handle leave", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');

    this.callback_roster_delete(Factories.deleteRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 0);
});

test("test messages", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');

    $("#chat .ui-chat-big .block.chat dl dt:eq(0)").click();
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));
    equals($("#chat .ui-chat-big .block-content[name='chat:fr'] ul").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content[name='chat:fr'] ul").children().size(), 1);
});

test("test message notification", function() {
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));
    equals($("#chat-dock .ui-chat-dock-notification").text(), "1");
});

test("test automatic update messages", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');

    $("#chat .ui-chat-big .block.chat dl dt:eq(0)").click();

    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));
    equals($("#chat .ui-chat-big .block-content[name='chat:fr'] ul").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content[name='chat:fr'] ul").children().size(), 1);

    this.callback_chat(Factories.newChatEvent('chuck', 'hello2'));
    equals($("#chat .ui-chat-big .block-content[name='chat:fr'] ul").children().size(), 2);
    equals($("#chat .ui-chat-minus .block-content[name='chat:fr'] ul").children().size(), 2);
});

test("test translation", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');

    $("#chat .ui-chat-big .block.chat dl dt:eq(0)").click();
    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello', 'en'));
    equals($("#chat .ui-chat-big .block-content[name='chat:en'] ul").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content[name='chat:en'] ul").children().size(), 1);
});

test("test automatic update translations", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    equals($("#chat .ui-chat-big .block.chat dl").children().size(), 1);
    equals($("#chat .ui-chat-big .block.chat dl dt:eq(0)").text(), 'chuck');

    $("#chat .ui-chat-big .block.chat dl dt:eq(0)").click();
    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello', 'en'));
    equals($("#chat .ui-chat-big .block-content[name='chat:en'] ul").children().size(), 1);
    equals($("#chat .ui-chat-minus .block-content[name='chat:en'] ul").children().size(), 1);

    this.callback_translation(Factories.newTranslationEvent('chuck', 'hello2', 'en'));
    equals($("#chat .ui-chat-big .block-content[name='chat:en'] ul").children().size(), 2);
    equals($("#chat .ui-chat-minus .block-content[name='chat:en'] ul").children().size(), 2);
});

test("can show chatroom and go back", function() {
    this.callback_roster_add(Factories.addRosterEvent('chuck'));
    this.callback_chat(Factories.newChatEvent('chuck', 'hello'));

    $("#chat .ui-chat-minus .ui-chat-minus-header.chat h3").click();
    equals($("#chat .ui-chat-minus .ui-chat-title").text(), 'Chatroom (fr)');
    equals($("#chat .ui-chat-minus .block-content[name='chat:fr'] ul").children().size(), 1);

    $('#chat .ui-chat-minus .block-content[name="chat:fr"] .ui-chat-back').click();
    equals($("#chat .ui-chat-minus .block-content[name='main']").css('display'), 'block');
});
