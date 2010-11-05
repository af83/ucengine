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

test("create some elements", function() {
    $('#chat').chat();
    ok($('#chat').hasClass("ui-chat"), "should have class ui-chat");
    equals($('#chat').children().size(), 2);
    equals($("#chat .ui-chat-minus > div").children().size(), 3);
    equals($("#chat .ui-chat-big > div").children().size(), 2);
});

test("handle default minus mode", function() {
    $('#chat').chat();
    equals($("#chat .ui-chat-big").css('display'), 'none');
    equals($("#chat .ui-chat-minus").css('display'), 'block');
    equals($("#chat .ui-chat-minus .ui-chat-screen1").css('display'), 'block');
    equals($("#chat .ui-chat-minus .ui-chat-screen2").css('display'), 'none');
    equals($("#chat .ui-chat-minus .ui-chat-screen3").css('display'), 'none');
    equals($("#chat .ui-chat-minus .ui-chat-screen1 li a").css("display"), "none", "hide nb tweets");
});

test("toggle to big mode", function() {
    $('#chat').chat().chat("toggleMode", "big");
    equals($("#chat .ui-chat-big").css('display'), 'block');
    equals($("#chat .ui-chat-minus").css('display'), 'none');
});

test("destroy delete all elements", function() {
    $('#chat').chat();
    $('#chat').chat("destroy");
    ok(!$('#chat').hasClass("ui-chat"), "should not have class ui-chat");
    equals($('#chat > *').size(), 0);
});

module("uce.chat", {
    setup: function() {
        var that = this;
        var ucemeeting = {
            bind: function(eventName, callback) {
                if (eventName == "twitter.hashtag.add") {
                    that.callback_hashtag = callback;
                } else {
                    that.callback_tweet = callback;
                }
            }
        };
        $('#chat').chat({
            ucemeeting: ucemeeting
        });
    },
    teardown: function() {
        $('#chat').chat('destroy');
    }});

test("handle new hash event", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    // minus mode
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1").children().size(), 2);
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:last").text(), '#chuck (0)');
    // big mode
    equals($("#chat .ui-chat-big .block.tweets dl").children().size(), 2);
    equals($("#chat .ui-chat-big .block.tweets dl dt:eq(1)").text(), '#chuck (0)');
});

test("handle duplicate hashtag", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1").children().size(), 2);
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:last").text(), '#chuck (0)');
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1").children().size(), 2);
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:last").text(), '#chuck (0)');
});

test("sort hashtag by alphabetical order", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#atomic'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#TED'}));
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1").children().size(), 4);
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(1)").text(), '#atomic (0)');
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:last").text(), '#ted (0)');
});

test("handle tweets events", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck,#norris'}));

    equal($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(0)").css('display'), 'list-item', 'show nb total of tweets');
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(0)").text(), 'Tweets (2)');
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(1)").text(), '#chuck (2)');
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:last").text(), '#norris (1)');
    // big mode
    equals($("#chat .ui-chat-big .block.tweets dl dt:eq(0)").text(), 'All (2)');
    equals($("#chat .ui-chat-big .block.tweets dl dt:eq(1)").text(), '#chuck (2)');
    equals($("#chat .ui-chat-big .block.tweets dl dt:last").text(), '#norris (1)');
});

test("can show all tweets in minus mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(0) a").click();
    equals($("#chat .ui-chat-minus .ui-chat-screen1").css('display'), 'none');
    equals($("#chat .ui-chat-minus .ui-chat-screen2").css('display'), 'block');
    equals($("#chat .ui-chat-minus .ui-chat-screen3").css('display'), 'none');
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-title").text(), 'All tweets');
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-content").children().size(), 2);
});

test("can show all tweets in big mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat").chat("toggleMode", "big");
    $("#chat .ui-chat-big .block.tweets dl dt:eq(0)").click();
    equals($("#chat .ui-chat-big .block.msg ul").children().size(), 2);
});

test("can show hastag tweet and go back", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(1)").click();
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-title").text(), '#chuck');
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-content").children().size(), 1);

    $("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-back").click();
    equals($("#chat .ui-chat-minus .ui-chat-screen1").css('display'), 'block');
    equals($("#chat .ui-chat-minus .ui-chat-screen2").css('display'), 'none');
    equals($("#chat .ui-chat-minus .ui-chat-screen3").css('display'), 'none');

    $("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(0) a").click();
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-content").children().size(), 2);
});

test("automatic update tweets in minus mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));

    $("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(1)").click();
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-title").text(), '#chuck');
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-content").children().size(), 1);

    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    equals($("#chat .ui-chat-minus .ui-chat-screen2 .ui-chat-content").children().size(), 2);
});

test("automatic update tweets in big mode", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#norris'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#norris'}));
    
    $("#chat").chat("toggleMode", "big");
    $("#chat .ui-chat-big .block.tweets dl dt:eq(1)").click();
    equals($("#chat .ui-chat-big .msg ul").children().size(), 1);

    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));
    equals($("#chat .ui-chat-big .msg ul").children().size(), 2);
});

test("clear chat", function() {
    this.callback_hashtag(Factories.newHashTagEvent({hashtag: '#chuck'}));
    this.callback_tweet(Factories.newTweetEvent({hashtags: '#chuck'}));

    $('#chat').chat('clear');
    equals($("#chat .ui-chat-minus .block-content .ui-chat-screen1").children().size(), 1);
    equal($("#chat .ui-chat-minus .block-content .ui-chat-screen1 li:eq(0) a").css('display'), 'none', 'hide nb total of tweets');

    equals($("#chat .ui-chat-big .block.tweets dl").children().size(), 1);
});
