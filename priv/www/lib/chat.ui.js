$.widget("uce.chat", {
    options: {
        ucemeeting: null
    },
    _create: function() {
        this.element.addClass('ui-chat');
        var template = '<div class="ui-chat-big">' +
                         '<div class="block-content">' +
                           '<div class="col1">' +
                             '<div class="block tweets">' +
                               '<div class="block-header">' +
                                 '<h3>Tweets</h3>' +
                               '</div>' +
                               '<div class="block-content">' +
                                 '<dl>' +
                                   '<dt>All (0)</dt>' +
                                 '</dl>' +
                                 '<form action="" method="post">' +
                                   '<p>' +
                                     '<label for="hashtag">#</label>' +
                                     '<input name="new-hashtag" size="14" />' +
                                     '<input type="submit" value="OK" />' +
                                   '</p>' +
                                 '</form>' +
                               '</div>' +
                             '</div>' +
                           '</div>' +
                           '<div class="block msg">' +
                             '<div class="block-content">' +
                               '<ul></ul>' +
                             '</div>' +
                           '</div>' +
                         '</div>' +
                       '</div>' +
                        '<div class="ui-chat-minus"><div class="block-content screen_1">' +
                          '<ul class="ui-chat-screen1">' +
                            '<li><a href="#"></a></li>' +
                          '</ul>' +
                          '<div class="ui-chat-screen2">' +
            '<p class="ui-chat-back"><span class="ui-chat-title">Chatroom</span></p>' +
            '<ul class="ui-chat-content"></ul>' +
            '</div>' +
            '<div class="ui-chat-screen3">' +
            '</div>' +
                        '</div></div>';
        $.tmpl(template, {}).appendTo(this.element);
        var that = this;
        this._hashTagOrder = {};
        var all = this.element.find('.ui-chat-minus .ui-chat-screen1 li a');
        // back button
        this.element.find('.ui-chat-minus .ui-chat-screen2 .ui-chat-back').click(function() {
            that._showScreen(1);
        });
        all.hide().click(function(e) {
            e.preventDefault();
            that._showTweets('all');
        });
        this.element.find('.ui-chat-big .tweets dl dt:eq(0)').click(function(e) {
            e.preventDefault();
            that._showTweets('all');
        });
        this.toggleMode('minus');
        if (this.options.ucemeeting) {
            this._nbTweets = 0;
            this._tweets = {all: [all]};
            this.options.ucemeeting.bind("twitter.hashtag.add", function(event) {
                that._handleHashTag(event);
            });
            this.options.ucemeeting.bind("twitter.tweet.new", function(event) {
                that._handleTweet(event);
            });
        }
    },

    clear: function() {
        var all = this._tweets.all[0].hide();
        this._nbTweets = 0;
        this._tweets = {all: [all]};
        $.each(this.element.find('ul.ui-chat-screen1').children(), function(i, li) {
            if (i == 0)
                return;
            $(li).remove();
        });
        $.each(this.element.find('.tweets dl').children(), function(i, dt) {
            if (i == 0)
                return;
            $(dt).remove();
        });
        this.toggleMode(this.element.data('mode'));
    },

    /**
     * 
     */
    toggleMode: function(mode) {
        this.element.data('mode', mode);
        if (mode == "minus") {
            this.element.find('.ui-chat-big').hide();
            this.element.find('.ui-chat-minus').show();
            this._showScreen(1);
        } else {
            this.element.find('.ui-chat-big').show();
            this.element.find('.ui-chat-minus').hide();
            this._showScreen(1);
            this.element.data('hashtag', 'all');
        }
    },

    _showScreen: function(screen) {
        this.element.find('.ui-chat-minus .ui-chat-screen'+ screen).show();
        var others = [1, 2, 3];
        others.splice(others.indexOf(screen), 1);
        var that = this;
        $.each(others, function(i, item) {
            that.element.find('.ui-chat-minus .ui-chat-screen'+ item).hide();
        });
    },

    _handleHashTag: function(event) {
        var hashtag = event.metadata.hashtag.toLowerCase();
	if (!this._tweets[hashtag]) {
            var index = this._sortHashtags(hashtag);
            var li = $('<li>').append($('<a>').attr('href', '#').text(hashtag + ' (0)'));
            var dt = $('<dt>').append($('<a>').attr('href', '#').text(hashtag + ' (0)'));
            var that = this;
            li.click(function(e) {
                e.preventDefault();
                that._showTweets(hashtag);
            });
            dt.click(function(e) {
                e.preventDefault();
                that._showTweets(hashtag);
            });
            li.insertAfter(this.element.find('ul.ui-chat-screen1 > li:eq('+ (index) +')'));
            dt.insertAfter(this.element.find('.block.tweets dl dt:eq('+ (index) +')'));
            this._tweets[hashtag] = [li.find('a')];
	}
    },

    _sortHashtags: function(hashtag) {
        var hashtags = [];
        $.each(this._tweets, function(name, item) {
            if (name == "all")
                return;
            hashtags.push(name.substr(1));
        });
        hashtags.push(hashtag.substr(1));
        hashtags.sort();
        var that = this;
        $.each(hashtags, function(index, hashtag) {
            that._hashTagOrder['#'+ hashtag] = index;
        });
        return hashtags.indexOf(hashtag.substr(1));
    },

    _handleTweet: function(event) {
        var hashtags = event.metadata.hashtags.toLowerCase().split(',');
        var that = this;
        this._tweets.all[0].show().text('Tweets ('+ ++this._nbTweets +')');
        this.element.find('.block.tweets dl dt:eq(0)').text('All ('+ this._nbTweets +')');
        $.each(hashtags, function(i, hashtag) {
            that._tweets[hashtag].push(event);
            //that._tweets[hashtag][0].text(hashtag +" ("+ (that._tweets[hashtag].length - 1) +")");
            var index = that._hashTagOrder[hashtag] + 1;
            var text = hashtag +" ("+ (that._tweets[hashtag].length - 1) +")";
            that.element.find('ul.ui-chat-screen1 > li:eq('+ (index) +')').text(text);
            that.element.find('.block.tweets dl dt:eq('+ (index) +')').text(text);
        });
        this._tweets.all.push(event);
        // should we refresh tweets ?
        var current = this.element.data('hashtag');
        if ((current == 'all' || hashtags.indexOf(current) != -1)) {
            this._showTweets(current);
        }
    },

    _showTweets: function(hashtag) {
        var that = this;
        this.element.data('hashtag', hashtag);
        if (this.element.data('mode') == 'big') {
            this.element.find('.ui-chat-big .block.msg ul').empty();
            $.each(this._tweets[hashtag], function(i, item) {
                if (i == 0)
                    return;
                that.element.find('.ui-chat-big .block.msg ul').append($('<li>').text(item.metadata.text));
            });
        } else {
            var title = (hashtag == 'all' ? 'All tweets' : hashtag);
            this.element.find('.ui-chat-screen2 .ui-chat-title').text(title);
            this.element.find('.ui-chat-screen2 .ui-chat-content').empty();
            $.each(this._tweets[hashtag], function(i, item) {
                if (i == 0)
                    return;
                that.element.find('.ui-chat-screen2 .ui-chat-content').append($('<li>').text(item.metadata.text));
            });
            this._showScreen(2);
        }
    },

    setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-chat');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
