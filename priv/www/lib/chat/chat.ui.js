$.uce.widget("chat", {
    options: {
        ucemeeting: null,
        title: "Conversations",
        lang: "fr",
        langs: ["fr", "en", "it"],
        mode: 'reduced',
        buttons: {right:[]}
    },
    // ucengine events
    meetingsEvents: {
        "twitter.hashtag.add"    : "_handleHashTag",
        "twitter.tweet.new"      : "_handleTweet",
        "internal.roster.add"    : "_handleJoin",
        "internal.roster.delete" : "_handleLeave",
        "chat.translation.new"   : "_handleMessage",
        "chat.message.new"       : "_handleMessage"
    },
    _create: function() {
        var that = this;

        this.element.addClass('ui-chat ui-widget');

        var content = $('<div>').attr('class', 'ui-widget-content').appendTo(this.element);

        var templateBig =
         '<div class="ui-chat-big">' +
            '<div class="block-content">' +
               '<div class="column">' +
               '<div class="block chat">' +
                  '<div class="block-header">' +
                     '<h3>Chatroom</h3>' +
                  '</div>' +
                  '<div class="block-content">' +
                     '<dl />' +
                  '</div>' +
               '</div>' +
               '<div class="block tweets">' +
                  '<div class="block-header">' +
                     '<h3>Tweets (0)</h3>' +
                  '</div>' +
                  '<div class="block-content">' +
                     '<dl />' +
                     '<form action="#/" method="post">' +
                        '<p>' +
                           '<label>#<input name="new-hashtag" size="14" /></label>' +
                           '<input type="submit" value="OK" />' +
                        '</p>' +
                     '</form>' +
                  '</div>' +
               '</div>' +
               '</div>' +
               '<div class="block msg" />' +
            '</div>' +
         '</div>';
        $.tmpl(templateBig, {}).appendTo(content);

        var templateMinus =
            '<div class="ui-chat-minus">' +
               '<div class="block-content" name="main">' +
                  '<div class="ui-chat-minus-header chat">' +
                        '<h3>Chatroom (0)</h3>' +
                  '</div>' +
                  '<div class="ui-chat-minus-header tweets">' +
                     '<h3>Tweets (0)</h3>' +
                  '</div>' +
                  '<div class="block-content hashtags">' +
                     '<ul />'
                  '</div>' +
               '</div>' +
            '</div>';
        $.tmpl(templateMinus, {}).appendTo(content);

        var flags = $('<span>')
            .attr('class', 'ui-chat-flags');

        /* create message block for each language */
        $.each(this.options.langs, function(i, lang) {
            var flag = $('<span>')
                .attr('class', 'ui-chat-flag')
                .button({
                    text: false,
                    icons: {primary: 'ui-chat-flag-' + lang}
                })
                .click(function(e) {
                    e.preventDefault();
                    that._showChat('chat:' + lang, "Chatroom (" + lang + ")");
                });
            flag.appendTo(flags);

            that._addChat('chat:' + lang);

            /* create form to send messages */
            var form = $('<form>').attr({'action': '#/', 'method': 'post'});
            $('<input>').attr({'name': lang, 'size': '14'})
                .appendTo('<label>').appendTo(form);
            $('<input>').attr({'type': 'submit', 'value': 'Send'}).appendTo(form);
            form.appendTo(that.element.find('.ui-chat-big .block-content[name="chat:' + lang + '"]'));
            form.bind('submit', function(e) {
                e.preventDefault();
                e.stopPropagation();
                var input = that.element.find('.block.msg form input[name="' + lang + '"]');
                var val = input.val();
                input.val("");
                that.options.ucemeeting.push("chat.message.new", {text: val, lang: lang});
            });
        });

        var rightButtons = [flags].concat(this.options.buttons.right);
        this._addHeader(this.options.title, {left: this.options.buttons.left,
                                             right: rightButtons});

        this._addChat('hashtag:all');

        this.element.find('.ui-chat-big .block.chat .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('chat:' + that.options.lang);
        });
        this.element.find('.ui-chat-big .block.tweets .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('hashtag:all');
        });
        this.element.find('.ui-chat-minus .ui-chat-minus-header.tweets').click(function(e) {
            e.preventDefault();
            that._showChat('hashtag:all', "All tweets");
        });
        this.element.find('.ui-chat-minus .ui-chat-minus-header.chat').click(function(e) {
            e.preventDefault();
            that._showChat('chat:' + that.options.lang, "Chatroom (" + that.options.lang + ")");
        });
        if (this.options.ucemeeting) {
            this._hashtags = {};
            this._roster = [];
            this._messages = 0;
            this.element.find('.block.tweets form').bind('submit', function(e) {
                e.preventDefault();
                e.stopPropagation();
                var input = that.element.find('.block.tweets form input[name="new-hashtag"]');
                var val = '#'+input.val();
                input.val("");
                that.options.ucemeeting.push("twitter.hashtag.add", {hashtag: val}, function() {});
            });

            this._showChat(this.options.lang);
        }

        /* create dock */
        if (this.options.dock) {
            this._dock = dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .button({
                    text: false,
                    icons: {primary: "ui-icon-comment"}
                }).click(function() {
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
        this._dock.addClass('ui-chat-dock');
            this._dock.appendTo(this.options.dock);

            this._startCount = new Date().getTime();
            this._newCount = 0;

            this._new = $('<div>')
                .attr('class', 'ui-widget-dock-notification')
                .text(this._newCount)
                .appendTo(this._dock);

                this._updateNotifications();

            content.bind('mouseover', function() {
                    that._newCount = 0;
                    that._updateNotifications();
            });

        }

    },

    clear: function() {
        this._hashtags = {};
        this._messages = 0;

        $.each(this.element.find('.tweets ul').children(), function(i, li) {
            $(li).remove();
        });
        $.each(this.element.find('.tweets dl').children(), function(i, dt) {
            $(dt).remove();
        });

        if (this.options.mode == "reduced") {
            this.reduce();
        } else if (this.options.mode == "expanded") {
            this.expand();
        }
    },

    /**
     *  Modes
     */
    reduce: function() {
        this.options.mode = 'reduced';

        this.element.find('.ui-chat-big').hide();
        this.element.find('.ui-chat-flags').hide();
        this.element.find('.ui-chat-minus').show();
        this._showChat("main");
    },

    expand: function() {
        this.options.mode = 'expanded';

        this.element.find('.ui-chat-big').show();
        this.element.find('.ui-chat-flags').show();
        this.element.find('.ui-chat-minus').hide();
        this._showChat("chat:" + this.options.lang);
    },

    /**
     * Event callbacks
     */

    _handleHashTag: function(event) {
        var hashtag = event.metadata.hashtag.toLowerCase();
        if (this._hashtags[hashtag] == undefined) {
            this._addChat('hashtag:' + hashtag);
            this._hashtags[hashtag] = 0;
            this._updateHashTags();
        }
    },

    _handleTweet: function(event) {
        var hashtags = event.metadata.hashtags.toLowerCase().split(',');
        var that = this;
        $.each(hashtags, function(i, hashtag) {
            var from = event.metadata.from;
            var text = event.metadata.text;
            that._addMessage('hashtag:' + hashtag, from, text);
            that._addMessage('hashtag:all', from, text);
            that._hashtags[hashtag] += 1;
        });
        this._updateHashTags();
    },

    _handleMessage: function(event) {
        if (event.metadata.lang) {
            this._addMessage('chat:' + event.metadata.lang,
                             event.from,
                             event.metadata.text);
            this._messages += 1;
            this.element.find('.ui-chat-big .block.chat .block-header h3')
                .text('Chatroom');
            this.element.find('.ui-chat-minus .ui-chat-minus-header.chat h3')
                .text('Chatroom ('+ this._messages +')');
            // XXX: Can we refactor this to have a general behaviour when there is no dock ?
                if (this.options.dock &&
                        event.from != this.options.ucemeeting.uid &&
                        event.datetime > this._startCount) {
                        this._newCount++;
                        this._updateNotifications();
                }
        }
    },

    _handleJoin: function(event) {
        for (var index = 0; index < this._roster.length; index++) {
            if (this._roster[index] == event.from) {
                return;
            }
        }
        this._roster.push(event.from);
        this._updateRoster();
    },

    _handleLeave: function(event) {
        for (var index = 0; index < this._roster.length; index++) {
            if (this._roster[index] == event.from) {
                this._roster.splice(index, 1);
            }
        }
        this._updateRoster();
    },


    /**
     * Internal functions
     */

    _updateHashTags: function() {
        this.element.find('.ui-chat-big .block.tweets dl').empty();
        this.element.find('.ui-chat-minus .block-content.hashtags ul').empty();
        var totalTweets = 0;
        var that = this;


        var orderedHashtags = [];
        $.each(this._hashtags, function(hashtag) {
            orderedHashtags.push(hashtag);
        });
        orderedHashtags.sort();

        $.each(orderedHashtags, function(i, hashtag) {
            len = that._hashtags[hashtag];
            totalTweets += len;

            onClick = function(e) {
                e.preventDefault();
                that._showChat('hashtag:' + hashtag, hashtag);
            };
            var dtBig = $('<dt>').append($('<a>').attr('href', '#').text(hashtag + ' (' + len + ')'));
            dtBig.click(onClick);
            dtBig.appendTo(that.element.find('.ui-chat-big .block.tweets dl'));

            var liMinus = $('<li>').append($('<a>').attr('href', '#').text(hashtag + ' (' + len + ')'));
            liMinus.click(onClick);
            liMinus.appendTo(that.element.find('.ui-chat-minus .block-content.hashtags ul'));
        });
        this.element.find('.ui-chat-big .block.tweets .block-header h3')
            .text('Tweets ('+ totalTweets +')');
        this.element.find('.ui-chat-minus .ui-chat-minus-header.tweets h3')
            .text('Tweets ('+ totalTweets +')');
    },

    _showChat: function(name, title) {
        if (this.options.mode == 'expanded') {
            this.element.find('.ui-chat-big .block.msg .block-content').hide();
            this.element.find('.ui-chat-big .block.msg .block-content[name="' + name + '"]').show();
        } else {
            this.element.find('.ui-chat-minus .block-content').hide();
            this.element.find('.ui-chat-minus .block-content[name="' + name + '"]').show();
            this.element.find('.ui-chat-minus .block-content.hashtags').show();
            if (name != "main") {
                if (!title) {
                    title = name;
                }
                this.element.find('.ui-chat-minus .block-content[name="' + name + '"] .ui-chat-back').remove();
                var back = $('<p>').attr('class', 'ui-chat-back');
                $('<span>').attr('class', 'ui-chat-title').text(title).appendTo(back);
                back.prependTo(this.element.find('.ui-chat-minus .block-content[name="' + name + '"]'));
                var that = this;
                back.click(function() {
                    that._showChat("main");
                    back.remove();
                });
            }
        }
    },

    _addChat: function(name) {
        var contentBlockBig = $('<div>')
            .attr({'class': 'block-content messages', 'name': name});
        var msgBlockBig = this.element.find('.ui-chat-big .block.msg');
        contentBlockBig.appendTo(msgBlockBig);
        $('<ul>').appendTo(contentBlockBig);
        contentBlockBig.hide();

        var contentBlockMinus = $('<div>')
            .attr({'class': 'block-content messages', 'name': name});
        var msgBlockMinus = this.element.find('.ui-chat-minus');
        contentBlockMinus.appendTo(msgBlockMinus);
        $('<ul>').appendTo(contentBlockMinus);
        contentBlockMinus.hide();
    },

    _updateRoster: function() {
        this.element.find('.ui-chat-big .block.chat dl').empty();
        for (var i = 0; i < this._roster.length; i++) {
            var dt = $('<dt>').text(this._roster[i]);
            dt.appendTo(this.element.find('.ui-chat-big .block.chat dl'));
        }
    },

    _updateNotifications: function() {
        this._new.text(this._newCount);
        if (this._newCount == 0) {
            this._new.hide();
        } else {
            this._new.show();
        }
    },

    _addMessage: function(name, from, text) {
        var chatBig = this.element.find('.ui-chat-big .block.msg .block-content' +
                                        '[name="' + name + '"] ul');
        var messageBig = $('<li>').text(from + ' - ' + text);
        messageBig.appendTo(chatBig);

        var chatMinus = this.element.find('.ui-chat-minus .block-content' +
                                          '[name="' + name + '"] ul');
        var messageMinus = $('<li>').text(from + ' - ' + text);
        messageMinus.appendTo(chatMinus);
    },

    setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-chat ui-widget');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
