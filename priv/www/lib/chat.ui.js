$.widget("uce.chat", {
    options: {
        ucemeeting: null,
	title: "Conversations",
	lang: "fr",
	langs: ["fr", "en", "it"]
    },
    _create: function() {
        this.element.addClass('ui-chat');

	var title = $('<div>').attr('class', 'block-header');
	$('<h2>').text(this.options.title).appendTo(title);
	title.appendTo(this.element);

        var templateBig =
	 '<div class="ui-chat-big">' +
            '<div class="block-content">' +
	       '<div classe="column">' +
               '<div class="block chat">' +
                  '<div class="block-header">' +
                     '<h3>Chatroom (0)</h3>' +
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
        $.tmpl(templateBig, {}).appendTo(this.element);

        var templateMinus =
	    '<div class="ui-chat-minus">' +
               '<div class="block-content" name="main">' +
	          '<div class="block chat">' +
                     '<div class="block-header">' +
                        '<h3>Chatroom (0)</h3>' +
                     '</div>' +
	          '</div>' +
	          '<div class="block tweets">' +
                     '<div class="block-header">' +
                        '<h3>Tweets (0)</h3>' +
                     '</div>' +
                     '<div class="block-content">' +
	                 '<ul />'
                     '</div>' +
                  '</div>' +
               '</div>' +
	    '</div>';
	$.tmpl(templateMinus, {}).appendTo(this.element);

	/* display flags in the header */
	var languages = $('<ul>');

	/* create message block for each language */
	var that = this;
	$.each(this.options.langs, function(i, lang) {

	    $('<img>').attr({'src': 'images/flag/' + lang + '.png', 'alt': lang})
		.appendTo($('<a>').attr({'href': '#', 'class': 'on'})).appendTo(languages);
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
	languages.appendTo(title);

	this._addChat('hashtag:all');

        this.element.find('.ui-chat-big .block.chat .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('chat:' + that.options.lang);
        });
        this.element.find('.ui-chat-big .block.tweets .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('hashtag:all');
        });
        this.element.find('.ui-chat-minus .block.tweets .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('hashtag:all');
        });
        this.element.find('.ui-chat-minus .block.chat .block-header').click(function(e) {
            e.preventDefault();
            that._showChat('chat:' + that.options.lang);
        });
        this.toggleMode('minus');
        if (this.options.ucemeeting) {
            this._hashtags = {};
	    this._roster = [];
	    this._messages = 0;
            this.options.ucemeeting.bind("twitter.hashtag.add", function(event) {
                that._handleHashTag(event);
            });
            this.options.ucemeeting.bind("twitter.tweet.new", function(event) {
                that._handleTweet(event);
            });

	    this.options.ucemeeting.bind("internal.roster.add", function(event) {
		that._handleJoin(event);
	    });
	    this.options.ucemeeting.bind("internal.roster.delete", function(event) {
		that._handleLeave(event);
	    });
	    this.options.ucemeeting.bind("chat.translation.new", function(event) {
		that._handleMessage(event);
	    });
	    this.options.ucemeeting.bind("chat.message.new", function(event) {
		that._handleMessage(event);
	    });

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
	    this._showChat("main");
        } else {
            this.element.find('.ui-chat-big').show();
            this.element.find('.ui-chat-minus').hide();
	    this._showChat("chat:" + this.options.lang);
        }
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
		.text('Chatroom ('+ this._messages +')');
	    this.element.find('.ui-chat-minus .block.chat .block-header h3')
		.text('Chatroom ('+ this._messages +')');
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
	this.element.find('.ui-chat-minus .block.tweets ul').empty();
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
	    liMinus.appendTo(that.element.find('.ui-chat-minus .block.tweets ul'));
	});
	this.element.find('.ui-chat-big .block.tweets .block-header h3')
	    .text('Tweets ('+ totalTweets +')');
	this.element.find('.ui-chat-minus .block.tweets .block-header h3')
	    .text('Tweets ('+ totalTweets +')');
    },

    _showChat: function(name, title) {
        if (this.element.data('mode') == 'big') {	    
	    this.element.find('.ui-chat-big .block.msg .block-content').hide();
	    this.element.find('.ui-chat-big .block.msg .block-content[name="' + name + '"]').show();
        } else {
	    this.element.find('.ui-chat-minus .block-content').hide();
	    this.element.find('.ui-chat-minus .block-content[name="' + name + '"]').show();
	    this.element.find('.ui-chat-minus .block.tweets .block-content').show();
	    if (name != "main") {
		if (!title) {
		    title = name;
		}
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
	    .attr({'class': 'block-content', 'name': name});
	var msgBlockBig = this.element.find('.ui-chat-big .block.msg');
	contentBlockBig.appendTo(msgBlockBig);
	$('<ul>').appendTo(contentBlockBig);
	contentBlockBig.hide();

	var contentBlockMinus = $('<div>')
	    .attr({'class': 'block-content', 'name': name});
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
        this.element.removeClass('ui-chat');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
