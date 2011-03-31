$.uce.widget("management", {
    options: {
        ucemeeting: null,
        title: "Meeting Facilitation",
        mode: 'reduced'
    },
    // ucengine events
    meetingsEvents: {
        "internal.roster.add"           : "_handleJoin",
        "internal.roster.delete"        : "_handleLeave",

        "roster.nickname.update"        : "_handleNicknameUpdate"
    },
    _create: function() {
        var that = this;

        this.element.addClass('ui-management ui-widget');
        this._addHeader(this.options.title, this.options.buttons);

        this._content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);

        this._roster = $('<div>')
            .attr('class', 'ui-management-roster')
            .appendTo(this._content);

        this._state = {};
        this._state.roster = {};
        this._state.anonCounter = 1;

        /* create dock */
        if (this.options.dock) {
            this._dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .attr('title', this.options.title)
                .button({
                    text: false,
                    icons: {primary: "ui-icon-note"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            this._dock.addClass('ui-management-dock');
            this._dock.appendTo(this.options.dock);

            this._startCount = new Date().getTime();
            this._newCount = 0;

            this._new = $('<div>')
                .attr('class', 'ui-widget-dock-notification')
                .text(this._newCount)
                .appendTo(this._dock);

            this._updateNotifications();

            this._content.bind('mouseover', function() {
                that._newCount = 0;
                that._updateNotifications();
            });
        }
    },

    clear: function() {
    },

    reduce: function() {
    },

    expand: function() {
    },

    /**
     * Event callbacks
     */

    _handleJoin: function(event) {
        if (this._state.roster[event.from]) {
            return;
        }
        this._state.roster[event.from] = {uid: event.from, nickname:
                                          "Unnamed " + this._state.anonCounter};
        this._state.anonCounter++;
        this._updateRoster();
    },

    _handleLeave: function(event) {
        delete this._state.roster[event.from];
        this._updateRoster();
    },
    
    _handleNicknameUpdate: function(event) {
        if (this._state.roster[event.from]) {
            this._state.roster[event.from].nickname = event.metadata.nickname;
            this._updateRoster();
        }
    },

    /**
     * Internal functions
     */
    _updateRoster: function() {
        this._roster.empty();
        var meeting = this.options.ucemeeting;
        var that = this;
        $.each(this._state.roster, function(uid, user) {
            var userField = $('<li>').text(user.nickname);
            if (uid != that.options.uceclient.uid) {
                userField.click(function() {
                    meeting.trigger({type: 'chat.private.start',
                                     from: 'internal',
                                     metadata: {
                                         interlocutor: uid
                                     }});
                })
            } else {
                userField.editable({onSubmit: function(content) {
                    if (content.current == content.previous) {
                        return;
                    }
                    meeting.push('roster.nickname.update', {nickname: content.current});
                }});
            }
            userField.appendTo(that._roster);
        });
    },

    _updateNotifications: function() {
        this._new.text(this._newCount);
        if (this._newCount == 0) {
            this._new.hide();
        } else {
            this._new.show();
        }
    },

    setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-management ui-widget');
        $(this.options.dock).find('*').remove();
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
