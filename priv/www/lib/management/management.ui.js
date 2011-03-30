$.uce.widget("management", {
    options: {
        ucemeeting: null,
        title: "Meeting Facilitation",
        mode: 'reduced'
    },
    // ucengine events
    meetingsEvents: {
        "internal.roster.add"           : "_handleJoin",
        "internal.roster.delete"        : "_handleLeave"
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
        this._state.roster = [];

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
        for (var index = 0; index < this._state.roster.length; index++) {
            if (this._state.roster[index] == event.from) {
                return;
            }
        }
        this._state.roster.push(event.from);
        this._updateRoster();
    },

    _handleLeave: function(event) {
        for (var index = 0; index < this._state.roster.length; index++) {
            if (this._state.roster[index] == event.from) {
                this._state.roster.splice(index, 1);
            }
        }
        this._updateRoster();
    },
    
    /**
     * Internal functions
     */
    _updateRoster: function() {
        this._roster.empty();
        var meeting = this.options.ucemeeting;
        var that = this;
        $(this._state.roster).each(function(i, user) {
            $('<li>')
                .text(user)
                .click(function() {
                    // Don't chat with yourself, you idiot!
                    console.log(that.options.me, user);
                    if (that.options.me == user) {
                        return;
                    }
                    meeting.trigger({type: 'chat.private.start',
                                     from: 'internal',
                                     metadata: {
                                         interlocutor: user
                                     }});
                }).appendTo(that._roster);
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
