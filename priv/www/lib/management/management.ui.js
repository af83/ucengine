$.uce.widget("management", {
    options: {
        ucemeeting: null,
        uceclient: null,
        url: null,
        code: null,

        title: "Meeting Facilitation",
        mode: 'reduced'
    },
    // ucengine events
    meetingsEvents: {
        "internal.roster.add"           : "_handleJoin",
        "internal.roster.delete"        : "_handleLeave",

        "internal.user.role.add"        : "_handleUserRoleAdd",
        "internal.user.role.delete"     : "_handleUserRoleDelete",

        "roster.nickname.update"        : "_handleNicknameUpdate",

        "meeting.lead.request"          : "_handleLeadRequest",
        "meeting.lead.refuse"           : "_handleLeadRefuse"
    },
    _create: function() {
        var that = this;

        this.element.addClass('ui-management ui-widget');
        this._addHeader(this.options.title, this.options.buttons);

        this._content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);

        this._rosterHeader = $('<div>')
            .addClass('ui-management-roster-header')
            .append('<h1>')
            .appendTo(this._content);

        this._inviteLink = $('<a>')
            .addClass('ui-management-invite-link')
            .text("Invite")
            .click(function() {
                that._showInvite();
            })
            .appendTo(this._rosterHeader);

        this._roster = $('<ul>')
            .attr('class', 'ui-management-roster')
            .appendTo(this._content);

        this._inviteHeader = $('<div>')
            .addClass('ui-management-invite-header')
            .appendTo(this._content);

        this._rosterLink = $('<a>')
            .addClass('ui-management-roster-link')
            .text("Users")
            .click(function() {
                that._showRoster();
            })
            .appendTo(this._inviteHeader);

        this._inviteHeader.append($('<h1>').text('Add users'))

        this._invite = $('<div>')
            .addClass('ui-management-invite')
            .appendTo(this._content);

        $('<p>').text('URL').appendTo(this._invite);
        this._inviteUrl = $('<input>')
            .attr({'type': 'text',
                   'value': this.options.url,
                   'class': 'ui-management-url',
                   'readonly': 'readonly'})
            .appendTo(this._invite);

        $('<p>').text('Access Code').appendTo(this._invite);
        this._accessCode = $('<input>')
            .attr({'type': 'text',
                   'value': this.options.code,
                   'class': 'ui-management-code',
                   'readonly': 'readonly'})
            .appendTo(this._invite);

        this._state = {};
        this._state.roster = [];
        this._state.users = {};
        this._state.me = {};
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

        this._showRoster();
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
        if (this._state.users[event.from]) {
            return;
        }
        this._state.users[event.from] = {uid: event.from,
                                         nickname: "Unnamed " + this._state.anonCounter,
                                         you: false,
                                         owner: false,
                                         speaker: false,
                                         requestLead: false};
        if (event.from == this.options.uceclient.uid) {
            this._state.users[event.from].you = true;
        }
        this._state.anonCounter++;
        this._updateRoster();
    },

    _handleLeave: function(event) {
        delete this._state.users[event.from];
        this._updateRoster();
    },

    _handleNicknameUpdate: function(event) {
        if (this._state.users[event.from]) {
            this._state.users[event.from].nickname = event.metadata.nickname;
            this._updateRoster();
        }
    },

    _handleLeadRequest: function(event) {
        this._state.users[event.from].requestLead = true;
        this._updateRoster();
    },

    _handleLeadRefuse: function(event) {
        // Make sure the refusal come from an owner
        if (!this._state.users[event.from].owner) {
            return;
        }
        this._state.users[event.metadata.user].requestLead = false;
        this._updateRoster();
    },

    _handleUserRoleAdd: function(event) {
        if (!this._state.users[event.metadata.user]) {
            return;
        }
        if (event.metadata.role == "owner") {
            this._state.users[event.metadata.user].owner = true;
        }
        else if (event.metadata.role == "speaker") {
            this._state.users[event.metadata.user].speaker = true;
            this._state.users[event.metadata.user].requestLead = false;
        }
        this._updateRoster();
    },

    _handleUserRoleDelete: function(event) {
        if (!this._state.users[event.metadata.user]) {
            return;
        }

        if (event.metadata.role == "owner") {
            this._state.users[event.metadata.user].owner = false;
        }
        else if (event.metadata.role == "speaker") {
            this._state.users[event.metadata.user].speaker = false;
            this._state.users[event.metadata.user].requestLead = false;
        }
        this._updateRoster();
    },

    /**
     * Internal functions
     */
    _showInvite: function() {
        this._rosterHeader.hide();
        this._roster.hide();

        this._inviteHeader.show();
        this._invite.show()
    },

    _showRoster: function() {
        this._rosterHeader.show();
        this._roster.show();

        this._inviteHeader.hide();
        this._invite.hide()
    },

    _updateRoster: function() {
        this._roster.empty();
        var meeting = this.options.ucemeeting;

        var roster = [];
        $.each(this._state.users, function(uid, user) {
            roster.push(user);
        });
        roster = roster.sort(function(user1, user2) {
            if (user1.you) {
                return (-1);
            }
            if (user2.you) {
                return (1);
            }

            if (user1.owner) {
                return (-1);
            }
            if (user2.owner) {
                return (1);
            }

            if (user1.speaker) {
                return (-1);
            }
            if (user2.speaker) {
                return (1);
            }

            if (user1.requestLead) {
                return (-1);
            }
            if (user2.requestLead) {
                return (1);
            }

            if (user1.nickname > user2.nickname) {
                return (1);
            }
            return (0);
        });

        this._rosterHeader.find('h1').text("Connected users (" + roster.length + ")");

        var me = this._state.users[this.options.uceclient.uid];
        var that = this;
        $.each(roster, function(i, user) {

            var userField = $('<span>')
                .addClass('ui-management-user')
                .text(user.nickname);

            var roleField = $('<span>')
                .addClass('ui-management-role');
            if (user.owner) {
                roleField.text("Owner");
            } else if (user.speaker) {
                roleField.text("Speaker");
            } else if (user.you) {
                roleField.text("You");
            }

            var item = $('<li>').append(userField)
                .append(" ")
                .append(roleField)
                .hover(
                    function() {
                        item.addClass('ui-management-user-active');
                    },
                    function() {
                        item.removeClass('ui-management-user-active');
                    });

            if (user.uid != that.options.uceclient.uid) {
                userField.click(function() {
                    meeting.trigger({type: 'chat.private.start',
                                     from: 'internal',
                                     metadata: {
                                         interlocutor: user.uid
                                     }});
                })
                if (me && me.owner) {
                    if (user.requestLead) {
                        roleField.text("Request Lead");
                        that._createPictogram("ui-icon-circle-close", function() {
                            meeting.push('meeting.lead.refuse', {user: user.uid});
                        }).appendTo(item);
                        that._createPictogram("ui-icon-circle-check", function() {
                            that._giveLead(user);
                        }).appendTo(item);
                    } else if (user.speaker) {
                        that._createButton("Remove Lead", function() {
                            that._removeLead(user);
                        }).addClass('ui-managament-lead-button-remove')
                            .appendTo(item);

                    } else {
                        that._createButton("Give Lead", function() {
                            that._giveLead(user);
                        }).addClass('ui-management-lead-button-give')
                            .appendTo(item);
                    }
                }
            } else {
                userField.editable({onSubmit: function(content) {
                    if (content.current == content.previous) {
                        return;
                    }
                    if (content.current == "") {
                        userField.text(content.previous);
                        return;
                    }
                    meeting.push('roster.nickname.update', {nickname: content.current});
                }});

                // Add a 'Request Lead' button if we don't have the right to talk
                if (me && !me.owner && !me.speaker) {
                    if (me.requestLead) {
                        roleField.text("Lead Request Pending");
                    } else {
                        that._createButton("Request Lead", function() {
                            meeting.push('meeting.lead.request', {});
                        }).addClass('ui-management-lead-button-request')
                            .appendTo(item);
                    }
                }
            }

            item.appendTo(that._roster);
        });
    },

    _removeLead: function(speaker) {
        this.options.uceclient.user.delRole(speaker.uid,
                                            "speaker",
                                            this.options.ucemeeting.name,
                                            function(err, result, xhr) {});;
    },

    _giveLead: function(speaker) {
        // Remove the lead from the current speaker
        var that = this;
        $.each(this._state.users, function(uid, user) {
            if (user.speaker) {
                that._removeLead(user);
            }
        });
        // Give the lead to the new speaker
        this.options.uceclient.user.addRole(speaker.uid,
                                            "speaker",
                                            this.options.ucemeeting.name,
                                            function(err, result, xhr) {});;
    },

    _createButton: function(label, callback) {
        var button = $('<a>')
            .addClass('ui-management-lead-button')
            .button({label: label})
            .click(callback);
        return (button);
    },

    _createPictogram: function(icon, callback) {
        var button = $('<a>')
            .addClass('ui-management-lead-button')
            .button({text: false,
                     icons: {primary: icon}})
            .click(callback);
        return (button);
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
