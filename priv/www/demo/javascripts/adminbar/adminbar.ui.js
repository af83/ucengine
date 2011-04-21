$.widget("ui.adminbar", {
    options: {
        title: "Owner's features",
        ucemeeting: null,
        uceclient: null,
        widgets: {}
    },

    _create: function() {
        var that = this;

        this._selectedWidgets = [];

        this.element.addClass('ui-widget uce-adminbar');

        var that = this;

        var header = $('<div>')
            .addClass('uce-adminbar-header')
            .appendTo(this.element);

        var title = $('<span>')
            .addClass('uce-adminbar-title')
            .text(this.options.title)
            .appendTo(header);

        var buttons = $('<ul>')
            .addClass('uce-adminbar-buttons')
            .appendTo(header);

        var page = $('<div>')
            .addClass('uce-adminbar-page')
            .appendTo(this.element);

        /**
         * Customize workspace tab
         */
        var customizeWorkspace = $('<div>')
            .addClass('uce-adminbar-content')
            .addClass('uce-adminbar-workspace')
            .appendTo(page);
        var customizeWorkspaceButton = $('<li>')
            .addClass('uce-adminbar-button')
            .button({
                label: "Customize workspace"
            })
            .appendTo(buttons);

        var customizeWorkspaceDescription = $('<p>')
            .addClass('uce-adminbar-description')
            .text("Select widgets from the library, and add them in the workspace.")
            .appendTo(customizeWorkspace);

        var carousel = $('<div>')
            .addClass('uce-adminbar-carousel')
            .appendTo(customizeWorkspace);

        var carouselList = $('<ul>')
            .addClass('jcarousel-skin-ucengine')
            .appendTo(carousel);

        $.each(this.options.widgets, function(key, widget) {
            var widgetLabel = $('<div>')
                .addClass('uce-adminbar-widget');

            var linkClass = 'uce-adminbar-widget-'+key+'-link';
            var linkedWidget = key;

            var description = $('<p>')
                .addClass('uce-adminbar-widget-description')
                .text(widget.description)
                .hide()
                .appendTo(widgetLabel);

            var title = $('<p>')
                .addClass('uce-adminbar-widget-title')
                .text(widget.title)
                .appendTo(widgetLabel);

            var carouselItem = $('<li>')
                .append(widgetLabel)
                .css('background-image', 'url(' + (widget.thumbnail || "") + ")")
                .hover(function() {
                    description.show();
                    var height = title.offset().top + title.height() - description.offset().top + 10;
                    var marginTop = 120 - height;

                    widgetLabel.css({"height": height, "margin-top": marginTop})
                        .addClass('active');
                }, function() {
                    description.hide();
                    widgetLabel.removeClass('active');
                })
                .appendTo(carouselList);

            var link = $('<a>')
                .addClass("uce-adminbar-widget-hidden")
                .addClass(linkClass)
                .attr('href', '#')
                .appendTo(carouselItem);
            link.click(function() {
                        link.removeClass("uce-adminbar-widget-hidden");
                        link.parent().addClass("uce-adminbar-widget-visible");
                        link.hide();
                        that.addSelectedWidget(linkedWidget);
                        return false;
                       });

            if ($("#" + linkedWidget).is(":visible")) {
                that.hideAddWidgetLink(linkedWidget);
            }

        });
        carouselList.jcarousel({scroll: 1});

        var infos = $('<p>')
            .addClass('uce-adminbar-workspace-infos')
            .text("These changes affect everyone's view of the room.")
            .appendTo(customizeWorkspace);

        var validButton = $('<span>')
            .addClass('uce-adminbar-valid-button')
            .button({
                label: 'Valid'
            })
            .click(function() {
                if (that._selectedWidgets.length>0) {
                    that.options.ucemeeting.push("admin.meeting.widgets.add", {"widgets": that._selectedWidgets.toString()});
                    that._selectedWidgets = [];
                }
                return false;
            })
            .appendTo(customizeWorkspace);

        var cancelButton = $('<span>')
            .addClass('uce-adminbar-cancel-button')
            .button({
                label: 'Cancel'
            })
            .click(function() {
                that.cancelSelectedWidgets();
                return false;
            })
            .appendTo(customizeWorkspace);

        /**
         * Close meeting tab
         */
        var closeMeeting = $('<div>')
            .addClass('uce-adminbar-content')
            .addClass('uce-adminbar-meeting')
            .appendTo(page);
        var closeMeetingButton = $('<li>')
            .addClass('uce-adminbar-button')
            .button({
                label: "Close the meeting"
            })
            .appendTo(buttons);

        var closeMeetingInfos = $('<div>')
            .addClass('uce-adminbar-meeting-infos')
            .appendTo(closeMeeting);

        $('<p>')
            .text("You are closing the meeting room, are you sure?")
            .addClass('uce-adminbar-meeting-confirm')
            .appendTo(closeMeetingInfos);

        $('<p>')
            .text("Coming soon: possibility to archive and replay your meeting.")
            .addClass('uce-adminbar-meeting-comment')
            .appendTo(closeMeetingInfos);

        $('<span>')
            .addClass('uce-adminbar-button')
            .button({
                label: "Close the meeting",
            })
            .click(function() {
                that.options.uceclient.time.get(function(err, time, xhr) {
                    that.options.ucemeeting.get(function(err, meeting, xhr) {
                        meeting.end_date = time;
                        that.options.ucemeeting
                            .update(meeting.start_date,
                                    meeting.end_date,
                                    meeting.metadata,
                                    function(err, result, xhr) {
                                        that.options.ucemeeting.push("admin.meeting.close", {});
                                    });
                    })
                });
            })
            .appendTo(closeMeeting);

        $('<span>')
            .addClass('uce-adminbar-button')
            .button({
                label: "Continue the meeting",
            })
            .click(function() {
                closeMeetingButton.removeClass('uce-adminbar-active');
                closeMeeting.removeClass('uce-adminbar-active');
            })
            .appendTo(closeMeeting);

        /**
         * Infos tab
         */
        var infos = $('<div>')
            .addClass('uce-adminbar-content')
            .appendTo(page);
        var infosButton = $('<li>')
            .addClass('uce-adminbar-button')
            .button({
                text: false,
                icons: {primary: 'ui-icon-info'}
            })
            .appendTo(buttons);

            customizeWorkspaceButton.click(function() {
                customizeWorkspaceButton.toggleClass('uce-adminbar-active');
                closeMeetingButton.removeClass('uce-adminbar-active');
                infosButton.removeClass('uce-adminbar-active');
                customizeWorkspace.toggleClass('uce-adminbar-active');
                closeMeeting.removeClass('uce-adminbar-active');
                infos.removeClass('uce-adminbar-active');
            });

            closeMeetingButton.click(function() {
                customizeWorkspaceButton.removeClass('uce-adminbar-active');
                closeMeetingButton.toggleClass('uce-adminbar-active');
                infosButton.removeClass('uce-adminbar-active');
                customizeWorkspace.removeClass('uce-adminbar-active');
                closeMeeting.toggleClass('uce-adminbar-active');
                infos.removeClass('uce-adminbar-active');
            });

            infosButton.click(function() {
                customizeWorkspaceButton.removeClass('uce-adminbar-active');
                closeMeetingButton.removeClass('uce-adminbar-active');
                infosButton.toggleClass('uce-adminbar-active');
                customizeWorkspace.removeClass('uce-adminbar-active');
                closeMeeting.removeClass('uce-adminbar-active');
                infos.toggleClass('uce-adminbar-active');
            });
    },

    setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget uce-adminbar');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    },

    addSelectedWidget: function(widgetId) {
        $('#' + widgetId).show();
        this._selectedWidgets.push(widgetId);
    },

    getSelectedWidgets: function() {
        return this._selectedWidgets;
    },

    cancelSelectedWidgets: function() {
        $.each(this._selectedWidgets, function(index, widgetId) {
            $('#' + widgetId).hide();
            $('a.uce-adminbar-widget-' + widgetId + '-link').show()
                                                            .addClass("uce-adminbar-widget-hidden")
                                                            .parent().removeClass("uce-adminbar-widget-visible");
        });
        this._selectedWidgets = [];
    },

    hideAddWidgetLink: function(widget) {
        $('a.uce-adminbar-widget-' + widget + '-link').parent().addClass("uce-adminbar-widget-visible");
        $('a.uce-adminbar-widget-' + widget + '-link').hide();
    }

});
