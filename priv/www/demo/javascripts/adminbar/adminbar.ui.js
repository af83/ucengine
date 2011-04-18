$.widget("ui.adminbar", {
    options: {
        title: "Owner's features",
        widgets: {}
    },

    _create: function() {
        this.element.addClass('ui-widget uce-adminbar');

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

        var customizeWorkspace = $('<div>')
            .addClass('uce-adminbar-content')
            .appendTo(this.element);
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

        $.each(this.options.widgets, function(name, widget) {
            var widgetLabel = $('<div>')
                .addClass('uce-adminbar-widget');

            var description = $('<p>')
                .addClass('uce-adminbar-widget-description')
                .text(widget.description)
                .hide()
                .appendTo(widgetLabel);

            var title = $('<p>')
                .addClass('uce-adminbar-widget-title')
                .text(widget.title)
                .appendTo(widgetLabel);

            $('<li>')
                .append(widgetLabel)
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

        });
        carouselList.jcarousel({scroll: 1});

        var closeMeeting = $('<div>')
            .addClass('uce-adminbar-content')
            .appendTo(this.element);
        var closeMeetingButton = $('<li>')
            .addClass('uce-adminbar-button')
            .button({
                label: "Close the meeting"
            })
            .appendTo(buttons);

        var infos = $('<div>')
            .addClass('uce-adminbar-content')
            .appendTo(this.element);
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
    }
});
