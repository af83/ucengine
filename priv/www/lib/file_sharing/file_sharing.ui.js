$.uce.widget("filesharing", {
    options: {
        ucemeeting : null,
        title      : "File sharing",
        mode       : "expanded"
    },

    // ucengine events
    meetingsEvents: {
        'internal.file.add'           : '_handleFileAddEvent',
        'document.conversion.done'    : '_handleFileDocumentEvent',
        'document.share.start'        : '_handleShareStartEvent',
        'document.share.goto'         : '_handleShareGotoEvent',
        'document.share.stop'         : '_handleShareStopEvent',
        'internal.roster.delete'      : '_handleShareStopEvent'
    },

    _create: function() {
        var that = this;

        this.element.addClass('ui-widget ui-filesharing');
        this._addHeader(this.options.title, this.options.buttons);

        var content = $('<div>').attr('class', 'ui-widget-content').appendTo(this.element);

        /*
         * Toolbar
         */
        var toolbar = $('<div>').attr('class', 'ui-corner-all ui-filesharing-toolbar').appendTo(content);
        // previous button
        var previous = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-previous')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-n"
                }
            })
            .bind('click', function() {
                var page = that._shared.page - 1;
                if (page < 0) {
                    return false;
                }
                that.options.ucemeeting.push('document.share.goto', {id: that._shared.file.id,
                                                                     page: page});

                return false;
            })
            .appendTo(toolbar);

        // next button
        var next = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-next')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-s"
                }
            })
            .bind('click', function() {
                var page = that._shared.page + 1;
                if (page >= that._shared.file.pages.length) {
                    return false;
                }
                that.options.ucemeeting.push('document.share.goto', {id: that._shared.file.id,
                                                                     page: page});

                return false;
            })
            .appendTo(toolbar);

        // stop button
        var stop = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-stop')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-circle-close"
                }
            })
            .bind('click', function() {
                that.options.ucemeeting.push('document.share.stop', {id: that._shared.file.id});
                return false;
            })
            .appendTo(toolbar);

        // page selector
        var pageSelector = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-selector')
            .appendTo(toolbar);
        $('<span>')
            .attr('class', 'ui-filesharing ui-selector-current')
            .appendTo(pageSelector);
        $('<span>')
            .attr('class', 'ui-filesharing ui-selector-separator')
            .text('/')
            .appendTo(pageSelector);
        $('<span>')
            .attr('class', 'ui-filesharing ui-selector-total')
            .appendTo(pageSelector);

        /*
         * Page Container
         */
        var pageContainer = $('<div>')
            .attr('class', 'ui-filesharing-page')
            .append('<img>')
            .appendTo(content);

        var noSharing = $('<p>')
            .attr('class', 'ui-filesharing-nosharing')
            .text('There is no shared file for now')
            .appendTo(content);

        this.stopSharing();
        this._files = [];
        this._shared = null;

        /*
         * Dock
         */
        if (this.options.dock) {
            this._dock = dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .attr('title', this.options.title)
                .button({
                    text: false,
                    icons: {primary: "ui-icon-transferthick-e-w"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            this._dock.addClass('ui-filesharing-dock');
            this._dock.appendTo(this.options.dock);
        }
    },

    clear: function() {
        this.element.find('.ui-filesharing-new').text(this._nbNewFiles);
        this._files = [];
    },

    stopSharing: function() {
        this._shared = null;
        this.element.find('.ui-filesharing-toolbar').hide();
        this.element.find('.ui-filesharing-page').hide();
        this.element.find('.ui-filesharing-nosharing').show();
    },

    startSharing: function() {
        this.element.find('.ui-filesharing-toolbar').show();
        this.element.find('.ui-filesharing-page').show();
        this.element.find('.ui-filesharing-nosharing').hide();
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    /**
     *  Modes
     */
    reduce: function() {
        this.options.mode = "reduced";
    },

    expand: function() {
        this.options.mode = "expanded";
    },

    _handleFileAddEvent: function(event) {
        if (event.from == "document") {
            return;
        }

        this._files.push($.extend({}, event, {pages: []}));
    },

    _handleFileDocumentEvent: function(event) {
        $(this._files).each(
            function(index, file) {
                if (file.id == event.parent) {
                    for (var key in event.metadata) {
                        var value = event.metadata[key];
                        file.pages[key] = value;
                    };
                }
            }
        );
    },

    _refreshPage: function() {
        this.element.find('.ui-selector-current')
            .text(this._shared.page + 1);

        var pageImg = this.element.find('.ui-filesharing-page img');

        if (this._shared.file.pages.length > 0) {
            this.element.find('.ui-selector-total')
                .text(this._shared.file.pages.length);
            var src = this.options.ucemeeting
                .getFileDownloadUrl(this._shared.file.pages[this._shared.page]);
        }
        else {
            this.element.find('.ui-selector-total')
                .text("1");
            var src = this.options.ucemeeting
                .getFileDownloadUrl(this._shared.file.metadata.id);
        }

        pageImg.attr('src', src);
    },

    _handleShareStartEvent: function(event) {
        var that = this;
        $(this._files).each(
            function(i, file) {
                if (file.metadata.id == event.metadata.id) {
                    that._shared = {
                        master : event.from,
                        page   : 0,
                        file   : file
                    };
                    that._refreshPage();
                    that.startSharing();
                    that._setTitle(file.metadata.name);
                }
            }
        );

    },

    _handleShareGotoEvent: function(event) {
        if (!this._shared) {
            return;
        }
        this._shared.page = parseInt(event.metadata.page);
        this._refreshPage();
    },

    _handleShareStopEvent: function(event) {
        if (!this._shared) {
            return;
        }
        this.stopSharing();
        this._setTitle(this.options.title);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-filesharing');
        $(this.options.dock).find('*').remove();
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
