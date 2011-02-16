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

        var all = $('<div>').attr('class', 'ui-filesharing-all');
        var preview = $('<div>').attr('class', 'ui-filesharing-preview');
        // init preview content
        $('<div>').attr('class', 'ui-filesharing-preview-title').appendTo(preview);
        var toolbar = $('<div>').attr('class', 'ui-corner-all ui-preview-toolbar');

        var previous = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-previous')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-n"
                }
            });

        var next = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-next')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-s"
                }
            });

        previous.appendTo(toolbar);
        next.appendTo(toolbar);

        var pageSelector = $('<span>').attr('class', 'ui-filesharing ui-toolbar-selector');
        $('<span>').attr('class', 'ui-filesharing ui-selector-current')
            .appendTo(pageSelector);
        $('<span>').attr('class', 'ui-filesharing ui-selector-separator').text('/')
            .appendTo(pageSelector);
        $('<span>').attr('class', 'ui-filesharing ui-selector-total')
            .appendTo(pageSelector);
        pageSelector.appendTo(toolbar);

        var stop = $('<span>')
            .attr('class', 'ui-filesharing ui-toolbar-button ui-button-stop')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-circle-close"
                }
            });

        stop.appendTo(toolbar);

        toolbar.appendTo(preview);
        var pageContainer = $('<div>').attr('class', 'ui-filesharing-preview-page').appendTo(preview);
        $('<img>').appendTo(pageContainer);

        previous.bind('click', function() {
            var page = that._shared.page - 1;
            if (page < 0) {
                return false;
            }
            that.options.ucemeeting.push('document.share.goto', {id: that._shared.file.id,
                                                                 page: page});

            return false;
        });
        next.bind('click', function() {
            var page = that._shared.page + 1;
            if (page >= that._shared.file.pages.length) {
                return false;
            }
            that.options.ucemeeting.push('document.share.goto', {id: that._shared.file.id,
                                                                 page: page});

            return false;
        });
        stop.bind('click', function() {
            that.options.ucemeeting.push('document.share.stop', {id: that._shared.file.id});
            return false;
        });

        var noSharing = $('<p>')
            .attr('class', 'ui-filesharing-nopreview')
            .text('There is no shared file for now');

        var content = $('<div>').attr('class', 'ui-widget-content').appendTo(this.element);
        all.appendTo(content);
        preview.appendTo(content);
        noSharing.appendTo(content);
        this.hideShare();
        $('<ul>').attr('class', 'ui-filesharing-list').appendTo(all);
        this._listFiles = [];
        this._shared = null;

        /* create dock */
        if (this.options.dock) {
            this._dock = dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .button({
                    text: false,
                    icons: {primary: "ui-icon-document"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            this._dock.addClass('ui-filesharing-dock');
            this._dock.appendTo(this.options.dock);

            this._startCount = new Date().getTime();
            this._newCount = 0;

            this._new = $('<div>')
                .attr('class', 'ui-widget-dock-notification')
                .text(this._newCount)
                .appendTo(this._dock);

            this._updateNotifications();

            all.bind('mouseover', function() {
                that._newCount = 0;
                that._updateNotifications();
            });
        }
    },

    clear: function() {
        this.element.find('.ui-filesharing-new').text(this._nbNewFiles);
        this._listFiles = [];
    },

    hideShare: function() {
        this._shared = null;
        this.element.find('.ui-filesharing-all').hide();
        this.element.find('.ui-filesharing-preview').hide();
        this.element.find('.ui-filesharing-nopreview').show();
    },

    viewShare: function() {
        this.element.find('.ui-filesharing-all').hide();
        this.element.find('.ui-filesharing-preview').show();
        this.element.find('.ui-filesharing-nopreview').hide();
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

        this._listFiles.push($.extend({}, event, {pages: []}));
    },

    _handleFileDocumentEvent: function(event) {
        $(this._listFiles).each(
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

    _refreshSharing: function() {
        var preview = this.element.find('.ui-filesharing-preview');
        this.element.find('.ui-filesharing-preview-title')
            .text(this._shared.file.name);
        this.element.find('.ui-selector-current')
            .text(this._shared.page + 1);
        this.element.find('.ui-selector-total')
            .text(this._shared.file.pages.length);
        var pageImg = this.element.find('.ui-filesharing-preview-page img');
        var src = this.options.ucemeeting
            .getFileDownloadUrl(this._shared.file.pages[this._shared.page]);
        pageImg.attr('src', src);
    },

    _updateNotifications: function() {
        this._new.text(this._newCount);
        if (this._newCount == 0) {
            this._new.hide();
        } else {
            this._new.show();
        }
    },

    _handleShareStartEvent: function(event) {
        var that = this;
        $(this._listFiles).each(
            function(i, file) {
                if (file.metadata.id == event.metadata.id) {
                    that._shared = {
                        master : event.from,
                        page   : 0,
                        file   : file
                    };
                    that._refreshSharing();
                    that.viewShare();
                    that._setTitle(file.metadata.name);
                }
            }
        );

    },

    _handleShareGotoEvent: function(event) {
        if (!this._shared) {
            return;
        }
        if (this._shared.master != event.from) {
            return;
        }
        this._shared.page = parseInt(event.metadata.page);
        this._refreshSharing();
    },

    _handleShareStopEvent: function(event) {
        if (!this._shared) {
            return;
        }
        if (this._shared.master != event.from) {
            return;
        }
        this.hideShare();
        this._setTitle(this.options.title);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-filesharing');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
