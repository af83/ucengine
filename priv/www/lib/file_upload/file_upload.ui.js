$.uce.widget("fileupload", {
    options: {
        ucemeeting : null,
        upload     : true,
        title      : "Files",
        mode       : "expanded"
    },

    // ucengine events
    meetingsEvents: {
        'internal.file.add'           : '_handleFileAddEvent',
        'document.conversion.done'    : '_handleFileDocumentEvent'
    },

    _create: function() {
        var that = this;

        this.element.addClass('ui-widget ui-fileupload');
        this._addHeader(this.options.title, this.options.buttons);

        var content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);
        var files = $('<div>')
            .attr('class', 'ui-fileupload-files')
            .appendTo(content);
        $('<ul>')
            .attr('class', 'ui-fileupload-list')
            .appendTo(files);
        var preview = $('<div>')
            .attr('class', 'ui-fileupload-preview')
            .appendTo(content);

        /*
         * Toolbar
         */
        var toolbar = $('<div>')
            .attr('class', 'ui-corner-all ui-preview-toolbar')
            .appendTo(preview);

        // previous button
        var previous = $('<span>')
            .attr('class', 'ui-fileupload ui-toolbar-button ui-button-previous')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-n"
                }
            })
            .bind('click', function() {
                if (that.currentPreview) {
                    if (that.currentPreview.currentPage>0)
                        that.currentPreview.currentPage--;
                    var page = that.currentPreview.currentPage;
                    that._preview(that.currentPreview);
                }
                return false;
            })
            .appendTo(toolbar);

        // next button
        var next = $('<span>')
            .attr('class', 'ui-fileupload ui-toolbar-button ui-button-next')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-arrowthick-1-s"
                }
            })
            .bind('click', function() {
                if (that.currentPreview) {
                    if (that.currentPreview.currentPage<that.currentPreview.pages.length-1)
                        that.currentPreview.currentPage++;
                    var page = that.currentPreview.currentPage;
                    that._preview(that.currentPreview);
                }
                return false;
            })
            .appendTo(toolbar);

        // stop button
        var stop = $('<span>')
            .attr('class', 'ui-fileupload ui-toolbar-button ui-button-stop')
            .attr('href', '#')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-circle-close"
                }
            })
            .bind('click', function() {
                that.currentPreview = null;
                that.stopPreview();
                that._setTitle(that.options.title);
                return false;
            })
            .appendTo(toolbar);

        // share link
        var share = $('<a>')
            .attr('href', '#')
            .attr('class', 'ui-fileupload ui-share-link')
            .text("Share")
            .bind('click', function() {
                that.options.ucemeeting.push("document.share.start",
                                             {id: that.currentPreview.metadata.id,
                                              page: that.currentPreview.currentPage});
                return false;
            })
            .appendTo(toolbar);

        // page selector
        var pageSelector = $('<span>')
            .attr('class', 'ui-fileupload ui-toolbar-selector')
            .appendTo(toolbar);
        $('<span>')
            .attr('class', 'ui-fileupload ui-selector-current')
            .appendTo(pageSelector);
        $('<span>')
            .attr('class', 'ui-fileupload ui-selector-separator')
            .text('/')
            .appendTo(pageSelector);
        $('<span>')
            .attr('class', 'ui-fileupload ui-selector-total')
            .appendTo(pageSelector);

        /*
         * Page container
         */
        var pageContainer = $('<div>')
            .attr('class', 'ui-fileupload-page')
            .append('<img>')
            .appendTo(preview);

        this.stopPreview();
        this._files = [];
        this._shared = null;
        if (this.options.ucemeeting) {
            if (this.options.upload) {
                var uploadButton = $('<a>').attr('href', '#')
                    .button({label: "Upload New File"})

                var uploadContainer = $('<div>').append($('<p>').attr('class', 'ui-fileupload-add')
                                                        .append(uploadButton)).appendTo(files);
                new AjaxUpload(uploadContainer.find('a'), {
                    action: this.options.ucemeeting.getFileUploadUrl(),
                    name: 'content',
                    onComplete : function(file, response){
                        return true;
                    }
                });
            }
        }

        /* create dock */
        if (this.options.dock) {
            this._dock = dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .attr('title', this.options.title)
                .button({
                    text: false,
                    icons: {primary: "ui-icon-document"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            this._dock.addClass('ui-fileupload-dock');
            this._dock.appendTo(this.options.dock);

            this._startCount = new Date().getTime();
            this._newCount = 0;

            this._new = $('<div>')
                .attr('class', 'ui-widget-dock-notification')
                .text(this._newCount)
                .appendTo(this._dock);

            this._updateNotifications();

            files.bind('mouseover', function() {
                that._newCount = 0;
                that._updateNotifications();
            });
        }
    },

    clear: function() {
        this.element.find('.ui-fileupload-new').text(this._nbNewFiles);
        this._files = [];
        this._refreshFiles();
    },

    stopPreview: function() {
        this.element.find('.ui-fileupload-files').show();
        this.element.find('.ui-fileupload-preview').hide();
    },

    startPreview: function() {
        this.element.find('.ui-fileupload-files').hide();
        this.element.find('.ui-fileupload-preview').show();
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        switch (key) {
        case 'upload':
            this.element.find('.ui-fileupload-add').toggle();
            break;
        }
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

        if (event.datetime > this._startCount) {
            this._newCount++;
            this._updateNotifications();
        }

        this._files.push($.extend({}, event, {pages: []}));
        this._refreshFiles();
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
        this._refreshFiles();
    },

    _refreshFiles: function() {
        var that = this;
        var ucemeeting = this.options.ucemeeting;
        var files = this.element.find('.ui-fileupload-list');
        var items = $();

        $(this._files).each(function(index, file) {
            var id = file.metadata.id;
            var mimes = { 'application/pdf' : 'pdf'
                        , 'image/gif'       : 'image'
                        , 'image/png'       : 'image'
                        , 'image/jpeg'      : 'image'
                        };
            var mime = (file.metadata.mime in mimes) ? mimes[file.metadata.mime] : 'default';
            var date = $.strftime("%m-%d-%y", file.datetime);

            var filename = $('<span>')
                .attr('class', 'ui-fileupload ui-fileupload-filename')
                .text(file.metadata.name);
            var fileowner = $('<span>')
                .attr('class', 'ui-file-owner')
                .text(" " + date + " by " + file.from);
            var downloadLink = $('<a>')
                .attr('href', ucemeeting.getFileDownloadUrl(id))
                .attr('class', 'ui-fileupload ui-download-link')
                .text('Download');

            var li = $('<li>').attr('class', 'mime ' + mime);
            $('<p>').append(filename).appendTo(li);
            $('<p>').append(fileowner).appendTo(li);

            if (file.pages.length > 0 || mime == 'image') {
                var viewLink = $('<a>')
                    .attr('href', '#')
                    .text('Open in the viewer')
                    .bind('click', function() {
                        that._preview(file);
                        if (!file.currentPage) {
                            file.currentPage = 0;
                        }
                        that._preview(file);
                        that.startPreview();
                        return false;
                    })
                    .attr('class', 'ui-fileupload ui-preview-link');

                var shareLink = $('<a>')
                    .attr('href', '#')
                    .text('Share')
                    .bind('click', function() {
                        that.options.ucemeeting.push("document.share.start", {id: file.metadata.id});
                        return false;
                    })
                    .attr('class', 'ui-fileupload ui-share-link');

                $('<p>')
                    .append(downloadLink)
                    .append(' | ')
                    .append(viewLink)
                    .append(' | ')
                    .append(shareLink)
                    .appendTo(li);
            } else {
                $('<p>')
                    .append(downloadLink)
                    .appendTo(li);
            }
            items = items.add(li);
        });

        files.empty().append(items);
    },

    _preview: function(file) {
        this._setTitle(file.metadata.name);
        var preview = this.element.find('.ui-fileupload-preview');

        if (file.pages.length > 0) {
            var currentPage = file.currentPage + 1;
            var length = file.pages.length;
            var id = file.pages[file.currentPage];
        }
        else {
            var currentPage = 1;
            var length = 1;
            var id = file.metadata.id;
        }

        this.element.find('.ui-selector-current')
            .text(currentPage);
        this.element.find('.ui-selector-total')
            .text(length);
        var pageImg = this.element.find('.ui-fileupload-page img');
        var src = this.options.ucemeeting
            .getFileDownloadUrl(id);
        pageImg.attr('src', src);
        this.currentPreview = file;
    },

    _updateNotifications: function() {
        this._new.text(this._newCount);
        if (this._newCount == 0) {
            this._new.hide();
        } else {
            this._new.show();
        }
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-fileupload');
        $(this.options.dock).find('*').remove();
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
