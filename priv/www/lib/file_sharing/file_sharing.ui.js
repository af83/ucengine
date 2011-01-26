$.uce.widget("file_sharing", {
    options: {
        ucemeeting : null,
        upload     : true,
        title      : "File sharing"
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
        this.element.addClass('ui-widget ui-filesharing');
        $("<div>").addClass("ui-widget-header").append($("<span>").text(this.options.title)).appendTo(this.element);
        var that = this;
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

        //
        all.appendTo(this.element);
        preview.appendTo(this.element);
        this.viewAll();
        $('<ul>').attr('class', 'ui-filesharing-list').appendTo(all);
        this._listFiles = [];
        this._shared = null;
        if (this.options.ucemeeting) {
            if (this.options.upload) {
                var uploadButton = $('<a>').attr('href', '#')
                    .button({label: "Upload New File"})

                var uploadContainer = $('<div>').append($('<p>').attr('class', 'ui-filesharing-add')
                                                        .append(uploadButton)).appendTo(all);
                new AjaxUpload(uploadContainer.find('a'), {
                    action: this.options.ucemeeting.getFileUploadUrl(),
                    name: 'content',
                    onComplete : function(file, response){
                           return true;
                    }
                });
            }
        }
    },

    clear: function() {
        this.element.find('.ui-filesharing-new').text(this._nbNewFiles);
        this._listFiles = [];
        this._refreshListFiles();
    },

    viewAll: function() {
        this.element.find('.ui-filesharing-all').show();
        this.element.find('.ui-filesharing-preview').hide();
    },

    viewPreview: function() {
        this.element.find('.ui-filesharing-all').hide();
        this.element.find('.ui-filesharing-preview').show();
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        switch (key) {
            case 'upload':
            this.element.find('.ui-filesharing-add').toggle();
            break;
        }
    },

    _handleFileAddEvent: function(event) {
        if (event.from == "document") {
            return;
        }

        this._listFiles.push($.extend({}, event, {pages: []}));
        this._refreshListFiles();
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
        this._refreshListFiles();       
    },

    _refreshListFiles: function() {
        var ul = $();
        var ucemeeting = this.options.ucemeeting;
        var that = this;
        $(this._listFiles).each(
            function(index, file) {
                var id = file.metadata.id;
                var mime = (file.metadata.mime == "application/pdf") ? "pdf" : "default";
                var filename = $('<p>').text(file.metadata.name);
                if (file.pages.length != 0) {
                    var sharelink = $('<a>');
                    sharelink.attr('href', '#');
		    sharelink.attr('class', 'ui-filesharing ui-preview-link');
                    sharelink.text(' (preview)');
                    sharelink.bind('click', function() {
                        that.options.ucemeeting.push("document.share.start", {id: file.metadata.id});
                        return false;
                    });
                    filename.append(sharelink);                
                }
		var fileowner = $('<p>').attr('class', 'ui-file-owner').text("From: " + file.from);
		var li = $('<li>').attr('class', 'mime ' + mime).append(filename).append(fileowner);
                ul = ul.add(li);
            }      
        );
        this.element.find('.ui-filesharing-list').empty().append(ul);   
    },

    _refreshPreview: function() {
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
                    that._refreshPreview();
                    that.viewPreview();
                }
            }      
        );

    },

    _handleShareGotoEvent: function(event) {
        if (this._shared.master != event.from) {
            return;
        }
        this._shared.page = parseInt(event.metadata.page);
        this._refreshPreview();
    },

    _handleShareStopEvent: function(event) {
        if (this._shared.master != event.from) {
            return;
        }
        this.viewAll();
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-filesharing');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
