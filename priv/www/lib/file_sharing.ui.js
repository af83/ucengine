$.uce.widget("file_sharing", {
    options: {
        ucemeeting : null,
        upload     : true
    },
   
    // ucengine events
    meetingsEvents: {
	'internal.file.add'        : '_handleFileAddEvent',
	'document.conversion.done' : '_handleFileDocumentEvent',
        'document.share.start'     : '_handleShareStartEvent',
        'document.share.goto'      : '_handleShareGotoEvent',
        'document.share.stop'      : '_handleShareStopEvent'
    },
 
    _create: function() {
        this.element.addClass('ui-widget ui-filesharing');
	var that = this;
        var all = $('<div>').attr('class', 'ui-filesharing-all');
        var preview = $('<div>').attr('class', 'ui-filesharing-preview');
        // init preview content
        $('<div>').attr('class', 'ui-filesharing-preview-title').appendTo(preview);
        var toolbox = $('<div>').attr('class', 'ui-filesharing-preview-toolbox');
        var previous = $('<a>').attr('class', 'ui-filesharing-preview-toolbox-previous').attr('href', '#').text('⇑');
        var next = $('<a>').attr('class', 'ui-filesharing-preview-toolbox-next').attr('href', '#').text('⇓');
	var stop = $('<a>').attr('class', 'ui-filesharing-preview-toolbox-stop').attr('href', '#').text('↶');

        $('<span>').attr('class', 'ui-filesharing-preview-toolbox-currentpage').appendTo(toolbox);
        $('<span>').attr('class', 'ui-filesharing-preview-toolbox-separator').text('/').appendTo(toolbox);
        $('<span>').attr('class', 'ui-filesharing-preview-toolbox-totalpages').appendTo(toolbox);
        $('<span>').attr('class', 'ui-filesharing-preview-toolbox-closebutton').appendTo(toolbox);
	previous.appendTo(toolbox);
	next.appendTo(toolbox);
	stop.appendTo(toolbox);
        toolbox.appendTo(preview);
        $('<div>').attr('class', 'ui-filesharing-preview-page').appendTo(preview);

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
                var upload = ($('<div>')).append($('<p>').attr('class', 'ui-filesharing-add').append($('<a>').attr('href', '#').text('Upload new file'))).appendTo(all);
                new AjaxUpload(upload.find('a'), {
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
        this._listFiles.push({  eventId: event.id,
				from: event.from,
				id: event.metadata.id, 
                                size: event.metadata.size,
                                name: event.metadata.name,
                                pages: [] });
        this._refreshListFiles();
    },

    _handleFileDocumentEvent: function(event) {
        $(this._listFiles).each(
            function(index, file) {
                if (file.eventId == event.parent) {
		    for (var key in event.metadata) {
			var value = event.metadata[key]; 
                        file.pages.push(value);
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
            function(i, item) {
                var id = item.id;
                var li = $('<li>').text(item.name);
                if (item.pages.length != 0) {
                    var sharelink = $('<a>');
                    sharelink.attr('href', '#');
                    sharelink.text(' (preview)');
		    sharelink.bind('click', function() {
			that.options.ucemeeting.push("document.share.start", {id: item.id});
			return false;
		    });
                    li.append(sharelink);                
                }
                ul = ul.add(li);
            }      
        );
        this.element.find('.ui-filesharing-list').empty().append(ul);	
    },

    _refreshPreview: function() {
	var preview = this.element.find('.ui-filesharing-preview');
	this.element.find('.ui-filesharing-preview-title')
	    .text(this._shared.file.name);
	this.element.find('.ui-filesharing-preview-toolbox-currentpage')
	    .text(this._shared.page + 1);
	this.element.find('.ui-filesharing-preview-toolbox-totalpages')
	    .text(this._shared.file.pages.length);
	this.element.find('.ui-filesharing-preview-toolbox-totalpages')
	    .text(this._shared.file.pages.length);
	var src = this.options.ucemeeting
	    .getFileDownloadUrl(this._shared.file.pages[this._shared.page]);
	var pageImg = $('<img>').attr('src', src);
	var pageElement = this.element.find('.ui-filesharing-preview-page')
	pageElement.empty();
	pageElement.append(pageImg);
    },

    _handleShareStartEvent: function(event) {
	var that = this;
        $(this._listFiles).each(
            function(i, file) {
                if (file.id == event.metadata.id) {
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
