$.uce.widget("file_sharing", {
    options: {
        ucemeeting : null,
        upload     : true
    },
   
    // ucengine events
    meetingsEvents: {
	    'internal.file.add'        : '_handleFileAddEvent',
	    'document.conversion.done' : '_handleFileDocumentEvent'
    },
 
    _create: function() {
        this.element.addClass('ui-widget ui-filesharing');
        $('<ul>').attr('class', 'ui-filesharing-list').appendTo(this.element);
        this._listFiles = [];
        if (this.options.ucemeeting) {
            if (this.options.upload) {
                var upload = ($('<div>')).append($('<p>').attr('class', 'ui-filesharing-add').append($('<a>').attr('href', '#').text('Upload new file'))).appendTo(this.element);
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

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        switch (key) {
            case 'upload':
            this.element.find('.ui-filesharing-add').toggle();
            break;
        }
    },

    _handleFileAddEvent: function(event) {
        this._listFiles.push({  id: event.metadata.id, 
                                size: event.metadata.size,
                                name: event.metadata.name,
                                pages: [] });
        this._refreshListFiles();
    },

    _handleFileDocumentEvent: function(event) {
        $(this._listFiles).each(
            function(i, item) {
                if (item.id == event.parent) {
                    $(event.metadata.pages).each(function(i, value) {
                        item.pages.push(value);
                    });
                }
            }
        );
        this._refreshListFiles();	
    },

    _refreshListFiles: function() {
        var ul = $();
        var ucemeeting = this.options.ucemeeting;
        $(this._listFiles).each(
            function(i, item) {
                var id   = item.id;
                var li = $('<li>').text(item.name);
                if (item.pages.length != 0) {
                    var sharelink = $('<a>');
                    sharelink.attr('href', '#');
                    sharelink.text(' (preview)');
                    li.append(sharelink);                
                }
                ul = ul.add(li);
            }      
        );
        this.element.find('.ui-filesharing-list').empty().append(ul);	
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-filesharing');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
