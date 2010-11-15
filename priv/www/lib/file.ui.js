$.widget("uce.file", {
    options: {
        ucemeeting : null,
        upload     : true
    },
    _create: function() {
        this._nbFiles    = 0;
        this._nbNewFiles = 0;
        this.element.addClass('ui-widget ui-file');
        var list = ($('<div>')).attr('class', 'list').appendTo(this.element);
        var newFiles = $("<p>").attr('class', 'new').text(this._nbNewFiles).appendTo(list);
        this._nbFilesP = ($("<p>")).attr('class', 'nb').text("Files (0)").appendTo(list);
        this._listFiles = $("<ul>").appendTo(list);
        if (this.options.ucemeeting) {
            if (this.options.upload) {
                var upload = ($('<div>')).append($('<p>').attr('class', 'add').append($('<a>').attr('href', '#').text('Upload Files'))).appendTo(this.element);
                new AjaxUpload(upload.find('a'), {
	            action: this.options.ucemeeting.getFileUploadUrl(),
	            name: 'content',
	            onComplete : function(file, response){
		        return true;
	            }
                });
            }
            var that = this;
            this.options.ucemeeting.bind("internal.file.add", function(event) {
                that._handleFileEvent(event);
            });
        }
        var that = this;
        list.bind('mouseover', function() {
            newFiles.text(0);
            that._nbNewFiles = 0;
        });
    },

    clear: function() {
        this._nbFiles    = 0;
        this._nbNewFiles = 0;
        this._nbFilesP.text('Files ('+ this._nbFiles +')');
        this.element.find('.new').text(this._nbNewFiles);
        this._listFiles.empty();
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        switch (key) {
            case 'upload':
            this.element.find('.add').toggle();
            break;
        }
    },

    _handleFileEvent: function(event) {
        this._nbFilesP.text('Files ('+ ++this._nbFiles +')');
        this.element.find('.new').text(++this._nbNewFiles);
        var id   = event.metadata.id;
        var text = id;
        var link = $('<a>');
        link.attr('href', this.options.ucemeeting.getFileDownloadUrl(id));
        link.text(text);
        $("<li>").append(link).appendTo(this._listFiles);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-file');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
