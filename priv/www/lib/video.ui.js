$.widget("uce.video", {
    _publish : false,
    // Default options
    options: {
        title      : 'Video',
        ucemeeting : null,
        token  : null,
        domain : "localhost",
        stream : "ucengine",
        width  : 610,
        height : 415
    },
    _getFlashSrc: function() {
        if (this._publish)
            return '/publish_video.swf';
        else
            return '/receive_video.swf';
    },
    _getFlashVar: function() {
        if (this._publish)
            return "stream="+ this.options.stream +"&server=rtmp://" + this.options.domain +"&token="+ this.options.token;
        else
            return "stream="+ this.options.stream +"&streamtype=live&server=rtmp://" + this.options.domain +"&token="+ this.options.token +"&width="+ this.options.width  +"&height="+ this.options.height;
    },
    _create: function() {
        var that = this;
        this.element.addClass('ui-widget ui-video');
        $('<div>').addClass('ui-widget-header')
            .appendTo(this.element)
            .append($('<a>').attr('href', '#')
                            .button({label: 'Publish'})
                            .toggle(function() {
                                that.publish();
                                $(this).button('option', 'label', 'Stop publish');
                            }, function() {
                                $(this).button('option', 'label', 'Publish');
                                that.receive();
                            }))
            .append($('<span>').addClass('ui-video-title').text(this.options.title));
        this._content = $('<div>').addClass('ui-widget-content').appendTo(this.element);
        if (this.options.ucemeeting != null)
            this.options.ucemeeting.bind('video.stream.new', $.proxy(this.handleEvent, this));
        else
            this._updateEmbed();
    },
    _videoAttr: function() {
        return {wmode     : 'transparent',
                src       : this._getFlashSrc(),
                flashvars : this._getFlashVar(),
                quality   : 75,
                width     : this.options.width,
                height    : this.options.height,
                type      : 'application/x-shockwave-flash'};
    },
    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        this.element.find("embed").attr(this._videoAttr());
    },
    _updateEmbed: function() {
        this.element.find("embed").remove();
        $('<embed>').attr(this._videoAttr()).appendTo(this._content);
    },
    publish: function() {
        this._publish = true;
        this._updateEmbed();
    },
    receive: function() {
        this._publish = false;
        this._updateEmbed();
    },
    handleEvent: function(event) {
        this.options.stream = event.metadata.channel;
        this.options.token = event.metadata.token;
        this._updateEmbed();
    },
    destroy: function() {
        this.element.children().remove();
        this.element.removeClass('ui-widget ui-video');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
