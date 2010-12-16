$.uce.widget("video", {
    _publish : false,
    // ucengine events
    meetingsEvents: {
        'video.stream.new'   : 'onStreamNew',
        'video.stream.start' : 'onStreamStarted',
        'video.stream.stop'  : 'onStreamStopped'
    },
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
        this._button = $('<a>').attr('href', '#')
                            .button({label: 'Publish'})
                            .click(function(e) {
                                e.preventDefault();
                                if ($(this).button('option', 'disabled'))
                                    return;
                                if ($(this).button('option', 'label') == 'Publish') {
                                    that.publish();
                                    $(this).button('option', 'label', 'Stop publish');
                                } else {
                                    $(this).button('option', 'label', 'Publish');
                                    that.receive();
                                }
                            });
        $('<div>').addClass('ui-widget-header')
            .appendTo(this.element)
            .append(this._button)
            .append($('<span>').addClass('ui-video-title').text(this.options.title));
        this._content = $('<div>').addClass('ui-widget-content').appendTo(this.element);
        if (this.options.ucemeeting == null)
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
    onStreamNew: function(event) {
        this.options.stream = event.metadata.channel;
        this.options.token = event.metadata.token;
        this._updateEmbed();
    },
    onStreamStarted: function(event) {
        if (event.metadata.broadcaster != this.options.ucemeeting.uid)
            this._button.button("disable");
    },
    onStreamStopped: function(event) {
        this._button.button("enable");
    },
    destroy: function() {
        this.element.children().remove();
        this.element.removeClass('ui-widget ui-video');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
