/**
 * Video widget
 * show stream from erlyvideo and publish stream to erlyvideo
 */
$.uce.widget("video", {
    /**
     * Internal state: current publishing or not
     */
    _publish : false,
    /**
     * Internal state: should we show receive flash widget
     */
    _showReceive : false,
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
        domain : "localhost",
        stream : "ucengine",
        width  : null,
        height : null,
        ratio  : 0.75,
        buttons: {left: [], right: []}
    },
    labels : {
        "button.publish"     : "Publish",
        "button.stoppublish" : "Stop publish",
        "content.nostream"   : "No stream available"
    },
    _getFlashSrc: function() {
        if (this._publish)
            return '/lib/video/publish_video.swf';
        else
            return '/lib/video/receive_video.swf';
    },
    _getFlashVar: function() {
        return $.param({wmode      : 'opaque',
                        stream     : this.options.stream,
                        server     : "rtmp://" + this.options.domain,
                        streamtype : "live",
                        token      : this.options.token,
                        width      : this.options.width - 40,
                        height     : this.options.height - 20});
    },
    _create: function() {
        var that = this;

        /* create dock */
        if (this.options.dock) {
            var dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .attr('title', this.options.title)
                .button({
                    text: false,
                    icons: {primary: "ui-icon-person"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            dock.addClass('ui-video-dock');
            dock.appendTo(this.options.dock);
        }

        this.element.addClass('ui-widget ui-video');
        this._button = $('<a>').attr('href', '#')
            .button({label: 'Publish'})
            .click($.proxy(this._onClickButton, this));
        var rightButtons = [this._button].concat(this.options.buttons.right);
        this._addHeader(this.options.title, {left: this.options.buttons.left,
                                             right: rightButtons});
        this._content = $('<div>').addClass('ui-widget-content').appendTo(this.element);

        if (!this.options.width && !this.options.height) {
            this.options.width = this.element.width();
        }
        if (!this.options.width && this.options.height) {
            this.options.width = this.options.height / this.options.ratio;
        }
        if (this.options.height) {
            this.options.ratio = this.options.height / this.options.width;
        }

        this.options.height = parseInt(this.options.width * this.options.ratio, 10);
        this._content.css('width', this.options.width);
        this._content.css('min-height', this.options.height);

        if (this.options.ucemeeting == null) {
            this._showReceive = true;
            this._updateEmbed();
        }
        else if (!this.options.token) {
            this._button.button("disable");
        } 
    },
    _onClickButton: function(e) {
        e.preventDefault();
        if (this._button.button('option', 'disabled'))
            return;
        if (this._button.button('option', 'label') == this.labels["button.publish"]) {
            this.publish();
            this._button.button('option', 'label', this.labels["button.stoppublish"]);
        } else {
            this._button.button('option', 'label', this.labels["button.publish"]);
            this.receive();
        }
    },
    _videoAttr: function() {
        return {src       : this._getFlashSrc(),
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
        this._content.children().remove();
        if (this._showReceive || this._publish) {
            $('<embed>').attr(this._videoAttr()).appendTo(this._content);
        } else {
            $('<p>')
                .attr('class', 'ui-video-error')
                .text(this.labels["content.nostream"])
                .appendTo(this._content);
        }
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
        this.options.token  = event.metadata.token;
        this._button.button("enable");
        this._showReceive = false;
        this._updateEmbed();
    },
    onStreamStarted: function(event) {
        if (event.metadata.broadcaster != this.options.ucemeeting.uid) {
            this._button.button("disable");
            this._showReceive = true;
            this._updateEmbed();
        }
    },
    onStreamStopped: function(event) {
        this._button.button("enable");
        this._showReceive = false;
        this._updateEmbed();
    },
    reduce: function() {
        this._resize();
        this._updateEmbed();
    },
    expand: function() {
        this._resize();
        this._updateEmbed();
    },
    _resize: function() {
        this.options.width = this.element.width();
        this.options.height = parseInt(this.options.width * this.options.ratio, 10);
        this._content.css('width', this.options.width);
        this._content.css('min-height', this.options.height);
    },
    destroy: function() {
        this.element.children().remove();
        this.element.removeClass('ui-widget ui-video');
        $(this.options.dock).children().remove();
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
