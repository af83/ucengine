$.widget("uce.video", {
    _publish : false,
    // Default options
    options: {
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
            return "stream="+ this.options.stream +"&server=rtmp://" + this.options.domain;
        else
            return "stream="+ this.options.stream +"&streamtype=live&server=rtmp://" + this.options.domain +"&token=p&width="+ this.options.width  +"&height="+ this.options.height;
    },
    _create: function() {
        this.element.addClass('ui-widget ui-video');
        $('<embed>').attr(this._videoAttr()).appendTo(this.element);
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
    publish: function() {
        this._publish = true;
        this.element.find("embed").attr(this._videoAttr());
    },
    receive: function() {
        this._publish = false;
        this.element.find("embed").attr(this._videoAttr());
    },
    destroy: function() {
        this.element.find('embed').remove();
        this.element.removeClass('ui-widget ui-video');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
