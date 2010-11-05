$.widget("uce.video", {
    _publish : false,
    // Default options
    options: {
        domain : "localhost",
        width  : 610,
        height : 415
    },
    _getFlashSrc: function() {
        if (this._publish)
            return '/publish_video.swf?stream=test_stream%20&streamtype=live&server=rtmp://' + this.options.domain + '/encrev1/room1&token=token1';
        else
            return '/receive_video.swf?stream=test_stream%20&streamtype=live&server=rtmp://' + this.options.domain + '/encrev1/room1&token=token1';
    },
    _getFlashVar: function() {
        return "stream=test_stream&streamtype=live&server=rtmp://" + this.options.domain + "/encrev1/room1&width="+ this.options.width  +"&height="+ this.options.height +"&token=token1";
    },
    _create: function() {
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
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
