$.widget("uce.player", {
    // Default options
    options: {
        start  : 0,
        src    : null,
        width  : 636,
        height : 477
    },
    _create: function() {
        this.element.addClass("ui-widget ui-player");
        this.player = $('<video>').attr(this._videoAttr());
        $('<source>').attr(this._sourceAttr('mp4', 'avc1.42E01E, mp4a.40.2')).appendTo(this.player);
        $('<source>').attr(this._sourceAttr('webm', 'vp8, vorbis')).appendTo(this.player);
        this.player.appendTo(this.element);
    },
    _videoAttr: function() {
        return {"class"   : 'ui-player-video',
                preload   : 'auto',
                width     : this.options.width,
                height    : this.options.height}
    },
    _sourceAttr: function(format, codec) {
        return {src       : this.options.src + "." + format,
                name      : format,
                type      : 'video/' + format,
                codecs    : codec}
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        this.player.attr(this._videoAttr());
        this.player.find("source[name=mp4]").attr(this._sourceAttr('mp4', 'avc1.42E01E, mp4a.40.2'));
        this.player.find("source[name=webm]").attr(this._sourceAttr('webm', 'vp8, vorbis'));
    },
    play: function() {
        this.player.get(0).play();
    },
    pause: function() {
        this.player.get(0).pause();
    },
    stop: function() {
        this.player.get(0).pause();
        this.player.get(0).currentTime = 0;
    },
    seek: function(timestamp) {
        var value = timestamp - this.options.start;
        if (value < 0) {
            return;
        }

        var current = this.player.get(0).currentTime;
        if (current == NaN || current == Infinity) {
            return;
        }
        this.player.get(0).currentTime = value / 1000;
    },
    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass("ui-widget ui-player");
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
