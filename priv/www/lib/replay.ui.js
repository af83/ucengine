/**
 * Depends of ui.slider
 */
$.widget("uce.replay", {
    options: {
        date_start: null,
        date_end : null
    },
    _create: function() {
        var that = this;
        this.element.addClass('ui-widget ui-replay');
        $('<a>').appendTo(this.element).button({label: "Play"}).click(function(e) {
            if ($(this).text() == 'Play') {
                $(this).button({label: 'Stop'});
                that._trigger('start', e);
                that._startTimer();
            } else {
                $(this).button({label: 'Play'});
                that._stopTimer();
                that._trigger('stop', e);
            }
        });
        $('<div>').appendTo(this.element).slider({range:"min",
                                                  stop: function(event, ui) {
                                                      var timecode = that._getTimecode(ui.value);
                                                      that.jump(timecode);
                                                  }});
        $('<p>').attr('class', 'ui-replay-time').appendTo(this.element);
        this._updateTime(0);
    },

    _getTimecode: function(value) {
        return ((value * (this.options.date_end - this.options.date_start)) / 100) + this.options.date_start;
    },

    _getValue: function(timecode) {
        return ((timecode - this.options.date_start) * 100 / (this.options.date_end - this.options.date_start));
    },

    jump: function(timecode) {
        this._stopTimer();
        this.element.find('.ui-button').button({label: 'Stop'});
        var value = this._getValue(timecode);
        var slider = this.element.find(".ui-slider").slider("option", "value", value);
        var ui = {timecode: timecode, value: value};
        this._startTimer(ui.timecode - this.options.date_start);
        this._trigger("jump", {}, ui);
    },

    _updateTime: function(current) {
        this.element.find('.ui-replay-time').text(this._formatDate(current) +' / ' + this._formatDate(this.options.date_end - this.options.date_start));
    },

    _updateSlider: function(current) {
        this.element.find(".ui-slider").slider("option", "value", (current * 100) / (this.options.date_end - this.options.date_start));
    },

    _formatDate: function(duration) {
        function pad(n){return n<10 ? '0'+n : n};
        var hours    = parseInt(duration/(1000 * 60 * 60), 10);
        var minutes  = parseInt(duration/(1000 * 60) - hours * 60, 10);
        var seconds  = parseInt(duration/1000 % 60);
        return pad(hours) + ':' + pad(minutes) + ':' + pad(seconds);
    },

    _startTimer: function(current) {
        var that    = this;
        current = current || 0;
        this._updateTime(current);
        this._updateSlider(current);
        this._interval = setInterval(function() {
            if (current >= (that.options.date_end - that.options.date_start)) {
                that.element.find('.ui-button').button({label: 'Play'});
                that._stopTimer();
                that._trigger('stop', {});
            }
            else {
                current += 1000;
                that._updateTime(current);
                that._updateSlider(current);
            }
        }, 1000);
    },

    _stopTimer: function() {
        clearInterval(this._interval);
    },

    setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this._stopTimer();
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-replay');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
