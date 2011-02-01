$.uce.widget("timer", {
    options: {
        ucemeeting : null,
        start      : 0
    },

    _create: function() {
        var that = this;

        this.element.addClass('ui-widget ui-timer');
        this._content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);

        this.options.ucemeeting.get(function(err, meeting, xhr) {
            that._elapsed = that.options.start;
            that._meetingStart = meeting.start_date;
            that._meetingEnd = meeting.end_date;
            that._handleTimeout(that);
        });
    },

    clear: function() {
    },

    _handleTimeout: function(that) {
        if (that._elapsed > that._meetingEnd) {
            that._elapsed = that._meetingEnd;
        }

        var elapsed = that._elapsed - that._meetingStart;

        var valueText = that._format(elapsed)

        if (that._meetingEnd != "never") {
            var remaining = that._meetingEnd - that._elapsed;
            var duration = that._meetingEnd - that._meetingStart;
            valueText += " / " + that._format(remaining) + " / " + that._format(duration);
        }
        else {
            valueText += " / \u221E"
        }

        var value = $('<span>')
            .attr('class', 'ui-timer-value')
            .text(valueText);

        that._elapsed += 1000;

        that._content.empty();
        that._content.append(value);
        that._timeout = setTimeout(function() {
            that._handleTimeout(that);
        }, 1000);
    },

    _format: function(timestamp) {
        var neg = false;
        if (timestamp < 0) {
            timestamp *= -1;
            neg = true;
        }

        var date = new Date(timestamp);

        var hours = date.getHours() - 1;
        if (hours < 10) {
            hours = "0" + hours;
        }
        var minutes = date.getMinutes();
        if (minutes < 10) {
            minutes = "0" + minutes;
        }
        var seconds = date.getSeconds();
        if (seconds < 10) {
            seconds = "0" + seconds;
        }

        var valueText = hours + ":" + minutes + ":" + seconds;

        if (neg) {
            valueText = "-" + valueText;
        }

        return (valueText);
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-timer');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
