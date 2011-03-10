$.uce.widget("results", {
    options: {
        ucemeeting : null,
        start      : 0
    },
    meetingsEvents : {
        "internal.search.result" : 'handleSearchResultsEvent'
    },
    _create: function() {
        this.element.addClass('ui-widget ui-results');
    },
    handleSearchResultsEvent: function(event) {
        var results = event.metadata.events;
        var that = this;
        this.clear();
        $('<div>').attr('class', 'ui-widget-header').text("Related events").appendTo(this.element);
        var list = $('<div>').attr('class', 'ui-widget-content');
        list.appendTo(this.element);
        this.list = $('<ul>');
        this.list.appendTo(list);
        var that = this;
        $.each(results, function(i, event) {
            if (event.metadata.text) {
                var listElem = $('<li>').attr({'class': 'ui-results-list-element'});
                var from = $('<div>').attr({'class': 'ui-results-list-from'}).text(event.metadata.from + ' at ' + that._formatDate(event.datetime - that.options.start) + ':');
                from.appendTo(listElem);
                var link = $('<span>').attr({'class': 'ui-results-list-element-content'})
                    .text(event.metadata.text);
                listElem.click(function(e) {
                    that._trigger("jump", event, parseInt(event.datetime, 10));
                });
                link.appendTo(listElem);
                listElem.appendTo(that.list);
            }
        });
    },

    clear: function() {
        this.element.empty();
    },

    _formatDate: function(duration) {
        function pad(n){return n<10 ? '0'+n : n};
        var hours    = parseInt(duration/(1000 * 60 * 60), 10);
        var minutes  = parseInt(duration/(1000 * 60) - hours * 60, 10);
        var seconds  = parseInt(duration/1000 % 60);
        return pad(hours) + ':' + pad(minutes) + ':' + pad(seconds);
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-results');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
