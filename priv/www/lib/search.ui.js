$.widget("uce.search", {
    options: {
        ucemeeting  : null,
        typevent    : "twitter.tweet.new",
        placeholder : "Search"
    },
    _create: function() {
        this.element.addClass('ui-widget ui-search');
        this.form = $('<form>').attr('method', 'post');
        $('<input>').attr({'type': 'text', 'name': 'keywords', 'placeholder': this.options.placeholder}).appendTo(this.form);
        $('<input>').attr({'type': 'submit', 'value': 'ok'}).appendTo(this.form);
        this.form.appendTo(this.element);

        var meeting = this.options.ucemeeting;
        var type    = this.options.typeevent;
        this.form.bind('submit', function() {
            var keywords = this.elements['keywords'].value;
            meeting.getEvents({'search': keywords, 'type': type}, function(err, events) {
                meeting.trigger({'type': 'internal.search.result',
                                 'from': 'internal',
                                 'metadata': {'events': events}});
            });
            return false;
        });
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-search');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
