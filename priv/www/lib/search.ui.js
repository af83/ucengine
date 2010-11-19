$.widget("uce.search", {
    options: {
        ucemeeting : null
    },
    _create: function() {
        this.element.addClass('ui-widget ui-search');
        this.form = $('<form>').attr('method', 'post');
        $('<input>').attr({'type': 'text', 'name': 'keywords', 'placeholder': 'Search'}).appendTo(this.form);
        $('<input>').attr({'type': 'submit', 'value': 'ok'}).appendTo(this.form);
        this.form.appendTo(this.element);
        var toggle = $('<div>').attr({'class': 'ui-search-toggle'});
        toggle.appendTo(this.form);
        this.toggle = toggle;
        var that = this;
        toggle.bind('click', function() {
            if ($('#search-results')[0].style.display == 'none') {
                $('#search-results')[0].style.display = 'block';
                that.toggle.text("↓");
            } else {
                $('#search-results')[0].style.display = 'none';
                that.toggle.text("↑");
            }
        });
        var that = this;
        this.form.bind('submit', function() {
            var keywords = this.elements['keywords'].value;
            that.options.ucemeeting.getEvents({'search': keywords, 'type': 'twitter.tweet.new'}, function(err, events) {
                that.options.ucemeeting.trigger({'type': 'internal.search.result',
                                                 'from': 'internal',
                                                 'metadata': {'events': events}});
                $('#search-results')[0].style.display='block';
                that.toggle.text("↓");
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
