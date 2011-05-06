$.widget("uce.activity", {
    options: {
        ucemeeting          : null,
        start               : 0
    },
    _create: function() {
        this.element.addClass('ui-widget ui-activity');
        if (this.options.ucemeeting) {
            var that = this;
            this.options.ucemeeting.bind("internal.search.result", function(event) {
                that.handleSearchResultsEvent(event.metadata.events);
            });
        }
    },
    handleSearchResultsEvent: function(results) {
        var uce   = this.options.ucemeeting;
        var values = [];
        var timebox = 60 * 1000; // 1 min

        if (results.length > 0) {
            var start = results[0].datetime;
            var end = results[results.length - 1].datetime;
            var nb_slices = (end - start) / timebox + 1;

            /* initialize */
            for (var slice = 0; slice < nb_slices; slice++) {
                values[slice] = 0;
            }
            /* fill */
            $.each(results, function(i, event) {
                values[parseInt((event.datetime - start) / timebox)]++;
            });

        }

        var max_slice = 0;
        for (var slice = 0; slice < nb_slices; slice++) {
            if (values[slice] > max_slice) {
                max_slice = slice;
            }
        }

        this.clear();
        var id = $(this.element).attr('id');
        var graph = Raphael(id, 600, 180).g;
        var fin = function () {
            this.flag = graph.popup(this.bar.x, this.bar.y, this.bar.value || "0").insertBefore(this);
        };
        var fout = function () {
            this.flag.animate({opacity: 0}, 100, function () {this.remove();});
        };

        graph.barchart(10, 10, 600, 180, [values]).hover(fin, fout);
        $('<div>').attr('class', 'ui-search-elem-title').text("Activity chart").prependTo(this.element);
        var rec_scene = $('<div>').attr('class', 'ui-search-elem-title').text("Go to the recommended scene");
        var that = this;
        rec_scene.bind('click', function(event) {
            var datetime = start + (max_slice * 60000);
            that._trigger("jump", event, datetime);
        });
        rec_scene.appendTo(this.element);
    },

    clear: function() {
        this.element.empty();
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-activity');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
