/**
 * U.C.Engine UI Widget
 * http://ucengine.org/
 * (c) 2010 af83
 */
(function($) {
    $.uce = {};
    $.uce.widget = function (name, base, prototype) {
        var constructor = base._create;
        var ucewidget = {
            _create: function() {
                if (constructor) {
                    constructor.apply(this, []);
                }
                if (this.options.ucemeeting != null) {
                    for (var ucevent in this.meetingsEvents) {
                        var method = this.meetingsEvents[ucevent];
                        if (method) {
                            method = $.proxy(this[method], this);
                            this.options.ucemeeting.on(ucevent, method);
                        }
                    }
                }
            },
            /**
             * Trigger ucengine event for testing purpose
             * Handle only events in meetingsEvents hash
             */
            triggerUceEvent: function(event) {
                var methodName = this.meetingsEvents[event.type];
                this[methodName](event);
            },

            _addHeader: function(title, buttons) {
                var header = $('<div>')
                    .attr('class', 'ui-widget-header ui-corner-all ui-helper-clearfix');
                header.prependTo(this.element);

                if (buttons && buttons.left) {
                    var left = $('<span>').attr('class', 'ui-widget-header-left');
                    left.appendTo(header);
                    $.each(buttons.left, function(index, elem) {
                        elem.appendTo(left);
                    });
                }

                $('<span>')
                    .addClass('ui-widget-header-title')
                    .text(title)
                    .appendTo(header);

                if (buttons && buttons.right) {
                    var right = $('<span>').attr('class', 'ui-widget-header-right');
                    right.appendTo(header);
                    $.each(buttons.right, function(index, elem) {
                        elem.appendTo(right);
                    });
                }
            },

            _setTitle: function(title) {
                this.element.find('.ui-widget-header-title').text(title);
            }
        }
        $.widget("uce." + name, $.extend(true, base, ucewidget), prototype);
    }
})(jQuery);
