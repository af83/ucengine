/**
 * UC Engine UI Widget
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
                            this.options.ucemeeting.bind(ucevent, method);
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
            }
        }
        $.widget("uce." + name, $.extend(true, base, ucewidget), prototype);
    }
})(jQuery);
