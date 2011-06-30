(function($) {
$.uce.Sample = function() {}
$.uce.Sample.prototype = {
    options: {
         // uce meeting instance
        ucemeeting          : null,
        // widget title
        title               : "My widget",
    },
    // ucengine events
    meetingsEvents : {},
    //
    _widgetClass: 'ui-widget uce-widget uce-sample',
    /**
     * Constructor
     */
    _create: function() {
        var that = this;
        this.element.addClass(this._widgetClass);
        this.addHeader();

        this._content = $("<div>").addClass("ui-widget-content").appendTo(this.element);
        this._content.append($("<p>").text("insert text here"));
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass(this._widgetClass);
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
};
$.uce.widget("sample", new $.uce.Sample());
})(jQuery);
