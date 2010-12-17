$.uce.widget("whiteboard", {
    options: {
        /**
         * uce meeting instance
         */
        ucemeeting          : null,
        // widget title
        title               : "Whiteboard",
        /**
         *
         */
        disabled            : false,
        /**
         * width of the main canvas
         */
        width               : 400,
        /**
         * height of the main canvas
         */
        height              : 400,
        /**
         * id of canvas
         */
        canvas_id_bottom    : "canvas",
        canvas_id_interface : "canvasInterface",
        controls_id         : "controls",
        choosers_id         : "chooserWidgets",
        // show color widget
        widget_color        : true,
        widget_color_id     : "colorChooser",
        // show linewidth widget
        widget_linewidth    : true,
        widget_linewidth_id : "lineWidthChooser",
        // show transport widget
        widget_transport    : true,
        widget_transport_id : "transportWidget",
    },
    // ucengine events
    meetingsEvents : {
        "whiteboard_draw_event"  : 'handleUceEvent',
        "whiteboard_clear_event" : 'handleUceEvent'
    },
    _create: function() {
        this.element.addClass('ui-widget ui-whiteboard');
        $("<div>").addClass("ui-widget-header").append($("<span>").text(this.options.title)).appendTo(this.element);
        this._content = $("<div>").addClass("ui-widget-content").appendTo(this.element);
        $('<canvas>').attr({
            id     : this.options.canvas_id_bottom,
            width  : this.options.width,
            height : this.options.height,
        }).appendTo(this._content);
        $('<canvas>').attr({
            id     : this.options.canvas_id_interface,
            width  : this.options.width,
            height : this.options.height,
        }).appendTo(this._content);
        var controls = ($('<div>')).attr({
            id: this.options.controls_id
        }).appendTo(this._content);
        // TODO: customize actions
        $.each(["brush", "brush2", "line", "rectangle", "circle", "clear"], function(index, value) {
                    $("<div>").addClass("ui-whiteboard-btn").attr("id", "btn_"+ index).text(value).appendTo(controls);
                });
        var chooser = $('<div>').attr({
            id: this.options.choosers_id
        });
        chooser.appendTo(this._content);
        if (CanvasHelper.canvasExists(this.options.canvas_id_bottom)) {
	    var canvasPainter  = new CanvasPainter(this.options.canvas_id_bottom, this.options.canvas_id_interface, {x: 90, y: 10}, $.proxy(this._handleWhiteboardEvents, this));

	    var saveDrawing    = new CPDrawing(canvasPainter);
	    var canvasAnimator = new CPAnimator(canvasPainter);
	    //init widgets
            if (this.options.widget_color) {
                $("<canvas>").attr("id", this.options.widget_color_id).appendTo(chooser);
	        var colorWidget    = new ColorWidget(this.options.widget_color_id, {x: 500, y: 10});
	        colorWidget.addWidgetListener(function() {
		    canvasPainter.setColor(colorWidget.colorString);
	        });
            }
            if (this.options.widget_linewidth) {
                $("<canvas>").attr("id", this.options.widget_linewidth_id).appendTo(chooser);
	        var lineWidthWidget = new LineWidthWidget(this.options.widget_linewidth_id, 10, {x: 500, y: 120});
	        canvasPainter.setLineWidth(10);
	        lineWidthWidget.addWidgetListener(function() {
		    canvasPainter.setLineWidth(lineWidthWidget.lineWidth);
	        });
            }
             if (this.options.widget_transport) {
                $("<canvas>").attr("id", this.options.widget_transport_id).appendTo(chooser);
	         var transportWidget = new TransportWidget(this.options.widget_transport_id, {x: 500, y: 190}, canvasAnimator);
             }
            var that = this;
            $('#'+ this.options.controls_id +' .ui-whiteboard-btn').click(function() {
                $('#'+ that.options.controls_id +' .ui-whiteboard-btn.ui-state-active').removeClass('ui-state-active');
                $(this).addClass("ui-state-active");
            });
            $('#btn_0').click(function() {canvasPainter.setDrawAction(0)});
            $('#btn_1').click(function() {canvasPainter.setDrawAction(1)});
            $('#btn_2').click(function() {canvasPainter.setDrawAction(2)});
            $('#btn_3').click(function() {canvasPainter.setDrawAction(3)});
            $('#btn_4').click(function() {canvasPainter.setDrawAction(4)});
            $('#btn_5').click(function() {canvasPainter.setDrawAction(5)});
            this.canvasPainter = canvasPainter;
        }
    },
    _handleWhiteboardEvents: function(event) {
        if (!this.options.disabled)
        {
            var type = "whiteboard_draw_event";
	    //Mandatory optimization for whiteboard loading
	    if (event.event == "clearCanvas") {
	        type = "whiteboard_clear_event";
	    }
            this.options.ucemeeting.push(type, {"wevent": JSON.stringify(event)});
        }
    },
    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
        switch (key) {
        case 'widget_color':
            if (value)
                this.element.find('#'+ this.options.widget_color_id).show();
            else
                this.element.find('#'+ this.options.widget_color_id).hide();
            break;
        case 'widget_linewidth':
            if (value)
                this.element.find('#'+ this.options.widget_linewidth_id).show();
            else
                this.element.find('#'+ this.options.widget_linewidth_id).hide();
            break;
        case 'widget_transport':
            if (value)
                this.element.find('#'+ this.options.widget_transport_id).show();
            else
                this.element.find('#'+ this.options.widget_transport_id).hide();
            break;
        }
    },

    hideControls: function() {
        this.element.find('#'+ this.options.controls_id).hide();
    },

    showControls: function() {
        this.element.find('#'+ this.options.controls_id).show();
    },

    handleUceEvent: function(event) {
        this.canvasPainter.handleActionEvent(JSON.parse(event.metadata.wevent));
    },

    clear: function() {
        this.canvasPainter.clearCanvas();
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-whiteboard');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
