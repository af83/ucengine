$.widget("uce.whiteboard", {
    options: {
        /**
         * uce meeting instance
         */
        ucemeeting          : null,
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
    _create: function() {
        this.element.addClass('ui-widget ui-whiteboard');
        $('<canvas>').attr({
            id     : this.options.canvas_id_bottom,
            width  : this.options.width,
            height : this.options.height,
        }).appendTo(this.element);
        $('<canvas>').attr({
            id     : this.options.canvas_id_interface,
            width  : this.options.width,
            height : this.options.height,
        }).appendTo(this.element);
        var controls = ($('<div>')).attr({
            id: this.options.controls_id
        }).appendTo(this.element);
        // TODO: customize actions
        $.each(["brush", "brush2", "line", "rectangle", "circle", "clear"], function(index, value) {
                    $("<div>").addClass("ui-whiteboard-btn").attr("id", "btn_"+ index).text(value).appendTo(controls);
                });
        var chooser = $('<div>').attr({
            id: this.options.choosers_id
        });
        chooser.appendTo(this.element);
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
        if (this.options.ucemeeting) {
            var that = this;
            this.options.ucemeeting.bind("whiteboard_draw_event", function(event) {
                that.handleUceEvent(event.metadata);
            });
            this.options.ucemeeting.bind("whiteboard_clear_event", function(event) {
                that.handleUceEvent(event.metadata);
            });
        }
        return;
        // ugly inside CanvasPainter.js
        var self = this;
        // just for remember old code
        $('#btn_9').click(function() {canvasAnimator.newAnimation()});
        $('#btn_10').click(function() {saveDrawing.removeLastNode()});
        $('#btn_11').click(function() {saveDrawing.paintDrawing()});
        $('#btn_12').click(function() { // upload the file
            $.ajax({
                url: '/api/0.1/file/' + MYSESSION.user.uid + ',' + MYSESSION.sid + '/push/' +
                    MYSESSION.org + '/' + MYMEETING.name + '/whiteboard.png',
                type: 'POST',
                contentType: 'base64',
                data: canvasPainter.canvas.toDataURL('image/png').slice(22),
                success: function() {
                    alert('upload succeeded');
                }
            });
        });

        $('#btn_13').click(function() {saveDrawing.addLastRemovedNode()});

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

    handleUceEvent: function(metadata) {
        this.canvasPainter.handleActionEvent(JSON.parse(metadata.wevent));
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
