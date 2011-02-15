$.uce.widget("whiteboard", {
    options: {
        /**
         * uce meeting instance
         */
        ucemeeting          : null,
        // widget title
        title               : "Whiteboard",
        disabled            : false,
        colors              : [],
        width                   : null,
        height              : null,
        ratio                   : 0.8
    },
    // ucengine events
    meetingsEvents : {
        "whiteboard.shape.draw"    : 'handleWhiteboardEvent',
        "whiteboard.drawing.clear" : 'handleWhiteboardEvent'
    },
    _create: function() {
        /* create dock */
        var that = this;

        // Default colors
        this.options.colors = [this._randomColor(), this._randomColor(), '#ffffff', '#000000'];

        if (this.options.dock) {
            var dock = $('<a>')
                .attr('class', 'ui-dock-button')
                .attr('href', '#')
                .button({
                    text: false,
                    icons: {primary: "ui-icon-image"}
                }).click(function() {
                    that.element.effect('bounce');
                    $(window).scrollTop(that.element.offset().top);
                    return false;
                });
            dock.addClass('ui-whiteboard-dock');
            dock.appendTo(this.options.dock);
        }

        this.element.addClass('ui-widget ui-whiteboard');
        this._addHeader(this.options.title, this.options.buttons);

        this._content = $("<div>").addClass("ui-widget-content").appendTo(this.element);

        /* create toolbar */
        this.tool = "pencil";
        this.color = "#000000";

        var toolbar = $('<div>').attr('class', 'ui-whiteboard-toolbar');

        var brush = $('<span>')
            .attr('class', 'ui-whiteboard ui-toolbar-button ui-button-left')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-pencil"
                }
            }).click(function() {
                that._changeTool('pencil');
            });
        brush.appendTo(toolbar);

        var rectangle = $('<span>')
            .attr('class', 'ui-whiteboard ui-toolbar-button ui-button-left')
            .button({
                text: false,
                icons: {
                    primary: "ui-whiteboard-icon-rectangle"
                }
            }).click(function() {
                that._changeTool('rectangle');
            });
        rectangle.appendTo(toolbar);

        var circle = $('<span>')
            .attr('class', 'ui-whiteboard ui-toolbar-button ui-button-left')
            .button({
                text: false,
                icons: {
                    primary: "ui-whiteboard-icon-circle"
                }
            }).click(function() {
                that._changeTool('circle');
            });
        circle.appendTo(toolbar);

        var clear = $('<span>')
            .attr('class', 'ui-whiteboard ui-toolbar-button ui-button-right')
            .button({
                text: false,
                icons: {
                    primary: "ui-icon-trash"
                }
            }).click(function() {
                that._clear();
            });
        clear.appendTo(toolbar);

        $.each(this.options.colors, function(index, hex) {
            var color = $('<span>')
                .attr('class', 'ui-whiteboard ui-toolbar-button ui-button-right ui-button-color')
                .button()
                .click(function() {
                    that.color = hex;
                })
                .css('background-color', hex);

            color.appendTo(toolbar);
        });

        toolbar.appendTo(this._content);

        var canvasId = this.element.attr('id') + "-canvas";
        this.canvas = $('<canvas>')
            .attr('id', canvasId)
            .bind('mousedown', function(event) {
                Whiteboard.setStrokeStyle(that.color);

                that.canvas.unbind("mouseup");
                that.canvas.unbind("mouseout");

                if (that.tool == "pencil") {
                    var x1 = that._getX(event);
                    var y1 = that._getY(event);
                    that.canvas.bind("mousemove", function(event) {
                        var x2 = that._getX(event);
                        var y2 = that._getY(event);
                        that._draw(x1, y1, x2, y2);
                        x1 = x2;
                        y1 = y2;
                    });
                    that.canvas.bind("mouseup", function(event) {
                        that._endDraw();
                    });
                    that.canvas.bind("mouseout", function(event) {
                        that._endDraw();
                    });
                };

                if (that.tool == "rectangle") {
                    var x1 = that._getX(event);
                    var y1 = that._getY(event);
                    Whiteboard.beginShape(x1, y1);
                    that.canvas.bind("mousemove", function(event) {
                        var x2 = that._getX(event);
                        var y2 = that._getY(event);
                        Whiteboard.drawRectangle(x2, y2);
                        that.canvas.unbind('mouseup');
                        that.canvas.bind("mouseup", function(event) {
                            that._draw(x1, y1, x2, y2);
                            that._endDraw();
                        });
                    })
                };
                if (that.tool == "circle") {
                    var x1 = that._getX(event);
                    var y1 = that._getY(event);
                    Whiteboard.beginShape(x1, y1);
                    that.canvas.bind("mousemove", function(event) {
                        var x2 = that._getX(event);
                        var y2 = that._getY(event);
                        Whiteboard.drawOval(x2, y2);
                        that.canvas.unbind('mouseup');
                        that.canvas.bind("mouseup", function(event) {
                            that._draw(x1, y1, x2, y2);
                            that._endDraw();
                        });
                    });
                };
            });

        $('<div>')
            .attr('class', 'ui-whiteboard-drawing')
            .append(this.canvas)
            .appendTo(this._content);

        Whiteboard.init(canvasId);

        if (!this.options.width && !this.options.height) {
            this.options.width = this.element.width();
        }
        if (!this.options.width && this.options.height) {
            this.options.width = this.options.height / this.options.ratio;
        }
        if (this.options.height) {
            this.options.ratio = this.options.height / this.options.width;
        }

        this.canvas.attr('width', this.options.width);
        this.canvas.attr('height', this.options.width * this.options.ratio);
    },

    _getX: function(event) {
        var clientX = event.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
        var cssx = (clientX - this.canvas.offset().left);
        var xrel = Whiteboard.getRelative().width;
        var canvasx = cssx * xrel;
        return parseInt(canvasx);
    },

    _getY: function(event) {
        var clientY = event.clientY + document.body.scrollTop + document.documentElement.scrollTop;
        var cssy = (clientY - this.canvas.offset().top);
        var yrel = Whiteboard.getRelative().height;
        var canvasy = cssy * yrel;
        return parseInt(canvasy);
    },

    _clear: function() {
        if (this.options.disabled == false) {
            this.options.ucemeeting.push("whiteboard.drawing.clear", {});
        }
    },

    _draw: function(x1, y1, x2, y2) {
        if (this.options.disabled == false) {
            this.options.ucemeeting.push("whiteboard.shape.draw",
                                         {"tool": this.tool,
                                          "color": this.color,
                                          "x1": x1,
                                          "y1": y1,
                                          "x2": x2,
                                          "y2": y2});
        }
    },

    _endDraw: function(event) {
        this.canvas.unbind("mousemove");
        this.canvas.unbind("mouseup");
        this.canvas.unbind("mouseout");
    },

    _changeTool: function(tool) {
        this.tool = tool;
    },

    _randomColor: function() {
        var rint = Math.round(0xffffff * Math.random());
        var color = ('#0' + rint.toString(16)).replace(/^#0([0-9a-f]{6})$/i, '#$1');
        return color;
    },

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    reduce: function() {
        this._resize();
    },

    expand: function() {
        this._resize();
    },

    clear: function() {
        var canvas = this.canvas[0];
        var context = canvas.getContext('2d');
        context.clearRect(0, 0, canvas.width, canvas.height);
        this._endDraw();
    },

    _resize: function() {
        var width = this.element.width();
        this.canvas.css('height', width * this.options.ratio);
    },

    hideControls: function() {
        this.element.find('.ui-whiteboard-toolbar').hide();
    },

    showControls: function() {
        this.element.find('.ui-whiteboard-toolbar').show();
    },

    handleWhiteboardEvent: function(event) {
        var x1 = parseInt(event.metadata.x1);
        var y1 = parseInt(event.metadata.y1);
        var x2 = parseInt(event.metadata.x2);
        var y2 = parseInt(event.metadata.y2);

        Whiteboard.setStrokeStyle(event.metadata.color);

        if (event.type == "whiteboard.shape.draw") {
            switch (event.metadata.tool) {
            case "pencil":
                Whiteboard.beginPencilDraw(x1, y1);
                Whiteboard.pencilDraw(x2, y2);
                break;
            case "rectangle":
                Whiteboard.beginShape(x1, y1);
                Whiteboard.drawRectangle(x2, y2);
                break;
            case "circle":
                Whiteboard.beginShape(x1, y1);
                Whiteboard.drawOval(x2, y2);
                break;
            }
        }
        if (event.type == "whiteboard.drawing.clear") {
            var canvas = this.canvas[0];
            var context = canvas.getContext('2d');
            context.clearRect(0, 0, canvas.width, canvas.height);
            this._endDraw();
        }
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-whiteboard');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
});
