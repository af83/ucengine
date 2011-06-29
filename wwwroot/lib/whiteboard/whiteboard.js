/**
 * HTML5 Canvas Whiteboard
 * 
 * Authors:
 * Antti Hukkanen
 * Kristoffer Snabb
 * 
 * Aalto University School of Science and Technology
 * Course: T-111.2350 Multimediatekniikka / Multimedia Technology
 * 
 * Under MIT Licence
 * 
 */

(function() {
	
/**
 * =============
 *     MODEL
 * =============
 */

/* === BEGIN Event objects === */

/* Begin path event */
function BeginPath(x, y) {
    this.coordinates = [x, y];
    this.type="beginpath";
	this.time = new Date().getTime();
}
/* Begin shape event */
function BeginShape(x, y, canvas) {
	this.type = "beginshape";
	this.canvas = canvas;
	this.coordinates = [x, y];
	this.time = new Date().getTime();
}
/* End path event */
function ClosePath() {
    this.type = "closepath";
	this.time = new Date().getTime();
}
/* Point draw event */
function DrawPathToPoint(x, y) {
    this.type = "drawpathtopoint";
    this.coordinates = [x, y];
    this.time = new Date().getTime();
}
/*Erase event */
function Erase(x, y) {
    this.type = "erase";
    this.coordinates = [x, y];
    this.height = 5;
    this.width = 5;
    this.time = new Date().getTime();
}
/* Rectangle event */
function Rectangle(sx, sy, ex, ey, canvas) {
	this.type = "rectangle";
	this.coordinates = [sx, sy, ex, ey];
	this.canvas = canvas;
	this.time = new Date().getTime();
}
function Oval(x, y, w, h, canvas) {
	this.type = "oval";
	this.coordinates = [x, y, w, h];
	this.canvas = canvas;
	this.time = new Date().getTime();
}
/* Storke style event */
function StrokeStyle(color) {
    this.type = "strokestyle";
    this.color = color;
    this.time = new Date().getTime();
}
/* Zoom event */
function Zoom(factor) {
    this.type = "zoom";
    this.factor = factor;
    this.time = new Date().getTime();
}
/* Restore event */
function Restore(canvas) {
	this.type = "restore";
	if (canvas !== undefined) {
		this.canvas = canvas;
	}
	this.time = new Date().getTime();
}
/* Rotate event
   angle in degrees
*/
function Rotate(angle) {
    this.type = "rotate";
    this.angle = angle;
    this.time = new Date().getTime();
}
/* === END Event objects === */


	
/**
 * ====================
 *    STATIC CONTROL
 * ====================   
 */
window.Whiteboard = {

    context: null,
    canvas: null,
    type: '',
    coordinates: [0,0],
    events: [],
    animationind: 0,

    drawColor: '#000000',

    /**
     * Initializes the script by setting the default
     * values for parameters of the class.
     * 
     * @param canvasid The id of the canvas element used
     */
    init: function(canvasid) {
	    // set the canvas width and height
	    // the offsetWidth and Height is default width and height
	    this.canvas = document.getElementById(canvasid);
	    this.canvas.width = this.canvas.offsetWidth;
	    this.canvas.height = this.canvas.offsetHeight;
	
	    this.context = this.canvas.getContext('2d');
	
	    //initial values for the drawing context
	    this.context.lineWidth = 5;
	    this.context.lineCap = "round";
	    var zoomFactor = 1.0;
	
	    // Initialize the selected color
	    var col = this.drawColor;
	    this.drawColor = null;
	    this.setStrokeStyle(col);
    },

    /**
     * Executes the event that matches the given event
     * object
     * 
     * @param wbevent The event object to be executed.
     * @param firstexecute tells the function if the event is new and
     *          should be saved to this.events
     * This object should be one of the model's event objects.
     */
    execute: function(wbevent, firstexecute) {
        var type = wbevent.type;
        var wid;
        var hei;
        var tmp;
        if(firstexecute || firstexecute === undefined) {
            wbevent.time = new Date().getTime();
            this.events.push(wbevent);
        }

        if(type === "beginpath") {
            this.context.beginPath();
            this.context.moveTo(wbevent.coordinates[0],
                           wbevent.coordinates[1]);
            this.context.stroke();
        } else if(type === "beginshape") {
	        this.context.save();
	        this.context.beginPath();
        } else if (type === "drawpathtopoint") {  
            this.context.lineTo(wbevent.coordinates[0],
                           wbevent.coordinates[1]);
            this.context.stroke();
        } else if (type === "closepath") {
            this.context.closePath();
        } else if(type === "strokestyle") {
            this.context.strokeStyle = wbevent.color;
        } else if(type === "zoom") {
            var newWidth = this.canvas.offsetWidth * wbevent.factor;
            var newHeight = this.canvas.offsetHeight * wbevent.factor;
            this.canvas.style.width = newWidth + "px";
            this.canvas.style.height = newHeight + "px";
        } else if (type === "restore") {
            wid = this.canvas.width;
            hei = this.canvas.height;
	        this.context.clearRect(0, 0, wid, hei);
	        if (wbevent.canvas !== undefined) {
		        this.context.drawImage(wbevent.canvas, 0, 0);
	        }
        } else if(type === "rotate") {
            var radian = wbevent.angle * Math.PI / 180;
            wid = this.canvas.width;
            hei = this.canvas.height;
            
            tmp = document.createElement("canvas");
            var tmpcnv = tmp.getContext('2d');
            tmp.width = wid;
            tmp.height = hei;
            tmpcnv.drawImage(this.canvas, 0, 0);
            
            // TODO: Fix: the image blurs out after multiple rotations 
            this.context.save();
            this.context.clearRect(0, 0, wid, hei);
            this.context.translate(wid/2,hei/2);
            this.context.rotate(radian);
            this.context.translate(-wid/2,-hei/2);
            this.context.drawImage(tmp, 0, 0);
            this.context.restore();
            
            tmp = tmpcnv = undefined;
        } else if (type === "erase") {
            this.context.clearRect(wbevent.coordinates[0],
                              wbevent.coordinates[1],
                              wbevent.width,
                              wbevent.height);
        } else if (type === "rectangle") {
	        var sx = wbevent.coordinates[0];
	        var sy = wbevent.coordinates[1];
	        var ex = wbevent.coordinates[2];
	        var ey = wbevent.coordinates[3];
	        tmp = 0;
	        if (ex < sx) {
		        tmp = sx;
		        sx = ex;
		        ex = tmp;
	        }
	        if (ey < sy) {
		        tmp = sy;
		        sy = ey;
		        ey = tmp;
	        }
	
	        if (wbevent.canvas !== undefined) {
                wid = this.canvas.width;
                hei = this.canvas.height;
		        this.context.clearRect(0, 0, wid, hei);
		        this.context.drawImage(wbevent.canvas, 0, 0);
	        }
	        this.context.beginPath();
	        this.context.rect(sx, sy, ex-sx, ey-sy);
	        this.context.closePath();
	        this.context.stroke();
        } else if (type === "oval") {
	        var x = wbevent.coordinates[0];
	        var y = wbevent.coordinates[1];
	        var w = wbevent.coordinates[2];
	        var h = wbevent.coordinates[3];
	
	        var kappa = 0.5522848;
	        var ox = (w / 2) * kappa;
	        var oy = (h / 2) * kappa;
	        var xe = x + w;
	        var ye = y + h;
	        var xm = x + w / 2;
	        var ym = y + h / 2;
	
	        if (wbevent.canvas !== undefined) {
                wid = this.canvas.width;
                hei = this.canvas.height;
		        this.context.clearRect(0, 0, wid, hei);
		        this.context.drawImage(wbevent.canvas, 0, 0);
	        }
	
	        this.context.beginPath();
	        this.context.moveTo(x, ym);
	        this.context.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);
	        this.context.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);
	        this.context.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);
	        this.context.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);
	        this.context.closePath();
	        this.context.stroke();
        }
    },

    /**
     * Resolves the relative width and height of the canvas
     * element. Relative parameters can vary depending on the
     * zoom. Both are equal to 1 if no zoom is encountered.
     * 
     * @return An array containing the relative width as first
     * element and relative height as second.
     */
    getRelative: function() {
	    return {width: this.canvas.width/this.canvas.offsetWidth,
			    height: this.canvas.height/this.canvas.offsetHeight};
    },

    /**
     * Opens a new window and writes the canvas image data
     * to an element of type "img" in the new windows html body.
     * The image is written in the specified image form.
     * 
     * @param type The output image type.
     * Alternatives: png, jpg/jpeg, bmp
     */
    saveAs: function(type) {
	    if (type === undefined) {
		    type = "png";
	    }
	    type = type.toLowerCase();
	
	    var img = null;
	    if (type == 'jpg' || type == 'jpeg') {
		    img = Canvas2Image.saveAsJPEG(Whiteboard.canvas, true);
	    } else if (type == 'bmp') {
		    img = Canvas2Image.saveAsBMP(Whiteboard.canvas, true);
	    } else {
		    img = Canvas2Image.saveAsPNG(Whiteboard.canvas, true);
	    }
	    var options = 'width=' + Whiteboard.canvas.width + ',' +
		    'height=' + Whiteboard.canvas.height;
	    var win = window.open('','Save image',options);
	    win.innerHTML = "";
	    win.document.body.appendChild(img);
    },


    /* === BEGIN ACTIONS === */

    /**
     * Starts the animation action in the canvas. This clears
     * the whole canvas and starts to execute actions from
     * the action stack by calling Whiteboard.animatenext().
     */
    animate: function() {
	    Whiteboard.animationind = 0;
	    Whiteboard.context.clearRect(0,0,Whiteboard.canvas.width,Whiteboard.canvas.height);
	    Whiteboard.animatenext();
    },

    /**
     * This function animates the next event in the event 
     * stack and waits for the amount of time between the 
     * current and next event before calling itself again.
     */
    animatenext: function() {
        if(Whiteboard.animationind === 0) {
            Whiteboard.execute(Whiteboard.events[0], false);
            Whiteboard.animationind++;   
        }
        
        Whiteboard.execute(Whiteboard.events[Whiteboard.animationind], false);
        Whiteboard.animationind++;
        
        if (Whiteboard.animationind < Whiteboard.events.length - 1) {
            var now = new Date().getTime();
	        var dtime = Whiteboard.events[Whiteboard.animationind+1].time - Whiteboard.events[Whiteboard.animationind].time;
            setTimeout(Whiteboard.animatenext, dtime);
        }
    },

    /**
     * Begins a drawing path.
     * 
     * @param x Coordinate x of the path starting point
     * @param y Coordinate y of the path starting point
     */
    beginPencilDraw: function(x, y) {
        var e = new BeginPath(x, y);
        Whiteboard.execute(e);
    },

    /**
     * Draws a path from the path starting point to the
     * point indicated by the given parameters.
     * 
     * @param x Coordinate x of the path ending point
     * @param y Coordinate y of the path ending point
     */
    pencilDraw: function(x, y) {
        var e = new DrawPathToPoint(x, y);
        Whiteboard.execute(e);
    },

    /**
     * Begins erasing path.
     * 
     * @param x Coordinate x of the path starting point
     * @param y Coordinate y of the path starting point
     */
    beginErasing: function(x, y) {
        var e = new BeginPath(x, y);
        Whiteboard.execute(e);
    },

    /**
     * Erases the point indicated by the given coordinates.
     * Actually this doesn't take the path starting point
     * into account but erases a rectangle at the given
     * coordinates with width and height specified in the
     * Erase object.
     * 
     * @param x Coordinate x of the path ending point
     * @param y Coordinate y of the path ending point
     */
    erasePoint: function(x, y) {
        var e = new Erase(x, y);
        Whiteboard.execute(e);
    },

    /**
     * Begins shape drawing. The shape starting point
     * should be the point where user click the canvas the
     * first time after choosing the shape tool.
     * 
     * @param x Coordinate x of the shape starting point
     * @param y Coordinate y of the shape starting point
     */
    beginShape: function(x, y) {
        var tmp = document.createElement("canvas");
        var tmpcnv = tmp.getContext('2d');
        tmp.width = Whiteboard.canvas.width;
        tmp.height = Whiteboard.canvas.height;
        tmpcnv.drawImage(Whiteboard.canvas, 0, 0);
	    var e = new BeginShape(x, y, tmp);
	    Whiteboard.execute(e);
    },

    /**
     * Draws a rectangle that has opposite corner to the
     * shape starting point at the given point.
     * 
     * @param x Coordinate x of the shape ending point
     * @param y Coordinate y of the shape ending point
     */
    drawRectangle: function(x, y) {
	    var i = Whiteboard.events.length - 1;
	    while (i >= 0) {
		    var e = Whiteboard.events[i];
		    if (e.type === "beginshape") {
			    var ev = new Rectangle(e.coordinates[0], e.coordinates[1], x, y, e.canvas);
			    Whiteboard.execute(ev);
			    e = ev = undefined;
			    break;
		    }
		    i--;
	    }
    },

    /**
     * Draws an oval that has opposite corner to the
     * shape starting point at the given point. The oval
     * drawing can be visualized by the same way as drawing
     * a rectangle but the corners are round.
     * 
     * @param x Coordinate x of the shape ending point
     * @param y Coordinate y of the shape ending point
     */
    drawOval: function(x, y) {
	    var i = Whiteboard.events.length - 1;
	    while (i >= 0) {
		    var e = Whiteboard.events[i];
		    if (e.type === "beginshape") {
			    var sx = e.coordinates[0];
			    var sy = e.coordinates[1];
			    var wid = x-sx;
			    var hei = y-sy;
			
			    var ev = new Oval(sx, sy, wid, hei, e.canvas);
			    Whiteboard.execute(ev);
			    e = ev = undefined;
			    break;
		    }
		    i--;
	    }
    },

    /**
     * Sets stroke style for the canvas. Stroke
     * style defines the color with which every
     * stroke should be drawn on the canvas.
     * 
     * @param color The wanted stroke color
     */
    setStrokeStyle: function(color) {
	    if (color != Whiteboard.drawColor) {
		    var e = new StrokeStyle(color);
		    Whiteboard.execute(e);
	    }
    },

    /**
     * Zooms in the canvas 50% of the current canvas size
     * resulting a 150% image size.
     */
    zoomin: function() {
        var e = new Zoom(1.5);
        Whiteboard.execute(e);
    },

    /**
     * Zooms out the canvas 50% of the current canvas size
     * resulting a 50% image size.
     */
    zoomout: function() {
        var e = new Zoom(0.5);
        Whiteboard.execute(e);
    },

    /**
     * Zooms the canvas amount specified by the factor
     * parameter.
     * 
     * @param factor The zoom factor. > 1 if zooming in
     * and < 1 if zooming out.
     */
    zoom: function(factor) {
        var e = new Zoom(factor);
        Whiteboard.execute(e);
    },

    /**
     * Rotates the canvas by the degrees defined by
     * the degree parameter.
     * 
     * @param degree The degree of the rotation event.
     */
    rotate: function(degree) {
        var e = new Rotate(degree);
        Whiteboard.execute(e);
    },

    /**
     * This function redraws the entire canvas 
     * according to the events in events.
    */
    redraw: function() {
        //this.init();
	    Whiteboard.context.clearRect(0,0,Whiteboard.canvas.width,Whiteboard.canvas.height);
        var redrawEvents = this.events;
        this.events = [];
        
        for(var i=0;i < redrawEvents.length; i++) {
            this.execute(redrawEvents[i]);
        }
    },

    /**
     * This removes the last event from this events 
     * and redraws (it can be made more 
     * effective then to redraw but time is limited)
    */
    undo: function() {
        reverseEvent = this.events.pop();
        console.log(reverseEvent.type);
        this.redraw();
    }

    /* === END ACTIONS === */

    };
})();
