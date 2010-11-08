function printError(error) {
  console.log("Error: ", error);
}

//(function() {
  var canvasPainter;
	var saveDrawing;
	var canvasAnimator;
	var colorWidget;
	var lineWidthWidget;
	var transportWidget;

	if(CanvasHelper.canvasExists("canvas")) {
		canvasPainter = new CanvasPainter("canvas", "canvasInterface", {x: 90, y: 10});
		//init save objects
		saveDrawing = new CPDrawing(canvasPainter);
		canvasAnimator = new CPAnimator(canvasPainter);

		//init widgets
		colorWidget = new ColorWidget('colorChooser', {x: 500, y: 10});
		colorWidget.addWidgetListener(function() {
			canvasPainter.setColor(colorWidget.colorString);
		});

		lineWidthWidget = new LineWidthWidget('lineWidthChooser', 10, {x: 500, y: 120});
		canvasPainter.setLineWidth(10);
		lineWidthWidget.addWidgetListener(function() {
			canvasPainter.setLineWidth(lineWidthWidget.lineWidth);
		});
		
		transportWidget = new TransportWidget('transportWidget', {x: 500, y: 190}, canvasAnimator);
	} else {
    // The browser can not handle canvas, do whatever is appropriated here.
	}

//})();


// Events binding:
$('#btn_0').click(function() {canvasPainter.setDrawAction(0)});
$('#btn_1').click(function() {canvasPainter.setDrawAction(1)});
$('#btn_2').click(function() {canvasPainter.setDrawAction(2)});
$('#btn_3').click(function() {canvasPainter.setDrawAction(3)});
$('#btn_4').click(function() {canvasPainter.setDrawAction(4)});
$('#btn_5').click(function() {canvasPainter.setDrawAction(5)});

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

// Just for colors:
$('#controls .draw_fct').click(function() {
  $('#controls .draw_fct.active').removeClass('active');
  $(this).addClass("active");
});


