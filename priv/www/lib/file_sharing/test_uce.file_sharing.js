module("uce.file_sharing", {teardown: function() {
    $('#files_shared').file_sharing('destroy');
}});

test("create basic structure", function() {
    var ucemeeting = jack.create("ycemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    ok($('#files_shared').hasClass('ui-widget'), 'class ui-widget');
    ok($('#files_shared').hasClass('ui-filesharing'), 'class ui-filesharing');
    equals($('#files_shared').find('.ui-filesharing-list').size(), 1);
    equals($('#files_shared').find('.ui-filesharing-list').children().size(), 0);
    equals($('#files_shared').find('.ui-filesharing-all').children().size(), 2);
    equals($('#files_shared > div .ui-filesharing-add').size(), 1);
    equals($('#files_shared').find('.ui-filesharing-preview').children().size(), 3);
    equals($('#files_shared').find('.ui-preview-toolbar').children().size(), 4);
    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'none');
});

test("destroy everything", function() {
    $('#files_shared').file_sharing();
    $('#files_shared').file_sharing("destroy");
    equals($('#files_shared').children().size(), 0);
    ok(!$('#files_shared').hasClass('ui-widget'), 'class ui-widget');
    ok(!$('#files_shared').hasClass('ui-filesharing'), 'class ui-filesharing');
});

jackTest("handle new file upload", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting}).file_sharing('triggerUceEvent', Factories.createFileEvent({id : 'norris_pop_12.pdf',
                                                                                                                         name : 'norris_pop.pdf'}));
    equals($('#files_shared ul > li').size(), 1);
    equals($('#files_shared ul > li:eq(0)').text(), 'norris_pop.pdfFrom: test_user');
});

jackTest("handle 2 files upload", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $([Factories.createFileEvent(),
       Factories.createFileEvent({id: 'lee.pdf', name: 'lee.pdf'})]).each(function(i, item) {
        $('#files_shared').file_sharing('triggerUceEvent', item);
    });
    equals($('#files_shared').find('ul > li').size(), 2);
    equals($('#files_shared').find('ul > li:eq(0)').text(), 'norris.pdfFrom: test_user');
    equals($('#files_shared').find('ul > li:eq(1)').text(), 'lee.pdfFrom: test_user');
});

test("handle conversion done event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $([Factories.createFileEvent({eventId: "id_upload_event"}),
       Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: {"0": "page_1.jpg"}}),
       Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"})]).each(function(i, item) {
	   $('#files_shared').file_sharing('triggerUceEvent', item);
    });
    equals($('#files_shared').find('ul > li:eq(0)').text(), 'norris.pdf (preview)From: test_user');
    equals($('#files_shared').find('ul > li:eq(1)').text(), '');
});

jackTest("when clicking the preview link, fire a event", function() {
    expect(3);

    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"})];

    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "document.share.start");
            equals(metadata.id, "norris.pdf");
        });

    $('#files_shared').find('ul > li a').click();
});

test("can hide upload button", function() {
    $('#files_shared').file_sharing({upload: false});
    equals($('#files_shared .ui-filesharing-add').size(), 0);
});

test("can hide upload button after init", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    equals($('#files_shared .ui-filesharing-add').size(), 1);
    $('#files_shared').file_sharing("option", "upload", false);
    equals($('#files_shared .ui-filesharing-add').css('display'), 'none');
    $('#files_shared').file_sharing("option", "upload", true);
    equals($('#files_shared .ui-filesharing-add').css('display'), 'block');
});

test("clear file to share", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $('#files_shared').file_sharing('triggerUceEvent', Factories.createFileEvent());
    $('#files_shared').file_sharing("clear");
    equals($('#files_shared').find('ul > li').size(), 0);
});

test("view all", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $('#files_shared').file_sharing("viewAll");
    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'none');
});

test("view preview", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $('#files_shared').file_sharing("viewPreview");
    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'block');
});

jackTest("handle new document share start", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"})];

    $('#files_shared').file_sharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');

    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    $('#files_shared').file_sharing('triggerUceEvent', Factories.createDocumentShareStartEvent({id : 'norris.pdf'}));
    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing .ui-selector-current").text(), "1");
    equals($("#files_shared").find(".ui-filesharing .ui-selector-total").text(), "2");
    equals($("#files_shared").find(".ui-filesharing-preview-page").children().size(), 1);
    equals($("#files_shared .ui-filesharing-preview-page img").attr('src'), "toto");
});

jackTest("when a 'document.share.goto' event is received, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf'}),
	 Factories.createDocumentShareGotoEvent({page: "1"})];
    
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("2 time")
        .returnValue('toto');
    
    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing .ui-selector-current")
	   .text(), "2", "Current page");
    equals($("#files_shared").find(".ui-filesharing .ui-selector-total")
	   .text(), "2", "Total number of pages");
    equals($("#files_shared").find(".ui-filesharing-preview-page")
	   .children().size(), 1, "There is one image");
    equals($("#files_shared .ui-filesharing-preview-page img")
	   .attr('src'), "toto", "The image's url");
});

jackTest("check the from field when a 'document.share.goto' event is received", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf', from: "chuck"}),
	 Factories.createDocumentShareGotoEvent({page: "1", from: "bruce"})];
    
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    
    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing-preview .ui-selector-current")
	   .text(), "1", "Current page");
    equals($("#files_shared").find(".ui-filesharing-preview .ui-selector-total")
	   .text(), "2", "Total number of pages");
    equals($("#files_shared").find(".ui-filesharing-preview-page")
	   .children().size(), 1, "There is one image");
    equals($("#files_shared .ui-filesharing-preview-page img")
	   .attr('src'), "toto", "The image's url");
});

jackTest("when click on next, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf'})];

    $('#files_shared').file_sharing({ucemeeting: ucemeeting});

    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });
    
    jack.expect("ucemeeting.push")
	.exactly("1 time")
	.mock(function(type, metadata, callback) {
	    equals(type, "document.share.goto");
	    equals(metadata.page, 1);
	});

    $('#files_shared .ui-button-next').click();
});

jackTest("when click on previous, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf'}),
	 Factories.createDocumentShareGotoEvent({page: "1"})];

    $('#files_shared').file_sharing({ucemeeting: ucemeeting});

    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });
    
    jack.expect("ucemeeting.push")
	.exactly("1 time")
	.mock(function(type, metadata, callback) {
	    equals(type, "document.share.goto");
	    equals(metadata.page, 0);
	});

    $('#files_shared .ui-button-previous').click();
});

jackTest("when click on stop, send a 'document.share.stop' event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf'})];

    $('#files_shared').file_sharing({ucemeeting: ucemeeting});

    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });
    
    jack.expect("ucemeeting.push")
	.exactly("1 time")
	.mock(function(type, metadata, callback) {
	    equals(type, "document.share.stop");
	});

    $('#files_shared .ui-button-stop').click();
});

jackTest("when a 'document.share.stop' event is received, stop the file sharing", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf', from: 'chuck'}),
	 Factories.createDocumentShareStopEvent({id: 'norris.pdf', from: 'chuck'})];
    
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    
    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'none');
});

jackTest("check the from field when a 'document.share.stop' event is received", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
	[Factories.createFileEvent({eventId: "id_upload_event"}),
	 Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
										 "page_2.jpg"]}),
	 Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
    	 Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
	 Factories.createDocumentShareStartEvent({id: 'norris.pdf', from: 'chuck'}),
	 Factories.createDocumentShareStopEvent({id: 'norris.pdf', from: 'bruce'})];
    
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    
    $(events).each(function(index, event) {
	   $('#files_shared').file_sharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'block');
});
