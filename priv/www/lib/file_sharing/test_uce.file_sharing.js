module("uce.file_sharing", {teardown: function() {
    $('#files_shared').filesharing('destroy');
}});

test("create basic structure", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').filesharing({ucemeeting: ucemeeting});
    ok($('#files_shared').hasClass('ui-widget'), 'class ui-widget');
    ok($('#files_shared').hasClass('ui-filesharing'), 'class ui-filesharing');
    equals($('#files_shared').find('.ui-widget-content').children().size(), 3);
    equals($('#files_shared').find('.ui-filesharing-toolbar').children().size(), 4);
    equals($("#files_shared").find(".ui-filesharing-toolbar").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-page").css('display'), 'none');
});

test("destroy everything", function() {
    $('#files_shared').filesharing();
    $('#files_shared').filesharing("destroy");
    equals($('#files_shared').children().size(), 0);
    ok(!$('#files_shared').hasClass('ui-widget'), 'class ui-widget');
    ok(!$('#files_shared').hasClass('ui-filesharing'), 'class ui-filesharing');
});

test("clear file to share", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('toto');
    $('#files_shared').filesharing({ucemeeting: ucemeeting});
    $('#files_shared').filesharing('triggerUceEvent', Factories.createFileEvent());
    $('#files_shared').filesharing("clear");
    equals($('#files_shared').find('ul > li').size(), 0);
});

jackTest("handle roster delete event", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"})];

    $('#files_shared').filesharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    $('#files_shared').filesharing('triggerUceEvent', Factories.createDocumentShareStartEvent({id : 'norris.pdf'}));
    $('#files_shared').filesharing('triggerUceEvent', Factories.createRosterDeleteEvent({id : 'chuck_in_roster_deletion'}));
    equals($("#files_shared").find(".ui-filesharing-toolbar").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-page").css('display'), 'none');
});

jackTest("handle new document share start", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"})];

    $('#files_shared').filesharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    $('#files_shared').filesharing('triggerUceEvent', Factories.createDocumentShareStartEvent({id : 'norris.pdf'}));
    equals($("#files_shared").find(".ui-filesharing-toolbar").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing-page").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing .ui-selector-current").text(), "1");
    equals($("#files_shared").find(".ui-filesharing .ui-selector-total").text(), "2");
    equals($("#files_shared").find(".ui-filesharing-page").children().size(), 1);
    equals($("#files_shared .ui-filesharing-page img").attr('src'), "#");
});

jackTest("handle new document share start with a starting page number", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg",
                                                                                 "page_3.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_3.jpg", name: "page_3.jpg", from: "document"})];

    $('#files_shared').filesharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    $('#files_shared').filesharing('triggerUceEvent', Factories.createDocumentShareStartEvent({id : 'norris.pdf',
                                                                                               page: '1'}));
    equals($("#files_shared").find(".ui-filesharing-toolbar").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing-page").css('display'), 'block');
    equals($("#files_shared").find(".ui-filesharing .ui-selector-current").text(), "2");
    equals($("#files_shared").find(".ui-filesharing .ui-selector-total").text(), "3");
    equals($("#files_shared").find(".ui-filesharing-page").children().size(), 1);
    equals($("#files_shared .ui-filesharing-page img").attr('src'), "#");
});

jackTest("when a 'document.share.goto' event is received, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createDocumentShareStartEvent({id: 'norris.pdf'}),
         Factories.createDocumentShareGotoEvent({page: "1"})];

    $('#files_shared').filesharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing .ui-selector-current")
           .text(), "2", "Current page");
    equals($("#files_shared").find(".ui-filesharing .ui-selector-total")
           .text(), "2", "Total number of pages");
    equals($("#files_shared").find(".ui-filesharing-page")
           .children().size(), 1, "There is one image");
    equals($("#files_shared .ui-filesharing-page img")
           .attr('src'), "#", "The image's url");
});

jackTest("when click on next, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createDocumentShareStartEvent({id: 'norris.pdf'})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue("#");

    $('#files_shared').filesharing({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
        $('#files_shared').filesharing('triggerUceEvent', event);
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
    var ucemeeting = jack.create("ucemeeting", ['on', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createDocumentShareStartEvent({id: 'norris.pdf'}),
         Factories.createDocumentShareGotoEvent({page: "1"})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue("#");

    $('#files_shared').filesharing({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
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
    var ucemeeting = jack.create("ucemeeting", ['on', 'push', 'getFileUploadUrl', 'getFileDownloadUrl']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createDocumentShareStartEvent({id: 'norris.pdf'})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue("#");

    $('#files_shared').filesharing({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .mock(function(type, metadata, callback) {
            equals(type, "document.share.stop");
        });

    $('#files_shared .ui-button-stop').click();
});

jackTest("when a 'document.share.stop' event is received, stop the file sharing", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"}),
         Factories.createFileEvent({id: "page_2.jpg", name: "page_2.jpg", from: "document"}),
         Factories.createDocumentShareStartEvent({id: 'norris.pdf', from: 'chuck'}),
         Factories.createDocumentShareStopEvent({id: 'norris.pdf', from: 'chuck'})];

    $('#files_shared').filesharing({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_shared').filesharing('triggerUceEvent', event);
    });

    equals($("#files_shared").find(".ui-filesharing-toolbar").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-page").css('display'), 'none');
});

