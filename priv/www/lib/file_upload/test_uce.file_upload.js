module("uce.file_upload", {teardown: function() {
    $('#files_uploaded').fileupload('destroy');
}});

test("create basic structure", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    ok($('#files_uploaded').hasClass('ui-widget'), 'class ui-widget');
    ok($('#files_uploaded').hasClass('ui-fileupload'), 'class ui-fileupload');
    equals($('#files_uploaded').find('.ui-fileupload-list').size(), 1);
    equals($('#files_uploaded').find('.ui-fileupload-list').children().size(), 0);
    equals($('#files_uploaded').find('.ui-fileupload-files').children().size(), 2);
    equals($('#files_uploaded > div .ui-fileupload-add').size(), 1);
    equals($('#files_uploaded').find('.ui-fileupload-preview').children().size(), 2);
    equals($('#files_uploaded').find('.ui-preview-toolbar').children().size(), 5);
    equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'block');
    equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'none');
});

test("destroy everything", function() {
    $('#files_uploaded').fileupload();
    $('#files_uploaded').fileupload("destroy");
    equals($('#files_uploaded').children().size(), 0);
    ok(!$('#files_uploaded').hasClass('ui-widget'), 'class ui-widget');
    ok(!$('#files_uploaded').hasClass('ui-fileupload'), 'class ui-fileupload');
});

jackTest("handle new file upload", function() {
    var timestamp = new Date().getTime();
    var date = $.strftime('%m-%d-%y', timestamp);
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded')
        .fileupload({ucemeeting: ucemeeting})
        .fileupload('triggerUceEvent', Factories.createFileEvent({id : 'norris_pop_12.pdf',
                                                                   name : 'norris_pop.pdf',
                                                                   datetime : timestamp}));
    equals($('#files_uploaded ul > li').size(), 1);
    equals($('#files_uploaded ul > li:eq(0)').text(), 'norris_pop.pdf ' + date + ' by test_userDownload');
});

jackTest("handle new image upload", function() {
    var timestamp = new Date().getTime();
    var date = $.strftime('%m-%d-%y', timestamp);
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded')
        .fileupload({ucemeeting: ucemeeting})
        .fileupload('triggerUceEvent', Factories.createFileEvent({id : 'norris_pop_12.jpg',
                                                                   name : 'norris_pop.jpg',
                                                                   mime : 'image/jpeg',
                                                                   datetime : timestamp}));
    equals($('#files_uploaded ul > li').size(), 1);
    equals($('#files_uploaded ul > li:eq(0)').text(), 'norris_pop.jpg ' + date + ' by test_userDownload | Open in the viewer | Share');
});

jackTest("handle 2 files upload", function() {
    var timestamp = new Date().getTime();
    var date = $.strftime('%m-%d-%y', timestamp);
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $([Factories.createFileEvent({datetime: timestamp}),
       Factories.createFileEvent({id: 'lee.pdf', name: 'lee.pdf', datetime: timestamp})]).each(function(i, item) {
           $('#files_uploaded').fileupload('triggerUceEvent', item);
    });
    equals($('#files_uploaded').find('ul > li').size(), 2);
    equals($('#files_uploaded').find('ul > li:eq(0)').text(), 'norris.pdf ' + date + ' by test_userDownload');
    equals($('#files_uploaded').find('ul > li:eq(1)').text(), 'lee.pdf ' + date + ' by test_userDownload');
});

test("handle conversion done event", function() {
    var timestamp = new Date().getTime();
    var date = $.strftime('%m-%d-%y', timestamp);
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $([Factories.createFileEvent({eventId: "id_upload_event", datetime: timestamp, mime: 'application/pdf'}),
       Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: {"0": "page_1.jpg"}}),
       Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document"})]).each(function(i, item) {
           $('#files_uploaded').fileupload('triggerUceEvent', item);
    });
    equals($('#files_uploaded').find('ul > li:eq(0)').text(), 'norris.pdf ' + date + ' by test_userDownload | Open in the viewer | Share');
    equals($('#files_uploaded').find('ul > li:eq(1)').text(), '');
});

jackTest("when clicking the share link, fire an event", function() {
    var timestamp = new Date().getTime();
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event", datetime: timestamp}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document", datetime: timestamp})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
           $('#files_uploaded').fileupload('triggerUceEvent', event);
    });

    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .returnValue('#');

    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-share-link').click();
});

jackTest("when clicking the share link in toolbar, fire an event", function() {
    var timestamp = new Date().getTime();
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event", datetime: timestamp}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document", datetime: timestamp})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
           $('#files_uploaded').fileupload('triggerUceEvent', event);
    });

    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-preview-link').click();

    jack.expect("ucemeeting.push")
        .exactly("1 time")
        .returnValue('#');

    $('#files_uploaded').find('.ui-preview-toolbar .ui-share-link').click();
});

test("when clicking the view link, launch preview of document", function() {
    expect(3);
    var timestamp = new Date().getTime();
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event", datetime: timestamp}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg"]}),
         Factories.createFileEvent({id: "page_1.jpg", name: "page_1.jpg", from: "document", datetime: timestamp})];

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $(events).each(function(index, event) {
           $('#files_uploaded').fileupload('triggerUceEvent', event);
    });

    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-preview-link').click(function() {
        equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'none');
        equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'block');
        equals($('#files_uploaded .ui-fileupload.ui-selector-current').text(), "1", "The current page");
    });
    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-preview-link').click();
});

test("when clicking the view link, launch preview of image", function() {
    expect(3);
    var timestamp = new Date().getTime();
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl', 'push']);
    var event =
        Factories.createFileEvent({id: "page_1.jpg",
                                   name: "page_1.jpg",
                                   mime: 'image/jpeg',
                                   from: "chuck",
                                   datetime: timestamp});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $('#files_uploaded').fileupload('triggerUceEvent', event);

    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-preview-link').click(function() {
        equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'none');
        equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'block');
        equals($('#files_uploaded .ui-fileupload.ui-selector-current').text(), "1", "The current page");
    });
    $('#files_uploaded').find('ul > li a.ui-fileupload.ui-preview-link').click();
});

test("can hide upload button", function() {
    $('#files_uploaded').fileupload({upload: false});
    equals($('#files_uploaded .ui-fileupload-add').size(), 0);
});

test("can hide upload button after init", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    equals($('#files_uploaded .ui-fileupload-add').size(), 1);
    $('#files_uploaded').fileupload("option", "upload", false);
    equals($('#files_uploaded .ui-fileupload-add').css('display'), 'none');
    $('#files_uploaded').fileupload("option", "upload", true);
    equals($('#files_uploaded .ui-fileupload-add').css('display'), 'block');
});

test("clear file to share", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('toto');
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $('#files_uploaded').fileupload('triggerUceEvent', Factories.createFileEvent());
    $('#files_uploaded').fileupload("clear");
    equals($('#files_uploaded').find('ul > li').size(), 0);
});

test("view all", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $('#files_uploaded').fileupload("stopPreview");
    equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'block');
    equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'none');
});

test("view preview", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});
    $('#files_uploaded').fileupload("startPreview");
    equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'none');
    equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'block');
});

jackTest("when click on next, go to the right page", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg"]})]
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_uploaded').fileupload('triggerUceEvent', event);
    });

    $('#files_uploaded .ui-preview-link').click();
    $('#files_uploaded .ui-button-next').click();

    equals($('#files_uploaded .ui-fileupload.ui-selector-current').text(), "2", "The current page");
});

test("when click on previous, go to the right preview page", function() {
    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl', 'getFileDownloadUrl', 'push']);
    var events =
        [Factories.createFileEvent({eventId: "id_upload_event"}),
         Factories.createConversionDoneEvent({parent: 'id_upload_event', pages: ["page_1.jpg",
                                                                                 "page_2.jpg",
                                                                                 "page_3.jpg"]})]
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});

    jack.expect("ucemeeting.getFileDownloadUrl")
        .atLeast("1 time")
        .returnValue('#');

    $(events).each(function(index, event) {
           $('#files_uploaded').fileupload('triggerUceEvent', event);
    });

    $('#files_uploaded .ui-preview-link').click();
    $('#files_uploaded .ui-button-next').click();
    $('#files_uploaded .ui-button-next').click();
    $('#files_uploaded .ui-button-previous').click();

    equals($('#files_uploaded .ui-fileupload.ui-selector-current').text(), "2", "The current page");
});

test("when click on stop, hide preview", function() {
    expect(2);

    var ucemeeting = jack.create("ucemeeting", ['on', 'getFileUploadUrl']);
    $('#files_uploaded').fileupload({ucemeeting: ucemeeting});

    $('#files_uploaded .ui-button-stop').click(function() {
        equals($("#files_uploaded").find(".ui-fileupload-files").css('display'), 'block');
        equals($("#files_uploaded").find(".ui-fileupload-preview").css('display'), 'none');
    });

    $('#files_uploaded .ui-button-stop').click();
});

