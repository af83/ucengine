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
    equals($('#files_shared').find('.ui-filesharing-preview-toolbox').children().size(), 6);
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
    equals($('#files_shared ul > li:eq(0)').text(), 'norris_pop.pdf');
});

jackTest("handle 2 files upload", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $([Factories.createFileEvent(),
       Factories.createFileEvent({id: 'lee.pdf', name: 'lee.pdf'})]).each(function(i, item) {
        $('#files_shared').file_sharing('triggerUceEvent', item);
    });
    equals($('#files_shared').find('ul > li').size(), 2);
    equals($('#files_shared').find('ul > li:eq(0)').text(), 'norris.pdf');
    equals($('#files_shared').find('ul > li:eq(1)').text(), 'lee.pdf');
});

test("handle conversion done event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting});
    $([Factories.createFileEvent({eventId: "id_upload_event"}),
       Factories.createConversionDoneEvent({parent: 'id_upload_event', metadata: {"0": ""}}),
       Factories.createFileEvent()]).each(function(i, item) {
	   $('#files_shared').file_sharing('triggerUceEvent', item);
    });
    equals($('#files_shared').find('ul > li:eq(0)').text(), 'norris.pdf (preview)');
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

test("handle new document share start", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files_shared').file_sharing({ucemeeting: ucemeeting}).file_sharing('triggerUceEvent', Factories.createDocumentShareStartEvent({id : 'norris_pop_25.pdf'}));
    equals($("#files_shared").find(".ui-filesharing-all").css('display'), 'none');
    equals($("#files_shared").find(".ui-filesharing-preview").css('display'), 'block');
    //equals($('#files_shared ul > li:eq(0)').text(), 'norris_pop.pdf');
});

