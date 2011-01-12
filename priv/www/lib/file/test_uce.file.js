module("uce.file", {teardown: function() {
    $('#files').file('destroy');
}});

test("create basic structure", function() {
    var ucemeeting = jack.create("ycemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files').file({ucemeeting: ucemeeting});
    ok($('#files').hasClass('ui-widget'), 'class ui-widget');
    ok($('#files').hasClass('ui-file'), 'class ui-file');
    equals($('#files').find('.ui-file-list').size(), 1);
    equals($('#files').find('.ui-file-list').children().size(), 3);
    equals($('#files').find('.ui-file-nb').text(), 'Files (0)');
    equals($('#files').find('.ui-file-new').text(), '0');
    equals($('#files').find('ul > li').size(), 0);
    equals($('#files > div').size(), 2);
    equals($('#files > div:eq(1) .ui-file-add').size(), 1);
});

test("destroy everything", function() {
    $('#files').file();
    $('#files').file("destroy");
    equals($('#files').children().size(), 0);
    ok(!$('#files').hasClass('ui-widget'), 'class ui-widget');
    ok(!$('#files').hasClass('ui-file'), 'class ui-file');
});

jackTest("bind event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileUploadUrl']);
    jack.expect("ucemeeting.bind")
        .exactly("1 time")
        .mock(function(eventname) {
            equals(eventname, "internal.file.add");
        });
    $('#files').file({ucemeeting: ucemeeting});
});

jackTest("handle new file event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.bind")
        .exactly("1 time")
        .mock(function(eventname, callback) {
            callback(Factories.createFileEvent({id : 'norris_pop.pdf'}));
        });
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    $('#files').file({ucemeeting: ucemeeting});
    equals($('#files').find('.ui-file-nb').text(), 'Files (1)');
    equals($('#files').find('ul > li').size(), 1);
    equals($('#files').find('ul > li:eq(0) a').text(), 'norris_pop.pdf');
    equals($('#files').find('ul > li:eq(0) a').attr('href'), 'toto');
    equals($('#files').find('.ui-file-new').text(), '1');
});

jackTest("handle 2 file event", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.bind")
        .exactly("1 time")
        .mock(function(eventname, callback) {
            callback(Factories.createFileEvent());
            callback(Factories.createFileEvent({id : 'lee.pdf'}));
            callback(Factories.createFileEvent({id : 'unknown.pdf'}));
        });
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("3 times")
        .returnValue('toto');
    $('#files').file({ucemeeting: ucemeeting});
    equals($('#files').find('.ui-file-nb').text(), 'Files (3)');
    equals($('#files').find('ul > li').size(), 3);
    equals($('#files').find('ul > li:eq(0)').text(), 'norris.pdf');
    equals($('#files').find('ul > li:eq(1)').text(), 'lee.pdf');
    equals($('#files').find('ul > li:eq(2)').text(), 'unknown.pdf');
    equals($('#files').find('.ui-file-new').text(), '3');
});

test("reinit nb new files counter on mouseover", function() {
     var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.bind")
        .exactly("1 time")
        .mock(function(eventname, callback) {
            callback(Factories.createFileEvent());
        });
    $('#files').file({ucemeeting: ucemeeting});
    equals($('#files').find('.ui-file-new').text(), '1');
    $('#files').find('.ui-file-list').mouseover();
    equals($('#files').find('.ui-file-new').text(), '0');
});

test("can hide upload button", function() {
    $('#files').file({upload: false});
    equals($('#files .ui-file-add').size(), 0);
});

test("can hide upload button after init", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    $('#files').file({ucemeeting: ucemeeting});
    equals($('#files .ui-file-add').size(), 1);
    $('#files').file("option", "upload", false);
    equals($('#files .ui-file-add').css('display'), 'none');
    $('#files').file("option", "upload", true);
    equals($('#files .ui-file-add').css('display'), 'block');
});

test("clear file", function() {
    var ucemeeting = jack.create("ucemeeting", ['bind', 'getFileDownloadUrl', 'getFileUploadUrl']);
    jack.expect("ucemeeting.bind")
        .exactly("1 time")
        .mock(function(eventname, callback) {
            callback(Factories.createFileEvent());
        });
    jack.expect("ucemeeting.getFileDownloadUrl")
        .exactly("1 time")
        .returnValue('toto');
    $('#files').file({ucemeeting: ucemeeting});
    equals($('#files').find('.ui-file-new').text(), '1');
    $('#files').file("clear");
    equals($('#files').find('.ui-file-new').text(), '0');
    equals($('#files').find('.ui-file-nb').text(), 'Files (0)');
    equals($('#files').find('ul > li').size(), 0);
});
