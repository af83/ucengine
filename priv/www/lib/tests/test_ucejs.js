module('ucejs', {
    setup: function() {
        this.client = uce.createClient();
    },
    teardown: function() {
        $.mockjaxClear();
    }
});

var Factories = {
    createFileEvent: function(params) {
        params = params || {}

        var metadata = $.extend({}, {id   : 'norris.pdf',
                                     name : 'norris.pdf'}, params);
        var eventId = params['eventId'] || "upload_event_id";
        var from = params['from'] || "test_user";

        return {
            id: eventId,
            from: from,
            type: "internal.file.add",
            metadata: metadata
        };
    },
    createConversionDoneEvent: function(params) {
        var metadata = {};

        $(params.pages).each(function(index, page) {
            metadata[index] = page;
        });

        return {
            type: "document.conversion.done",
            parent: params.parent,
            metadata: metadata
        };
    },
    createDocumentShareStartEvent: function(params) {
        var from = params['from'] || "chuck";
        var metadata = {id: params.id};

        if (params.page) {
            metadata.page = params.page;
        }
        return {
            type: "document.share.start",
            from: from,
            metadata: metadata
        };
    },
    createDocumentShareGotoEvent: function(params) {
        var from = params['from'] || "chuck";
        return {
            type: "document.share.goto",
            from: from,
            metadata: {
                page: params.page
            }
        };
    },
    createDocumentShareStopEvent: function(params) {
        var from = params['from'] || "chuck";
        return {
            type: "document.share.stop",
            from: from,
            metadata: {
                id: params.id
            }
        };
    },
    createRosterDeleteEvent: function(params) {
        var from = params['from'] || "chuck";
        return {
            type: "internal.roster.delete",
            from: from,
            metadata: {
                id: params.id
            }
        };
    },
    createPresence: function() {
        return {"user": "myuid", "id": "mysid"};
    },
    getDefaultMeeting: function() {
        return uce.createClient().attachPresence(Factories.createPresence()).meeting("mymeeting");
    },
    createStreamNew: function() {
        return {type: "video.stream.new",
                metadata : {token : "123456",
                            channel : "channel_1"}};
    },
    createStreamStart: function(broadcaster) {
        return {type: "video.stream.start",
                metadata: {broadcaster: broadcaster}}
    },
    createStreamStop: function(broadcaster) {
        return {type: "video.stream.stop",
                metadata: {broadcaster: broadcaster}}
    },
    createMeeting: function(start, end) {
        return {name: "ucemeeting",
                start_date: start,
                end_date: end,
                roster: ["chuck", "bruce"],
                metadata: {description: "test_description"}};
    }
};

function jackTest(name, fun) {
    test(name, function() {
        var that = this;
        jack($.proxy(fun, that));
        ok(true, "mock are ok");
    });
}

function addUceApiCall(method, url, data, status, responseText, xhr, callback) {
    jack.expect("$.ajax")
        .exactly("1 time")
        .mock(function(args) {
            equals(args.type, method, "");
            equals(args.url, url, "");
            same(args.data, data, "");
            setTimeout(function() {
                args.complete({
                    status      : status,
                    responseText: responseText
                });
                (callback || $.noop)();
            }, 1);
            return xhr;
        });
}

test("can be accessed via window.uce", function() {
    same(window.uce, uce, "");
});

jackTest("can create a presence", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/presence/", { "uid": "uid",
                                                  "credential": "pwd" }, 200, '{"result": "sid"}');
    var client = this.client;
    ok(!this.client.connected, "not connected");
    this.client.auth("uid", "pwd", function(err, presence, xhr) {
        start();
        equals(err, null, "shoud not have error");
        equals(presence.user, "uid");
        equals(presence.id, "sid");
        ok(client.connected, "client connected");
        equals(client.uid, "uid");
    });
});

jackTest("can create a presence with metadata", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/presence/", { "uid": "uid",
                                                                  "credential": "pwd",
                                                                  "metadata" : {"nickname": "nick"} }, 200, '{"result": "sid"}');
    this.client.auth("uid", "pwd", {nickname: "nick"}, function(err, presence, xhr) {
        start();
        equals(err, null, "shoud not have error");
        equals(presence.user, "uid", "");
        equals(presence.id, "sid", "");
    });
});

jackTest("can get a presence", function() {
    stop();
    $.mockjax({
        url : '/api/' + uce.version + '/presence/',
        responseText: {"result": "mysid"}
    });
    $.mockjax({
        url : '/api/' + uce.version + '/presence/mysid',
        data: { "uid": "myuid", "sid": "mysid"},
        response: function() {
            this.responseText = {"result": {"id": "mysid"}};
        }
    });
    var client = this.client;
    this.client.auth("myuid", "pwd", function(err, presence) {
        client.presence(function(err, r, xhr) {
            start();
            equals(r.result.id, presence.id);
            equals(err, null);
        });
    });
});

jackTest("can close a presence", function() {
    stop();
    $.mockjax({
        url : '/api/' + uce.version + '/presence/',
        responseText: {"result": "mysid"}
    });
    $.mockjax({
        url : '/api/' + uce.version + '/presence/mysid',
        data: { "_method": "delete", "uid": "myuid", "sid": "mysid"},
        response: function() {
            this.responseText = {"result": "mysid"}
        }
    });
    var client = this.client;
    this.client.auth("myuid", "pwd", function(err, presence) {
        client.close(function(err, r, xhr) {
            start();
            ok(!client.connected, "not connected");
            equals(err, null);
        });
    });
});

jackTest("can get current domain informations", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/infos/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : {"domain": "localhost", "metadata": {"name": "myuser", "plop": "plip"}}}');
    this.client.attachPresence(Factories.createPresence()).infos.get(function(err, r, xhr) {
        start();
        equals(err, null);
        same(r, {"domain": "localhost", "metadata": {name: 'myuser', plop: 'plip'}});
    });
});

jackTest("can update current domain informations", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/infos/", {"_method": "put", "uid": "myuid", "sid": "mysid", "metadata": {"pouet" : "pouet"}}, 200, '{"result" : "ok"}');
    var client = uce.createClient();
    this.client.attachPresence(Factories.createPresence()).infos.update({pouet: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
        same({result: "ok"}, r);
    });
});

jackTest("can list users", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/user/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "myuser"}, {}]}');
    this.client.attachPresence(Factories.createPresence()).users.get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can get opened meetings", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/meeting/opened", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "mymeeting"}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meetings.opened(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can get closed meetings", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/meeting/closed", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "mymeeting"}]}');
    this.client.attachPresence(Factories.createPresence()).meetings.closed(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get upcoming meetings", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/meeting/upcoming", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "mymeeting"}]}');
    this.client.attachPresence(Factories.createPresence()).meetings.upcoming(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get all meetings", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/meeting/all", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "mymeeting"}]}');
    this.client.attachPresence(Factories.createPresence()).meetings.all(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get meeting", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/meeting/all/mymeeting", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : {"name": "mymeeting"}}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
    });
});

jackTest("can join meeting", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/meeting/all/mymeeting/roster/",  {"uid": "myuid", "sid": "mysid"}, 200, '{"name": "mymeeting"}');
    var meeting = this.client.attachPresence(Factories.createPresence()).meeting("mymeeting");
    meeting.join(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
        equals(meeting.uid, "myuid");
    });
});

jackTest("can leave meeting", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/meeting/all/mymeeting/roster/myuid",  {"_method" :"delete", "uid": "myuid", "sid": "mysid"}, 200, '{"name": "mymeeting"}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").leave(function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can push event on meeting", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "type": "test_event", "metadata": {_mymetadata: "myvalue"}}, 200, '');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").push('test_event', {_mymetadata:"myvalue"}, function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("getEvents with callback on global success", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({}, function(err, r, xhr) {
        start();
        equals(err, null);
        same(r, [{}, {}]);
    }, false);
});

jackTest("getEvents with callback on each event", function() {
    stop();
    expect(8);
    var called = 0;
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({}, function(err, r, xhr) {
        equals(err, null);
        called++;
        if (called == 2) {
            start();
            same(r, {});
            ok(true, "callback have been called 2 times");
        }
    }, true);
});

jackTest("getEvents with start", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({start: "pouet" }, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with end", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "end": "plop"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({end: "plop"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with type", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "type": "chuck_norris"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({type: "chuck_norris"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("waitEvents without type param", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents with type param", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet", "type" : "chuck_norris"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({"start": "pouet", type: "chuck_norris"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents without wait param", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents can be stopped", function() {
    expect(5);
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}', {
        abort: function() {
            start();
            ok(true, "waitEvent stopped");
        }
    });
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {}, true).stop();
});

test("waitEvents auto restart after wait", function() {
    expect(2);
    stop();
    var ajaxcall = 0; // nb of ajax request/response
    var called   = 0;
    $.mockjax({
        url: '/api/' + uce.version + '/event/mymeeting',
        responseTime: 10,
        response: function() {
            this.responseText = {
                result: [{"datetime":"14"}]
            };
            ajaxcall++;
        }
    });
    var client = this.client;
    var env = {
        start: function() {
            var that = this;
            this.polling = client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
                called++;
                if (ajaxcall > 2)
                {
                    that.polling.stop(); // stop polling
                    // wait 1s to be sure long polling is stopped
                    setTimeout(function() {
                        start();
                        equals(3, ajaxcall, "ajaxcall");
                        equals(3, called, "callback");
                    }, 1000);
                }
            });
        }
    };
    env.start();
});

jackTest("waitEvents, callback is called on each events", function() {
    stop();
    expect(5);
    var called = 0;
    addUceApiCall("get", "/api/" + uce.version + "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        called++;
        if (called == 2) {
            start();
            ok(true, "callback have been called 2 times");
        }
    }, true);
});

function longPollingTest(events, test) {
    var env = {
        ajaxcall : 0,
        start: function() {
            this.mock();
            test(this);
        },
        mock : function() {
            var that = this;
            $.mockjax({
                url: '/api/' + uce.version + '/event/mymeeting',
                responseTime: 1,
                response: function() {
                    this.responseText = {
                        result: events
                    };
                    that.ajaxcall++;
                }
            });
        }
    };
    env.start();
}

test("startLoop with bind", function() {
    stop();
    var client = this.client.attachPresence(Factories.createPresence());
    longPollingTest([{type: "chuck_norris", datetime : 14}], function(longPolling) {
        longPolling.xhr = client.meeting("mymeeting").bind(function(event) {
            same(event, {type: "chuck_norris", datetime : 14});
            equals(longPolling.ajaxcall, 1);
            longPolling.xhr.stop();
            start();
        }).startLoop(1213);
    });
});

test("startLoop widgets/whatever can bind event handler with special type", function() {
    stop();
    expect(2);
    var client = this.client.attachPresence(Factories.createPresence());
    longPollingTest([{type: "chuck_norris", datetime : 14}, {type: "plop"}],
                    function(longPolling) {
                        longPolling.xhr = client.meeting("mymeeting").on("chuck_norris", function(event) {
                            same(event, {type : "chuck_norris", datetime: 14});
                            equals(longPolling.ajaxcall, 1);
                            longPolling.xhr.stop();
                            start();
                        }).startLoop(1213);
                    });
});

test("startLoop with 'bind', alias of 'on'", function() {
    stop()
    expect(2);
    var client = this.client.attachPresence(Factories.createPresence());
    longPollingTest([{type: "chuck_norris", datetime : 14}, {type: "plop"}],
                    function(longPolling) {
        longPolling.xhr = client.meeting("mymeeting").on("chuck_norris", function(event) {
            same(event, {type : "chuck_norris", datetime: 14});
            equals(longPolling.ajaxcall, 1);
            longPolling.xhr.stop();
            start();
        }).startLoop(1213);
    });
});

test("get upload url", function() {
    var url = this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getFileUploadUrl();
    equals(url, "/api/" + uce.version + "/file/mymeeting?uid=myuid&sid=mysid");
    var url = this.client.attachPresence({"user": "myuid2", "id": "mysid2"}).meeting("mymeeting2").getFileUploadUrl();
    equals(url, "/api/" + uce.version + "/file/mymeeting2?uid=myuid2&sid=mysid2");
});

test("get download file url", function() {
    var meeting = this.client.attachPresence(Factories.createPresence()).meeting("mymeeting");
    var url = meeting.getFileDownloadUrl('mydoc.pdf');
    equals(url, "/api/" + uce.version + "/file/mymeeting/mydoc.pdf?uid=myuid&sid=mysid");
    var url = meeting.getFileDownloadUrl('mydoc2.pdf');
    equals(url, "/api/" + uce.version + "/file/mymeeting/mydoc2.pdf?uid=myuid&sid=mysid");
});

jackTest("this.client.time",  function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/time",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": "4"}');
    this.client.attachPresence(Factories.createPresence()).time.get(function(err, result, xhr) {
        start();
        equals(null, err);
        equals(result, 4);
    });
});

test("uce waiter", function() {
    var called = 0;
    var waiter = this.client.getWaiter(4, function() {
        called++;
        equals(called, 1);
    });
    waiter();waiter();waiter();waiter();
});

jackTest("register new user", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/user/",  {uid: "test@example.net", auth: 'password', credential: 'mypwd', metadata: {nickname: 'test'}}, 200, '{"result":"created"}');
    this.client.user.register('test@example.net', 'password', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"created"});
    });
});

jackTest("register with password", function() {
    stop();
    addUceApiCall("post", "/api/" + uce.version + "/user/",  {uid: "test@example.net", auth: 'password', credential: 'mypwd', metadata: {nickname: 'test'}}, 200, '{"result":"created"}');
    this.client.user.registerWithPassword('test@example.net', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"created"});
    });
});

jackTest("get user", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/user/test@example.net",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": {}}');
    this.client.attachPresence(Factories.createPresence()).user.get('test@example.net', function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":{}});
    });
});

jackTest("custom api url", function() {
    stop();
    addUceApiCall("get", "http://example.com/api/" + uce.version + "/user/test@example.net",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": {}}');
    var client = uce.createClient('http://example.com');
    var meeting = client.attachPresence(Factories.createPresence()).meeting("mymeeting");
    var url = meeting.getFileDownloadUrl('mydoc.pdf');
    equals(url, "http://example.com/api/" + uce.version + "/file/mymeeting/mydoc.pdf?uid=myuid&sid=mysid");
    var url = meeting.getFileUploadUrl();
    equals(url, "http://example.com/api/" + uce.version + "/file/mymeeting?uid=myuid&sid=mysid");
    client.attachPresence(Factories.createPresence()).user.get('test@example.net', function(err, result) {
        start();
    });
});

module("ucejs.replay", {
    setup:function() {
        this.meeting = Factories.getDefaultMeeting();
    },
    teardown: function() {
        this.meeting.stopReplay();
    }
});

test("start orchestrator", function() {
    expect(2);
    stop();
    var that = this;
    this.meeting.bind("chuck_norris", function(event) {
        start();
        equal(event.type, "chuck_norris");
        equal(that.meeting.getCurrentReplay(), 100);
    }).startReplay(0, [{"type": "chuck_norris", "datetime":"14"},
                       {"type": "bruce_lee",    "datetime":"42"}]);
});

test("temporize events", function() {
    expect(4);
    stop();
    var called = 0;
    this.meeting.bind(function(event) {
        called++;
    }).startReplay(0, [{"type": "chuck_norris", "datetime":"1000"},
                       {"type": "jacky_chan",   "datetime":"3000"},
                       {"type": "bruce_lee",    "datetime":"5000"}]);
    // no events
    setTimeout(function() {
        equals(called, 0);
    }, 900);
    // chuck_norris == aie
    setTimeout(function() {
        equals(called, 1);
    }, 2000);
    // chuck_norris + jacky_chan == outch
    setTimeout(function() {
        equals(called, 2);
    }, 4000);
    // chuck_norris + jacky_chan + bruce_less == Armageddon
    setTimeout(function() {
        start();
        equals(called, 3); // everyone is dead ?
    }, 6500);
});

test("can stop orchestrator", function() {
    expect(2);
    stop();
    var called = 0, that = this;
    this.meeting.bind(function(event) {
        called++;
    }).startReplay(0, [{"type": "chuck_norris", "datetime":"1000"},
                       {"type": "bruce_lee",    "datetime":"2000"}]);
    // no events
    setTimeout(function() {
        that.meeting.stopReplay();
        equals(called, 1);
    }, 1500);
    setTimeout(function() {
        start();
        equals(called, 1);
    }, 2500);
});

test("can jump to a specific datetime in the future", function() {
    stop();
    var called = 0, that = this;
    this.meeting.bind(function(event) {
        called++;
    }).startReplay(0, [{"type": "chuck_norris", "datetime":1000},
                       {"type": "bruce_lee",    "datetime":2000},
                       {"type": "bruce_lee",    "datetime":4000}]);
    setTimeout(function() {
        that.meeting.jumpToReplay(2000);
    }, 100);
    setTimeout(function() {
        start();
        equals(called, 3);
    }, 2500);
});

test("can jump to a specific datetime in the past", function() {
    stop();
    var called = 0, that = this;
    this.meeting.bind(function(event) {
        called++;
    }).startReplay(0, [{"type": "chuck_norris", "datetime":1000},
                       {"type": "bruce_lee",    "datetime":2000},
                       {"type": "bruce_lee",    "datetime":2500}]);
    setTimeout(function() {
        that.meeting.jumpToReplay(100);
        equal(called, 1);
    }, 1500);
    setTimeout(function() {
        start();
        equals(called, 4);
    }, 4500);
});

module("ucejs.acl", {
    setup:function() {
        this.client = uce.createClient();
    }
});

jackTest("user.can", function() {
    stop();
    addUceApiCall("get", "/api/" + uce.version + "/user/otheruid/acl/all/all", {"uid": "myuid", "sid": "mysid"}, 200, '{"result":"true"}');
    this.client.attachPresence(Factories.createPresence()).user.can("otheruid", "all", "all", function(err, result) {
        start();
        equals(err, null);
        same(result, true);
    });
});
