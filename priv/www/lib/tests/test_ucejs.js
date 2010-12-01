module('ucejs', {
    teardown: function() {
        $.mockjaxClear();
    }
});

var Factories = {
    createFileEvent: function(params) {
        var p = $.extend({}, {id : 'norris.pdf'}, params);
        return {
            type: "internal.file.add",
            metadata: {id : p.id}
        };
    },
    createPresence: function() {
        return {"uid": "myuid", "sid": "mysid"};
    },
    getDefaultMeeting: function() {
        return uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting");
    }
};

function jackTest(name, fun) {
    test(name, function() {
        jack(fun);
        ok(true, "mock are ok");
    });
}

// [[method, url, data, status, responseText, xhr, callback], ...]
function addUceApiCalls(calls) {
    jack.expect("$.ajax")
        .exactly(calls.length + " times")
        .mock(function(args) {
            $.each(calls, function(index, value) {
                if (value && value[1] == args.url) { //url
                    calls.splice(index, 1);
                    equals(args.type, value[0]); // method
                    equals(args.url, value[1]);
                    same(args.data, value[2]);
                    setTimeout(function() {
                        args.complete({
                            status      : value[3],
                            responseText: value[4]
                        });
                        (value[6] || $.noop)();
                    }, 1);
                    return value[5];
                }
            });
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

jackTest("can open a new presence", function() {
    stop();
    addUceApiCall("post", "/api/0.1/presence/uid", { "_method": "put", "auth": "password", "metadata": {"nickname": "nickname", "org": "af83"}, 200, '{"result": "sid"}');
    uce.presence.create("password", "", "org", "uid", "nickname",
                         function(err, presence, xhr) {
                             start();
                             equals(err, null, "shoud not have error");
                             equals(presence.uid, "uid", "");
                             equals(presence.sid, "sid", "");
                         });
});

jackTest("can close a presence", function() {
    stop();
    addUceApiCall("post", "/api/0.1/presence/myuid/mysid", { "_method": "delete", "uid": "myuid", "sid": "mysid"}, 200, '');
    uce.attachPresence(Factories.createPresence()).presence.close("af83", function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can list users", function() {
    stop();
    addUceApiCall("get", "/api/0.1/user/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "myuser"}, {}]}');
    uce.attachPresence(Factories.createPresence()).users.get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can get org", function() {
    stop();
    addUceApiCall("get", "/api/0.1/org/myorg", {}, 200, '{"result" : {"name": "myorg"}}');
    uce.org('myorg').get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "myorg");
    });
});

jackTest("can create org", function() {
    stop();
    addUceApiCall("post", "/api/0.1/org/myorg", {"_method": "put", "uid": "myuid", "sid": "mysid"}, 201, '{"result" : "ok"}');
    uce.attachPresence(Factories.createPresence()).org('myorg').create(function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can list orgs", function() {
    stop();
    addUceApiCall("get", "/api/0.1/org/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "myorg"}]}');
    uce.attachPresence(Factories.createPresence()).orgs.get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get opened meetings", function() {
    stop();
    addUceApiCall("get", "/api/0.1/meeting/myorg/opened", {}, 200, '{"result" : [{"name": "mymeeting"}, {}]}');
    uce.org("myorg").meetings.opened(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can get closed meetings", function() {
    stop();
    addUceApiCall("get", "/api/0.1/meeting/myorg/closed", {}, 200, '{"result" : [{"name": "mymeeting"}]}');
    uce.org("myorg").meetings.closed(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get upcoming meetings", function() {
    stop();
    addUceApiCall("get", "/api/0.1/meeting/myorg/upcoming", {}, 200, '{"result" : [{"name": "mymeeting"}]}');
    uce.org("myorg").meetings.upcoming(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can get all meetings", function() {
    stop();
    addUceApiCall("get", "/api/0.1/meeting/myorg/all", {}, 200, '{"result" : [{"name": "mymeeting"}]}');
    uce.org("myorg").meetings.all(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

jackTest("can chain get org and opened meetings", function() {
    stop();
    expect(11);
    addUceApiCalls([["get", "/api/0.1/org/myorg", {}, 200, '{"result" : {"name": "myorg"}}'],
                      ["get", "/api/0.1/meeting/myorg/opened", {}, 200, '{"result" : [{"name": "mymeeting"}, {}]}']]);
    uce.org('myorg').get(function(err, r, xhr) {
        equals(err, null);
        equals(r.name, "myorg");
    }).meetings.opened(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can get meeting", function() {
    stop();
    addUceApiCall("get", "/api/0.1/meeting/myorg/all/mymeeting", {}, 200, '{"result" : {"name": "mymeeting"}}');
    uce.org("myorg").meeting("mymeeting").get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
    });
});

jackTest("can join meeting", function() {
    stop();
    addUceApiCall("post", "/api/0.1/meeting/myorg/all/mymeeting/roster/myuid",  {"_method" :"put", "uid": "myuid", "sid": "mysid"}, 200, '{"name": "mymeeting"}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").join(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
    });
});

jackTest("can leave meeting", function() {
    stop();
    addUceApiCall("post", "/api/0.1/meeting/myorg/all/mymeeting/roster/myuid",  {"_method" :"delete", "uid": "myuid", "sid": "mysid"}, 200, '{"name": "mymeeting"}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").leave(function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can push event on meeting", function() {
    stop();
    addUceApiCall("post", "/api/0.1/event/myorg/mymeeting",  {"_method" :"put", "uid": "myuid", "sid": "mysid", "type": "test_event", "metadata": {_mymetadata: "myvalue"}}, 200, '');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").push('test_event', {_mymetadata:"myvalue"}, function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("getEvents with callback on global success", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getEvents({}, function(err, r, xhr) {
        start();
        equals(err, null);
        same(r, [{}, {}]);
    }, false);
});

jackTest("getEvents with callback on each event", function() {
    stop();
    expect(8);
    var called = 0;
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getEvents({}, function(err, r, xhr) {
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
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getEvents({start: "pouet" }, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with end", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "end": "plop"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getEvents({end: "plop"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with type", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "type": "chuck_norris"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getEvents({type: "chuck_norris"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("waitEvents without type param", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents with type param", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet", "type" : "chuck_norris"}, 200, '{"result": [{}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({"start": "pouet", type: "chuck_norris"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents without wait param", function() {
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true);
});

jackTest("waitEvents can be stopped", function() {
    expect(5);
    stop();
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}]}', {
        abort: function() {
            start();
            ok(true, "waitEvent stopped");
        }
    });
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {}, true).stop();
});

test("waitEvents auto restart after wait", function() {
    expect(2);
    stop();
    var ajaxcall = 0; // nb of ajax request/response
    var called   = 0;
    $.mockjax({
        url: '/api/0.1/event/myorg/mymeeting',
        responseTime: 10,
        response: function() {
            this.responseText = {
                result: [{"datetime":"14"}]
            };
            ajaxcall++;
        }
    });
    var env = {
        start: function() {
            var that = this;
            this.polling = uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
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
    addUceApiCall("get", "/api/0.1/event/myorg/mymeeting",  {"uid": "myuid", "sid": "mysid", "_async": "lp", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        called++;
        if (called == 2) {
            start();
            ok(true, "callback have been called 2 times");
        }
    }, true);
});

test("startLoop with bind", function() {
    stop();
    var env = {
        ajaxcall : 0,
        start: function() {
            this.mock();
            var that = this;
            this.xhr = uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").bind(function(event) {
                start();
                same(event, {datetime : "14"});
                equals(that.ajaxcall, 1);
                that.xhr.stop();
            }).startLoop(1213);
        },
        mock : function() {
            var that = this;
            $.mockjax({
                url: '/api/0.1/event/myorg/mymeeting',
                responseTime: 1,
                response: function() {
                    this.responseText = {
                        result: [{"datetime":"14"}]
                    };
                    that.ajaxcall++;
                }
            });
        }
    };
    env.start();
});

test("startLoop widgets/whatever can bind event handler with special type", function() {
    stop();
    expect(2);
    var env = {
        ajaxcall : 0,
        start: function() {
            this.mock();
            var that = this;
            this.xhr = uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").bind("chuck_norris", function(event) {
                start();
                same(event, {type : "chuck_norris"});
                equals(that.ajaxcall, 1);
                that.xhr.stop();
            }).startLoop(1213);
        },
        mock : function() {
            var that = this;
            $.mockjax({
                url: '/api/0.1/event/myorg/mymeeting',
                responseTime: 1,
                response: function() {
                    this.responseText = {
                        result: [{type: "chuck_norris"}, {type: "plop"}]
                    };
                    that.ajaxcall++;
                }
            });
        }
    };
    env.start();
});

test("get upload url", function() {
    var url = uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting").getFileUploadUrl();
    equals(url, "/api/0.1/file/myorg/mymeeting?uid=myuid&sid=mysid&_method=put");
    var url = uce.attachPresence({"uid": "myuid2", "sid": "mysid2"}).org("myorg2").meeting("mymeeting2").getFileUploadUrl();
    equals(url, "/api/0.1/file/myorg2/mymeeting2?uid=myuid2&sid=mysid2&_method=put");
});

test("get download file url", function() {
    var meeting = uce.attachPresence(Factories.createPresence()).org("myorg").meeting("mymeeting");
    var url = meeting.getFileDownloadUrl('mydoc.pdf');
    equals(url, "/api/0.1/file/myorg/mymeeting/mydoc.pdf?uid=myuid&sid=mysid");
    var url = meeting.getFileDownloadUrl('mydoc2.pdf');
    equals(url, "/api/0.1/file/myorg/mymeeting/mydoc2.pdf?uid=myuid&sid=mysid");
});

jackTest("uce.time",  function() {
    stop();
    addUceApiCall("get", "/api/0.1/time",  {}, 200, '{"result": "4"}');
    uce.time(function(err, result, xhr) {
        start();
        equals(null, err);
        equals(result, 4);
    });
});

test("uce waiter", function() {
    var called = 0;
    var waiter = uce.getWaiter(4, function() {
        called++;
        equals(called, 1);
    });
    waiter();waiter();waiter();waiter();
});

jackTest("register new user", function() {
    stop();
    addUceApiCall("post", "/api/0.1/user/test@example.net",  {auth: 'password', credential: 'mypwd', _method: 'put', metadata: {nickname: 'test'}}, 200, '{"result":"created"}');
    uce.user.register('test@example.net', 'password', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"created"});
    });
});

jackTest("register with password", function() {
    stop();
    addUceApiCall("post", "/api/0.1/user/test@example.net",  {auth: 'password', credential: 'mypwd', _method: 'put', metadata: {nickname: 'test'}}, 200, '{"result":"created"}');
    uce.user.registerWithPassword('test@example.net', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"created"});
    });
});

jackTest("get user", function() {
    stop();
    addUceApiCall("get", "/api/0.1/user/test@example.net",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": {}}');
    uce.attachPresence(Factories.createPresence()).user.get('test@example.net', function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":{}});
    });
});

module("ucejs.replay",
       {
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
    }, 1500);
    // chuck_norris + jacky_chan == outch
    setTimeout(function() {
        equals(called, 2);
    }, 3500);
    // chuck_norris + jacky_chan + bruce_less == Armageddon
    setTimeout(function() {
        start();
        equals(called, 3); // everyone is dead ?
    }, 6000);
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

module("ucejs.acl",
       {
           setup:function() {

           },
           teardown: function() {

           }
       });

jackTest("user.can", function() {
    stop();
    addUceApiCall("get", "/api/0.1/user/otheruid/acl/all/all", {"uid": "myuid", "sid": "mysid"}, 200, '{"result":"true"}');
    uce.attachPresence(Factories.createPresence()).user.can("otheruid", "all", "all", function(err, result) {
        start();
        equals(err, null);
        same(result, true);
    });
});
