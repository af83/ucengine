module('ucejs', {
    setup: function() {
        this.client = uce.createClient();
    },
    teardown: function() {
        $.mockjaxClear();
    }
});

var Factories = {
    createPresence: function() {
        return {"user": "myuid", "id": "mysid"};
    },
    getDefaultMeeting: function() {
        return uce.createClient().attachPresence(Factories.createPresence()).meeting("mymeeting");
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
    if (url[0] == "/")
        url = "/api/"+ uce.version + url;
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
    addUceApiCall("post", "/presence/", { "name": "name",
                                          "credential": "pwd" }, 200, '{"result": {"uid":"uid","sid":"sid"}}');
    var client = this.client;
    ok(!this.client.connected, "not connected");
    this.client.auth("name", "pwd", function(err, presence, xhr) {
        start();
        equals(err, null, "shoud not have error");
        equals(presence.user, "uid");
        equals(presence.id, "sid");
        equals(client.uid, "uid");
        ok(client.connected, "client connected");
    });
});

jackTest("can get a presence", function() {
    stop();
    $.mockjax({
        url : '/api/' + uce.version + '/presence/',
        responseText: {"result": {"uid":"myuid", "sid":"mysid"}}
    });
    $.mockjax({
        url : '/api/' + uce.version + '/presence/mysid',
        data: { "uid": "myuid", "sid": "mysid"},
        response: function() {
            this.responseText = {"result": {"uid": "myuid", "sid":"mysid"}};
        }
    });
    var client = this.client;
    this.client.auth("name", "pwd", function(err, presence) {
        client.presence(function(err, r, xhr) {
            start();
            equals(r.result.sid, presence.id);
            equals(err, null);
        });
    });
});

jackTest("can close a presence", function() {
    stop();
    $.mockjax({
        url : '/api/' + uce.version + '/presence/',
        responseText: {"result": {"uid":"myuid", "sid":"mysid"}}
    });
    $.mockjax({
        url : '/api/' + uce.version + '/presence/mysid',
        data: { "_method": "delete", "uid": "myuid", "sid": "mysid"},
        response: function() {
            this.responseText = {"result": "mysid"}
        }
    });
    var client = this.client;
    this.client.auth("name", "pwd", function(err, presence) {
        client.close(function(err, r, xhr) {
            start();
            ok(!client.connected, "not connected");
            equals(err, null);
        });
    });
});

jackTest("can list users", function() {
    stop();
    addUceApiCall("get", "/user/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "myuser"}, {}]}');
    this.client.attachPresence(Factories.createPresence()).users.get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 2);
    });
});

jackTest("can list meetings", function() {
    stop();
    addUceApiCall("get", "/meeting/", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : [{"name": "mymeeting"}]}');
    this.client.attachPresence(Factories.createPresence()).meetings(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.length, 1);
    });
});

test("meetings are the same", function() {
    var client = this.client.attachPresence(Factories.createPresence());
    ok(client.meeting("mymeeting") === client.meeting("mymeeting"));
    ok(client.meeting("mymeeting") !== client.meeting("meeting"));
});

jackTest("can get meeting", function() {
    stop();
    addUceApiCall("get", "/meeting/mymeeting", {"uid": "myuid", "sid": "mysid"}, 200, '{"result" : {"name": "mymeeting"}}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").get(function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
    });
});

jackTest("can update meeting", function() {
    stop();
    addUceApiCall("post", "/meeting/mymeeting",
                  {"uid": "myuid",
                   "sid": "mysid",
                   "_method": "put",
                   "metadata": {'description': "a brand new description"}},
                  200, '{"result" : "ok"}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting")
        .update({'description': 'a brand new description'},
                function(err, r, xhr) {
                    start();
                    equals(err, null);
                    equals(r.result, "ok");
                });
});

jackTest("can join meeting", function() {
    stop();
    addUceApiCall("post", "/meeting/mymeeting/roster/",  {"uid": "myuid", "sid": "mysid", "metadata": {nickname: "plop"}}, 200, '{"name": "mymeeting"}');
    var meeting = this.client.attachPresence(Factories.createPresence()).meeting("mymeeting");
    meeting.join({nickname: 'plop'}, function(err, r, xhr) {
        start();
        equals(err, null);
        equals(r.name, "mymeeting");
        equals(meeting.uid, "myuid");
    });
});

jackTest("can leave meeting", function() {
    stop();
    addUceApiCall("post", "/meeting/mymeeting/roster/myuid",  {"_method" :"delete", "uid": "myuid", "sid": "mysid"}, 200, '{"name": "mymeeting"}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").leave(function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can push event on meeting", function() {
    stop();
    addUceApiCall("post", "/event/mymeeting",  JSON.stringify({"uid": "myuid", "sid": "mysid", "type": "test_event", "metadata": {_mymetadata: "myvalue"}}, 200, ''));
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").push('test_event', {_mymetadata:"myvalue"}, function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("can push event on meeting with a parent", function() {
    stop();
    addUceApiCall("post", "/event/mymeeting",  JSON.stringify({"uid": "myuid", "sid": "mysid", "type": "test_event", "parent": "test", "metadata": {_mymetadata: "myvalue"}}, 200, ''));
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").push({parent: "test", type: 'test_event'}, {_mymetadata:"myvalue"}, function(err, r, xhr) {
        start();
        equals(err, null);
    });
});

jackTest("getEvents with callback on global success", function() {
    stop();
    addUceApiCall("get", "/event/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
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
    addUceApiCall("get", "/event/mymeeting",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": [{}, {}]}');
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
    addUceApiCall("get", "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({start: "pouet" }, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with end", function() {
    stop();
    addUceApiCall("get", "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "end": "plop"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({end: "plop"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("getEvents with type", function() {
    stop();
    addUceApiCall("get", "/event/mymeeting",  {"uid": "myuid", "sid": "mysid", "type": "chuck_norris"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").getEvents({type: "chuck_norris"}, function(err, r, xhr) {
        start();
    }, false);
});

jackTest("waitEvents without type param", function() {
    stop();
    addUceApiCall("get", "/live/mymeeting",  {"uid": "myuid", "sid": "mysid", "mode": "longpolling", "start": "pouet"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true, ["longpolling"]);
});

jackTest("waitEvents with type param", function() {
    stop();
    addUceApiCall("get", "/live/mymeeting",  {"uid": "myuid", "sid": "mysid", "mode": "longpolling", "start": "pouet", "type" : "chuck_norris"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({"start": "pouet", type: "chuck_norris"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true, ["longpolling"]);
});

jackTest("waitEvents without wait param", function() {
    stop();
    addUceApiCall("get", "/live/mymeeting",  {"uid": "myuid", "sid": "mysid", "mode": "longpolling", "start": "pouet"}, 200, '{"result": [{}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        start();
        equals(err, null);
    }, true, ["longpolling"]);
});

jackTest("waitEvents can be stopped", function() {
    expect(5);
    stop();
    addUceApiCall("get", "/live/mymeeting",  {"uid": "myuid", "sid": "mysid", "mode": "longpolling", "start": "pouet"}, 200, '{"result": [{}]}', {
        abort: function() {
            start();
            ok(true, "waitEvent stopped");
        }
    }, false, ["longpolling"]);
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {}, true, ["longpolling"]).stop();
});

test("waitEvents auto restart after wait", function() {
    expect(2);
    stop();
    var ajaxcall = 0; // nb of ajax request/response
    var called   = 0;
    $.mockjax({
        url: '/api/' + uce.version + '/live/mymeeting',
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
            }, false, ["longpolling"]);
        }
    };
    env.start();
});

jackTest("waitEvents, callback is called on each events", function() {
    stop();
    expect(5);
    var called = 0;
    addUceApiCall("get", "/live/mymeeting",  {"uid": "myuid", "sid": "mysid", "mode": "longpolling", "start": "pouet"}, 200, '{"result": [{}, {}]}');
    this.client.attachPresence(Factories.createPresence()).meeting("mymeeting").waitEvents({start: "pouet"}, function(err, r, xhr) {
        called++;
        if (called == 2) {
            start();
            ok(true, "callback have been called 2 times");
        }
    }, true, ["longpolling"]);
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
                url: '/api/' + uce.version + '/live/mymeeting',
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
        }).startLoop(1213, ["longpolling"]);
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
                        }).startLoop(1213, ["longpolling"]);
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
        }).startLoop(1213, ["longpolling"]);
    });
});

jackTest("this.client.time",  function() {
    stop();
    addUceApiCall("get", "/time",  {}, 200, '{"result": "4"}');
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
    addUceApiCall("post", "/user/",  {name: "test@example.net", auth: 'password', credential: 'mypwd', metadata: {nickname: 'test'}}, 200, '{"result":"uid"}');
    this.client.user.register('test@example.net', 'password', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"uid"});
    });
});

jackTest("register with password", function() {
    stop();
    addUceApiCall("post", "/user/",  {name: "test@example.net", auth: 'password', credential: 'mypwd', metadata: {nickname: 'test'}}, 200, '{"result":"uid"}');
    this.client.user.registerWithPassword('test@example.net', 'mypwd', {nickname: 'test'}, function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":"uid"});
    });
});

jackTest("get user", function() {
    stop();
    addUceApiCall("get", "/user/test@example.net",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": {}}');
    this.client.attachPresence(Factories.createPresence()).user.get('test@example.net', function(err, result) {
        start();
        equals(null, err);
        same(result, {"result":{}});
    });
});

jackTest("custom api url", function() {
    stop();
    addUceApiCall("get", "http://example.com/api/" + uce.version + "/user/test@example.net",  {"uid": "myuid", "sid": "mysid"}, 200, '{"result": {}}');
    var client = uce.createClient('http://example.com/api');
    var meeting = client.attachPresence(Factories.createPresence()).meeting("mymeeting");
    client.attachPresence(Factories.createPresence()).user.get('test@example.net', function(err, result) {
        start();
    });
});

module("ucejs.userAccess", {
    setup:function() {
        this.client = uce.createClient();
    }
});

jackTest("user.addRole", function() {
    stop();
    addUceApiCall("post", "/user/otheruid/roles", {"uid": "myuid",
                                                   "sid": "mysid",
                                                   "role": "newrole",
                                                   "location": "testmeeting"},
                  201, '{"result":"created"}');
    this.client
        .attachPresence(Factories.createPresence())
        .user.addRole("otheruid",
                      "newrole",
                      "testmeeting",
                      function(err, result) {
                          start();
                          equals(err, null);
                          same(result.result, "created");
                      });
});

jackTest("user.deleteRole", function() {
    stop();
    addUceApiCall("post", "/user/otheruid/roles/newrole/testmeeting",
                  {uid: "myuid",
                   sid: "mysid",
                   _method: "delete"},
                  200, '{"result":"ok"}');
    this.client.attachPresence(Factories.createPresence())
        .user.delRole("otheruid",
                         "newrole",
                         "testmeeting",
                         function(err, result) {
                             start();
                             equals(err, null);
                             same(result.result, "ok");
                         });
});

jackTest("user.can", function() {
    stop();
    addUceApiCall("get", "/user/otheruid/can/action/object/mymeeting",
                  {"conditions": {'condition_1': 'value'},
                   "uid": "myuid",
                   "sid": "mysid"}, 200, '{"result":"true"}');
    this.client.attachPresence(Factories.createPresence()).user.can("otheruid", "action", "object", {'condition_1': 'value'}, "mymeeting", function(err, result) {
        start();
        equals(err, null);
        same(result, true);
    });
});

jackTest("meeting.can", function() {
    stop();
    addUceApiCall("get", "/user/otheruid/can/action/object/mymeeting",
                  {"conditions": {'condition_1': 'value'},
                   "uid": "myuid",
                   "sid": "mysid"}, 200, '{"result":"true"}');
    this.client.attachPresence(Factories.createPresence()).
        meeting('mymeeting').can("otheruid", "action", "object", {'condition_1': 'value'}, function(err, result) {
        start();
        equals(err, null);
        ok(result);
    });
});

jackTest("meeting.canCurrentUser", function() {
    stop();
    addUceApiCall("get", "/user/myuid/can/action/object/mymeeting",
                  {"conditions": {'condition_1': 'value'},
                   "uid": "myuid",
                   "sid": "mysid"}, 200, '{"result":"true"}');
    this.client.attachPresence(Factories.createPresence()).
        meeting('mymeeting').canCurrentUser("action", "object", {'condition_1': 'value'}, function(err, result) {
        start();
        equals(err, null);
        ok(result);
    });
});

module("ucejs.role", {
    setup:function() {
        this.client = uce.createClient();
    }
});

jackTest("role.add", function() {
    stop();
    addUceApiCall("post", "/role",
                  {"name": "newrole",
                   "uid": "myuid",
                   "sid": "mysid"}, 201, '{"result":"created"}');
    this.client.attachPresence(Factories.createPresence())
        .role.add("newrole", function(err, result) {
            start();
            equals(err, null);
            same(result.result, "created");
        });
});

jackTest("role.delete", function() {
    stop();
    addUceApiCall("post", "/role/newrole",
                  {"uid": "myuid",
                   "sid": "mysid",
                   "_method": "delete"}, 200, '{"result":"ok"}');
    this.client.attachPresence(Factories.createPresence())
        .role.del("newrole", function(err, result) {
            start();
            equals(err, null);
            same(result.result, "ok");
        });
});

jackTest("role.addAccess", function() {
    stop();
    addUceApiCall("post", "/role/myrole/acl",
                  {"action": "access_action",
                   "object": "access_object",
                   "conditions": {'a': 'b', 'c': 'd'},
                   "uid": "myuid",
                   "sid": "mysid"}, 201, '{"result":"created"}');
    this.client.attachPresence(Factories.createPresence())
        .role.addAccess("myrole", "access_action", "access_object", {'a': 'b', 'c': 'd'},
                        function(err, result) {
                            start();
                            equals(err, null);
                            same(result.result, "created");
                        });
});

jackTest("role.deleteAccess", function() {
    stop();
    addUceApiCall("post", "/role/myrole/acl/access_action/access_object",
                  {"conditions": {'a': 'b', 'c': 'd'},
                   "uid": "myuid",
                   "sid": "mysid",
                   "_method": "delete"}, 200, '{"result":"ok"}');
    this.client.attachPresence(Factories.createPresence())
        .role.delAccess("myrole", "access_action", "access_object", {'a': 'b', 'c': 'd'},
                        function(err, result) {
                            start();
                            equals(err, null);
                            same(result.result, "ok");
                        });
});
