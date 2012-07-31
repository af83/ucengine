/**
 * U.C.Engine library
 * http://ucengine.org/
 * (c) 2011 af83
 */
(function(g) {
    var VERSION = "0.6";

    function UCEngine(baseUrl) {

        baseUrl = baseUrl || '/api';

        function getCollection(url, params, callback) {
            get(url, params, function(err, result, xhr) {
                if (!err) {
                    callback(err, result.result, xhr);
                } else {
                    callback(err, result, xhr);
                }
            });
        }

        function format_api_url(url) {
            return baseUrl +'/' + VERSION + url;
        }

        function uce_api_call(method, url, data, callback, overridedOptions) {
            var call_back = callback || $.noop;
            url = format_api_url(url);
            var options = {
                type     : method,
                dataType : "json",
                url      : url,
                data     : data,
                complete : function(xhr, textStatus) {
                    var response = jQuery.parseJSON(xhr.responseText);
                    if (xhr.status >= 400) {
                        call_back(xhr.status, response, xhr);
                    } else {
                        call_back(null, response, xhr);
                    }
                }
            };
            return $.ajax($.extend(options, overridedOptions || {}));
        }

        function get(url, data, callback, options) {
            return uce_api_call("get", url, data, callback, options);
        }

        function post(url, data, callback, options) {
            return uce_api_call("post", url, data, callback, options);
        }

        function put(url, data, callback, options) {
            data = $.extend({"_method": "put"}, data);
            return uce_api_call("post", url, data, callback, options);
        }

        function del(url, data, callback, options) {
            data = $.extend({"_method": "delete"}, data);
            return uce_api_call("post", url, data, callback, options);
        }

        function UCEMeeting(client, meetingname, presence) {
            this.handlers = [];
            this.client = client;
            this.name = meetingname;
            this.uid = (presence || {}).user;
            this.sid = (presence || {}).id;
            this.params = {
                merge: function() {
                    var args = Array.prototype.slice.call(arguments);
                    args.unshift({}, {'uid': presence.user,
                                      'sid': presence.id});
                    return $.extend.apply($, args);
                }
            };
        }

        UCEMeeting.prototype = {
            get: function(callback) {
                get("/meeting/" + this.name, this.params.merge(),
                    function(err, result, xhr) {
                        if (!err) {
                            callback(err, result.result, xhr);
                        } else {
                            callback(err, result, xhr);
                        }
                    });
                return this;
            },
            update: function(metadata, callback) {
                var params = this.params.merge({'metadata': metadata});
                put("/meeting/" + this.name, params,
                    function(err, result, xhr) {
                        if (!callback) {
                            return;
                        }
                        callback(err, result, xhr);
                    });
            },
            join: function(metadata, callback) {
                post("/meeting/" + this.name + "/roster/",
                     this.params.merge({metadata: metadata}), callback);
                return this;
            },
            leave: function(callback) {
                del("/meeting/" + this.name + "/roster/" + this.uid,
                    this.params.merge(),
                    callback);
                return this;
            },
            getRoster: function(callback) {
                get("/meeting/" + this.name + "/roster",
                    this.params.merge(),
                    function (err, result, xhr) {
                        if (!callback) {
                            return;
                        }
                        var roster = result.result;
                        callback(err, roster, xhr);
                    });
            },

            /**
             * Generic Push event
             */
            _push: function(params, callback) {
                post("/event/" + this.name,
                     JSON.stringify(this.params.merge(params)),
                     callback, {contentType: "application/json"});
                return this;
            },
            /**
             * Push event to all users in the current meeting room
             * @param params can be a string with the type or an object
             */
            push: function(params, metadata, callback) {
                // is params a string ?
                if (params.charAt) {
                    params = {
                        type: params
                    }
                }
                var type = params.type;
                var parent = params.parent;
                this._push({type: type,
                            parent: parent,
                            metadata: metadata},
                           callback);
                return this;
            },
            /**
             * Push private event to the user in the current meeting room
             */
            pushTo: function(to, type, metadata, callback) {
                this._push({'type': type,
                            'to': to,
                            'metadata': metadata},
                           callback);
                return this;
            },

            /**
             * @param Object params
             *    search
             *    start you can use uce.time() (mandatory)
             *    type
             *    from
             * @param Function callback
             * @param Boolean one_shot
             * @param Array transports
             * @return Object with a stop() method
             */
            waitEvents: function(params, callback, one_shot, transportsSelected) {
                transportsSelected = transportsSelected || ['eventsource', 'longpolling'];
                var that = this;
                var transports = {
                    // EventSource transport
                    // http://dev.w3.org/html5/eventsource/
                    eventsource : {
                        available: window.EventSource,
                        fun: function() {
                            var getParams = that.params.merge({'mode': 'eventsource'}, params);
                            var source = new EventSource(format_api_url("/live/" + that.name) +"?"+ $.param(getParams));
                            source.onmessage = function(event) {
                                try {
                                    callback(null, $.parseJSON(event.data), null);
                                } catch (e) {
                                    // naive but it's better than nothing
                                    if (window.console) console.error(e);
                                }
                            };
                            return {
                                close: function() {
                                    source.close();
                                }
                            };
                        }
                    },
                    // long polling transport
                    longpolling: {
                        available: true,
                        fun: function() {
                            function startLongPolling(p, callback) {
                                var getParams = that.params.merge({'mode': 'longpolling'}, p);
                                return get("/live/" + that.name, getParams, callback);
                            }
                            var longPolling = {
                                aborted : false,
                                _start : function() {
                                    var that = this;
                                    this.xhr = startLongPolling(params, function(err, result, xhr) {
                                        try {
                                            var events = result.result;
                                            $.each(events, function(index, event) {
                                                try {
                                                    callback(err, event, xhr);
                                                } catch (e) {
                                                    // naive but it's better than nothing
                                                    if (window.console) console.error(e);
                                                }
                                            });
                                            if (events.length > 0) {
                                                params.start = parseInt(events[events.length - 1].datetime, 10) + 1;
                                            }
                                        } catch (e) {
                                            // do nothing
                                        }
                                        if (that.aborted === false && one_shot !== true) {
                                            that._start(params, callback);
                                        }
                                    });
                                },
                                stop: function() {
                                    this.aborted = true;
                                    this.xhr.abort();
                                }
                            };
                            longPolling._start();
                            return longPolling;
                        }
                    }
                };
                for (var i = 0; i < transportsSelected.length; i++) {
                    var transport = transports[transportsSelected[i]];
                    if (transport.available) {
                        return transport.fun();
                    }
                }
                throw new Error("no transport available");
            },

            /**
             * @param Object params
             *    search
             *    start
             *    end
             *    type
             *    from
             *    count
             *    page
             *    order
             * @param Function callback
             * @param Boolen onEachEvent
             */
            getEvents: function(params, callback, onEachEvent) {
                var that = this;
                params = this.params.merge(params);
                get("/event/" + this.name,
                    params,
                    function(err, result, xhr) {
                        if (!callback) {
                            return;
                        }
                        var events = result.result;
                        if (!onEachEvent) {
                            callback(err, events, xhr);
                        } else {
                            $.each(events, function(index, event) {
                                callback(err, event, xhr);
                            });
                        }
                    });
                return this;
            },

            /**
             * Trigger event on the internal queue
             * @param Object event
             *  - type
             */
            trigger: function(event) {
                $.each(this.handlers, function(i, item) {
                    if (!item.type) {
                        item.callback(event);
                    } else {
                        if (item.type == event.type)
                            item.callback(event);
                    }
                });
            },

            /**
             * Start main loop event
             * [@param Integer start]
             * [@param Array transport longpolling or eventsource]
             */
            startLoop: function(start, transports) {
                var that = this;
                return this.waitEvents({start: start || 0}, function(err, result, xhr) {
                    that.trigger(result);
                }, false, transports);
            },

            /**
             * Alias of on
             */
            bind: function() {
                var args = Array.prototype.slice.call(arguments);
                return this.on.apply(this, args);
            },

            /**
             * Bind event handler
             * use it with startLoop
             * [@param String type]
             * @param Function callback
             */
            on: function(type, callback) {
                if (!callback) {
                    callback  = type;
                    type = null;
                }
                this.handlers.push({type: type,
                                    callback: callback});
                return this;
            },
            /**
             * Remove event listener
             */
            unbind: function(type, callback) {
                 if (!callback) {
                    callback  = type;
                    type = null;
                }
                this.handlers = $(this.handlers).filter(function(index, handler) {
                    return !(handler.callback == callback && handler.type == type);
                });
                return this;
            },
            /**
             * Can the user make the action in the current meeting ?
             */
            can: function(uid, action, object, conditions, callback) {
                return this.client.user.can(uid, action, object, conditions, this.name, callback);
            },
            /**
             *
             */
            canCurrentUser: function(action, object, conditions, callback) {
                return this.can(this.uid, action, object, conditions, callback);
            }
        };

        var _presence = null;
        return {
            connected : false,
            uid: null,
            name: null,
            /**
             * Create user presence
             */
            auth: function(uname, credential, callback) {
                var params = {name: uname};
                name = uname;
                if (credential) {
                    params.credential = credential;
                }
                var that = this;
                post("/presence/", params, function(err, result, xhr) {
                    if (err) {
                        callback(err, result, xhr);
                    } else {
                        var uid = result.result.uid;
                        var p = {"user": uid, "id": result.result.sid, "name": name};
                        that.attachPresence(p);
                        callback(err, p, xhr);
                    }
                });
                return this;
            },
            /**
             * Get user presence
             */
            presence: function(callback) {
                get("/presence/" + _presence.id, {'uid': _presence.user,
                                                  'sid': _presence.id},
                    callback);
                return this;
            },
            /**
             * Close user presence
             */
            close: function(callback) {
                del("/presence/" + _presence.id, {'uid': _presence.user,
                                                  'sid': _presence.id},
                    callback);
                this.uid = null;
                this.connected = false;
                _presence = null;
                return this;
            },
            getWaiter : function(calls_needed, callback) {
                if(calls_needed == 0)
                    callback();
                var ok = true;
                var waiter = function(){
                    --calls_needed;
                    if (calls_needed == 0 && ok)
                        callback();
                    // XXX: should we raise an error if waiter called too many times?
                };
                return waiter;
            },
            /**
             * Attach presence to a new uce object
             */
            attachPresence : function(p) {
                _presence = p;
                this.connected = true;
                this.uid = p.user;
                this.name = p.name;
                return this;
            },
            _meetingsCache : {},
            meeting: function(meetingname) {
                if (!this._meetingsCache[meetingname])
                    this._meetingsCache[meetingname] = new UCEMeeting(this, meetingname, _presence);
                return this._meetingsCache[meetingname];
            },
            meetings : function(callback) {
                getCollection("/meeting/", {'uid': _presence.user,
                                            'sid': _presence.id}, callback);
                return this;
            },
            user: {
                register: function(name, auth, credential, metadata, callback) {
                    post("/user/", $.extend({}, {name: name, auth: auth, credential:credential, metadata:metadata}), function(err, result, xhr) {
                        callback(err, result, xhr);
                    });
                    return this;
                },
                registerWithPassword: function(name, credential, metadata, callback) {
                    return this.register(name, "password", credential, metadata, callback);
                },
                get: function(uid, callback) {
                    get("/user/"+ uid, $.extend({}, {'uid': _presence.user,
                                                     'sid': _presence.id}),
                        function(err, result, xhr) {
                            callback(err, result, xhr);
                        });
                },
                addRole: function(uid, role, location, callback) {
                    post("/user/" + uid + "/roles", {'uid': _presence.user,
                                                     'sid': _presence.id,
                                                     'role': role,
                                                     'location': location},
                         function(err, result, xhr) {
                             callback(err, result, xhr);
                         });
                },
                delRole: function(uid, role, location, callback) {
                    del("/user/" + uid + "/roles/" + role + "/" + location,
                        {'uid': _presence.user,
                         'sid': _presence.id},
                        function(err, result, xhr) {
                            callback(err, result, xhr);
                        });

                },
                can: function(uid, action, object, conditions, location, callback) {
                    get("/user/" + uid + "/can/" + action + "/" + object + "/" + location,
                        {'conditions': conditions,
                         'uid': _presence.user,
                         'sid': _presence.id},
                        function(err, result, xhr) {
                            if (err)
                                callback(err, result, xhr);
                            else
                                callback(err, result.result === "true", xhr);
                        });
                }
            },
            role: {
                add: function(name, callback) {
                    post("/role", {'name': name,
                                   'uid': _presence.user,
                                   'sid': _presence.id}, callback);
                },
                del: function(name, callback) {
                    del("/role/" + name,
                        {'uid': _presence.user,
                         'sid': _presence.id}, callback);
                },
                addAccess: function(role, action, object, conditions, callback) {
                    post("/role/" + role + "/acl",
                         {'action': action,
                          'object': object,
                          'conditions': conditions,
                          'uid': _presence.user,
                          'sid': _presence.id}, callback);
                },
                delAccess: function(role, action, object, conditions, callback) {
                    del("/role/" + role + "/acl/" + action + "/" + object,
                        {'conditions': conditions,
                         'uid': _presence.user,
                         'sid': _presence.id}, callback);
                }
            },
            users: {
                get: function(callback) {
                    getCollection("/user/", {'uid': _presence.user,
                                             'sid': _presence.id}, callback);
                    return this;
                }
            },
            time: {
                get: function(callback) {
                    get("/time", {},
                        function(err, result, xhr) {
                            callback(err, result.result, xhr);
                        });
                    return this;
                }
            }
        };
    }
    g.uce = {
        version: VERSION,
        createClient : function(baseUrl) {
            return new UCEngine(baseUrl || '');
        },
        getWaiter : function(calls_needed, callback) {
            if(calls_needed == 0)
                callback();
            var ok = true;
            var waiter = function(){
                --calls_needed;
                if (calls_needed == 0 && ok)
                    callback();
                // XXX: should we raise an error if waiter called too many times?
            };
            return waiter;
        }
    };

})(window);
