/**
 * U.C.Engine library
 * http://ucengine.org/
 * (c) 2011 af83
 */
(function(g) {
    var VERSION = "0.4";

    function UCEngine(baseUrl) {

        function getCollection(url, params, callback) {
            get(url, params, function(err, result, xhr) {
                if (!err) {
                    callback(err, result.result, xhr);
                } else {
                    callback(err, result, xhr);
                }
            });
        }

        function uce_api_call(method, url, data, callback) {
            var call_back = callback || $.noop;
            url = baseUrl +'/api/' + VERSION + url;
            return $.ajax({
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
            });
        }

        function get(url, data, callback) {
            return uce_api_call("get", url, data, callback);
        }

        function post(url, data, callback) {
            return uce_api_call("post", url, data, callback);
        }

        function put(url, data, callback) {
            data = $.extend({"_method": "put"}, data);
            return uce_api_call("post", url, data, callback);
        }

        function del(url, data, callback) {
            data = $.extend({"_method": "delete"}, data);
            return uce_api_call("post", url, data, callback);
        }

        var _presence = null;
        return {
            connected : false,
            uid: null,
            name: null,
            /**
             * Create user presence
             */
            auth: function(uname, credential, metadata, callback) {
                var params = {name: uname};
                name = uname;
                if (credential) {
                    params.credential = credential;
                }
                if (!callback) {
                    callback = metadata;
                } else {
                    params.metadata = metadata;
                }
                var that = this;
                post("/presence/", params, function(err, result, xhr) {
                    if (err) {
                        callback(err, result, xhr);
                    } else {
                        uid = result.result.uid;
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
            /**
             * Domain infos
             */
            infos: {
                /**
                 * Get infos
                 */
                get: function(callback) {
                    get("/infos/", {'uid': _presence.user,
                                    'sid': _presence.id},
                        function(err, result, xhr) {
                            if (!err) {
                                callback(err, result.result, xhr);
                            } else {
                                callback(err, result, xhr);
                            }
                        });
                    return this;
                },
                /**
                 * Update infos
                 */
                update: function(metadata, callback) {
                    put("/infos/", {'uid': _presence.user,
                                    'sid': _presence.id,
                                    metadata: metadata},
                        function(err, result, xhr) {
                        if (!err) {
                            callback(err, result, xhr);
                        } else {
                            callback(err, result, xhr);
                        }
                    });
                    return this;
                }
            },

            /**
             * Search events
             */
            search: function(terms, params, callback) {
                if (!callback) {
                    callback = params;
                    params = {};
                }
                var query = terms.query || '';
                delete terms.query;
                var searchTerms = [];
                for (var i in terms) {
                    searchTerms.push(i+":"+terms[i]);
                }
                searchTerms.push(query);
                get("/search/event",
                    $.extend({'uid': _presence.user,
                              'sid': _presence.id,
                              'searchTerms' : searchTerms.join(' ')}, params),
                    function (err, result, xhr) {
                        if (!callback) {
                            return;
                        }
                        callback(err, result.result, xhr);
                    });
            },

            meeting: function(meetingname) {
                var handlers = [];
                var client = this;
                return {
                    name: meetingname,
                    uid: (_presence || {}).user,
                    get: function(callback) {
                        get("/meeting/all/" + meetingname, {'uid': _presence.user,
                                                            'sid': _presence.id},
                            function(err, result, xhr) {
                            if (!err) {
                                callback(err, result.result, xhr);
                            } else {
                                callback(err, result, xhr);
                            }
                        });
                        return this;
                    },
                    join: function(callback) {
                        post("/meeting/all/" + meetingname + "/roster/",
                            {'uid': _presence.user,
                             'sid': _presence.id},
                            callback);
                        return this;
                    },
                    leave: function(callback) {
                        del("/meeting/all/" + meetingname + "/roster/" + _presence.user,
                            {'uid': _presence.user,
                             'sid': _presence.id},
                            callback);
                        return this;
                    },
                    getRoster: function(callback) {
                        get("/meeting/all/" + meetingname + "/roster",
                            {'uid': _presence.user,
                             'sid': _presence.id},
                            function (err, result, xhr) {
                                if (!callback) {
                                    return;
                                }
                                var roster = result.result;
                                callback(err, roster, xhr);
                            });
                    },

                    /**
                     * Push event
                     */
                    _push: function(params, callback) {
                        post("/event/" + meetingname,
                             $.extend({'uid': _presence.user, 'sid': _presence.id}, params),
                             callback);
                        return this;
                    },
                    push: function(type, metadata, callback) {
                        this._push({'type': type,
                                    'metadata': metadata},
                                   callback);
                        return this;
                    },
                    pushTo: function(to, type, metadata, callback) {
                        this._push({'type': type,
                                    'to': to,
                                    'metadata': metadata},
                                   callback);
                        return this;
                    },

                    /**
                     * Get file upload url for this meeting
                     */
                    getFileUploadUrl: function() {
                        return baseUrl +"/api/"+ VERSION +"/file/"+meetingname+"?uid="+_presence.user+"&sid="+_presence.id;
                    },

                    /**
                     * Get file download url
                     * @param String filename
                     */
                    getFileDownloadUrl: function(filename) {
                        return baseUrl +"/api/"+ VERSION +"/file/"+meetingname+"/"+ filename +"?uid="+_presence.user+"&sid="+_presence.id;
                    },

                    /**
                     * @param Object params
                     *    search
                     *    start you can use uce.time() (mandatory)
                     *    type
                     *    from
                     * @param Function callback
                     * @param Boolean one_shot
                     * @return Object with a stop() method
                     */
                    waitEvents: function(params, callback, one_shot) {
                        function startLongPolling(p, callback) {
                            var getParams = $.extend({}, {'uid': _presence.user,
                                                          'sid': _presence.id,
                                                          '_async': 'lp'}, params);
                            return get("/event/" + meetingname,
                                       getParams,
                                       callback);
                        }
                        var longPolling = {
                            aborted : false,
                            _start : function(p, callback) {
                                var that = this;
                                this.xhr = startLongPolling(p, function(err, result, xhr) {
                                    try {
                                        var events = result.result;
                                        $.each(events, function(index, event) {
                                            callback(err, event, xhr);
                                        });
                                        if (events.length > 0) {
                                            p.start = parseInt(events[events.length - 1].datetime, 10) + 1;
                                        }
                                    } catch (e) {
                                        // do nothing
                                    }
                                    if (that.aborted === false && one_shot !== true) {
                                        that._start(p, callback);
                                    }
                                });
                            },
                            stop: function() {
                                this.aborted = true;
                                this.xhr.abort();
                            }
                        };
                        longPolling._start(params, callback);
                        return longPolling;
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
                        params = $.extend({}, {'uid': _presence.user,
                                               'sid': _presence.id}, params);
                        get("/event/" + meetingname,
                            params,
                            function(err, result, xhr) {
                                if (!callback) {
                                    return;
                                }
                                var events = result.result;
                                if (!onEachEvent) {
                                    callback(err, events, xhr)
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
                        $.each(handlers, function(i, item) {
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
                     */
                    startLoop: function(start) {
                        var that = this;
                        return this.waitEvents({start: start || 0}, function(err, result, xhr) {
                            that.trigger(result);
                        });
                    },

                    /**
                     * Start replay loop event
                     * @param Integer start offset
                     * @param Array events
                     */
                    startReplay: function(start, events, index) {
                        this._replay_current_time = start;
                        if (!index) {
                            this._replay_events = events;
                            index = 0;
                        }
                        var next = null;
                        while (next = events[index]) {
                            if (next && start > next.datetime) {
                                this.trigger(next);
                                index++;
                            } else {
                                break;
                            }
                        }
                        if (next) {
                            this._replay_next_index = index;
                            var that = this;
                            var offset = 100; // each 100 milisecond
                            this._replay_temporized = setTimeout(function() {
                                that.startReplay(start + offset, events, index);
                            }, offset);
                        }
                    },

                    getCurrentReplay: function() {
                        return this._replay_current_time;
                    },

                    /**
                     * Jump to a specific datetime
                     */
                    jumpToReplay: function(datetime) {
                        this.stopReplay();
                        if (datetime > this._replay_current_time) {
                            this.startReplay(datetime, this._replay_events, this._replay_next_index);
                        } else {
                            this.startReplay(datetime, this._replay_events);
                        }
                    },

                    stopReplay: function() {
                        clearTimeout(this._replay_temporized);
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
                        handlers.push({type: type,
                                       callback: callback});
                        return this;
                    },
                    /**
                     * Search event in current meeting
                     */
                    search: function(terms, options, callback) {
                        terms.location = meetingname;
                        client.search(terms, options, callback);
                    }
                };
            },
            meetings : {
                opened: function(callback) {
                    return this._getCollection("opened", callback);
                },
                closed: function(callback) {
                    return this._getCollection("closed", callback);
                },
                upcoming: function(callback) {
                    return this._getCollection("upcoming", callback);
                },
                all: function(callback) {
                    return this._getCollection("all", callback);
                },
                _getCollection: function(type, callback) {
                    getCollection("/meeting/"+ type, {'uid': _presence.user,
                                                      'sid': _presence.id}, callback);
                    return this;
                }
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
                                                     'name': role,
                                                     'location': location},
                         function(err, result, xhr) {
                             callback(err, result, xhr);
                         });
                },
                deleteRole: function(uid, role, location, callback) {
                    del("/user/" + uid + "/roles/" + role + "/" + location,
                        {'uid': _presence.user,
                         'sid': _presence.id},
                        function(err, result, xhr) {
                            callback(err, result, xhr);
                        });

                },
                can: function(uid, action, object, location, callback) {
                    get("/user/" + uid + "/can/" + action + "/" + object + "/" + location,
                        {'uid': _presence.user,
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
                                   'sid': _presence.id},
                         function(err, result, xhr) {
                             callback(err, result, xhr);
                         });
                },
                del: function(name, callback) {
                    del("/role/" + name,
                        {'uid': _presence.user,
                         'sid': _presence.id},
                        function(err, result, xhr) {
                            callback(err, result, xhr);
                        });
                },
                addAccess: function(role, action, object, conditions, callback) {
                    post("/role/" + role + "/acl",
                         {'action': action,
                          'object': object,
                          'conditions': conditions,
                          'uid': _presence.user,
                          'sid': _presence.id},
                         function(err, result, xhr) {
                             callback(err, result, xhr);
                         });
                },
                deleteAccess: function(role, action, object, conditions, callback) {
                    del("/role/" + role + "/acl/" + action + "/" + object,
                        {'conditions': conditions,
                         'uid': _presence.user,
                         'sid': _presence.id},
                        function(err, result, xhr) {
                            callback(err, result, xhr);
                        });
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
