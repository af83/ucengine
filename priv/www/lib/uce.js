/**
 * UCengine library
 * http://ucengine.org/
 * (c) 2010 af83
 */
(function(g) {
    var VERSION = "0.3";

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
        url = '/api/' + VERSION + url;
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

    function Uce(presence) {
        return {
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
            time: function(callback) {
                get("/time", {}, function(err, result, xhr) {
                    callback(err, result.result, xhr);
                });
                return this;
            },
            presence: {
                /**
                 * Create user presence
                 */
                create: function(credential, name, nickname, callback)
                {
                    var params = {"metadata" : {"nickname": nickname}};
                    if (credential) {
                        params.credential = credential;
                    }
                    params.uid = name;
                    post("/presence/", params, function(err, result, xhr) {
                        if (err) {
                            callback(err, result, xhr);
                        } else {
                            var presence = {"uid": name, "sid": result.result};
                            callback(err, presence, xhr);
                        }
                    });
                    return this;
                },
                /**
                 * Close user presence
                 */
                close: function(callback) {
                    del("/presence/" + presence.sid, presence, callback);
                    return this;
                }
            },
            /**
             * Attach presence to a new uce object
             */
            attachPresence : function(presence) {
                return new Uce(presence);
            },

            /**
             * Domain infos
             */
            infos: {
                /**
                 * Get infos
                 */
                get: function(callback) {
                    get("/infos/", {}, function(err, result, xhr) {
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
                    put("/infos/", $.extend({}, presence, {metadata: metadata}), function(err, result, xhr) {
                        if (!err) {
                            callback(err, result, xhr);
                        } else {
                            callback(err, result, xhr);
                        }
                    });
                    return this;
                }
            },

            meeting : function(meetingname) {
                var handlers = [];
                return {
                    name: meetingname,
                    uid: (presence || {}).uid,
                    get: function(callback) {
                        get("/meeting/all/" + meetingname, {}, function(err, result, xhr) {
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
                            presence,
                            callback);
                        return this;
                    },
                    leave: function(callback) {
                        del("/meeting/all/" + meetingname + "/roster/" + presence.uid, presence, callback);
                        return this;
                    },
                    getRoster: function(callback) {
                        get("/meeting/all/" + meetingname + "/roster",
                            presence,
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
                    push: function(type, metadata, callback) {
                        post("/event/" + meetingname,
                            $.extend({}, presence, {type: type, metadata: metadata}),
                            callback);
                        return this;
                    },
                    /**
                     * Get file upload url for this meeting
                     */
                    getFileUploadUrl: function() {
                        return "/api/"+ VERSION +"/file/"+meetingname+"?uid="+presence.uid+"&sid="+presence.sid;
                    },
                    /**
                     * Get file download url
                     * @param String filename
                     */
                    getFileDownloadUrl: function(filename) {
                        return "/api/"+ VERSION +"/file/"+meetingname+"/"+ filename +"?uid="+presence.uid+"&sid="+presence.sid;
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
                            var getParams = $.extend({}, presence, {"_async": "lp"}, p);
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
                        params = $.extend({}, presence, params);
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
                     * Bind event handler
                     * use it with startLoop
                     * [@param String type]
                     * @param Function callback
                     */
                    bind: function(type, callback) {
                        if (!callback) {
                            callback  = type;
                            type = null;
                        }
                        handlers.push({type: type,
                                       callback: callback});
                        return this;
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
                    getCollection("/meeting/"+ type, {}, callback);
                    return this;
                }
            },
            user: {
                register: function(uid, auth, credential, metadata, callback) {
                    post("/user/", $.extend({}, {uid: uid, auth: auth, credential:credential, metadata:metadata}), function(err, result, xhr) {
                        callback(err, result, xhr);
                    });
                    return this;
                },
                registerWithPassword: function(uid, credential, metadata, callback) {
                    return this.register(uid, "password", credential, metadata, callback);
                },
                get: function(uid, callback) {
                     get("/user/"+ uid, $.extend({}, presence), function(err, result, xhr) {
                        callback(err, result, xhr);
                    });
                },
                can: function(uid, object, action, callback) {
                    get("/user/"+ uid +"/acl/"+ action +"/"+ action, presence, function(err, result) {
                        if (err)
                            callback(err, result);
                        else
                            callback(err, result.result === "true");
                    });
                }
            },
            users: {
                get: function(callback) {
                    getCollection("/user/", presence, callback);
                    return this;
                }
            }
        };
    }
    g.uce = Uce();

})(window);
