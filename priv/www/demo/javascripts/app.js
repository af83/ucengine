function sammyapp() {
    this.use('Mustache', 'tpl');
    this.use('NestedParams');
    this.use('Title');
    var client = uce.createClient();
    var infos = null;

    this.setTitle(function(title) {
        return [title, " - U.C.Engine"].join('');
    });

    /**
     * select menu entry
     */
    function selectMenu(callback) {
        var href = this.app.getLocation();
        $("nav .page ul:first  li.on").removeClass('on');
        $("nav .page ul:first  li a[href='"+ href +"']").parent().addClass('on');
        callback();
    };

    this.helpers({
        loadPage : function(tpl, c, callback) {
            var that = this;
            // trick for tests
            if (window.location.pathname.match(/tests.html$/))
                tpl = 'demo/'+ tpl;
            this.render(tpl, c, function(data) {
                that.swap(data);
                (callback || $.noop)();
            });
        }
    });

    /**
     * Is infos loaded before anything else ?
     */
    function loadInfos(callback) {
        if (!infos) {
            client.infos.get(function(err, result, xhr) {
                if (err) {
                    return;
                }
                $("#logoPartner").html('<img src="images/' + result.metadata.logo +'" style="vertical-align: middle;" />');
                infos = result.metadata;
                callback();
            });
        } else {
            callback();
        }
    }

    function connectUser(callback) {
        if (!client.connected) {
            client.auth("anonymous", "", function(err, result, xhr) {
                if (err) {
                    return;
                }
                callback();
            });
        } else {
            callback();
        }
    }

    this.around(selectMenu);
    this.around(connectUser);
    this.around(loadInfos);

    function buildHome(callback) {
        var c = {welcome         : 'Welcome To U.C.Engine by af83',
                 description     : infos.description,
                 not_connected   : (client.name == "anonymous" || !client.connected),
                 format: function() {
                     return function(text, render) {
                         var timestamp = render(text);
                         var date = new Date(new Number(timestamp));
                         var minutes = date.getMinutes();
                         minutes = (minutes < 10) ? "0" + minutes : minutes;
                         var hours = date.getHours();
                         hours = (hours < 10) ? "0" + hours : hours;
                         var time = hours + ":" + minutes;
                         var month = date.getMonth();
                         month = (month < 10) ? "0" + month : month;
                         var day = date.getDate();
                         day = (day < 10) ? "0" + day : day;
                         var datetime = date.getFullYear() + "-" + day + "-" + month + " " + hours + ":" + minutes;
                         return datetime;
                     };
                 }};
        var waiter = uce.getWaiter(3, function() {
            callback(c);
        });
        client.meetings.opened(function(err, result, xhr) {
            if (err) {
                return;
            }
            c.currentmeetings = result;
            waiter();
        }).closed(function(err, result, xhr) {
            if (err) {
                return;
            }
            c.closedmeetings = result;
            waiter();
        }).upcoming(function(err, result, xhr) {
            if (err) {
                return;
            }
            c.upcomingmeetings = result;
            waiter();
        });
    };

    this.get('#/', function(context) {
        this.title('Home');
        buildHome(function(c) {
            context.loadPage('templates/index.tpl', c);
        });
    });
    this.post('#/user/login', function() {
        var name     = this.params['email'];
        var nickname = name;
        var password = this.params['password'];
        if (!name) {
            return false;
        }
        var that = this;
        client.auth(name, password, {nickname: nickname},
                    function(err, result, xhr) {
                        if (err) {
                            return;
                        }
                        that.trigger('connected', {me:name});
                    });
    });
    this.get('#/user/logout', function() {
        var that = this;
        client.close(function () {
            that.trigger('disconnect');
            that.redirect('#/');
        });
    });
    this.get('#/meeting/:name/quit', function(context) {
        client.meeting(this.params['name']).leave(function(err) {
            if (err) {
                return;
            }
            context.redirect('#/');
        });
    });
    this.get('#/meeting/:name', function(context) {
        if (!client.connected || client.name == 'anonymous')
        {
            return this.redirect('#/');
        }
        this.title('Meeting');
        var meeting = client.meeting(this.params['name']);

        var that = this;
        meeting.join(function(err, result, xhr) {})
            .get(function(err, result, xhr) {

                /* Make the user leave the meeting on window unload */
                window.onbeforeunload = function() {
                    client.meeting(that.params['name'])
                        .leave(function(err, result, xhr) {});
                };

                var c = {meeting_name  : result.name,
                         meeting_desc  : result.metadata.description,
                         meeting_users : ""};
                context.loadPage('templates/meeting.tpl', c, function() {
                    $.sammy.apps['#meeting'].run().trigger('connect-meeting', [meeting, result, client]);
                });
            });
    });
    this.get('#/register', function(context) {
        this.title('Register');
        context.loadPage('templates/register.tpl');
    });

    this.post('#/register', function(context) {
        errors = [];
        if (this.params['email'] == '') {
            errors.push("Email address required");
        }
        if (this.params['nickname'] == '') {
            errors.push("Nickname required");
        }
        if (this.params['pwd1'] != this.params['pwd2']) {
            errors.push("Passwords mismatch");
        }
        if (this.params['pwd1'] == '') {
            errors.push("Passwords required");
        }
        if (this.params['terms'] != 'on') {
            errors.push("Please accept the terms and conditions of U.C.Engine");
        }
        if (errors.length > 0) {
            context.loadPage('templates/register.tpl', {'errors': errors,
                                                        'has_error': true});
        }
        else {
            var that = this;
            client.user.registerWithPassword(this.params['email'],
                                             this.params['pwd1'],
                                             {'nickname': this.params['nickname'],
                                              'company': this.params['company']},
                                             function(err, result) {
                                                 if (err) {
                                                     context.loadPage('templates/register.tpl', {'errors': ['Email conflict'],
                                                                                                 'has_error': true});
                                                     return;
                                                 }
                                                 that.redirect("#/");
                                             });
        }
    });

    this.get('#/about', function() {
        this.title('About');
        this.loadPage('templates/about.tpl', {});
    });
    this.get('#/tests', function() {
        this.loadPage('templates/tests.tpl');
    });
    this.bind('connected', function(event, data) {
        var p = $('<p>')
            .attr('class', 'signout')
            .appendTo('header .page');
        var span = $('<span>')
            .text(client.uid)
            .appendTo(p);
        var a = $('<a>')
            .attr('href', '#/user/logout')
            .text('Sign out')
            .appendTo(p);

        this.app.runRoute('get', '#/');
    });
    this.bind('disconnect', function(event, data) {
        $('header .page p').remove();
    });
    this.notFound = function() {
        this.setLocation('#/');
    };
};

$.sammy("#meeting", function() {
    this.use('Mustache', 'tpl');
    this.use('NestedParams');

    var meeting = null;
    var result_meeting = null;
    var client = null;
    var loop = null;
    var inReplay = false;

    function fold(element) {
        element.find('.ui-button-toggle')
            .button('option', 'icons', {primary: "ui-icon-triangle-1-e"});
    };

    this.bind('connect-meeting', function(e, data) {
        meeting = data[0];
        result_meeting = data[1];
        client = data[2];

        var start = parseInt(result_meeting.start_date, 10);

        inReplay = new Date(parseInt(result_meeting.end_date, 10)) < new Date().getTime();

        function addWidget(id, widgetName, options) {
            var fold = $('<span>')
                .attr('class', 'ui-toolbar-button ui-button-fold')
                .button({
                    text: false,
                    icons: {primary: "ui-icon-triangle-1-s"}
                })
                .toggle(
                    function() {
                        widget.addClass('folded');
                        $(this).button('option', 'icons', {primary: "ui-icon-triangle-1-e"});
                        widget.find('.ui-widget-content').hide();
                    },
                    function() {
                        widget.removeClass('folded');
                        $(this).button('option', 'icons', {primary: "ui-icon-triangle-1-s"});
                        widget.find('.ui-widget-content').show();
                    });

            var widget = $(id);

            function expand() {
                widget.detach();
                widget.prependTo('#expanded');
                widget.addClass('expanded');
                $(this).button('option', 'icons', {primary: "ui-icon-circle-minus"});
                widget[widgetName]('expand');
                $(this).unbind('click');
                $(this).bind('click', reduce);
            };

            function reduce() {
                widget.detach();
                widget.prependTo('#reduced');
                widget.removeClass('expanded');
                $(this).button('option', 'icons', {primary: "ui-icon-circle-plus"});
                widget[widgetName]('reduce');
                $(this).unbind('click');
                $(this).bind('click', expand);
            }

            var toggle = $('<span>')
                .attr('class', 'ui-toolbar-button ui-button-toggle')
                .button({
                    text: false,
                    icons: {primary: "ui-icon-circle-plus"}
                })
                .click(expand);

            options['buttons'] = {left: [fold], right: [toggle]};
            widget[widgetName](options);

            widget.bind('received', function(event, container) {
                if (container == 'reduced') {
                    widget.removeClass('expanded');
                    toggle.button('option', 'icons', {primary: "ui-icon-circle-plus"});
                    widget[widgetName]('reduce');
                    toggle.unbind('click');
                    toggle.bind('click', expand);
                } else {
                    widget.addClass('expanded');
                    toggle.button('option', 'icons', {primary: "ui-icon-circle-minus"});
                    widget[widgetName]('expand');
                    toggle.unbind('click');
                    toggle.bind('click', reduce);
                }
            });

            if (options.mode == "expanded") {
                toggle.click();
            } else {
                widget[widgetName]('reduce');
            }
        };

        $('#reduced').sortable({connectWith: '.slots',
                                handle: '.ui-widget-header',
                                placeholder: 'highlight',
                                receive: function(event, widget) {
                                    $(widget.item).trigger('received', ['reduced']);
                                }});
        $('#expanded').sortable({connectWith: '.slots',
                                 handle: '.ui-widget-header',
                                 placeholder: 'highlight',
                                 receive: function(event, widget) {
                                     $(widget.item).trigger('received', ['expanded']);
                                 }});

        client.time.get(function(err, time, xhr) {
            addWidget("#timer", 'timer', {ucemeeting: meeting, start: time});
        });

        addWidget("#fileupload", 'fileupload', {ucemeeting: meeting,
                                                  mode: 'reduced',
                                                  dock: '#fileupload-dock'});

        addWidget("#filesharing", 'filesharing', {ucemeeting: meeting,
                                                  mode: 'reduced',
                                                  dock: '#filesharing-dock'});

        if (inReplay) {
            addWidget("#video", 'player', {src: result_meeting.metadata.video,
                                           start: result_meeting.start_date,
                                           dock: '#video-dock',
                                           width: 568,
                                           mode: 'expanded'});
        } else {
            addWidget("#video", 'video', {domain: document.location.hostname + "/ucengine",
                                          ucemeeting : meeting,
                                          dock: '#video-dock',
                                          mode: 'expanded'});
        }

        addWidget("#chat", 'chat', {ucemeeting: meeting,
                                    title: "Conversations",
                                    dock: '#chat-dock',
                                    mode: 'reduced'});

        addWidget("#whiteboard", 'whiteboard', {ucemeeting       : meeting,
                                                dock         : '#whiteboard-dock',
                                                width        : 574,
                                                mode         : 'reduced'});

        $("#replay-mode").hide();

        if (inReplay) {
            // disabled some widgets
            $('#whiteboard').whiteboard("option", {disabled: true});

            $('#search').search({ucemeeting: meeting});
            $("#replay-mode").show();

            // toggle results pane
            meeting.bind("internal.search.result", function() {
                $('.toggle-results').click();
            });
            $('.toggle-results').toggle(function() {
                $(this).toggleClass('show');
                $('#search-results').show(4);
            }, function() {
                $(this).toggleClass('show');
                $('#search-results').hide(3);
            });

            var events = meeting.getEvents({start: start}, function(err, result) {
                if (err) {
                    return;
                }
                function clearWidgets() {
                    $('#whiteboard').whiteboard("clear");
                    $('#chat').chat("clear");
                    $('#fileupload').fileupload("clear");
                }
                $("#replay").replay({
                    date_start: start,
                    date_end  : parseInt(result_meeting.end_date, 10),
                    start: function() {
                        var events = result.slice(0);
                        meeting.startReplay(start, events);
                        $('#video').player("play");
                        clearWidgets();
                    },
                    stop: function() {
                        meeting.stopReplay();
                        $('#video').player("stop");
                    },
                    jump: function(event, ui) {
                        $('#video').player("play");
                        var current = meeting.getCurrentReplay();
                        if (ui.timecode < current) {
                            clearWidgets();
                        }
                        meeting.jumpToReplay(ui.timecode);
                        $('#video').player("seek", ui.timecode);
                    }
                });
                $('#results').results({ucemeeting: meeting, start: parseInt(result_meeting.start_date, 10),
                                       jump: function(event, timecode) {
                                           $("#replay").replay("jump", timecode);
                                       }});
                $('#activity').activity({ucemeeting: meeting, start: parseInt(result_meeting.start_date, 10),
                                         jump: function(event, timecode) {
                                             $("#replay").replay("jump", timecode);
                                         }});

            }, false);
        } else {
            $('#video').player("play");
            // start main loop
            loop = meeting.startLoop(0);
        }
    });

    this.get('#/meeting/:id', function(context) {});

    this.notFound = function() {
        // destroy all
        if (inReplay) {
            meeting.stopReplay();
            $("#replay").replay("destroy");
            $('#video').player("destroy");

        } else {
            loop.stop();
            loop = null;
            $('#video').video("destroy");
        }
        $('#chat').chat("destroy");
        $('#whiteboard').whiteboard("destroy");
        $('#fileupload').fileupload("destroy");
        this.unload();
    };
});
