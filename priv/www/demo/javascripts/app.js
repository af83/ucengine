function sammyapp() {
    this.use('Mustache', 'tpl');
    this.use('NestedParams');
    this.use('Title');
    /**
     * State of user presence
     */
    var presence = {
        presence: null,
        user: null
    };
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
    function isLoaded(callback) {
        if (!infos) {
            uce.infos.get(function(err, result, xhr) {
                if (err) {
                    return;
                }
                $("#logoPartner").html('<img src="images/' + result.logo +'" style="vertical-align: middle;" />');
                infos = result;
                callback();
            });
        } else {
            callback();
        }
    }

    this.around(selectMenu);
    this.around(isLoaded);
    function build_home(callback) {
        var c = {welcome         : 'Welcome To U.C.Engine by af83',
                 description     : infos.description,
                 not_connected   : (presence.presence == null),
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
        uce.meetings.opened(function(err, result, xhr) {
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
        build_home(function(c) {
            context.loadPage('templates/index.tpl', c);
        });
    });
    this.post('#/user/login', function() {
        var uid      = this.params['email'];
        var nickname = uid;
        var password = this.params['password'];
        if (!uid) {
            return false;
        }
        var that = this;
        uce.presence.create(password, uid, nickname, function(err, result, xhr) {
            if (err) {
                return;
            }
            var presence = uce.attachPresence(result);
            that.trigger('connected', {me:uid, presence:presence});
        });
    });
    this.get('#/user/logout', function() {
        var that = this;
        presence.presence.presence.close(function () {
            that.trigger('disconnect');
            that.redirect('#/');
        });
    });
    this.get('#/meeting/:name/quit', function(context) {
        presence.presence.meeting(this.params['name']).leave(function(err) {
            if (err) {
                return;
            }
            context.redirect('#/');
        });
    });
    this.get('#/meeting/:name', function(context) {
        if (!presence.presence)
        {
            return this.redirect('#/');
        }
        this.title('Meeting');
        var meeting = presence.presence.meeting(this.params['name']);
        meeting.join(function(err, result, xhr) {})
            .get(function(err, result, xhr) {
                var c = {meeting_name  : result.name,
                         meeting_desc  : result.metadata.description,
                         meeting_users : ""};
                context.loadPage('templates/meeting.tpl', c, function() {
                    $.sammy.apps['#meeting'].run().trigger('connect-meeting', [meeting, result, presence]);
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
            uce.user.registerWithPassword(this.params['email'],
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

    this.before('#/admin', function() {
        if (!presence.user)
            this.redirect('#/');
    });
    this.get('#/admin', function(context) {
        this.title('Admin');
        var c = {users: []};
        var waiter = uce.getWaiter(2, function() {
            context.loadPage('templates/admin.tpl', c);
        });
        presence.presence.users.get(function(err, users) {
            waiter();
            if (err) throw err;
            c.users = users;
        });
    });

    this.post('#/admin/meeting', function() {
        var meeting = new EncreMeeting(this.params['name'],
                                       this.params['start'],
                                       this.params['end'],
                                       {description: this.params['description']});
        var that = this;
        meeting.create(presence.presence,
                       function() { that.app.runRoute('get', that.app.getLocation()); },
                       function() {});
    });

    this.get('#/admin/meeting/:meeting', function(context) {
        var meeting = presence.presence.meeting(this.params['meeting']);
        meeting.get(function(err, meeting) {
            context.loadPage("templates/meeting.tpl", meeting);
        });
    });

    this.del('#/admin/meeting/:meeting', function() {

    });

    this.del("#/admin/user/:user/acl/:action/:object/", function(e) {

    });

    this.del("#/admin/user/:user/acl/:action/:object/:domain", function(e) {

    });

    this.del('#/admin/user/:user', function(e) {

    });

    this.post('#/admin/user/:user', function(e) {

    });

    this.post('#/admin/user', function(e) {

    });

    this.get('#/admin/user/:user', function(e) {

    });

    this.get('#/about', function() {
        this.title('About');
        this.loadPage('templates/about.tpl', {});
    });
    this.get('#/tests', function() {
        this.loadPage('templates/tests.tpl');
    });
    this.bind('connected', function(event, data) {
        presence.presence = data.presence;
        presence.user    = data.me;
        //data.me.can(data.presence, "all", "all", [], function(user) {
        // add admin menu
        //$('<li><a href="#/admin">Admin</a></li>').insertBefore($("nav .page ul:first  li:last"));
        //}, function(user) {});
        $('header .page').append('<p><span>'+ presence.user +'</span> <a href="#/user/logout">Sign out</a></p>');
        this.app.runRoute('get', '#/');
    });
    this.bind('disconnect', function(event, data) {
        presence.presence = null;
        presence.user    = null;
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
    var loop = null;
    var inReplay = false;

    function fold(element) {
        element.find('.ui-button-toggle')
            .button('option', 'icons', {primary: "ui-icon-triangle-1-e"});
    };

    this.bind('connect-meeting', function(e, data) {
        meeting = data[0];
        result_meeting = data[1];
        presence = data[2];

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
        $('#reduced, #expanded').disableSelection();

        presence.presence.time(function(err, time, xhr) {
            console.log(time);
            addWidget("#timer", 'timer', {ucemeeting: meeting, start: time});
        });

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
            $('#files').file("option", "upload", false);
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
                    $('#files').file("clear");
                    $('#chat').chat("clear");
                    $('#filesharing').filesharing("clear");
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

    this.get('#/meeting/:name', function(context) {});

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
        $('#files').file("destroy");
        $('#filesharing').filesharing("destroy");
        this.unload();
    };
});
