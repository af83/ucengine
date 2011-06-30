(function($) {

    $(document).ready(onReady);

    function createAccount(client, uid) {
        return function(next) {
            client.user.register(uid, "none", "", {}, function(err, result) {
                console.log(result);
                next();
            });
        };
    }

    function authUser(client, uid) {
        return function(next) {
            client.auth(uid, "", function(err, result) {
                console.log(result);
                client.attachPresence(result);
                next();
            });
        };
    }

    function startLive(client) {
        return function() {
            var meeting = client.meeting("demo");
            $("<article>").attr("id", "whiteboard").appendTo($("#content")).whiteboard({ucemeeting: meeting});
            $("<article>").appendTo($("#content")).sample({ucemeeting: meeting});
            meeting.startLoop();
        };
    }

    function onReady() {
        var uid = "anonymous_"+ new Date().getTime();
        var client = uce.createClient();
        var sequence = require('futures').sequence();
        sequence.then(createAccount(client, uid));
        sequence.then(authUser(client, uid));
        sequence.then(startLive(client));
    }

})(jQuery);
