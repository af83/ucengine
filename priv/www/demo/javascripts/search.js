$(document).ready(function() {
    var timerange = {
        all: function() {
            return {};
        },
        less_1: function() {
            return {
                start: (Date.now() - 60 * 1000)
            };
        },
        more_1: function() {
            return {
                end: (Date.now() - 60 * 1000)
            };
        },
        last_24: function() {
            return {
                start: Date.now() - 60 * 60 * 60 * 24 * 1000,
                end: Date.now()
            };
        }
    };

    function addTime(event) {
        return $('<abbr>').attr('class', 'timeago')
                          .data('timeago', {datetime: new Date(event.datetime)})
                          .text(event.datetime);
    }

    function formatEvent(event) {
        if (event.type == "chat.message.new") {
            return $("<div>").append($("<p>").append("New message from "+ event.from +" ")
                                             .append(addTime(event)))
                             .append($("<blockquote>").text(event.metadata.text));
        } else if (event.type == "twitter.hashtag.add") {
            return $("<div>").append($("<p>").append("Add new twitter hashtag: "+ event.metadata.hashtag))
                             .append(" ").append(addTime(event));
        } else if (event.type == "internal.roster.add") {
            return $("<div>").append($("<p>").append(event.from +" join "+ event.location  +" meeting"))
                             .append(" ").append(addTime(event));
        } else if (event.type == "internal.presence.add") {
            return $("<div>").append($("<p>").append(event.from +" is connected"))
                             .append(" ").append(addTime(event));
        } else if (event.type == "internal.presence.delete") {
            return $("<div>").append($("<p>").append(event.from +" logout"))
                             .append(" ").append(addTime(event));
        }
        return $("<p>").text(event.type).append(" ").append(addTime(event));
    }
    var client = uce.createClient();
    // user authentication with uid "root" and password "root"
    client.auth("root", "root", function() {
        // on search
        $("#start").bind('click', function() {
            var query = $.extend({query: $("#search").val()}, timerange[$("#time").val()]());
            // search in all meetings
            client.search(query, {order: 'desc'}, function(err, result) {
                if (err) throw err;
                var items = result.entries.map(function(entry) {
                    return $("<li>").append(formatEvent(entry)).get(0);
                });
                $("#results").empty().append($("<p>").text(result.entries.length +" results"));
                $("<ul>").append(items).appendTo($("#results"));
                $(".timeago").timeago();
            });
        });
    });
});
