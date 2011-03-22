$(document).ready(function() {
    var client = uce.createClient();

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
        }
        return $("<p>").text(event.type);
    }

    client.auth("root", "root", function() {
        $("#start").bind('click', function() {
            var terms = {query: $("#search").val()};
            terms = $.extend({}, terms, timerange[$("#time").val()]());
            client.search(terms, function(err, result) {
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
