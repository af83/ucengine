$.uce.Information = function() {}
$.uce.Information.prototype = {
    options: {
        ucemeeting : null,
        uceclient : null,
        title: "Informations",
        maxlength: 140,
        fields: {'name': {title: "Meeting Name",
                          placeholder: "Enter the name of the meeting room"},
                 'description': {title: "Description",
                                 placeholder: "Summarize the topic"}}
    },

    // ucengine events
    meetingsEvents: {
        'internal.meeting.update'     : '_handleMeetingUpdateEvent'
    },

    _create: function() {
        var that = this;

        this.element.addClass('ui-widget ui-information');
        this.addHeader();

        this._content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);

        this._fields = {};

        this.options.uceclient.user
            .can(this.options.uceclient.uid, "update", "meeting", {}, this.options.ucemeeting.name,
                 function(err, result, xhr) {
                     that._canEdit = result;
                     that._updateInformations();
                 });
    },

    _updateFields: function() {
        this._content.children().remove();
        var informations = $('<ul>')
            .addClass('ui-information-list')
            .appendTo(this._content);

        var that = this;
        $.each(that.options.fields, function(name, field) {
            if (!that._canEdit && (!field.value || field.value == "")) {
                if (!field.text) {
                    return;
                }
                field.value = field.text;
            }
            that._createField(name,
                              field.title,
                              field.value || "",
                              field.placeholder || "Edit")
                .appendTo(informations);
        });
    },

    _createField: function(name, title, value, placeholder) {
        var field = $('<li>')
            .addClass('ui-information-' + name)
            .append($('<span>')
                    .addClass('ui-information-title')
                    .text(title));

        var that = this;
        if (placeholder && (!value || value == "")) {
            this._fields[name] = $('<span>')
                .addClass('ui-information-placeholder')
                .text(placeholder)
                .click(function() {
                    $(this).text("");
                })
                .appendTo(field);
        } else if (value) {
            this._fields[name] = $('<span>')
                .addClass('ui-information-value')
                .text(value)
                .appendTo(field)

            if (value.length > this.options.maxlength) {
                var subValue = value.substring(0, this.options.maxlength);
                this._fields[name].text(subValue);

                var sizeToggle = $('<a>')
                    .attr('href', '#')
                    .addClass('ui-information-size-toggle')
                    .text("More")
                    .toggle(
                        function() {
                            that._fields[name].text(value);
                            sizeToggle.text("Less");
                        },
                        function() {
                            that._fields[name].text(subValue);
                            sizeToggle.text("More");
                        })
                    .appendTo(field);
            }
        }

        if (this._canEdit) {
            this._fields[name]
                .addClass('ui-information-editable')
                .click(function () {
                    $(this).text(value);
                })
                .editable({onEdit: function() {
                    var $this = this;
                    $this.bind('keyup', function(e) {
                        if (e.keyCode == 13) { // ENTER key
                            // simulate blur event to save change
                            $this.trigger('blur');
                        }
                        if (e.keyCode == 27) { // ECHAP key
                            // holy hack, current options are saved by the plugin
                            var opts = $this.data('editable.options');
                            opts.toNonEditable($this, false);
                            e.preventDefault();
                        }
                    })
                },
                onSubmit: function(content) {
                    that.options.ucemeeting.get(function(err, meeting, xhr) {
                        meeting.metadata[name] = content.current;
                        that.options.ucemeeting
                            .update(meeting.start_date,
                                    meeting.end_date,
                                    meeting.metadata)
                    });
                    that._updateFields();
                }});
        }
        return (field);
    },

    _updateInformations: function() {
        var that = this;
        this.options.ucemeeting.get(function(err, meeting, xhr) {
            $.each(that.options.fields, function(name, field) {
                field.value = meeting.metadata[name];
            });
            that._updateFields();
        });
    },

    reduce: function() {},
    expand: function() {},

    _handleMeetingUpdateEvent: function(event) {
        this._updateInformations();
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-information');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
};
$.uce.widget("information", new $.uce.Information());
