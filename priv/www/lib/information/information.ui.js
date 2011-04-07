$.uce.widget("information", {
    options: {
        ucemeeting : null,
        uceclient : null,
        title: "Informations",
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
        this._addHeader(this.options.title, this.options.buttons);

        this._content = $('<div>')
            .attr('class', 'ui-widget-content')
            .appendTo(this.element);

        this._fields = {};

        this.options.uceclient.user
            .can(this.options.uceclient.uid, "update", "meeting", this.options.ucemeeting.name,
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
                return;
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
                .appendTo(field);
        }

        if (this._canEdit) {
            var that = this;
            this._fields[name].editable({onSubmit: function(content) {
                that.options.ucemeeting.get(function(err, meeting, xhr) {
                    meeting.metadata[name] = content.current;
                    that.options.ucemeeting
                        .update(meeting.start_date,
                                meeting.end_date,
                                meeting.metadata)
                });
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

    _setOption: function(key, value) {
        $.Widget.prototype._setOption.apply(this, arguments);
    },

    _handleMeetingUpdateEvent: function(event) {
        this._updateInformations();
    },

    destroy: function() {
        this.element.find('*').remove();
        this.element.removeClass('ui-widget ui-information');
        $.Widget.prototype.destroy.apply(this, arguments); // default destroy
    }
    });
