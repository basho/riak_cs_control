minispade.register('models', function() {

  RiakCsControl.User = DS.Model.extend({
    primaryKey: "key_id",

    // We don't use ID in the app right now, so ignore it and alias the
    // key_id to the id to work around outstanding ember-data bugs.
    //
    // id: DS.attr("string"),
    id: function() {
      return this.get('key_id');
    }.property('key_id'),

    name: DS.attr("string"),
    email: DS.attr("string"),
    key_id: DS.attr("string"),
    key_secret: DS.attr("string"),
    display_name: DS.attr("string"),
    new_key_secret: DS.attr("boolean"),

    admin: DS.attr("boolean"),

    status: DS.attr("string"),

    isDisabled: function() {
      return this.get('status') === 'disabled';
    }.property('status'),

    isAdmin: function() {
      return this.get('admin');
    }.property('admin'),

    disable: function() {
      this.set('status', 'disabled');
    },

    enable: function() {
      this.set('status', 'enabled');
    },

    revoke: function() {
      this.set('new_key_secret', true);
    },

    didUpdate: function() {
      this.reload();
    }
  });

});
