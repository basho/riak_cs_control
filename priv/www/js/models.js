minispade.register('models', function() {

  DS.Model.reopen({
    reload: function() {
      var store = this.get('store');
      var adapter = store.get('adapter');
      adapter.find(store, this.constructor, this.get('id'));
    }
  });

  DS.RecordArray.reopen({
    reload: function() {
      Ember.assert("Can only reload base RecordArrays",
        this.constructor === DS.RecordArray);
      var store = this.get('store');
      var adapter = store.get('adapter');
      adapter.findAll(store, this.get('type'));
      }
  });

  RiakCsControl.serializer = DS.JSONSerializer.create();

  RiakCsControl.serializer.configure('RiakCsControl.User', {
    primaryKey: 'key_id'
  });

  RiakCsControl.adapter = DS.RESTAdapter.create({
    serializer: RiakCsControl.serializer
  });

  RiakCsControl.Store = DS.Store.extend({
    revision: 11,
    adapter: RiakCsControl.adapter
  });

  RiakCsControl.store = RiakCsControl.Store.create();

  RiakCsControl.User = DS.Model.extend({
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

    isNormal: function() {
        return this.get('admin') === false;
    }.property('admin'),

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
