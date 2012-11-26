minispade.register('app', function() {

  RiakCsControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  RiakCsControl.ApplicationController = Ember.Controller.extend();

  RiakCsControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  DS.Model.reopen({
    reload: function() {
      var store = this.get('store');
      store.get('adapter').find(store, this.constructor, this.get('id'));
      }
  });

  DS.RecordArray.reopen({
    reload: function() {
      Ember.assert("Can only reload base RecordArrays",
        this.constructor === DS.RecordArray);
      var store = this.get('store');
      store.get('adapter').findAll(store, this.get('type'));
      }
  });

  RiakCsControl.Adapter = DS.RESTAdapter.extend({});

  RiakCsControl.Store = DS.Store.extend({
    revision: 4,
    adapter: RiakCsControl.Adapter.create()
  });

  RiakCsControl.store = RiakCsControl.Store.create({});

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

    status: DS.attr("string"),

    isDisabled: function() {
      return this.get('status') === 'disabled';
    }.property('status'),

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

  RiakCsControl.UsersView = Ember.View.extend({
    templateName: 'users'
  });

  RiakCsControl.UserItemView = Ember.View.extend({
    templateName: 'users_item',
    classNameBindings: 'isDisabled:disabled',

    isDisabled: function() {
      return this.get('content.isDisabled');
    }.property('content.status'),

    key_secret: function() {
      var key_secret = this.get('content.key_secret');
      var new_key_secret = this.get('content.new_key_secret');

      return new_key_secret ? "Revoking, please wait..." : key_secret;
    }.property('content.key_secret', 'content.new_key_secret')
  });

  RiakCsControl.UsersCollectionView = Ember.CollectionView.extend({
    tagName: 'tbody',
    itemViewClass: RiakCsControl.UserItemView
  });

  RiakCsControl.UsersController = Ember.ArrayController.extend({
    enableUser: function(user) {
      this.performUserUpdate(user, function() { user.enable(); });
    },

    disableUser: function(user) {
      this.performUserUpdate(user, function() { user.disable(); });
    },

    revokeCredentials: function(user) {
      this.performUserUpdate(user, function() { user.revoke(); });
    },

    performUserUpdate: function(user, update) {
      var store = RiakCsControl.get('store');
      var transaction = store.transaction();

      transaction.add(user);
      update.call(user);
      transaction.commit();
    }
  });

  RiakCsControl.ButtonView = Ember.View.extend({
    tagName: 'td',
    templateName: 'button',
    classNames: 'button-cell',
    click: function(ev) {
      ev.preventDefault();
      var content = this.get('content');
      var target = this.get('target');
      var controller = this.get('controller');
      var action = this.get('controller.' + target);
      action.call(controller, content);
    }
  });

  minispade.require('router');

});
