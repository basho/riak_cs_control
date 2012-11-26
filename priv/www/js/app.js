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
    new_key_secret: DS.attr("string"),

    status: DS.attr("string"),

    isDisabled: function() {
      return this.get('status') === 'disabled';
    }.property('status'),

    disable: function() {
      this.set('status', 'disabled');
    },

    revoke: function() {
      this.set('new_key_secret', 'true');
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
    }.property('content.key_secret', 'content.new_key_secret'),

    disableUser: function(ev) {
      ev.preventDefault();

      var controller = this.get('controller');
      controller.disableUser(ev.context);
    },

    enableUser: function(ev) {
      ev.preventDefault();

      var controller = this.get('controller');
      controller.enableUser(ev.context);
    },

    revokeCredentials: function(ev) {
      ev.preventDefault();

      var controller = this.get('controller');
      controller.revokeCredentials(ev.context);
    }
  });

  RiakCsControl.UsersCollectionView = Ember.CollectionView.extend({
    tagName: 'tbody',
    itemViewClass: RiakCsControl.UserItemView
  });

  RiakCsControl.UsersController = Ember.ArrayController.extend({
    enableUSer: function(user) {
      var store = RiakCsControl.get('store');
      var transaction = store.transaction();

      transaction.add(user);
      user.enable();
      transaction.commit();

      user.reload();
    },

    disableUser: function(user) {
      var store = RiakCsControl.get('store');
      var transaction = store.transaction();

      transaction.add(user);
      user.disable();
      transaction.commit();

      user.reload();
    },

    revokeCredentials: function(user) {
      var store = RiakCsControl.get('store');
      var transaction = store.transaction();

      transaction.add(user);
      user.revoke();
      transaction.commit();

      user.reload();
    }
  });

  minispade.require('router');

});
