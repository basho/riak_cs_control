minispade.register('app', function() {

  RiakCsControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  RiakCsControl.ApplicationController = Ember.Controller.extend();

  RiakCsControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  RiakCsControl.Store = DS.Store.extend({
    revision: 4,
    adapter: DS.RESTAdapter.create()
  });

  RiakCsControl.User = DS.Model.extend({
    primaryKey: "key_id",
    email: DS.attr("string"),
    display_name: DS.attr("string"),
    name: DS.attr("string"),
    key_id: DS.attr("string"),
    key_secret: DS.attr("string"),
    id: DS.attr("string"),
    status: DS.attr("string")
  });

  RiakCsControl.UserView = Ember.View.extend({
    templateName: 'user'
  });

  RiakCsControl.UsersView = Ember.View.extend({
    templateName: 'users'
  });

  RiakCsControl.UsersCollectionView = Ember.CollectionView.extend({
      tagName: 'tbody',
      itemViewClass: Ember.View.extend({
          templateName: 'users_item'
      })
  });

  RiakCsControl.UsersController = Ember.ArrayController.extend();

  RiakCsControl.UserController = Ember.ObjectController.extend();

  minispade.require('router');

});
