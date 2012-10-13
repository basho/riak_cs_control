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

  RiakCsControl.User = DS.Model.extend();

  RiakCsControl.UsersView = Ember.View.extend();
  RiakCsControl.UsersController = Ember.ArrayController.extend();

  minispade.require('router');

});
