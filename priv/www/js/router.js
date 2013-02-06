minispade.register('router', function() {

  RiakCsControl.Router.map(function() {
    this.resource('users', function() {
      this.route('new');
    });
  });

  RiakCsControl.IndexRoute = Ember.Route.extend({
    redirect: function() {
      this.transitionTo('users.index');
    }
  });

  RiakCsControl.UsersIndexRoute = Ember.Route.extend({
    model: function() {
      return RiakCsControl.User.find();
    },

    setupController: function(controller, model) {
      controller.set('content', model);
    }
  });

});
