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

  RiakCsControl.UsersNewRoute = Ember.Route.extend({
    model: function() {
      var transaction = this.get('store').transaction();
      return transaction.createRecord(RiakCsControl.User, {});
    },

    setupController: function(controller, model) {
      controller.set('content', model);
    },

    exit: function() {
      this._super();
      this.get('store').transaction().rollback();
    }
  });

});
