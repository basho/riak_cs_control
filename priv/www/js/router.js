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

  RiakCsControl.UsersRoute = Ember.Route.extend({
    renderTemplate: function() {
      this.render('users');
    }
  });

  RiakCsControl.UsersIndexRoute = Ember.Route.extend({
    model: function() {
      return RiakCsControl.User.find();
    },

    setupController: function(controller, model) {
      controller.set('content', model);
    },

    renderTemplate: function() {
      this.render('users_index');
    }
  });

  RiakCsControl.UsersNewRoute = Ember.Route.extend({
    enter: function() {
      var controller = this.get('controller');
      console.log(controller);
    },

    renderTemplate: function() {
      this.render('users_new');
    }
  });

});
