minispade.register('router', function() {

  RiakCsControl.Router = Ember.Router.extend({
    root: Ember.Route.extend({
      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'users.index'
      }),

      viewUsers: Ember.Route.transitionTo('users.index'),

      users: Ember.Route.extend({
        route: '/users',

        connectOutlets: function(router, context) {
          router.get('applicationController').
            connectOutlet('users', RiakCsControl.User.find());
        },

        index: Ember.Route.extend({
          route: '/'
        })
      })
    })
  });

});
