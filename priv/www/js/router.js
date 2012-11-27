minispade.register('router', function() {

  RiakCsControl.Router = Ember.Router.extend({
    root: Ember.Route.extend({
      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'users.index'
      }),

      viewUsers: Ember.Route.transitionTo('users.index'),

      createUser: Ember.Route.transitionTo('users.create'),

      users: Ember.Route.extend({
        route: '/users',

        index: Ember.Route.extend({
          route: '/',

          connectOutlets: function(router, context) {
            router.get('applicationController').
              connectOutlet('users', RiakCsControl.User.find());
          }
        }),

        create: Ember.Route.extend({
          route: '/new',

          enter: function(router) {
            router.get('createUserController').enter();
          },

          exit: function(router) {
            router.get('createUserController').exit();
          },

          connectOutlets: function(router, context) {
            router.get('applicationController').
              connectOutlet('createUser');
          }
        })
      })
    })
  });

});
