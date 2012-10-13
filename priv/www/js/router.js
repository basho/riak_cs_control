minispade.register('router', function() {

  RiakCsControl.Router = Ember.Router.extend({
    root: Ember.Route.extend({
      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'users.index'
      }),

      users: Ember.Route.extend({
        route: '/users',

        index: Ember.Route.extend({
          route: '/',

          connectOutlets: function(router, context) {
            router.get('applicationController').connectOutlet('users', RiakCsControl.User.find());
          }
        }),

        show: Ember.Route.extend({
          route: '/:user_id',

          connectOutlets: function(router, context) {
            router.get('applicationController').connectOutlet('users', context);
          }
        })
      })
    })
  });

});
