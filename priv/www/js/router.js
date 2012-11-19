minispade.register('router', function() {

  RiakCsControl.Router = Ember.Router.extend({
    root: Ember.Route.extend({
      index: Ember.Route.extend({
        route: '/',
        redirectsTo: 'users.index'
      }),

      viewUser: Ember.Route.transitionTo('users.show'),

      users: Ember.Route.extend({
        route: '/users',

        connectOutlets: function(router, context) {
          router.get('applicationController').connectOutlet('users', RiakCsControl.User.find());
        },

        index: Ember.Route.extend({
          route: '/'
        }),

        show: Ember.Route.extend({
          route: '/:key_id',

          // Nasty hack; when loading the object directly when it's not
          // in memory, key_id isn't available yet, even though it's the
          // primary key.  Use the _id property when we need to load on
          // demand, and key_id if it's available.
          serialize: function(router, context) {
            var key_id = context.get('key_id') || context.get('_id');

            return {
              key_id: key_id
            };
          },

          deserialize: function(router, params) {
            return RiakCsControl.User.find(params.key_id);
          },

          connectOutlets: function(router, context) {
            router.get('usersController').connectOutlet('user', context);
            router.get('usersController').set('selectedUser', context);
          },

          exit: function(router) {
            router.get('usersController').set('selectedUser', undefined);
          }
        })
      })
    })
  });

});
