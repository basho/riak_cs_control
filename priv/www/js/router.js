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
          route: '/'
        })
      })
    })
  });

});
