minispade.register('controllers', function() {

  RiakCsControl.ApplicationController = Ember.Controller.extend();

  RiakCsControl.UsersController = Ember.ArrayController.extend({
    enableUser: function(user) {
      this.performUserUpdate(user, function() { user.enable(); });
    },

    disableUser: function(user) {
      this.performUserUpdate(user, function() { user.disable(); });
    },

    revokeCredentials: function(user) {
      this.performUserUpdate(user, function() { user.revoke(); });
    },

    performUserUpdate: function(user, update) {
      var store = RiakCsControl.get('store');
      var transaction = store.transaction();

      transaction.add(user);
      update.call(user);
      transaction.commit();
    }
  });

});
