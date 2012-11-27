minispade.register('controllers', function() {

  RiakCsControl.ApplicationController = Ember.Controller.extend();

  RiakCsControl.CreateUserController = Ember.ObjectController.extend({
    enter: function() {
      this.transaction = this.get('store').transaction();
      this.set('content',
        this.transaction.createRecord(RiakCsControl.User, {}));
    },

    createUser: function() {
      this.transaction.commit();
      this.transaction = null;

      this.get('content').addObserver('id', this, 'showUsers');
    },

    exit: function() {
      this.transaction.rollback();
      this.transaction = null;
    },

    viewUsers: function() {
      RiakCsControl.router.send('viewUsers');
    }
  });

  RiakCsControl.UsersController = Ember.ArrayController.extend({
    persistedUsers: function() {
      return this.get('content').filterProperty('isNew', false);
    }.property('content.@each.isNew'),

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
      var transaction = RiakCsControl.get('store').transaction();

      transaction.add(user);
      update.call(user);
      transaction.commit();
    }
  });

});
