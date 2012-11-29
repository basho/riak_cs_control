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

      this.get('content').addObserver('id', this, 'viewUsers');
    },

    exit: function() {
      if(this.transaction) {
        this.transaction.rollback();
      }
      this.transaction = null;
    },

    viewUsers: function(user) {
      // HACK: Massive hack because the record array won't refresh when
      // the transaction is committed.  Force the record to load by
      // accessing it by id.
      //
      RiakCsControl.User.find(user.get('id'));

      RiakCsControl.router.send('viewUsers');
    }
  });

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
      var transaction = RiakCsControl.get('store').transaction();

      transaction.add(user);
      update.call(user);
      transaction.commit();
    }
  });

});
