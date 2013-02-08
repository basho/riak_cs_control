minispade.register('controllers', function() {

  RiakCsControl.UsersIndexController = Ember.ArrayController.extend({
    sortProperties: ['isNormal', 'email'],

    filteredContent: function() {
      var filterValue = this.get('filterValue');
      var arrangedContent = this.get('arrangedContent');

      if(arrangedContent && filterValue) {
        return arrangedContent.filter(function(item) {
          return item.get('name').match(new RegExp(filterValue)) ||
                 item.get('email').match(new RegExp(filterValue));
        });
      } else {
        return arrangedContent;
      }
    }.property('arrangedContent', 'filterValue'),

    filteredContentEmpty: function() {
      return this.get('filteredContent.length') <= 0;
    }.property('filteredContent', 'filteredContent.@each'),

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

  RiakCsControl.UsersNewController = Ember.ObjectController.extend({
    createUser: function() {
      var transaction = this.get('content.transaction');
      var content = this.get('content');

      // Handle the success case, once the record is confirmed,
      // materialize the record by forcing a load again (unfortunate)
      // and redirect back to the main page.
      content.addObserver('id', this, 'viewUsers');

      // Handle error states.
      //
      content.one('becameError', function() {
        this.set('errorState', true);
      });

      content.one('becameInvalid', function() {
        this.set('errorState', true);
      });

      content.one('becameClean', function() {
        this.set('errorState', false);
      });

      transaction.commit();
    },

    viewUsers: function(user) {
      RiakCsControl.User.find(user.get('id'));
      this.transitionToRoute('users.index');
    }
  });

});
