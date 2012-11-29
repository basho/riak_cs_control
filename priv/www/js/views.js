minispade.register('views', function() {

  RiakCsControl.ApplicationView = Ember.View.extend({
    templateName: 'application'
  });

  RiakCsControl.CreateUserView = Ember.View.extend({
    templateName: 'create_user',

    submit: function(ev) {
      ev.preventDefault();
      this.get('controller').createUser();
    }
  });

  RiakCsControl.UsersView = Ember.View.extend({
    templateName: 'users'
  });

  RiakCsControl.UserItemView = Ember.View.extend({
    templateName: 'users_item',
    classNameBindings: 'isDisabled:disabled',

    nameBinding: 'content.name',
    emailBinding: 'content.email',
    keyIdBinding: 'content.key_id',

    isDisabled: function() {
      return this.get('content.isDisabled');
    }.property('content.status'),

    keySecret: function() {
      var key_secret = this.get('content.key_secret');
      var new_key_secret = this.get('content.new_key_secret');

      return new_key_secret ? "Revoking, please wait..." : key_secret;
    }.property('content.key_secret', 'content.new_key_secret')
  });

  RiakCsControl.UsersCollectionView = Ember.CollectionView.extend({
    tagName: 'tbody',
    itemViewClass: RiakCsControl.UserItemView
  });

  RiakCsControl.ButtonView = Ember.View.extend({
    tagName: 'td',
    templateName: 'button',
    classNames: 'button-cell',
    click: function(ev) {
      ev.preventDefault();

      var content = this.get('content');
      var target = this.get('target');
      var controller = this.get('controller');
      var action = this.get('controller.' + target);

      action.call(controller, content);
    }
  });

});
