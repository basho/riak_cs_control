minispade.register('router', function() {

  RiakCsControl.Router.map(function() {
    this.resource('disk_usage');
    this.resource('users', function() {
      this.route('new');
    });
  });

  RiakCsControl.IndexRoute = Ember.Route.extend({
    redirect: function() {
      this.transitionTo('users.index');
    }
  });

  RiakCsControl.UsersIndexRoute = Ember.Route.extend({
    model: function() {
      return RiakCsControl.User.find();
    },

    setupController: function(controller, model) {
      controller.set('content', model);
    }
  });

  RiakCsControl.UsersNewRoute = Ember.Route.extend({
    model: function() {
      var transaction = this.get('store').transaction();
      return transaction.createRecord(RiakCsControl.User, {});
    },

    setupController: function(controller, model) {
      controller.set('content', model);
    },

    exit: function() {
      this._super();
      this.get('store').transaction().rollback();
    }
  });

  RiakCsControl.DiskUsageRoute = Ember.Route.extend({
    model: function() {
      var data = {"cluster_capacity":230398124,"cluster_disk_usage_kb":34559719,"cluster_disk_free_kb":195838405,"cluster_node_count":1,"n_val":3,"object_storage_capacity_remaining_kb":65279468};
      return RiakCsControl.DiskUsage.createRecord(data);
    },

    setupController: function(controller, model) {
      console.log(model);
      controller.set('content', model);
    }
  });

});
