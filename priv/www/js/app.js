minispade.register('app', function() {

  RiakCsControl = Ember.Application.create({
    LOG_TRANSITIONS: true
  });

  $("body").bind("ajaxSend", function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').attr('content');

    if (s.type === 'POST' || s.type === 'PUT') {
        xhr.setRequestHeader('X-CSRF-Token', csrf_token);
    }
  });

  DS.Model.reopen({
    reload: function() {
      var store = this.get('store');
      var adapter = store.get('adapter');
      adapter.find(store, this.constructor, this.get('id'));
    }
  });

  DS.RecordArray.reopen({
    reload: function() {
      Ember.assert("Can only reload base RecordArrays",
        this.constructor === DS.RecordArray);
      var store = this.get('store');
      var adapter = store.get('adapter');
      adapter.findAll(store, this.get('type'));
      }
  });

  RiakCsControl.Store = DS.Store.extend({
    revision: 11,
    adapter: DS.RESTAdapter.create()
  });

  RiakCsControl.store = RiakCsControl.Store.create();

  minispade.require('router');
  minispade.require('models');
  minispade.require('controllers');
  minispade.require('views');

});
