minispade.register('app', function() {

  RiakCsControl = Ember.Application.create({
    ready: Ember.alias('initialize')
  });

  $("body").bind("ajaxSend", function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').attr('content');

    if (s.type == "POST") {
        xhr.setRequestHeader('X-CSRF-Token', csrf_token);
    }
  });

  DS.Model.reopen({
    reload: function() {
      var store = this.get('store');
      store.get('adapter').find(store, this.constructor, this.get('id'));
      }
  });

  DS.RecordArray.reopen({
    reload: function() {
      Ember.assert("Can only reload base RecordArrays",
        this.constructor === DS.RecordArray);
      var store = this.get('store');
      store.get('adapter').findAll(store, this.get('type'));
      }
  });

  RiakCsControl.Adapter = DS.RESTAdapter.extend({});

  RiakCsControl.Store = DS.Store.extend({
    revision: 4,
    adapter: RiakCsControl.Adapter.create()
  });

  RiakCsControl.store = RiakCsControl.Store.create({});

  minispade.require('router');
  minispade.require('models');
  minispade.require('controllers');
  minispade.require('views');

});
