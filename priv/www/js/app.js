minispade.register('app', function() {

  RiakCsControl = Ember.Application.create();

  $("body").bind("ajaxSend", function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').attr('content');

    if (s.type === 'POST' || s.type === 'PUT') {
        xhr.setRequestHeader('X-CSRF-Token', csrf_token);
    }
  });

  minispade.require('router');
  minispade.require('models');
  minispade.require('controllers');
  minispade.require('views');

});
