App = Em.Application.create();

/* 
 * Models
 */
App.Bucket = Em.Object.extend({
    keys: [],

    loadKeys: function() { 
        var self = this;

        $.ajax({
            url: '/buckets/' + self.get('name') + '/keys',
            dataType: 'json',
            success: function(data) {
                var keys = data.keys;

                keys = keys.map(function(key) {
                    return App.Key.create(key);
                });

                self.set('keys', keys);
            },
        });
    }
});

App.Key = Em.Object.extend();

/*
 * Controllers
 */
App.bucketsController = Em.ArrayController.create({
    content: [],

    selectedBucket: null,

    loadBuckets: function() {
        var self = this;

        $.ajax({
            url: '/buckets',
            dataType: 'json',
            success: function(data) {
                var buckets = data.buckets;

                buckets = buckets.map(function(bucket) {
                    return App.Bucket.create(bucket);
                });

                self.set('content', buckets);
            }
        });
    }
});

App.keysController = Em.ArrayController.create({
    content: [],

    loadingKeys: true,

    triggerBucketKeyLoad: function() { 
        var selectedBucket = App.bucketsController.get('selectedBucket');

        if(selectedBucket !== null) { 
            App.stateManager.goToState('objectBrowser.withKeyListing.isLoading');
            App.bucketsController.get('selectedBucket').loadKeys();
        }
    }.observes('App.bucketsController.selectedBucket'),

    updateKeyListing: function() {
        var keys = App.bucketsController.getPath('selectedBucket.keys'); 

        this.set('content', keys);

        App.stateManager.goToState('objectBrowser.withKeyListing');
    }.observes('App.bucketsController.selectedBucket.keys')
});

/* 
 * Views
 */
App.objectBrowserView = Em.View.create({ templateName: 'object-browser' });

App.keysSpinner = Em.View.extend();

App.bucketListView = Em.View.extend({
    tagName: 'tr',

    isSelectedRow: function() { 
        var selectedBucket = App.bucketsController.get('selectedBucket'),
            content = this.get('content');

        return selectedBucket === content;
    }.property('App.bucketsController.selectedBucket').cacheable(),

    click: function() { 
        var selectedBucket = App.bucketsController.get('selectedBucket'),
            content = this.get('content');

        if(selectedBucket === content) {
          App.bucketsController.set('selectedBucket', null);
          App.stateManager.goToState('objectBrowser');
        } else { 
          App.bucketsController.set('selectedBucket', this.get('content'));
          App.stateManager.goToState('objectBrowser.withKeyListing');
        }
    }
});

/*
 * State Machines
 */
App.stateManager = Em.StateManager.create({
    rootElement: '#container',

    initialState: 'objectBrowser',

    objectBrowser: Em.ViewState.create({
        view: App.objectBrowserView,

        enter: function(manager, transition) { 
            this._super(manager, transition);
            App.bucketsController.loadBuckets();
        },

        withKeyListing: Em.State.create({
            enter: function(manager, transition) { 
                Em.run(function() { 
                    $('#buckets').animate({width : '316px'}, {queue : false, duration : 300, complete : function () {
                        $('#buckets').addClass('one-third-width').css('');
                        $('#keys').slideDown();
                    }});
                });
            },

            exit: function(manager, transition) { 
                Em.run(function() { 
                    $('#keys').slideUp(300, function() { 
                      $('#buckets').animate({width : '960px'}, {queue : true, duration : 300, complete : function () {
                          $('#buckets').removeClass('one-third-width').css('');
                      }});
                    });
                });
            },

            isLoading: Em.State.create({
                enter: function(manager, transition) { 
                    $('#keys .spinner').fadeIn();
                },

                exit: function(manager, transition) { 
                    $('#keys .spinner').fadeOut();
                }
            })
        })
    })
});
