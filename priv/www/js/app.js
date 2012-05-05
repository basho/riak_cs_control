App = Em.Application.create();

App.Bucket = Ember.Object.extend();

App.Key = Ember.Object.extend();

App.bucketListView = Ember.View.extend({
    click: function() { 
        App.bucketsController.set('selectedBucket', this.get('content'));
    }
});

App.bucketsController = Ember.ArrayController.create({
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
                    return self.createBucket(bucket);
                });

                self.set('content', buckets);
            }
        });
    },

    createBucket: function(bucket) {
        return App.Bucket.create(bucket);
    }
});

App.keysController = Ember.ArrayController.create({
    content: [],

    bucketChange: (function() { 
        var self = this;

        if(App.bucketsController.get('selectedBucket') !== null) {
            self.loadKeys();
        }
    }).observes('App.bucketsController.selectedBucket'),

    showKeys: function() {
        $('#buckets').animate({width : '316px'}, {queue : false, duration : 300, complete : function () {
            $('#buckets').addClass('one-third-width').css('');
            $('#keys').slideDown();
        }});
    },

    loadKeys: function() {
        var self = this;

        $.ajax({
            url: '/buckets/' + App.bucketsController.getPath('selectedBucket.name') + '/keys',
            dataType: 'json',
            success: function(data) {
                var keys = data.keys;

                keys = keys.map(function(key) {
                    return self.createKey(key);
                });

                self.set('content', keys);
                self.showKeys();

                // TODO: SHOW KEYS SHOULD BE COMPUTED PROPERTY
                // TODO: SET SELECTED ROW
            },
        });
    },

    createKey: function(key) {
        return App.Key.create(key);
    }
});

App.bucketsController.loadBuckets();
