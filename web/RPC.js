'use strict';

__define('RPC', function (my) {
   var assertDefined = Util.assertDefined;
   var assertString = Util.assertString;

   // creates an object that fluently accumulates rpc calls and then sends them.
   // tag goes into the query string to identify the call
   my.rpc = function (tag) {
      __assert(_.isUndefined(this), 'Do not new or bind this function.');
      // accumulates the tagged rpc calls.
      var accum = {};
      var proxy; // permanent this.
      function add(t, f) {
         assertDefined(t, 'tag required.');
         assertString(t, 'tag must be a string: ' + t);
         assertDefined(f, 'function required.');
         assertString(f, 'function must be a string: ' + f);
         __assert(!_.has(accum, t), 'call \'' + t + '\' already defined.');
         // NB we simply discard the first two arguments to get the list of params.
         accum[t] = { name: f, params: _.tail(arguments[2]) };
         return proxy;
      }
      proxy = {
         /**
          * Executes the accumulated RPC calls.
          */
         call: function () {
            __assert(_.keys(accum).length, 'No calls to execute.');
            var q = Q.defer();
            Q($.ajax({ type: 'POST', url: '/api?tag=' + encodeURIComponent(tag), data: JSON.stringify(accum), dataType: 'json'})).done(function (r) {
               if (_.has(r, 'success')) {
                  q.resolve(r.success);
               } else {
                  console.error(r.failure);
                  alert(r.failure);
                  q.reject(r.failure);
               }
            }, function () {
               var message = 'HTTP error ' + JSON.stringify(arguments);
               alert(message);
            });
            return q.promise;
         },
         listUsers: function (tag) {
            return add(tag, 'listUsers', arguments);
         },
         getUserByName: function (tag, name) {
            __assert(_.isString(name), 'name must be a string: ' + name);
            return add(tag, 'getUserByName', arguments);
         },
         createUser: function (tag, name) {
            __assert(_.isString(name), 'name must be a string: ' + name);
            return add(tag, 'createUser', arguments);
         },
         createMessage: function (tag, owner, recipients, content) {
            __assert(_.isArray(recipients), 'recipients must be an array: ' + recipients);
            __assert(_.isString(content), 'content must be a string: ' + name);
            return add(tag, 'createMessage', arguments);
         },
         listMessagesSent: function (tag, sender) {
            __assert(_.isNumber(sender), 'sender required.');
            return add(tag, 'listMessagesSent', arguments);
         },
         listMessagesReceived: function (tag, recipient) {
            __assert(_.isNumber(recipient), 'recipient required.');
            return add(tag, 'listMessagesReceived', arguments);
         },
         deleteMessage: function (tag, messageId) {
            __assert(_.isNumber(messageId), 'messageId required.');
            return add(tag, 'deleteMessage', arguments);
         }
      };
      return proxy;
   }
   my.withResponse = function (r, t, f) {
      var d = r[t];
      __assert(_.isObject(d), "No response object for tag '" + t + "' in: " + JSON.stringify(r));
      if (_.has(d, 'success')) {
         f(d.success);
      } else {
         console.error(d.failure);
         alert(d.failure);
      }
   }
});