'use strict';
$(function() {
   //-- utils

   function __error() {
      throw _.reduce(arguments, function (a, v) { return a + v; }, '');
   }
   function __assert(cond) {
      cond || __error.apply(null, _.tail(arguments));
   }
   function __assertString(v, n) {
      __assert(_.isString(v), "'" + n + "' must be a string: " + n);
   }

   /**
    * Adds lenses on a component's state to a component.
    * A lens proxies the getting and the setting of the state and provides the following advantages:
    * - Greatly minimizes the chance of naming the state segment wrong (the name only appears once, in the lens definition).
    * - Mistyping the name of the lens causes a run time failure rather than getting or setting the wrong thing silently.
    * - Allows convenient binding of the lens to an input control.
    * - Allows components to proxy their state changes deeply in the component hierarchy (i.e. as props).
    * requires: lodash, q.
    */
   function LensedState (config) {
      // the mixin
      return {
         // Install the lenses in the component, create initial state.
         getInitialState: function () {
            var component = this;
            var initialState = {};
            _.forOwn(config, function (value, key) {
               // Assigning to a var gives us, effectively, a permanent this pointer.
               // This, in turn, allows us to 'peel off' the functions, i.e. use references to them without calling them,
               // without losing track of the lens to which they refer.
               var lens = {
                  // Get the state segment.
                  // This should be the only thing called from render.
                  get: function () {
                     if (key === 'recipients') { console.log('recipients: ' + JSON.stringify(component.state[key])); }
                     return component.state[key]
                  },
                  // Set the state segment
                  set: function (v) {
                     __assert(! _.isUndefined(v), 'Lensing undefined value to \'', key, '\'.');
                     var s = {};
                     s[key] = v;
                     component.setState(s);
                  },
                  // promise a value to be set
                  promise: function () {
                     var q = Q.defer();
                     // take advantage of set's proxying.
                     q.promise.done(lens.set, function(reason) {
                        console.warn('Promise rejected for lens value \'' + key + '\': ' + reason);
                     });
                     return q;
                  },
                  // Change the state segment using a function that takes the current value and returns the new value.
                  // Rejects mutations to undefined as these are probably programming errors.
                  mutate: function (mutator) {
                     var m  = mutator(lens.get());
                     __assert(! _.isUndefined(m), 'Mutating lens \'', key, '\' to undefined. Did you forget to return something from the mutator?');
                     lens.set(m);
                  },
                  // Reverts the state to its initial value.
                  revert: function () {
                     lens.set(value);
                  },
                  // Binds the lens to an input control.
                  // ps is the props object, inline this call to the component
                  // Can't use this with JSX. :/
                  bindInput: function (ps) {
                     __assert(! ps.onChange, 'onChange property already bound for lens\'', key, '\'.');
                     ps.onChange = function (e) {
                        lens.set(e.target.value);
                     }
                     __assert(! ps.value, 'value property already bound for lens\'', key, '\'.');
                     ps.value = lens.get();
                     return ps;
                  }
               };
               initialState[key] = _.clone(value, true);
               component[key] = lens;
            });
            return initialState;
         }
      }
   };

   // insists on exactly one element, returns the one element.
   function domId(id) {
      var $el = $('#' + id);
      __assert(1 == $el.length, 'non-unique dom id \'', id, '\': ', $el.length);
      return $el[0];
   }

   //-- app

   // creates an object that fluently accumulates rpc calls and then sends them.
   // tag goes into the query string to identify the call
   function rpc(tag) {
      __assert(_.isUndefined(this), 'Do not new or bind this function.');
      // accumulates the tagged rpc calls.
      var accum = {};
      var proxy; // permanent this.
      function add(t, f) {
         __assert(!_.isUndefined(t), 'tag required.');
         __assert(_.isString(t), 'tag must be a string: ' + t);
         __assert(!_.isUndefined(f), 'function required.');
         __assert(_.isString(f), 'function must be a string: ' + f);
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
               console.log(message); 
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
   function withResponse(r, t, f) {
      var d = r[t];
      __assert(_.isObject(d), "No response object for tag '" + t + "' in: " + JSON.stringify(r));
      if (_.has(d, 'success')) {
         f(d.success);
      } else {
         console.error(d.failure);
         alert(d.failure);
      }
   }

   // for notational convenience
   var h = React.DOM;

   var Page = (function () {
      var self;
      function updateUsers(allUsers) {
         self.allUsers.set(allUsers);
      }
      function addRecipient(user) {
         self.recipients.mutate(function (rs) {
            rs[user.name] = user;
            return rs;
         });
      }
      function removeRecipient(user) {
         recipient.mutate(function (rs) {
            delete rs[user.name];
            return rs;
         });
      }
      function doCreateUser() {
         var tag = 'create-user';
         rpc('doCreateUser').createUser(tag, self.newUser.get()).call().done(function (data) {
            withResponse(data, tag, function (v) {
               console.log('created user: ' + v);
               self.newUser.revert();
               rpc('post-create-user').listUsers('list-users').call().done(function (data) {
                  withResponse(data, 'list-users', updateUsers);
               });
            });
         });
      }
      function doLogin(loginName) {
         return function() {
            var tag = 'login';
            __assert(_.isString(loginName), 'login name required but got ' + loginName);
            rpc('do-login').getUserByName(tag, loginName).call().done(function (data) {
               withResponse(data, tag, function(user) {
                  if (user) {
                     self.user.set(user);
                     rpc('post-login').
                           listUsers('list-users').
                           listMessagesSent('sent-messages', user.id).
                           listMessagesReceived('received-messages', user.id).
                           call().done(function(data) {
                        withResponse(data, 'list-users', updateUsers);
                        withResponse(data, 'sent-messages', self.sentMessages.set);
                        withResponse(data, 'received-messages', self.receivedMessages.set);
                     });
                  } else {
                     alert('Invalid login: ' + loginName);
                  }
               });
            });
         }
      }
      function doLogout() {
         self.user.revert();
         self.allUsers.revert();
         self.sentMessages.revert();
         self.receivedMessages.revert();
         rpc('do-logout').listUsers('list-users').call().done(function (data) {
            withResponse(data, 'list-users', updateUsers);
         });
      }
      function doNewMessage() {
         var tag = 'list-users';
         rpc('newMessage').listUsers(tag).call().done(function (data) {
            withResponse(data, tag, updateUsers);
         });
      }
      function update(tag) {
         var userId = self.user.get().id;
         rpc(tag).
               listUsers('list-users').
               listMessagesSent('sent-messages', userId).
               listMessagesReceived('received-messages', userId).
               call().done(function (data) {
            withResponse(data, 'list-users', updateUsers);
            withResponse(data, 'sent-messages', self.sentMessages.set);
            withResponse(data, 'received-messages', self.receivedMessages.set);
         });
      }
      function deleteMessage(messageId) {
         var tag = 'delete-message';
         rpc('delete-message').deleteMessage(tag, messageId).call().done(function (data) {
            console.log(data);
            update('post-delete-message');
         });
      }
      return React.createClass({
         displayName: 'SnAX',
         mixins: 
            [LensedState({
               newUser: '', 
               login: '', 
               user: '', 
               allUsers: [],
               sentMessages: [],
               receivedMessages: []
            })
         ],
         getInitialState: function () {
            self = this; // this is the first opportunity.
         },
         componentDidMount: function () {
            rpc('page-init').listUsers('list-users').call().done(function (data) {
               withResponse(data, 'list-users', updateUsers);
            });
         },
         render: function () {
            return h.div({ className: 'container' },
               h.h2(null, 'SnAX, powered by ', h.img({ src: 'images/haskell.jpg' })),
               h.span(null, 
                  h.a({href:'http://snapframework.com'}, 'Snap'), ', ',
                  h.a({href:'http://acid-state.seize.it'}, 'Acid State'), ', ',
                  h.a({href:'https://hackage.haskell.org/package/ixset'}, 'IxSet'), ', and ',
                  h.a({href:'http://facebook.github.io/react/'}, 'React')),
               h.br(),
               h.div(null, 
                  h.button({ onClick: doCreateUser, type: 'button', className: 'btn-primary', disabled: ! self.newUser.get() }, 'Create User'), ' ',
                  h.input(self.newUser.bindInput({ type: 'text' }))),
               h.br(),
               h.div(null, !! self.user.get() ? 
                  h.div(null, 
                     h.button({ type: 'button', className: 'btn-danger', onClick: doLogout }, 'Logout'),
                     ' Welcome, ', h.em(null, self.user.get().name),
                     h.div(null, 
                        MessageComposer({ user: self.user.get(), allUsers: self.allUsers.get(), onCreateMessage: function () {
                           update('post-create-message');
                        }}),
                        h.div({ className: 'row' },
                           MessageList({ title: 'Received', glyph: 'glyphicon-arrow-up', messages: self.receivedMessages.get(), deleteMessage: deleteMessage }),
                           MessageList({ title: 'Sent', glyph: 'glyphicon-arrow-down', messages: self.sentMessages.get(), deleteMessage: deleteMessage })))) :
                  h.div(null, 
                     h.span(null, 'Login: '),
                     _.map(self.allUsers.get(), function(user) {
                        return h.span({ key: user.id }, h.a({ onClick: doLogin(user.name) }, user.name), ' ');
                     }))));
         },
      });
   })();
   var MessageComposer = (function () {
      var self;
      function createMessage() {
         var tag = 'create-message';
         var rr = self.recipients.get();
         var rs = _.reduce(rr, function(a, on, id) {
            on && a.push(parseInt(id, 10));
            return a;
         }, []);
         rpc('create-message').createMessage(tag, self.props.user.id, rs, self.message.get()).call().done(function (data) {
            withResponse(data, tag, function (v) {
               console.log('created message: ' + v.messageId);
            });
            self.message.revert();
            self.recipients.revert();
            self.props.onCreateMessage();
         });
         self.recipients.set(_.reduce(self.props.allUsers, function (rs, u) {
            rs[u.id] = false;
            return rs;
         }, {}));
      }
      return React.createClass({
         mixins: [LensedState({
            message: '', 
            recipients: {}, 
            })
         ],
         propTypes: {
            user: React.PropTypes.object.isRequired,
            allUsers: React.PropTypes.array.isRequired,
            onCreateMessage: React.PropTypes.func.isRequired
         },
         getInitialState: function () {
            self = this;
         },
         componentWillReceiveProps : function (newProps) {
            // we have to account for the fact that if the component updates then users may have disappeared so we must nix deleted users from the 
            // recipients list.
            // furthermore, we must collect all the nixes and do them in one mutation because of the way state works in React.
            var nixed = [];
            var umap = _.reduce(newProps.allUsers, function (a, u) {
               a[u.id] = u;
               return a;
            }, {});
            _.map(self.recipients.get(), function (recip) {
               if (! _.has(umap, recip.id)) {
                  nixed.push(recip.id);
               }
            });
            self.recipients.mutate(function (recips) {
               _.map(nixed, function (nix) {
                  delete recips[nix];
               });
               return recips;
            });
         },
         render: function () {
            return h.div({ className: 'well' },
               h.h4(null, 'Create Message'),
               h.span(null, 'Recipients: '),
               (function () {
                  var recipients = self.recipients.get();
                  console.log('recipients in render: ' + JSON.stringify(recipients));
                  return _.map(self.props.allUsers, function (user) {
                     console.log('checked ' + user.name + ' ' + user.id + ' ' + recipients[user.id]);
                     return h.span({ key: user.name }, user.name, h.input({ type: 'checkbox', checked: _.has(recipients, user.id), onChange: function(e) {
                        self.recipients.mutate(function(rs) {
                           rs[user.id] = e.target.checked;
                           return rs;
                        });
                     } })) }) }) (),
               h.br(),
               h.span(null, 'Message: '),
               h.input(self.message.bindInput({ type: 'text' })),
               h.button({ onClick: createMessage }, 'Send'),
               h.br());
         }
      });
   })();
   var MessageList = (function () {
      return React.createClass({
         propTypes: {
            deleteMessage: React.PropTypes.func.isRequired,
            messages: React.PropTypes.array.isRequired,
            title: React.PropTypes.string.isRequired
         },
         displayName: 'MessageList',
         render: function () {
            var self = this;
            function onDeleteMessage(messageId) {
               return function () {
                  self.props.deleteMessage(messageId);
               }
            }
            return h.div({ className: 'col-lg-4' },
               h.span({ className: 'glyphicon ' + this.props.glyph }), h.span(null, this.props.title),
               _.map(this.props.messages, function (message) {
                  return h.div({key: message.id}, 
                     h.span({ className: 'glyphicon glyphicon-remove', onClick: onDeleteMessage(message.id) }), 
                     message.content, ' - ', moment.unix(message.time.time).fromNow());
               }));
         }
      });
   })();
   
   React.renderComponent(Page(), domId('api-test-form'));
});
