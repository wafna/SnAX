'use strict';
$(function() {
   //-- utils

   function __error() {
      throw _.reduce(arguments, function (a, v) { return a + v; }, '');
   }
   function __assert(cond) {
      cond || __error.apply(null, _.tail(arguments));
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
               initialState[key] = value;
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
         // NB we simply discard the first two arguments to get the list of params.
         accum[t] = { name: f, params: _.tail(arguments[2]) };
         return proxy;
      }
      proxy = {
         call: function () {
            __assert(_.keys(accum).length, 'No calls to execute.');
            return Q($.ajax({ type: 'POST', url: '/api?tag=' + encodeURIComponent(tag), data: JSON.stringify(accum), dataType: 'json'
            })).fail(function () {
               console.log('error ' + JSON.stringify(arguments)); 
            });
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
   function withResponse(r, f) {
      if (_.has(r, 'success')) {
         f(r.success);
      } else {
         console.error(r.failure);
         alert(r.failure);
      }
   }

   // for notational convenience
   var h = React.DOM;

   var Page = (function () {
      function focusLogin() {
         $('#login-name').focus();
      }
      return React.createClass({
         displayName: 'SnapChat',
         mixins: 
            [LensedState({ 
               newUser: '', 
               login: '', 
               user: '', 
               message: '', 
               recipients: {}, 
               allUsers: [],
               sentMessages: [],
               receivedMessages: []
            })
         ],
         render: function () {
            var self = this;

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
                  withResponse(data[tag], function (v) {
                     console.log('created user: ' + v);
                  });
               });
            }
            function doLogin() {
               var tag = 'login';
               var loginName = self.login.get();
               rpc('doLogin').getUserByName(tag, loginName).call().done(function (data) {
                  withResponse(data[tag], function(user) {
                     if (user) {
                        self.user.set(user);
                        rpc('postLogin').
                              listUsers('list-users').
                              listMessagesSent('sent-messages', user.id).
                              listMessagesReceived('received-messages', user.id).
                              call().done(function(data) {
                           withResponse(data['sent-messages'], self.sentMessages.set);
                           withResponse(data['received-messages'], self.receivedMessages.set);
                           withResponse(data['list-users'], updateUsers);
                        });
                     } else {
                        alert('Invalid login: ' + loginName);
                        focusLogin();
                     }
                  });
               });
            }
            function doLogout() {
               self.user.revert();
               self.allUsers.revert();
               self.message.revert();
               self.recipients.revert();
               self.sentMessages.revert();
               self.receivedMessages.revert();
            }
            function updateUsers(allUsers) {
               self.allUsers.set(allUsers);
               self.recipients.set(_.reduce(allUsers, function (rs, u) {
                  rs[u.id] = false;
                  return rs;
               }, {}));
            }
            function doNewMessage() {
               var tag = 'list-users';
               rpc('newMessage').listUsers(tag).call().done(function (data) {
                  withResponse(data[tag], updateUsers);
               });
            }
            function update(tag) {
               var userId = self.user.get().id;
               rpc(tag).
                     listUsers('list-users').
                     listMessagesSent('sent-messages', userId).
                     listMessagesReceived('received-messages', userId).
                     call().done(function (data) {
                  withResponse(data['list-users'], updateUsers);
                  withResponse(data['sent-messages'], self.sentMessages.set);
                  withResponse(data['received-messages'], self.receivedMessages.set);
               });
            }
            function createMessage() {
               var tag = 'create-message';
               var rr = self.recipients.get();
               var rs = _.reduce(rr, function(a, on, id) {
                  on && a.push(parseInt(id, 10));
                  return a;
               }, []);
               rpc('create-message').createMessage(tag, self.user.get().id, rs, self.message.get()).call().done(function (data) {
                  withResponse(data[tag], function (v) {
                     console.log('created message: ' + v.messageId);
                  });
                  self.message.revert();
                  self.recipients.revert();
                  update('post-create-message');
               });
            }
            function deleteMessage(messageId) {
               var tag = 'delete-message';
               rpc('delete-message').deleteMessage(tag, messageId).call().done(function(data) {
                  console.log(data);
                  update('post-delete-message');
               });
            }
            return h.div({ className: 'container' },
               h.h2(null, 'Snap Chat, powered by ', h.img({ src: 'images/haskell.jpg' })),
               h.br(),
               h.div(null, 
                  h.button({ onClick: doCreateUser, type: 'button', className: 'btn-primary', disabled: ! self.newUser.get() }, 'Create User'), ' ',
                  h.input(self.newUser.bindInput({ type: 'text' }))),
               h.br(),
               h.div(null, !! self.user.get() ? 
                  h.div(null, 
                     h.button({ type: 'button', className: 'btn-danger', onClick: doLogout }, 'Logout'),
                     ' Welcome ' + self.user.get().name, 
                     h.div(null, 
                        h.div({ className: 'well' },
                           h.h4(null, 'Create Message'),
                           h.span(null, 'Recipients: '),
                           _.map(self.allUsers.get(), function (user) {
                              return h.span({ key: user.name }, user.name, h.input({ type: 'checkbox', checked: self.recipients.get()[user.id], onChange: function(e) {
                                 self.recipients.mutate(function(rs) {
                                    rs[user.id] = e.target.checked;
                                    return rs;
                                 });
                              } }));
                           }),
                           h.br(),
                           h.span(null, 'Message: '),
                           h.input(self.message.bindInput({ type: 'text' })),
                           h.button({ onClick: createMessage }, 'Send'),
                           h.br()),
                        h.div({ className: 'row' },
                           MessageList({ title: 'Received', glyph: 'glyphicon-arrow-up', messages: self.receivedMessages.get(), deleteMessage: deleteMessage }),
                           MessageList({ title: 'Sent', glyph: 'glyphicon-arrow-down', messages: self.sentMessages.get(), deleteMessage: deleteMessage })))) :
                  h.div(null, 
                     h.button({ onClick: doLogin, disabled: ! self.login.get(), type: 'button', className: 'btn-success' }, 'Login'), ' ',
                     h.input(self.login.bindInput({ id: 'login-name', type: 'text' })))));
         },
         componentDidMount: focusLogin
      });
   })();

   var MessageList = (function () {
      return React.createClass({
         propTypes: {
            deleteMessage: React.PropTypes.func.isRequired,
            messages: React.PropTypes.array.isRequired,
            title: React.PropTypes.string.isRequired
         },
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
