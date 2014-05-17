'use strict';
$(function() {

   var assertString = Util.assertString;
   var assertDefined = Util.assertDefined;
   var domId = Util.domId;

   var rpc = RPC.rpc;
   var withResponse = RPC.withResponse;

   var LensedState = Lens.LensedState;


   // for notational convenience
   var h = React.DOM;

   var Page = (function () {
      var self;
      // lenses
      var newUser, login, user, allUsers, sentMessages, receivedMessages;

      function updateUsers(users) {
         allUsers.set(users);
      }
      function addRecipient(user) {
         recipients.mutate(function (rs) {
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
         rpc('doCreateUser').createUser(tag, newUser.get()).call().done(function (data) {
            withResponse(data, tag, function (v) {
               newUser.revert();
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
                     user.set(user);
                     rpc('post-login').
                           listUsers('list-users').
                           listMessagesSent('sent-messages', user.id).
                           listMessagesReceived('received-messages', user.id).
                           call().done(function(data) {
                        withResponse(data, 'list-users', updateUsers);
                        withResponse(data, 'sent-messages', sentMessages.set);
                        withResponse(data, 'received-messages', receivedMessages.set);
                     });
                  } else {
                     alert('Invalid login: ' + loginName);
                  }
               });
            });
         }
      }
      function doLogout() {
         user.revert();
         allUsers.revert();
         sentMessages.revert();
         receivedMessages.revert();
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
         var userId = user.get().id;
         rpc(tag).
               listUsers('list-users').
               listMessagesSent('sent-messages', userId).
               listMessagesReceived('received-messages', userId).
               call().done(function (data) {
            withResponse(data, 'list-users', updateUsers);
            withResponse(data, 'sent-messages', sentMessages.set);
            withResponse(data, 'received-messages', receivedMessages.set);
         });
      }
      function deleteMessage(messageId) {
         var tag = 'delete-message';
         rpc('delete-message').deleteMessage(tag, messageId).call().done(function (data) {
            update('post-delete-message');
         });
      }
      return React.createClass({
         displayName: 'SnAX',
         mixins: [LensedState()],
         getInitialState: function () {
            self = this; // this is the first opportunity.
            newUser = self.lens.narrow('newUser', '');
            login = self.lens.narrow('login', '');
            user = self.lens.narrow('user', '');
            allUsers = self.lens.narrow('allUsers', []);
            sentMessages = self.lens.narrow('sentMessages', []);
            receivedMessages = self.lens.narrow('receivedMessages', []);
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
                  h.button({ onClick: doCreateUser, type: 'button', className: 'btn-primary', disabled: ! newUser.get() }, 'Create User'), ' ',
                  h.input(newUser.bindInput({ type: 'text' }))),
               h.br(),
               h.div(null, !! user.get() ? 
                  h.div(null, 
                     h.button({ type: 'button', className: 'btn-danger', onClick: doLogout }, 'Logout'),
                     ' Welcome, ', h.em(null, user.get().name),
                     h.div(null, 
                        MessageComposer({ user: user.get(), allUsers: allUsers.get(), onCreateMessage: function () {
                           update('post-create-message');
                        }}),
                        h.div({ className: 'row' },
                           MessageList({ title: 'Received', glyph: 'glyphicon-arrow-up', messages: receivedMessages.get(), deleteMessage: deleteMessage }),
                           MessageList({ title: 'Sent', glyph: 'glyphicon-arrow-down', messages: sentMessages.get(), deleteMessage: deleteMessage })))) :
                  h.div(null, 
                     h.span(null, 'Login: '),
                     _.map(allUsers.get(), function(user) {
                        return h.span({ key: user.id }, h.a({ onClick: doLogin(user.name) }, user.name), ' ');
                     }))));
         },
      });
   })();
   var MessageComposer = (function () {
      var self;
      var message, recipients;
      function createMessage() {
         var tag = 'create-message';
         var rr = recipients.get();
         var rs = _.reduce(rr, function(a, on, id) {
            on && a.push(parseInt(id, 10));
            return a;
         }, []);
         rpc('create-message').createMessage(tag, self.props.user.id, rs, message.get()).call().done(function (data) {
            withResponse(data, tag, function (v) {
               console.log('created message: ' + v.messageId);
            });
            message.revert();
            recipients.revert();
            self.props.onCreateMessage();
         });
         recipients.set(_.reduce(self.props.allUsers, function (rs, u) {
            rs[u.id] = false;
            return rs;
         }, {}));
      }
      return React.createClass({
         mixins: [LensedState()],
         propTypes: {
            user: React.PropTypes.object.isRequired,
            allUsers: React.PropTypes.array.isRequired,
            onCreateMessage: React.PropTypes.func.isRequired
         },
         getInitialState: function () {
            self = this;
            message = self.lens.narrow('message', '');
            recipients = self.lens.narrow('recipients', {});
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
            _.map(recipients.get(), function (recip) {
               if (! _.has(umap, recip.id)) {
                  nixed.push(recip.id);
               }
            });
            recipients.mutate(function (recips) {
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
                  var recipients = recipients.get();
                  return _.map(self.props.allUsers, function (user) {
                     return h.span({ key: user.name }, user.name, h.input({ type: 'checkbox', checked: _.has(recipients, user.id), onChange: function(e) {
                        recipients.mutate(function(rs) {
                           rs[user.id] = e.target.checked;
                           return rs;
                        });
                     } })) }) }) (),
               h.br(),
               h.span(null, 'Message: '),
               h.input(message.bindInput({ type: 'text' })),
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
