'use strict';

__define('Lens', function (my) {
   var assertDefined = Util.assertDefined;
   /**
    * Adds lenses on a data structure.
    * A lens proxies the getting and the setting of the values within a data structure and provides the following advantages:
    * - Mistyping the name of the lens causes a run time failure rather than getting or setting the wrong thing silently.
    * - Allows convenient binding of the lens to an input control.
    * - Allows components to proxy their state changes deeply in the component hierarchy (i.e. as props).
    * requires: lodash, q.
    */
   my.LensedState = function () {
      var state = {};
      // the mixin
      return {
         // Install the root lens in the component.
         getInitialState: function () {
            var component = this;
            function refresh () {
               // this prevents an NPE when creating the initial lens.
               component.state && component.setState({counter: 1 + component.state.counter});
            }
            function createLens (keys, initialValue) {
               // Assigning to a var gives us, effectively, a permanent this pointer.
               // This, in turn, allows us to "peel off" the functions, i.e. use references to them without calling them,
               // without losing track of the lens to which they refer. (This is the screw 'this' pattern.)
               var lens = {
                  // Get the state segment.
                  // This should be the only thing called from render.
                  get: function () {
                     return _.reduce(keys, function(a, k) {
                        return a[k];
                     }, state);
                  },
                  // Set the state segment
                  set: function (v) {
                     assertDefined(v, 'Lensing undefined value to \'', keys, '\'.');
                     _.reduce(_.initial(keys), function (a, k) {
                        return a[k];
                     }, state)[_.last(keys)] = v;
                     refresh();
                  },
                  // promise a value to be set
                  promise: function () {
                     var q = Q.defer();
                     // take advantage of set's proxying.
                     q.promise.done(lens.set, function(reason) {
                        console.warn('Promise rejected for lens value \'' + keys + '\': ' + reason);
                     });
                     return q;
                  },
                  // Change the state segment using a function that takes the current value and returns the new value.
                  // Rejects mutations to undefined as these are probably programming errors.
                  mutate: function (mutator) {
                     var m  = mutator(lens.get());
                     assertDefined(m, 'Mutating lens \'', keys, '\' to undefined. Did you forget to return something from the mutator?');
                     lens.set(m);
                  },
                  // Reverts the state to its initial value.
                  revert: function () {
                     assertDefined(initialValue, 'Reverting to undefined value to \'', keys, '\'.');
                     lens.set(initialValue);
                  },
                  // Binds the lens to an input control.
                  // ps is the props object, inline this call to the component
                  // Can't use this with JSX. :/
                  bindInput: function (ps) {
                     __assert(! ps.onChange, 'onChange property already bound for lens\'', keys, '\'.');
                     ps.onChange = function (e) {
                        lens.set(e.target.value);
                     }
                     __assert(! ps.value, 'value property already bound for lens\'', keys, '\'.');
                     ps.value = lens.get();
                     return ps;
                  },
                  narrow: function (key, initialValue) {
                     return createLens(keys.concat(key), initialValue);
                  }
               };
               if (! _.isUndefined(initialValue)) {
                  lens.set(initialValue);
               }
               return lens;
            }
            component.lens = {
               narrow: function (key, initialValue) {
                  return createLens([key], initialValue);
               }
            }
            // the state is not managed by react; instead, we just give it a counter that we increment to provoke a render.
            return {counter: 0};
         }
      }
   };
});