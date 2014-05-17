'use strict';

function __error() {
  throw _.reduce(arguments, function (a, v) { return a + v; }, '');
}
function __assert(cond) {
  cond || __error.apply(null, _.tail(arguments));
}
function __define(module, defn) {
  __assert(_.isUndefined(window[module]), "'" + module + "' already defined.");
  window[module] = {};
  defn(window[module]);
}
