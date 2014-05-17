'use strict';

__define('Util', function (my) {
	my.assertString = function (v, n) {
		__assert(_.isString(v), "'" + n + "' must be a string: " + n);
	};
	my.assertDefined = function (v, n) {
		__assert(! _.isUndefined(v), "'" + n + "' is not defined.");
	};
	// insists on exactly one element, returns the one element.
	my.domId = function (id) {
		var $el = $('#' + id);
		__assert(1 == $el.length, 'non-unique dom id \'', id, '\': ', $el.length);
		return $el[0];
	};
});

