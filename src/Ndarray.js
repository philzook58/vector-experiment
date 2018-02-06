/* global exports */
"use strict";


var ndarray = require("ndarray");
var ops = require("ndarray-ops");


exports.fill = function(val, shape){
    var dest = ndarray(new Float64Array(size));
	ops.assigns(dest, val);
	return dest
}

exports.add_internal = function(a, b){
	var size = ;
	var dest = ndarray(new Float64Array(size));
	ops.add(a, b, c);
	return c
}



