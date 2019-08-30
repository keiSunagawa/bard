"use strict";

exports.undefImpl = function() {
  return null;
};

exports.unsafeLogImpl = function(a) {
  console.log(a)
  return a;
};
