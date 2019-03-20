'use strict';

exports.confirm = function (msg) {
  return function () { 
    return confirm(msg);
  };
}