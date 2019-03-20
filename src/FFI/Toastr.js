'use strict';

exports.sendToast = function (options) {
  return function () {
    toastr[options.level](options.message);
  }
};