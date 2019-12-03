"use strict";

exports.setRawModeImpl = function (stream) {
  return function (mode) {
    return function () {
      stream.setRawMode(mode)
    }
  }
}
