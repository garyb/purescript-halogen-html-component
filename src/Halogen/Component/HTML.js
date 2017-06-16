"use strict";

exports.setInnerHTML = function (el) {
  return function (html) {
    return function () {
      el.innerHTML = html;
    };
  };
};
