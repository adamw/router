/* global exports */
"use strict";

// module Pixi

var PIXI = require('pixi.js/bin/pixi.js');

exports.newRenderer = function(w, h) {
  return function() {
    return PIXI.autoDetectRenderer(w, h, { antialias: true });
  };
};

exports.newContainer = function() {
  return function() {
    return new PIXI.Container();
  };
};

exports.newText = function() {
  return function() {
    return new PIXI.Text('');
  };
};

exports.appendRendererToBody = function(renderer) {
  return function() {
    document.body.appendChild(renderer.view);
    return {};
  };
};

exports.render = function (dict) {
  return function(c, renderer) {
    return function() {
      renderer.render(c);
      return {};
    };
  };
};

exports.setBackgroundColor = function(c, renderer) {
  return function() {
    renderer.backgroundColor = c;
    return {};
  };
};

exports.setText = function(t, textObject) {
  return function() {
    textObject.text = t;
    return {};
  };
};

exports.setPosition = function (dict) {
  return function(x, y, obj) {
    return function() {
      obj.position.set(x, y);
      return {};
    };
  };
};

exports.addToContainer = function(dict1) {
  return function (dict2) {
    return function(obj, cont) {
      return function() {
        cont.addChild(obj);
        return {};
      };
    };
  };
};

