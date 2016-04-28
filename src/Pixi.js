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

exports.newGraphics = function() {
  return function() {
    return new PIXI.Graphics();
  };
};

exports.newCircle = function(coords, r) {
  return function() {
    return new PIXI.Circle(coords.x, coords.y, r);
  };
};

exports.appendRendererToBody = function(renderer) {
  return function() {
    document.body.appendChild(renderer.view);
    return {};
  };
};

exports.renderContainer = function (dict) {
  return function(c, renderer) {
    return function() {
      renderer.render(c);
      return {};
    };
  };
};

exports.setBgColor = function(c, renderer) {
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
  return function(coords, obj) {
    return function() {
      obj.position.set(coords.x, coords.y);
      return {};
    };
  };
};

exports.setInteractive = function (dict) {
  return function(int, obj) {
    return function() {
      obj.interactive = int;
      return {};
    };
  };
};

exports.setButtonMode = function (dict) {
  return function(bm, obj) {
    return function() {
      obj.buttonMode = bm;
      return {};
    };
  };
};

exports.setHitArea = function(dict1) { return function (dict2) {
  return function(s, obj) {
    return function() {
      obj.hitArea = s;
      return {};
    };
  };
}; };

exports.addToContainer = function(dict1) { return function (dict2) {
  return function(obj, cont) {
    return function() {
      cont.addChild(obj);
      return {};
    };
  };
}; };

// Graphics

exports.beginFill = function(color, alpha, g) {
  return function() {
    g.beginFill(color, alpha);
    return {};
  };
};

exports.lineStyle = function(lineWidth, color, alpha, g) {
  return function() {
    g.lineStyle(lineWidth, color, alpha);
    return {};
  };
};

exports.drawCircle = function(coords, r, g) {
  return function() {
    g.drawCircle(coords.x, coords.y, r);
    return {};
  };
};

exports.endFill = function(g) {
  return function() {
    g.endFill();
    return {};
  };
};

// Events

exports._onMouseDown = function(action, obj) {
  return function() {
    obj.mousedown = action;
    return {};
  };
};
