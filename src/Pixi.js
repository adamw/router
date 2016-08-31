/* global exports */
"use strict";

// module Pixi

var PIXI = require('pixi.js');

exports.newRenderer = function(w, h) {
  return PIXI.autoDetectRenderer(w, h, { antialias: true });
};

exports.newContainer = function() {
  return new PIXI.Container();
};

exports.newText = function() {
  return new PIXI.Text('');
};

exports.newGraphics = function() {
  return new PIXI.Graphics();
};

exports.newCircle = function(coords, r) {
  return function() {
    return new PIXI.Circle(coords.x, coords.y, r);
  };
};

exports.newRectangle = function(coords, w, h) {
  return function() {
    return new PIXI.Rectangle(coords.x, coords.y, w, h);
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

exports.setTextStyle = function(s, textObject) {
  return function() {
    textObject.style = s;
    return {};
  };
};

exports.setAnchor = function (dict) {
  return function(xf, yf, text) {
    return function() {
      text.anchor.set(xf, yf);
      return {};
    };
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

exports.setRotation = function (dict) {
  return function(angle, obj) {
    return function() {
      obj.rotation = angle;
      return {};
    };
  };
};

exports.setWidth = function (dict) {
  return function(w, obj) {
    return function() {
      obj.width = w;
      return {};
    };
  };
};

exports.setHeight = function (dict) {
  return function(h, obj) {
    return function() {
      obj.height = h;
      return {};
    };
  };
};

exports.getWidth = function (dict) {
  return function(obj) {
    return function() {
      return obj.width;
    };
  };
};

exports.getHeight = function (dict) {
  return function(obj) {
    return function() {
      return obj.height;
    };
  };
};


// Buttons

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

exports.removeAllFromContainer = function (dict) {
  return function(c) {
    return function() {
      c.removeChildren();
      return {};
    };
  };
};

exports.removeFromContainer = function(dict1) { return function (dict2) {
  return function(obj, cont) {
    return function() {
      cont.removeChild(obj);
      return {};
    };
  };
}; };

// Graphics

exports.clear = function(g) {
  return function() {
    g.clear();
    return {};
  };
};

exports._beginFill = function(color, alpha, g) {
  return function() {
    g.beginFill(color, alpha);
    return {};
  };
};

exports._lineStyle = function(lineWidth, color, alpha, g) {
  return function() {
    g.lineStyle(lineWidth, color, alpha);
    return {};
  };
};

exports._drawCircle = function(coords, r, g) {
  return function() {
    g.drawCircle(coords.x, coords.y, r);
    return {};
  };
};

exports._drawRect = function(coords, w, h, g) {
  return function() {
    g.drawRect(coords.x, coords.y, w, h);
    return {};
  };
};

exports._moveTo = function(coords, g) {
  return function() {
    g.moveTo(coords.x, coords.y);
    return {};
  };
};

exports._lineTo = function(coords, g) {
  return function() {
    g.lineTo(coords.x, coords.y);
    return {};
  };
};

exports._arc = function(coords, r, alpha, beta, g) {
  return function() {
    g.arc(coords.x, coords.y, r, alpha, beta);
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
    obj.on('mousedown', action);
    return {};
  };
};

exports._onMouseOver = function(action, obj) {
  return function() {
    obj.on('mouseover', action);
    return {};
  };
};

exports._onMouseOut = function(action, obj) {
  return function() {
    obj.on('mouseout', action);
    return {};
  };
};
