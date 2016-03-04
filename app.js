import PIXI from 'pixi.js/bin/pixi.js';

var gameState = {
  stops: {
    stop1: {
      x: 50,
      y: 150,
      selected: true,
      routes: []
    },
    stop2: {
      x: 250,
      y: 175,
      selected: false,
      routes: []
    },
    stop3: {
      x: 150,
      y: 350,
      selected: false,
      routes: []
    }
  },
  roads: [
    { f: "stop1", t: "stop2" },
    { f: "stop2", t: "stop3" }
  ]
};

class Stop {
  constructor(name) {
    this.name = name;
    this.g = new PIXI.Graphics();
  }
  redraw() {
    this.g.clear();
    
    this.g.beginFill(0x4679BD)
      .lineStyle(2, 0x4679BD, 1)
      .drawCircle(0, 0, 15)
      .endFill();

    if (this.selected) {
      this.g.lineStyle(5, 0xcfdc00, 1)
        .drawCircle(0, 0, 19);
    }
  }
  update(selected) {
    if (this.selected !== selected) {
      this.selected = selected;
      this.redraw();
    }
  }
  addTo(container, x, y) {
    container.addChild(this.g);
    this.g.position.set(x, y);
    this.redraw();
  }
}

function drawStops(container, state, stops) {
  var ss = state.stops;
  for (var s of Object.keys(ss)) {
    let stopState = ss[s];
    let stop = stops.get(s);
    if (!stop) {
      stop = new Stop(name);
      stops.set(s, stop);
      stop.addTo(container, stopState.x, stopState.y);
    }

    stop.update(stopState.selected);
  }
}

var renderer = PIXI.autoDetectRenderer(640, 480, { antialias: true });
renderer.backgroundColor = 0x555555;
document.body.appendChild(renderer.view);

var stage = new PIXI.Container();
var stopsMap = new Map();

var fps = {
  countInThisSecond: 0,
  fpsInLastSecond: 0,
  thisSecond: 0,
  t: new PIXI.Text(''),
  update: function() {
    var nowSecond = Math.floor(new Date().getTime() / 1000);
    if (nowSecond != this.thisSecond) {
      this.fpsInLastSecond = this.countInThisSecond;
      this.countInThisSecond = 0;
      this.thisSecond = nowSecond;
    }
    
    this.countInThisSecond += 1;
    fps.t.text = 'FPS: ' + this.fpsInLastSecond;
  }
};
fps.t.position.set(20);
stage.addChild(fps.t);

requestAnimationFrame(animate);

function animate() {
  requestAnimationFrame(animate);

  fps.update();
  
  drawStops(stage, gameState, stopsMap);
  
  renderer.render(stage);
}
