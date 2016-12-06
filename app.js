import main from './src/Main.purs';

main.main();
/*
var renderer = PIXI.autoDetectRenderer(640, 480, { antialias: true });
renderer.backgroundColor = 0x555555;
document.body.appendChild(renderer.view);

var stage = new PIXI.Container();

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

var c1 = new PIXI.Graphics();
c1.lineStyle(1.0, 0xFF0000, 1.0);
c1.drawCircle(0, 0, 15);
c1.position.set(100, 100);
stage.addChild(c1);

var c2 = new PIXI.Graphics();
c2.lineStyle(1.0, 0x00FF00, 1.0);
c2.drawCircle(0, 0, 15);
c2.position.set(400, 200);
stage.addChild(c2);

var r = new PIXI.Graphics();
r.beginFill(0x0000FF, 1.0);
r.lineStyle(1.0, 0x0000FF, 1.0);
r.drawRect(0, 0, 100, 10);
r.endFill();

var desiredWidth = Math.sqrt(Math.pow(c1.x-c2.x, 2)+Math.pow(c1.y-c2.y, 2));
var angle = Math.atan2(c2.y-c1.y, c2.x-c1.x);
console.log(angle);

r.width = desiredWidth;
r.rotation = angle;

r.position.set(c1.x, c1.y);

stage.addChild(r);

requestAnimationFrame(animate);

function animate() {
  requestAnimationFrame(animate);

  fps.update();
  
  renderer.render(stage);
}

*/
