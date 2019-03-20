/*
  Johan Karlsson (DonKarlssonSan) 2017
  Read more about how a Flow Field works on my blog:
  https://codepen.io/DonKarlssonSan/post/particles-in-simplex-noise-flow-field
*/

const config = {
  zoom: 90,
  noiseSpeed: 0.02,
  particleSpeed: 2,
  resolutionToParticlesRatio: 1/200,
};

const colorConfig = {
  particleOpacity: 0.7,
  baseHue: 230,
  hueRange: 20,
  hueSpeed: 0.02,
  colorSaturation: 80,
  clearAlpha: 0.12,
};

class Particle {
  constructor(x, y) {
    this.pos = new Vector(x, y);
    this.prevPos = new Vector(x, y);
    this.vel = new Vector(Math.random() - 0.5, Math.random() - 0.5);
    this.acc = new Vector(0, 0);
    this.size = Math.random() * 5;
  }

  move(acc) {
    this.prevPos.x = this.pos.x;
    this.prevPos.y = this.pos.y;
    if(acc) {
      this.acc.addTo(acc);
    }
    this.vel.addTo(this.acc);
    this.pos.addTo(this.vel);
    if(this.vel.getLength() > config.particleSpeed) {
      this.vel.setLength(config.particleSpeed);
    }
    this.acc.x = 0;
    this.acc.y = 0;
  }

  draw() {
    ctx.fillRect(this.pos.x, this.pos.y, this.size, this.size);
  }

  drawLine() {
    ctx.beginPath();
    ctx.moveTo(this.prevPos.x, this.prevPos.y);
    ctx.lineTo(this.pos.x, this.pos.y);
    ctx.stroke();
  }

  wrap() {
    if(this.pos.x > w) {
      this.prevPos.x = this.pos.x = 0;
    } else if(this.pos.x < 0) {
      this.prevPos.x = this.pos.x = w - 1;
    }
    if(this.pos.y > h) {
      this.prevPos.y = this.pos.y = 0;
    } else if(this.pos.y < 0) {
      this.prevPos.y = this.pos.y = h - 1;
    }
  }
}

let canvas;
let ctx;
let field;
let w, h;
let size;
let columns;
let rows;
let noiseZ;
let hueCounter;
let particles;

function setup() {
  size = 5;
  noiseZ = 0;
  canvas = document.querySelector("#canvas");
  ctx = canvas.getContext("2d");
  window.addEventListener("resize", reset);
  reset();
}

function reset() {
  hueCounter = 0;
  noise.seed(Math.random());
  w = canvas.width = window.innerWidth;
  h = canvas.height = window.innerHeight;
  columns = Math.floor(w / size) + 1;
  rows = Math.floor(h / size) + 1;
  initParticles();
  initField();
}

function initParticles() {
  particles = [];
  let numberOfParticles = w * h * config.resolutionToParticlesRatio;
  for(let i = 0; i < numberOfParticles; i++) {
    let particle = new Particle(Math.random() * w, Math.random() * h);
    particles.push(particle);
  }
}

function draw() {
  drawBackground();
  requestAnimationFrame(draw);
  calculateField();
  noiseZ += config.noiseSpeed;
  hueCounter += colorConfig.hueSpeed;
  drawParticles();
}

function initField() {
  field = new Array(columns);
  for(let x = 0; x < columns; x++) {
    field[x] = new Array(columns);
    for(let y = 0; y < rows; y++) {
      field[x][y] = new Vector(0, 0);
    }
  }
}

function calculateField() {
  for(let x = 0; x < columns; x++) {
    for(let y = 0; y < rows; y++) {
      let a = noise.simplex3(x/config.zoom, y/config.zoom, noiseZ) * Math.PI/4;
      let l = noise.simplex3(x/config.zoom + 40000, y/config.zoom + 40000, noiseZ)/2;
      field[x][y].setLength(l);
      field[x][y].setAngle(a);
    }
  }
}

function drawBackground() {
  ctx.fillStyle = `rgba(0, 0, 0, ${colorConfig.clearAlpha})`;
  ctx.fillRect(0, 0, w, h);
}

function drawParticles() {
  let h = Math.sin(hueCounter) * colorConfig.hueRange + colorConfig.baseHue;
   ctx.fillStyle = `hsla(${h}, ${colorConfig.colorSaturation}%, 50%, ${colorConfig.particleOpacity})`;

  let x;
  let y;
  particles.forEach((p, i) => {
    x = Math.floor(p.pos.x / size);
    y = Math.floor(p.pos.y / size);
    if(x >= 0 && x < columns && y >= 0 && y < rows) {
      p.move(field[x][y]);
    }
    p.wrap();
    p.draw();
  });
}

setup();
draw();
