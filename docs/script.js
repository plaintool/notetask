// get the canvas and context
const canvas = document.getElementById('stars');
const ctx = canvas.getContext('2d');
let width = canvas.width = window.innerWidth;
let height = canvas.height = window.innerHeight;

// settings
const dimStarsCount = 3000; // small dim stars
const brightStarsCount = 200; // big bright stars
const dimStars = [];
const brightStars = [];

// load image
const cometImg = new Image();
cometImg.src = "star.png";

// comet object
const comet = {
  active: false,
  x: 0,
  y: 0,
  z: 0,
  speed: 6
};

// create small dim stars
for (let i = 0; i < dimStarsCount; i++) {
  dimStars.push({
    x: Math.random() * width - width/2,
    y: Math.random() * height - height/2,
    z: Math.random() * width,
    speed: Math.random() * 0.5 + 0.2,
    radius: Math.random() * 1.5 + 0.5
  });
}

// create big bright stars
for (let i = 0; i < brightStarsCount; i++) {
  brightStars.push({
    x: Math.random() * width - width/2,
    y: Math.random() * height - height/2,
    z: Math.random() * width,
    speed: Math.random() * 3 + 2,
    radius: Math.random() * 3 + 1.5
  });
}

// draw stars
function drawStars(stars) {
  stars.forEach(star => {
    star.z -= star.speed; // move star towards camera
    if (star.z <= 0) {   // reset star when too close
      star.x = Math.random() * width - width/2;
      star.y = Math.random() * height - height/2;
      star.z = width;
    }
    const sx = (star.x / star.z) * width/2 + width/2;
    const sy = (star.y / star.z) * height/2 + height/2;
    const r = (1 - star.z / width) * star.radius;
    ctx.beginPath();
    ctx.arc(sx, sy, r, 0, Math.PI * 2);
    ctx.fill();
  });
}

// launch comet
function spawnComet() {
  comet.active = true;
  comet.x = Math.random() * width - width / 2;
  comet.y = Math.random() * height - height / 2;
  comet.z = width; // start far away
}

// draw comet
function drawComet() {
  if (!comet.active || !cometImg.complete) return;

  comet.z -= comet.speed;

  if (comet.z <= 0) {
    comet.active = false;
    return;
  }

  const sx = (comet.x / comet.z) * width / 2 + width / 2;
  const sy = (comet.y / comet.z) * height / 2 + height / 2;

  const scale = (1 - comet.z / width) * 1.5; // grows when close
  const size = 40 * scale;

  ctx.drawImage(cometImg, sx - size / 2, sy - size / 2, size, size);
}

// animation loop
function animate() {
  ctx.clearRect(0, 0, width, height);

  ctx.fillStyle = 'rgba(255,255,255,0.5)';
  drawStars(dimStars);

  ctx.fillStyle = 'white';
  drawStars(brightStars);

  // sometimes spawn comet
  if (!comet.active && Math.random() < 0.002) { // шанс примерно 1 раз в 500 кадров
    spawnComet();
  }

  drawComet();

  requestAnimationFrame(animate);
}


animate();

// handle resize
window.addEventListener('resize', () => {
  width = canvas.width = window.innerWidth;
  height = canvas.height = window.innerHeight;

  // recreate stars
  dimStars.length = 0;
  brightStars.length = 0;

  for (let i = 0; i < dimStarsCount; i++) {
    dimStars.push({
      x: Math.random() * width - width/2,
      y: Math.random() * height - height/2,
      z: Math.random() * width,
      speed: Math.random() * 0.5 + 0.2,
      radius: Math.random() * 1.5 + 0.5
    });
  }

  for (let i = 0; i < brightStarsCount; i++) {
    brightStars.push({
      x: Math.random() * width - width/2,
      y: Math.random() * height - height/2,
      z: Math.random() * width,
      speed: Math.random() * 3 + 2,
      radius: Math.random() * 3 + 1.5
    });
  }

  // reset comet too
  comet.active = false;
});

// scroll to top by default
window.scrollTo(0, 0);