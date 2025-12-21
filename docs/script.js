// get canvas and context
const canvas = document.getElementById('stars');
const ctx = canvas.getContext('2d');
let width = canvas.width = window.innerWidth;
let height = canvas.height = window.innerHeight;

// settings
const dimStarsCount = 3000;
const brightStarsCount = 200;
const dimStars = [];
const brightStars = [];

// scroll simulation
let lastScrollY = window.scrollY;
let worldOffsetY = 0;
let scrollSpeed = 0;

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

// create star
function createStar(speedMin, speedMax, radiusMin, radiusMax) {
  return {
    x: Math.random() * width - width / 2,
    y: Math.random() * height - height / 2,
    z: Math.random() * width,
    speed: Math.random() * (speedMax - speedMin) + speedMin,
    radius: Math.random() * (radiusMax - radiusMin) + radiusMin
  };
}

// init stars
function initStars() {
  dimStars.length = 0;
  brightStars.length = 0;

  for (let i = 0; i < dimStarsCount; i++) {
    dimStars.push(createStar(0.2, 0.7, 0.5, 2));
  }

  for (let i = 0; i < brightStarsCount; i++) {
    brightStars.push(createStar(2, 5, 1.5, 4));
  }
}

initStars();

// draw stars
function drawStars(stars, scrollFactor) {
  stars.forEach(star => {
    // forward motion
    star.z -= star.speed;
    if (star.z <= 0) star.z = width;

    // apply scroll in opposite direction
    let wy = star.y - worldOffsetY * scrollFactor;

    // infinite vertical wrap
    const halfH = height / 2;
    wy = ((wy + halfH) % height + height) % height - halfH;

    const sx = (star.x / star.z) * width / 2 + width / 2;
    const sy = (wy / star.z) * height / 2 + height / 2;
    const r = (1 - star.z / width) * star.radius;

    ctx.beginPath();
    ctx.arc(sx, sy, r, 0, Math.PI * 2);
    ctx.fill();
  });
}

// spawn comet
function spawnComet() {
  comet.active = true;
  comet.x = Math.random() * width - width / 2;
  comet.y = Math.random() * height - height / 2;
  comet.z = width;
}

// draw comet
function drawComet() {
  if (!comet.active || !cometImg.complete) return;

  comet.z -= comet.speed;
  if (comet.z <= 0) {
    comet.active = false;
    return;
  }

  const wy = comet.y - worldOffsetY * 0.6;
  const sx = (comet.x / comet.z) * width / 2 + width / 2;
  const sy = (wy / comet.z) * height / 2 + height / 2;

  const scale = (1 - comet.z / width) * 1.5;
  const size = 40 * scale;

  ctx.drawImage(cometImg, sx - size / 2, sy - size / 2, size, size);
}

// animation loop
function animate() {
  ctx.clearRect(0, 0, width, height);

  ctx.fillStyle = 'rgba(255,255,255,0.45)';
  drawStars(dimStars, 0.3);

  ctx.fillStyle = 'white';
  drawStars(brightStars, 0.6);

  if (!comet.active && Math.random() < 0.002) {
    spawnComet();
  }

  drawComet();

  // inertia
  scrollSpeed *= 0.9;
  worldOffsetY += scrollSpeed;

  requestAnimationFrame(animate);
}

animate();

// scroll handler
window.addEventListener('scroll', () => {
  const dy = window.scrollY - lastScrollY;
  lastScrollY = window.scrollY;

  // scroll down = fly down
  scrollSpeed += dy * 0.25;
});

// resize handler
window.addEventListener('resize', () => {
  width = canvas.width = window.innerWidth;
  height = canvas.height = window.innerHeight;
  initStars();
  comet.active = false;
});

// reset scroll
window.scrollTo(0, 0);
