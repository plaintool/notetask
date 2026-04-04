document.querySelectorAll('img').forEach(function(img) {
  // Skip images inside no-zoom container
  if (img.closest('.no-zoom')) {
    return;
  }

  img.classList.add('zoom-img');

  img.addEventListener('click', function () {
    var overlay = document.createElement('div');
    overlay.className = 'zoom-overlay';

    var clone = img.cloneNode();
    overlay.appendChild(clone);
    document.body.appendChild(overlay);

    overlay.addEventListener('click', function () {
      overlay.remove();
    });
  });
});