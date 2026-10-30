// Delegate help interactions so dynamically rebuilt level rows work too.
(function () {
  var tip, active;
  function hide() {
    if (tip) tip.hidden = true;
    active = null;
  }
  function show(target) {
    if (!tip) {
      tip = document.createElement('div');
      tip.id = 'stapelev-help-tooltip';
      tip.className = 'stapelev-help-tooltip';
      tip.setAttribute('role', 'tooltip');
      document.body.appendChild(tip);
    }
    active = target;
    tip.textContent = target.getAttribute('data-help');
    tip.hidden = false;
    var rect = target.getBoundingClientRect();
    tip.style.left = Math.max(8, Math.min(rect.left, window.innerWidth - tip.offsetWidth - 8)) + 'px';
    tip.style.top = (rect.bottom + 8 + tip.offsetHeight < window.innerHeight ? rect.bottom + 8 : Math.max(8, rect.top - tip.offsetHeight - 8)) + 'px';
  }
  document.addEventListener('mouseover', function (e) {
    var target = e.target.closest('.stapelev-info');
    if (target) show(target);
  });
  document.addEventListener('mouseout', function (e) {
    var target = e.target.closest('.stapelev-info');
    if (target && !target.contains(e.relatedTarget)) hide();
  });
  document.addEventListener('focusin', function (e) {
    var target = e.target.closest('.stapelev-info');
    if (target) show(target);
  });
  document.addEventListener('focusout', hide);
  document.addEventListener('click', function (e) {
    var target = e.target.closest('.stapelev-info');
    if (target) show(target); else hide();
  });
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hide();
    if ((e.key === 'Enter' || e.key === ' ') && e.target.matches('.stapelev-info')) {
      e.preventDefault();
      show(e.target);
    }
  });
  document.addEventListener('scroll', hide, true);
  window.addEventListener('resize', hide);
})();
