(function(config) {
  'use strict';

  var mobile = window.matchMedia('(max-width: 575.98px)');
  var drawer = null;
  var layout = null;
  var main = null;
  var closeToggle = null;
  var stateObserver = null;
  var footerObserver = null;
  var observedFooter = null;
  var initialized = false;
  var stableOpen = null;
  var invoker = null;
  var invokerId = null;
  var mainState = null;

  function deepGetById(root, id) {
    if (!root || !id) {
      return null;
    }
    if (typeof root.getElementById === 'function') {
      var direct = root.getElementById(id);
      if (direct) {
        return direct;
      }
    }
    if (typeof root.querySelectorAll !== 'function') {
      return null;
    }
    var elements = root.querySelectorAll('*');
    for (var i = 0; i < elements.length; i += 1) {
      if (elements[i].id === id) {
        return elements[i];
      }
      if (!elements[i].shadowRoot) {
        continue;
      }
      var nested = deepGetById(elements[i].shadowRoot, id);
      if (nested) {
        return nested;
      }
    }
    return null;
  }

  function deepActiveElement() {
    var active = document.activeElement;
    while (active && active.shadowRoot && active.shadowRoot.activeElement) {
      active = active.shadowRoot.activeElement;
    }
    return active;
  }

  function isWithin(root, node) {
    var current = node;
    while (current) {
      if (current === root) {
        return true;
      }
      if (current.parentNode) {
        current = current.parentNode;
      } else if (current.host) {
        current = current.host;
      } else {
        current = null;
      }
    }
    return false;
  }

  function focusElement(element) {
    if (!element || !element.isConnected || typeof element.focus !== 'function') {
      return;
    }
    try {
      element.focus({ preventScroll: true });
    } catch (error) {
      element.focus();
    }
  }

  function isVisible(element) {
    if (!element || element.hidden || element.getAttribute('aria-hidden') === 'true') {
      return false;
    }
    if (typeof element.getClientRects !== 'function') {
      return true;
    }
    return element.getClientRects().length > 0;
  }

  function drawerFocusables() {
    if (!drawer) {
      return [];
    }
    var selector = [
      'a[href]:not([tabindex="-1"])',
      'button:not([disabled]):not([tabindex="-1"])',
      'input:not([disabled]):not([type="hidden"]):not([tabindex="-1"])',
      'select:not([disabled]):not([tabindex="-1"])',
      'textarea:not([disabled]):not([tabindex="-1"])',
      '[tabindex]:not([tabindex="-1"])'
    ].join(',');
    var elements = Array.prototype.slice.call(drawer.querySelectorAll(selector));
    elements = elements.filter(function(element) {
      return isVisible(element) && element.getAttribute('aria-disabled') !== 'true';
    });
    if (isVisible(closeToggle)) {
      elements.push(closeToggle);
    }
    return elements;
  }

  function isOpen() {
    return !!layout && !layout.classList.contains('sidebar-collapsed');
  }

  function isTransitioning() {
    return !!layout && layout.classList.contains('transitioning');
  }

  function setTriggerState(trigger, open) {
    if (!trigger) {
      return;
    }
    trigger.setAttribute('aria-controls', config.drawerId);
    trigger.setAttribute('aria-expanded', open ? 'true' : 'false');
    trigger.setAttribute('data-tempest-settings-trigger', 'true');
  }

  function syncTriggers(open) {
    config.triggerIds.forEach(function(id) {
      setTriggerState(deepGetById(document, id), open);
    });
  }

  function syncCloseToggle(open) {
    if (!closeToggle) {
      return;
    }
    var label = open ? 'Close workspace settings' : 'Open workspace settings';
    closeToggle.setAttribute('aria-label', label);
    closeToggle.setAttribute('title', label);
    closeToggle.setAttribute('aria-controls', config.drawerId);
    closeToggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    closeToggle.setAttribute('data-tempest-settings-close', 'true');
  }

  function isolateMain() {
    if (!main || mainState) {
      return;
    }
    var active = deepActiveElement();
    if (isWithin(main, active)) {
      focusElement(closeToggle);
    }
    mainState = {
      inert: main.hasAttribute('inert'),
      inertValue: main.getAttribute('inert'),
      ariaHidden: main.hasAttribute('aria-hidden'),
      ariaHiddenValue: main.getAttribute('aria-hidden')
    };
    main.setAttribute('inert', '');
    main.setAttribute('aria-hidden', 'true');
  }

  function restoreMain() {
    if (!main || !mainState) {
      return;
    }
    if (mainState.inert) {
      main.setAttribute('inert', mainState.inertValue || '');
    } else {
      main.removeAttribute('inert');
    }
    if (mainState.ariaHidden) {
      main.setAttribute('aria-hidden', mainState.ariaHiddenValue || '');
    } else {
      main.removeAttribute('aria-hidden');
    }
    mainState = null;
  }

  function focusDrawer() {
    var heading = deepGetById(drawer, config.titleId);
    var target = heading || drawerFocusables()[0] || closeToggle;
    window.requestAnimationFrame(function() {
      focusElement(target);
    });
  }

  function focusInvoker() {
    var target = invoker;
    if (!target || !target.isConnected) {
      target = invokerId ? deepGetById(document, invokerId) : null;
    }
    window.requestAnimationFrame(function() {
      focusElement(target);
    });
  }

  function observeFooter() {
    var footer = deepGetById(document, config.footerId);
    if (!footer || footer === observedFooter) {
      return;
    }
    if (footerObserver) {
      footerObserver.disconnect();
    }
    observedFooter = footer;
    footerObserver = new MutationObserver(function() {
      syncTriggers(isOpen());
    });
    footerObserver.observe(footer, { childList: true, subtree: true });
  }

  function sync() {
    if (!initialized) {
      return;
    }
    var open = isOpen();
    syncTriggers(open);
    syncCloseToggle(open);
    observeFooter();

    if (open && mobile.matches) {
      isolateMain();
    } else {
      restoreMain();
    }

    if (isTransitioning() || stableOpen === open) {
      return;
    }
    stableOpen = open;
    if (open) {
      focusDrawer();
    } else {
      restoreMain();
      focusInvoker();
    }
  }

  function eventPath(event) {
    if (typeof event.composedPath === 'function') {
      return event.composedPath();
    }
    return [event.target];
  }

  function handleClick(event) {
    var path = eventPath(event);
    for (var i = 0; i < path.length; i += 1) {
      var node = path[i];
      if (!node || !node.id) {
        continue;
      }
      if (config.triggerIds.indexOf(node.id) !== -1) {
        invoker = node;
        invokerId = node.id;
        setTriggerState(node, isOpen());
        return;
      }
      if (node === closeToggle && !isOpen()) {
        invoker = closeToggle;
        invokerId = null;
        return;
      }
    }
  }

  function closeDrawer() {
    if (!isOpen() || !closeToggle) {
      return;
    }
    closeToggle.click();
  }

  function nestedWidgetOwnsEscape() {
    var active = deepActiveElement();
    if (!active || typeof active.closest !== 'function') {
      return false;
    }
    return !!active.closest([
      '[role="combobox"][aria-expanded="true"]',
      '[aria-haspopup][aria-expanded="true"]',
      '.selectize-input.dropdown-active',
      '.dropdown.show'
    ].join(','));
  }

  function handleKeydown(event) {
    if (!isOpen() || event.defaultPrevented) {
      return;
    }
    if (event.key === 'Escape') {
      if (!mobile.matches || nestedWidgetOwnsEscape()) {
        return;
      }
      event.preventDefault();
      event.stopImmediatePropagation();
      closeDrawer();
      return;
    }
    if (event.key !== 'Tab' || !mobile.matches) {
      return;
    }

    var focusable = drawerFocusables();
    if (focusable.length === 0) {
      event.preventDefault();
      focusElement(closeToggle);
      return;
    }
    var active = deepActiveElement();
    var index = focusable.indexOf(active);
    if (index === -1) {
      event.preventDefault();
      focusElement(event.shiftKey ? focusable[focusable.length - 1] : focusable[0]);
      return;
    }
    if (!event.shiftKey && index === focusable.length - 1) {
      event.preventDefault();
      focusElement(focusable[0]);
    } else if (event.shiftKey && index === 0) {
      event.preventDefault();
      focusElement(focusable[focusable.length - 1]);
    }
  }

  function handleFocusin(event) {
    if (!isOpen() || !mobile.matches || isTransitioning()) {
      return;
    }
    var target = eventPath(event)[0];
    if (target === closeToggle || isWithin(drawer, target)) {
      return;
    }
    focusDrawer();
  }

  function handleMobileChange() {
    sync();
    if (isOpen() && mobile.matches && !isTransitioning()) {
      focusDrawer();
    }
  }

  function initialize() {
    if (initialized) {
      syncTriggers(isOpen());
      observeFooter();
      return true;
    }
    drawer = deepGetById(document, config.drawerId);
    layout = drawer ? drawer.closest('.bslib-sidebar-layout') : null;
    main = layout ? layout.querySelector(':scope > .main') : null;
    closeToggle = layout ? layout.querySelector(':scope > .collapse-toggle') : null;
    if (!drawer || !layout || !main || !closeToggle) {
      return false;
    }

    initialized = true;
    drawer.setAttribute('aria-labelledby', config.titleId);
    drawer.setAttribute('data-tempest-settings-drawer', 'true');
    stableOpen = isOpen();
    sync();

    stateObserver = new MutationObserver(sync);
    stateObserver.observe(layout, {
      attributes: true,
      attributeFilter: ['class']
    });
    stateObserver.observe(drawer, {
      attributes: true,
      attributeFilter: ['hidden']
    });
    document.addEventListener('click', handleClick, true);
    document.addEventListener('keydown', handleKeydown);
    document.addEventListener('focusin', handleFocusin, true);
    if (typeof mobile.addEventListener === 'function') {
      mobile.addEventListener('change', handleMobileChange);
    } else if (typeof mobile.addListener === 'function') {
      mobile.addListener(handleMobileChange);
    }
    observeFooter();
    return true;
  }

  function start() {
    if (initialize()) {
      return;
    }
    window.requestAnimationFrame(initialize);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start, { once: true });
  } else {
    start();
  }
  document.addEventListener('shiny:connected', start);
})
