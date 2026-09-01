/* Global Selectize sizing: readable controls, wrapping chips, viewport menus. */
(function () {
  "use strict";

  var placeholder = "Select…";

  function sizeDropdown(instance) {
    var dropdown = instance.$dropdown && instance.$dropdown[0];
    var content = dropdown && dropdown.querySelector(".selectize-dropdown-content");
    if (!dropdown || !content) return;
    window.requestAnimationFrame(function () {
      var top = dropdown.getBoundingClientRect().top;
      var available = Math.max(120, Math.floor(window.innerHeight - top - 16));
      content.style.setProperty(
        "--cerebro-select-menu-max-height",
        available + "px"
      );
    });
  }

  function syncMultiSelectWidth(instance) {
    var host = instance.$wrapper.closest(".form-group, .cv-ctl")[0];
    var control = instance.$control && instance.$control[0];
    if (!host || !control) return;
    host.classList.remove("cerebro-multiselect-expanded");
    host.style.removeProperty("--cerebro-multiselect-width");
    window.requestAnimationFrame(function () {
      var items = instance.$control.children(".item");
      if (!items.length) return;
      var required = 32;
      items.each(function () {
        required += window.jQuery(this).outerWidth(true);
      });
      if (instance.$control_input && instance.$control_input[0]) {
        required += Math.max(24, instance.$control_input[0].scrollWidth);
      }
      var base = control.clientWidth;
      var available = required;
      var primary = host.closest(".cerebro-viz-primary, .cv-topbar");
      if (primary) {
        var hostBottom = host.getBoundingClientRect().bottom;
        var peers = primary.querySelectorAll(
          ".form-group, .cv-ctl, #cv-more-btn, .cv-topbar-right"
        );
        var used = 0;
        var peerCount = 0;
        Array.prototype.forEach.call(peers, function (peer) {
          var rect = peer.getBoundingClientRect();
          var parentHost = peer.parentElement &&
            peer.parentElement.closest(".form-group, .cv-ctl");
          if (
            peer !== host &&
            !parentHost &&
            rect.width > 0 &&
            Math.abs(rect.bottom - hostBottom) < 2
          ) {
            used += rect.width;
            peerCount += 1;
          }
        });
        var gap = parseFloat(window.getComputedStyle(primary).columnGap) || 0;
        available = Math.max(base, primary.clientWidth - used - gap * peerCount);
      }
      var target = Math.max(base, Math.min(required, available));
      if (target > base + 1) {
        host.style.setProperty(
          "--cerebro-multiselect-width",
          Math.ceil(target) + "px"
        );
        host.classList.add("cerebro-multiselect-expanded");
      }
    });
  }

  function enhance(select) {
    if (!select) return;
    var instance = select.selectize;
    if (!instance) return;

    if (!select.dataset.cerebroDropdownReady) {
      instance.on("dropdown_open", function () { sizeDropdown(instance); });
      select.dataset.cerebroDropdownReady = "true";
    }
    if (!select.multiple || select.dataset.cerebroMultiSelectReady) return;

    select.setAttribute("data-placeholder", placeholder);
    instance.settings.placeholder = placeholder;
    if (instance.$control_input && instance.$control_input.length) {
      instance.$control_input.attr("placeholder", placeholder);
    }
    instance.updatePlaceholder();
    instance.$wrapper.addClass("cerebro-multiselect");
    instance.$wrapper
      .closest(".form-group, .cv-ctl")
      .addClass("cerebro-multiselect-host");
    instance.on("item_add", function () { syncMultiSelectWidth(instance); });
    instance.on("item_remove", function () { syncMultiSelectWidth(instance); });
    instance.on("clear", function () { syncMultiSelectWidth(instance); });
    syncMultiSelectWidth(instance);
    select.dataset.cerebroMultiSelectReady = "true";
  }

  function enhanceAll(root) {
    if (root !== document && (!root || root.nodeType !== Node.ELEMENT_NODE)) {
      return;
    }
    Array.prototype.forEach.call(root.querySelectorAll("select"), enhance);
    if (root.matches && root.matches("select")) enhance(root);
  }

  var pendingRoots = new Set();
  var pendingTimer = null;
  function schedule(root) {
    if (root !== document && (!root || root.nodeType !== Node.ELEMENT_NODE)) return;
    if (
      root !== document &&
      !(root.matches("select") || root.querySelector("select"))
    ) {
      return;
    }
    pendingRoots.add(root);
    if (pendingTimer) return;
    pendingTimer = window.setTimeout(function () {
      var roots = Array.from(pendingRoots);
      pendingRoots.clear();
      pendingTimer = null;
      roots.forEach(enhanceAll);
    }, 0);
  }

  document.addEventListener("DOMContentLoaded", function () { schedule(document); });
  document.addEventListener("shiny:connected", function () { schedule(document); });
  document.addEventListener("shiny:value", function (event) { schedule(event.target); });
  new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
      if (mutation.type === "attributes") {
        schedule(mutation.target);
        return;
      }
      Array.prototype.forEach.call(mutation.addedNodes, schedule);
    });
  }).observe(document.documentElement, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: ["class"]
  });
}());
