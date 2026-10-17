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

  function enhance(select) {
    if (!select) return;
    var instance = select.selectize;
    if (!instance) return;

    if (select._cerebroDropdownInstance !== instance) {
      instance.on("dropdown_open", function () { sizeDropdown(instance); });
      select._cerebroDropdownInstance = instance;
    }
    if (!select.multiple || select._cerebroMultiSelectInstance === instance) return;

    select.setAttribute("data-placeholder", placeholder);
    instance.settings.placeholder = placeholder;
    if (instance.$control_input && instance.$control_input.length) {
      instance.$control_input.attr("placeholder", placeholder);
    }
    instance.updatePlaceholder();
    instance.$wrapper.addClass("cerebro-multiselect");
    select._cerebroMultiSelectInstance = instance;
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
