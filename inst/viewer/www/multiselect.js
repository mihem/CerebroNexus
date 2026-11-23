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
      // A collapsed multi-select's list (chosen items, then the options)
      // scrolls as one, as tall as the window allows.
      dropdown.style.setProperty("--cerebro-ms-menu-max-height", available + "px");
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
    collapse(instance);
    if (COUNTED.indexOf(select.id) >= 0) countTools(select, instance);
  }

  // A multi-select with more than one item shows only its first, and a badge
  // with the count ("12+"); the field is as wide as those two (or its minimum
  // width), so no other chip runs under the badge. Clicking the field or the
  // badge opens the list, which starts with every chosen item and its cross
  // (also when no option is left to offer) and shows everything when the
  // window is tall enough; clicking elsewhere closes it. The list is an
  // overlay, so the controls around the field keep their height.
  function collapse(instance) {
    var wrapper = instance.$wrapper[0];
    var control = instance.$control[0];
    if (!wrapper || !control || wrapper._cerebroCollapse) return;
    wrapper._cerebroCollapse = true;
    wrapper.classList.add("cerebro-ms-collapse");
    instance.$dropdown.addClass("cerebro-ms-dropdown");
    var badge = document.createElement("span");
    badge.className = "cerebro-ms-more";
    badge.setAttribute("role", "button");
    badge.setAttribute("tabindex", "-1");
    badge.addEventListener("mousedown", function (event) {
      event.preventDefault();
      event.stopPropagation();
      if (instance.isFocused) {
        openChosen();
      } else {
        instance.focus();
      }
    });
    wrapper.appendChild(badge);
    var chosen = document.createElement("div");
    chosen.className = "cerebro-ms-chosen";
    chosen.addEventListener("mousedown", function (event) {
      // Keep the field open; a cross removes its item.
      event.preventDefault();
      var cross = event.target.closest && event.target.closest("[data-remove]");
      if (cross) instance.removeItem(cross.getAttribute("data-remove"));
    });
    var labelOf = function (value) {
      var item = instance.getItem(value);
      var text = item && item.length ? item[0].textContent : value;
      return text.replace(/×\s*$/, "").trim();
    };
    var escape = function (text) {
      return String(text).replace(/[&<>"']/g, function (c) {
        return "&#" + c.charCodeAt(0) + ";";
      });
    };
    var many = function () { return instance.items.length > 1; };
    var fillChosen = function () {
      if (!wrapper.classList.contains("is-open") || !many()) {
        if (chosen.parentNode) chosen.parentNode.removeChild(chosen);
        return;
      }
      chosen.innerHTML = '<div class="cerebro-ms-chosen-head">Chosen (' +
        instance.items.length + ")</div>" +
        instance.items.map(function (value) {
          return '<span class="cerebro-ms-chip">' + escape(labelOf(value)) +
            '<span class="cerebro-ms-remove" data-remove="' + escape(value) +
            '" aria-label="Remove">×</span></span>';
        }).join("");
      var dropdown = instance.$dropdown[0];
      if (chosen.parentNode !== dropdown) dropdown.insertBefore(chosen, dropdown.firstChild);
    };
    var update = function () {
      var isMany = many();
      wrapper.classList.toggle("is-many", isMany);
      badge.textContent = isMany ? instance.items.length + "+" : "";
      badge.setAttribute("aria-label", isMany ? "Show all " + instance.items.length : "");
      fillChosen();
      if (instance.isOpen || forced) sizeDropdown(instance);
    };
    // With every option chosen the list has nothing to offer and Selectize
    // keeps it shut; a field with hidden items still opens to show them.
    var forced = false;
    // Open, the field hides its chip for typing: it keeps the width it had, so
    // the controls beside it stay where they are.
    var holdWidth = function () {
      if (!wrapper.style.getPropertyValue("width")) {
        wrapper.style.setProperty("width", wrapper.getBoundingClientRect().width + "px", "important");
      }
    };
    var openChosen = function () {
      if (!instance.isFocused || !many()) return;
      holdWidth();
      wrapper.classList.add("is-open");
      fillChosen();
      if (!instance.isOpen && chosen.parentNode) {
        instance.positionDropdown();
        instance.$dropdown.css({ display: "block", visibility: "visible" });
        forced = true;
      }
      sizeDropdown(instance);
    };
    var closeChosen = function () {
      if (forced) instance.$dropdown.hide();
      forced = false;
      wrapper.style.removeProperty("width");
      wrapper.classList.remove("is-open");
      update();
    };
    instance.on("change", update);
    instance.on("item_add", update);
    instance.on("item_remove", function () {
      update();
      if (instance.isFocused) window.setTimeout(openChosen, 0);
    });
    instance.on("focus", function () { window.setTimeout(openChosen, 0); });
    instance.on("blur", closeChosen);
    instance.on("dropdown_open", function () {
      forced = false;
      if (many()) holdWidth();
      wrapper.classList.add("is-open");
      fillChosen();
      sizeDropdown(instance);
    });
    instance.on("dropdown_close", function () {
      if (instance.isFocused) {
        window.setTimeout(openChosen, 0);
      } else {
        closeChosen();
      }
    });
    update();
  }

  // Fields that hold sections count them in their label and offer every
  // section (All) or only the first chosen (One; None in the grid's picture
  // field, which can be left empty). In a field grouped by
  // section (one item per group), All adds each missing group's first option.
  var COUNTED = [
    "spatial_projection_sections",
    "cv-pick-spatial",
    "spatial_projection_grid_background_images"
  ];
  function countTools(select, instance) {
    var host = select.closest(".form-group, .cv-ctl");
    var label = host && host.querySelector("label");
    if (!label) return;
    var tools = label.querySelector(".cv-sections-tools");
    if (!tools) {
      tools = document.createElement("span");
      tools.className = "cv-sections-tools";
      // The pictures of the grid can all be taken away: None; the sections
      // keep One.
      var second = select.id === "spatial_projection_grid_background_images"
        ? ["none", "None"] : ["one", "One"];
      tools.innerHTML = '<span class="cv-sections-count"></span>' +
        '<button type="button" data-sections="all">All</button>' +
        '<button type="button" data-sections="' + second[0] + '">' + second[1] + '</button>';
      label.appendChild(tools);
    }
    var groupOf = function (value) {
      var option = instance.options[value];
      return option && option.optgroup != null ? String(option.optgroup) : value;
    };
    var units = function () {
      var seen = {};
      Object.keys(instance.options).forEach(function (value) { seen[groupOf(value)] = true; });
      return Object.keys(seen).length;
    };
    var update = function () {
      var total = units();
      var chosen = instance.items.length;
      tools.style.display = total < 3 ? "none" : "";
      tools.querySelector(".cv-sections-count").textContent = chosen + " of " + total;
      tools.querySelector('[data-sections="all"]').disabled = chosen >= total;
      var one = tools.querySelector('[data-sections="one"]');
      if (one) one.disabled = chosen <= 1;
      var none = tools.querySelector('[data-sections="none"]');
      if (none) none.disabled = chosen < 1;
    };
    // The label's click would open the field: the tools keep their clicks.
    tools.onclick = function (event) {
      var button = event.target.closest && event.target.closest("button[data-sections]");
      event.preventDefault();
      event.stopPropagation();
      if (!button || button.disabled) return;
      if (button.getAttribute("data-sections") === "all") {
        var chosen = instance.items.slice();
        var groups = {};
        chosen.forEach(function (value) { groups[groupOf(value)] = true; });
        // In the list's order, after those already chosen.
        Object.keys(instance.options)
          .sort(function (a, b) { return instance.options[a].$order - instance.options[b].$order; })
          .forEach(function (value) {
            var group = groupOf(value);
            if (groups[group]) return;
            groups[group] = true;
            chosen.push(value);
          });
        instance.setValue(chosen);
      } else if (button.getAttribute("data-sections") === "none") {
        instance.setValue([]);
      } else if (instance.items.length) {
        instance.setValue([instance.items[0]]);
      }
    };
    instance.on("change", update);
    instance.on("option_add", update);
    update();
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
