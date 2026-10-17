(function () {
  "use strict";

  // shinydashboard 0.7.3 assumes its sidebar input is already bound when a
  // transition ends. Initial navigation animations can finish before Shiny
  // connects; defer those notifications until the binding actually exists.
  function guardSidebarTransition(event) {
    var sidebar = event.target.closest && event.target.closest(".main-sidebar");
    if (!sidebar || !window.jQuery) return;
    var binding = window.jQuery(sidebar).data("shiny-input-binding");
    if (!binding) event.stopPropagation();
  }
  ["transitionend", "webkitTransitionEnd", "otransitionend",
    "oTransitionEnd", "msTransitionEnd"].forEach(function (name) {
    document.addEventListener(name, guardSidebarTransition, true);
  });

  var MOBILE_QUERY = "(max-width: 767px)";
  function ready(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn, { once: true });
    } else {
      fn();
    }
  }

  // A first server flush (or a conditional page becoming available) can arrive
  // after the reader has already navigated. Author defaults must not undo that
  // choice. Record intent in the browser, including clicks before Shiny connects.
  function installInitialNavigation() {
    var navigated = false;
    var installed = false;
    document.addEventListener("click", function (event) {
      var target = event.target;
      if (target && target.closest &&
          target.closest('.main-sidebar a[data-toggle="tab"]')) {
        navigated = true;
      }
    }, true);
    function install() {
      if (installed || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
      installed = true;
      window.Shiny.addCustomMessageHandler("cerebro_initial_page", function (message) {
        if (navigated || !message || typeof message.selected !== "string") return;
        var links = document.querySelectorAll('.main-sidebar a[data-toggle="tab"]');
        for (var i = 0; i < links.length; i++) {
          if (links[i].getAttribute("data-value") === message.selected) {
            window.jQuery(links[i]).tab("show");
            break;
          }
        }
      });
    }
    if (window.jQuery) {
      window.jQuery(document).on("shiny:connected.cerebroInitialNavigation", install);
    }
    install();
  }
  ready(installInitialNavigation);

  ready(function () {
    var body = document.body;
    var sidebar = document.querySelector(".main-sidebar");
    var toggle = document.querySelector(".main-header .sidebar-toggle");
    var close = document.getElementById("cerebro-nav-close");
    var scrim = document.getElementById("cerebro-nav-scrim");
    if (!body || !sidebar || !toggle || !close || !scrim) return;
    if (!sidebar.id) sidebar.id = "cerebro-primary-navigation";
    if (scrim.parentNode !== body) body.appendChild(scrim);

    function isMobile() { return window.matchMedia(MOBILE_QUERY).matches; }
    function isOpen() {
      return isMobile() && body.classList.contains("sidebar-open");
    }
    function syncSemantics() {
      var mobile = isMobile();
      var open = mobile && body.classList.contains("sidebar-open");
      toggle.setAttribute("aria-controls", sidebar.id);
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
      toggle.setAttribute("aria-label", open ? "Close navigation" : "Open navigation");
      scrim.setAttribute("aria-hidden", open ? "false" : "true");
      if (mobile) {
        sidebar.setAttribute("role", "dialog");
        sidebar.setAttribute("aria-label", "Primary navigation");
        sidebar.setAttribute("aria-modal", "true");
        sidebar.setAttribute("aria-hidden", open ? "false" : "true");
        sidebar.inert = !open;
      } else {
        ["role", "aria-label", "aria-modal", "aria-hidden"].forEach(function (name) {
          sidebar.removeAttribute(name);
        });
        sidebar.inert = false;
      }
    }
    function focusActiveDestination(navLink) {
      var moved = false;
      function move() {
        if (moved) return;
        var pane = document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
        if (!pane) return;
        var target = pane.querySelector("h1, h2, h3") || pane;
        target.classList.add("cerebro-page-focus-target");
        target.setAttribute("tabindex", "-1");
        target.focus({ preventScroll: true });
        moved = true;
      }
      if (window.jQuery) window.jQuery(navLink).one("shown.bs.tab", move);
      window.setTimeout(move, 80);
    }
    function setOpen(open, restoreFocus) {
      if (!isMobile()) return;
      if (open) {
        document.dispatchEvent(new CustomEvent("cerebro:overlay-opening", {
          detail: { owner: "nav" }
        }));
      }
      body.classList.toggle("sidebar-open", open);
      syncSemantics();
      if (open) {
        window.requestAnimationFrame(function () {
          if (isOpen()) close.focus();
        });
      } else if (restoreFocus !== false) {
        toggle.focus();
      }
    }

    document.addEventListener("click", function (event) {
      var target = event.target;
      if (!target || !target.closest) return;
      if (target.closest(".main-header .sidebar-toggle") && isMobile()) {
        event.preventDefault();
        event.stopImmediatePropagation();
        setOpen(!isOpen());
        return;
      }
      if (target.closest("#cerebro-nav-close") || target.closest("#cerebro-nav-scrim")) {
        event.preventDefault();
        setOpen(false);
        return;
      }
      var navLink = target.closest(".main-sidebar a[href]");
      if (navLink) {
        document.dispatchEvent(new CustomEvent("cerebro:overlay-opening", {
          detail: { owner: "nav" }
        }));
        if (isOpen()) setOpen(false, false);
        focusActiveDestination(navLink);
      }
    }, true);

    document.addEventListener("keydown", function (event) {
      if (!isOpen()) return;
      if (event.key === "Escape") {
        event.preventDefault();
        event.stopImmediatePropagation();
        setOpen(false);
        return;
      }
      if (event.key !== "Tab") return;
      var candidates = Array.prototype.filter.call(
        sidebar.querySelectorAll(
          "button:not([disabled]), a[href], input:not([disabled]), select:not([disabled])"
        ),
        function (element) { return element.getClientRects().length > 0; }
      );
      if (!candidates.length) return;
      var first = candidates[0];
      var last = candidates[candidates.length - 1];
      if (event.shiftKey && document.activeElement === first) {
        event.preventDefault();
        last.focus();
      } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault();
        first.focus();
      }
    }, true);

    document.addEventListener("cerebro:overlay-opening", function (event) {
      if (event.detail && event.detail.owner !== "nav" && isOpen()) {
        setOpen(false, false);
      }
    });
    window.addEventListener("resize", function () {
      if (!isMobile()) body.classList.remove("sidebar-open");
      syncSemantics();
    });
    syncSemantics();
  });

  ready(function () {
    if (!window.jQuery) return;
    function syncTabs(root) {
      root.querySelectorAll('.nav-tabs > li > a[data-toggle="tab"]').forEach(function (tab) {
        var selected = tab.parentElement.classList.contains("active");
        tab.parentElement.parentElement.setAttribute("role", "tablist");
        tab.parentElement.setAttribute("role", "presentation");
        tab.setAttribute("role", "tab");
        var target = tab.getAttribute("href");
        var panel = target && target.charAt(0) === "#" && document.getElementById(target.slice(1));
        if (panel) {
          if (!tab.id) tab.id = panel.id + "-tab";
          tab.setAttribute("aria-controls", panel.id);
          panel.setAttribute("role", "tabpanel");
          panel.setAttribute("aria-labelledby", tab.id);
        }
        tab.setAttribute("aria-selected", selected ? "true" : "false");
        tab.setAttribute("aria-expanded", selected ? "true" : "false");
        tab.setAttribute("tabindex", selected ? "0" : "-1");
      });
    }
    syncTabs(document);
    // Capture before Bootstrap's legacy accessibility handler, so one key
    // produces one activation for both initial and renderUI-created tabsets.
    document.addEventListener("keydown", function (event) {
      var tab = event.target.closest && event.target.closest('.nav-tabs > li > a[data-toggle="tab"]');
      if (!tab || event.altKey || event.ctrlKey || event.metaKey) return;
      var keys = ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Home", "End", "Enter", " "];
      if (keys.indexOf(event.key) < 0) return;
      var tabs = Array.prototype.filter.call(tab.parentElement.parentElement.children, function (li) {
        var link = li.querySelector('a[data-toggle="tab"]');
        return link && !li.classList.contains("disabled") &&
          link.getAttribute("aria-disabled") !== "true" && link.getClientRects().length;
      }).map(function (li) { return li.querySelector('a[data-toggle="tab"]'); });
      var index = tabs.indexOf(tab);
      if (index < 0) return;
      if (event.key === "Home") index = 0;
      else if (event.key === "End") index = tabs.length - 1;
      else if (event.key === "ArrowLeft" || event.key === "ArrowUp") index = (index + tabs.length - 1) % tabs.length;
      else if (event.key === "ArrowRight" || event.key === "ArrowDown") index = (index + 1) % tabs.length;
      event.preventDefault();
      event.stopImmediatePropagation();
      window.jQuery(tabs[index]).tab("show");
      tabs[index].focus();
    }, true);
    window.jQuery(document).on("shown.bs.tab.cerebroTabs", '[data-toggle="tab"]', function () {
      // Bootstrap's accessibility activate wrapper marks every descendant of
      // the opened pane selected after emitting shown. Reconcile on return.
      Promise.resolve().then(function () { syncTabs(document); });
    }).on("shiny:bound.cerebroTabs", function (event) {
      // Dynamic tabsets bind after renderUI inserts their initial markup.
      if (event.target && event.target.querySelectorAll) syncTabs(event.target);
    });
  });

  ready(function () {
    if (!window.jQuery) return;
    // Shiny's legacy informational modals may omit both dialog semantics and
    // a visible dismiss control. Complete that shared chrome, preserving
    // mandatory dialogs (static backdrop / keyboard dismissal disabled).
    window.jQuery(document).on("show.bs.modal.cerebroDialog", "#shiny-modal", function () {
      var modal = this;
      var invoker = document.activeElement;
      if (invoker && invoker !== document.body && !modal.contains(invoker)) {
        modal.__cerebroInvoker = invoker;
      }
      modal.setAttribute("role", "dialog");
      modal.setAttribute("aria-modal", "true");
      var title = modal.querySelector(".modal-title");
      if (title) {
        if (!title.id) title.id = "cerebro-modal-title";
        modal.setAttribute("aria-labelledby", title.id);
      }
      var header = modal.querySelector(".modal-header");
      if (header && modal.getAttribute("data-keyboard") !== "false" &&
          modal.getAttribute("data-backdrop") !== "static" &&
          !modal.querySelector('[data-dismiss="modal"]')) {
        var close = document.createElement("button");
        close.type = "button";
        close.className = "close";
        close.setAttribute("data-dismiss", "modal");
        close.setAttribute("aria-label", "Close information");
        close.textContent = "×";
        header.insertBefore(close, header.firstChild);
      }
    }).on("hidden.bs.modal.cerebroDialog", "#shiny-modal", function () {
      var invoker = this.__cerebroInvoker;
      delete this.__cerebroInvoker;
      if (invoker && invoker.isConnected && !invoker.disabled &&
          invoker.getClientRects().length && !document.querySelector(".modal.in")) {
        invoker.focus();
      }
    });
  });

  ready(function () {
    var nextBoxId = 0;
    function syncBox(box) {
      var header = box.querySelector(':scope > .box-header');
      var body = box.querySelector(':scope > .box-body');
      var button = header && header.querySelector('[data-widget="collapse"]');
      if (!button || !body) return;
      if (!body.id) body.id = "cerebro-box-body-" + (++nextBoxId);
      var title = header.querySelector(".box-title");
      var name = "section";
      if (title) {
        var label = title.cloneNode(true);
        label.querySelectorAll("button, .cerebro-info-btn").forEach(function (node) { node.remove(); });
        name = label.textContent.trim() || name;
      }
      var expanded = !box.classList.contains("collapsed-box");
      button.setAttribute("aria-label", (expanded ? "Collapse " : "Expand ") + name);
      button.setAttribute("aria-expanded", String(expanded));
      button.setAttribute("aria-controls", body.id);
      button.querySelectorAll("i").forEach(function (icon) {
        icon.removeAttribute("aria-label");
        icon.setAttribute("aria-hidden", "true");
      });
    }
    function syncModebar(bar) {
      bar.querySelectorAll(".modebar-btn").forEach(function (button) {
        var name = button.getAttribute("data-title");
        if (name) button.setAttribute("aria-label", name);
        // Preserve actual links (for example Plotly's logo) and native buttons.
        if (button.tagName === "A" && !button.hasAttribute("href")) {
          button.setAttribute("role", "button");
          button.setAttribute("tabindex", button.getAttribute("aria-disabled") === "true" ? "-1" : "0");
        }
        if (button.getAttribute("data-toggle") === "true" ||
            button.getAttribute("data-attr") === "dragmode") {
          button.setAttribute("aria-pressed", String(button.classList.contains("active")));
        }
      });
    }
    function enhance(root) {
      if (!root.querySelectorAll) return;
      var nodes = Array.prototype.slice.call(root.querySelectorAll(".box, .modebar"));
      if (root.matches && root.matches(".box, .modebar")) nodes.push(root);
      nodes.forEach(function (node) {
        var isBox = node.classList.contains("box");
        var sync = isBox ? syncBox : syncModebar;
        sync(node);
        if (node.__cerebroAccessibleChrome || !window.MutationObserver) return;
        node.__cerebroAccessibleChrome = true;
        // Observe only each widget's chrome, never its plot data or canvas.
        new MutationObserver(function () { sync(node); }).observe(node, {
          attributes: true, attributeFilter: isBox ? ["class"] : ["class", "data-title"],
          childList: !isBox, subtree: !isBox
        });
      });
    }
    enhance(document);
    var content = document.getElementById("main-content");
    if (content && window.MutationObserver) {
      new MutationObserver(function (records) {
        records.forEach(function (record) {
          record.addedNodes.forEach(function (node) {
            if (node.nodeType === 1 && node.namespaceURI === "http://www.w3.org/1999/xhtml") enhance(node);
          });
        });
      }).observe(content, {childList: true, subtree: true});
    }
    document.addEventListener("keydown", function (event) {
      var button = event.target.closest && event.target.closest('.modebar-btn[role="button"]');
      if (!button || button.tagName !== "A" || (event.key !== "Enter" && event.key !== " ")) return;
      event.preventDefault();
      if (!event.repeat && button.getAttribute("aria-disabled") !== "true" && !button.classList.contains("disabled")) button.click();
    });
  });

  ready(function () {
    if (!window.jQuery) return;
    var busyOutputs = new Set();
    var updateAnnounced = false;
    var statusClearTimer = null;
    function secondaryStatus(element, message, failed) {
      // Only demanded result outputs own this label. Controls and the primary
      // canvas never wait for it; no hidden output is activated here.
      if (!element || /_UI$/.test(element.id) || !/(selected_cells|details_selected|expression_by_group|expression_by_gene|states_by_group_(plot|table)|distribution_along_pseudotime|trajectory_states_.+_(plot|text)|morans_i|hla_(network_table|overlap_table|coverage_table|qc_table_dt))/.test(element.id)) return;
      var status = element.__cerebroSecondaryStatus;
      if (!status && message) {
        status = document.createElement("div");
        status.className = "cerebro-secondary-status";
        status.setAttribute("role", "status");
        element.insertAdjacentElement("beforebegin", status);
        element.__cerebroSecondaryStatus = status;
      }
      if (status) {
        status.classList.toggle("is-error", !!failed);
        status.textContent = message || "";
        status.hidden = !message;
      }
    }
    function announce(message) {
      var status = document.getElementById("cerebro-update-status");
      if (!status) return;
      window.clearTimeout(statusClearTimer);
      status.textContent = message;
      if (message === "Content updated") {
        statusClearTimer = window.setTimeout(function () {
          status.textContent = "";
        }, 1500);
      }
    }
    function pruneBusyOutputs() {
      busyOutputs.forEach(function (element) {
        if (element.isConnected && element.getClientRects().length) return;
        window.clearTimeout(element.__cerebroWaitTimer);
        element.__cerebroWaitTimer = null;
        element.classList.remove("cerebro-output-waiting");
        element.removeAttribute("aria-busy");
        secondaryStatus(element, "");
        busyOutputs.delete(element);
      });
    }
    function finish(element) {
      if (!element || !element.classList) return;
      window.clearTimeout(element.__cerebroWaitTimer);
      element.__cerebroWaitTimer = null;
      element.classList.remove("cerebro-output-waiting");
      element.removeAttribute("aria-busy");
      secondaryStatus(element, "");
      busyOutputs.delete(element);
      pruneBusyOutputs();
      if (updateAnnounced && !busyOutputs.size) {
        updateAnnounced = false;
        announce("Content updated");
      }
    }
    window.jQuery(document)
      .on("shiny:outputinvalidated.cerebroMotion", function (event) {
        var element = event.target;
        if (!element || !element.classList ||
            !element.classList.contains("shiny-bound-output")) return;
        if (!element.getClientRects().length) {
          finish(element);
          return;
        }
        window.clearTimeout(element.__cerebroWaitTimer);
        element.classList.remove("cerebro-output-waiting");
        element.setAttribute("aria-busy", "true");
        busyOutputs.add(element);
        element.__cerebroWaitTimer = window.setTimeout(function () {
          if (element.getAttribute("aria-busy") !== "true") return;
          secondaryStatus(element, "Updating this result…");
          // Structural renderUI wrappers can nest. Dim only the concrete
          // result, never its parents and controls as well.
          if (!element.classList.contains("shiny-html-output") &&
              (element.textContent.trim() || element.children.length)) {
            element.classList.add("cerebro-output-waiting");
          }
          if (!updateAnnounced) {
            updateAnnounced = true;
            announce("Updating content");
          }
        }, 120);
      })
      .on("shiny:value.cerebroMotion shiny:error.cerebroMotion", function (event) {
        finish(event.target);
        if (event.type === "shiny:error" && event.error && event.error.message) {
          secondaryStatus(event.target, "Unable to update this result. See the error below.", true);
        }
      })
      .on("shown.bs.tab.cerebroMotion", function () {
        pruneBusyOutputs();
        if (updateAnnounced && !busyOutputs.size) {
          updateAnnounced = false;
          announce("Content updated");
        }
      });
  });

  ready(function () {
    if (!window.jQuery) return;
    var loader = document.getElementById("cerebro-page-loader");
    var detail = document.getElementById("cerebro-page-loader-detail");
    if (!loader || !detail) return;
    // Keep announcements outside aria-busy panes; their visual indicator moves
    // into a static result container, never into a renderUI-owned output.
    var announcement = document.getElementById("cerebro-page-loader-announcement");
    function showDetail(text) {
      if (detail.textContent !== text) detail.textContent = text;
      if (announcement && announcement.textContent !== text) announcement.textContent = text;
    }
    function mountLoader(pane) {
      var regions = pane.querySelectorAll('.cerebro-page-loading-region');
      var region = Array.from(regions).find(function (node) { return node.getClientRects().length; });
      if (!region) {
        region = pane.querySelector('.cerebro-viz-col, .box-body') || pane;
      }
      region.classList.add('cerebro-page-loader-host');
      if (loader.parentElement !== region) region.appendChild(loader);
    }

    var activePane = null;
    var slowTimer = null;
    var showTimer = null;
    var hideTimer = null;
    var settleToken = 0;
    var shinyBusy = false;
    var localPages = new Set(['loadData', 'groups', 'markerGenes', 'mostExpressedGenes',
      'enrichedPathways', 'extra_material']);
    var delivered = new Set();
    var drawnTables = new WeakSet();
    var drawnPlots = new WeakSet();
    var controlValues = new Map();
    var localFinishPending = false;
    var canvasTabs = new Set([
      "overview",
      "coordinated_views",
      "geneExpression",
      "trajectory",
      "spatial",
      "trekker",
      "hla_tcr_motifs"
    ]);

    function paneName(pane) {
      return pane && pane.id ? pane.id.replace(/^shiny-tab-/, "") : "";
    }
    function pageLabel(pane) {
      return ({ overview: 'Projection', geneExpression: 'Gene expression',
        coordinated_views: 'Linked views', trajectory: 'Trajectory', spatial: 'Spatial',
        hla_tcr_motifs: 'HLA / TCR motifs', groups: 'Groups',
        markerGenes: 'Marker genes', mostExpressedGenes: 'Most expressed genes',
        enrichedPathways: 'Enriched pathways', extra_material: 'Extra material',
        immune_repertoire: 'Immune repertoire', loadData: 'Data info',
        geneIdConversion: 'Gene ID conversion', color_management: 'Colour management',
        trekker: 'Trekker', about: 'About' })[paneName(pane)] || 'this page';
    }
    function currentPane() {
      return document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
    }
    function setBusy(pane, busy) {
      if (!pane) return;
      if (busy) pane.setAttribute("aria-busy", "true");
      else pane.removeAttribute("aria-busy");
    }
    function cancelScheduledFinish() {
      settleToken += 1;
      localFinishPending = false;
    }
    function localCompletion(pane) {
      // Open mode without a selected dataset still needs to expose its upload
      // UI. There is no dataset-ready event to wait for in that case.
      if (paneName(pane) === 'loadData') {
        return !!window.CerebroDatasetContext && window.CerebroDatasetContext.phase() !== 'idle';
      }
      return pane && localPages.has(paneName(pane));
    }
    // Shiny re-applies an output's last value when a renderUI re-render
    // replaces its element (bindOutput() feeds the cached value straight to the
    // binding), so the replacement is filled without another `shiny:value`
    // event. Delivery therefore belongs to the output, not to whichever DOM
    // node happened to receive the value first. A value cached before the
    // current dataset was selected is still rejected: the reset clears it.
    function outputKey(element) {
      return element && typeof element.id === "string" && element.id
        ? element.id
        : null;
    }
    function markDelivered(element) {
      var key = outputKey(element);
      if (key) delivered.add(key);
    }
    function clearDelivered(element) {
      var key = outputKey(element);
      if (key) delivered.delete(key);
    }
    function outputSettled(element) {
      var key = outputKey(element);
      if (!key || !delivered.has(key) ||
          element.classList.contains('recalculating') ||
          element.getAttribute('aria-busy') === 'true') return false;
      if (element.classList.contains('shiny-output-error')) return true;
      if (element.classList.contains('datatables')) return drawnTables.has(element);
      if (element.classList.contains('plotly')) return drawnPlots.has(element);
      if (element.classList.contains('shiny-plot-output')) {
        var img = element.querySelector('img');
        return !!img && img.complete && img.naturalWidth > 0;
      }
      return true;
    }
    function ownsOutput(pane, element) {
      if (!localCompletion(pane)) return true;
      if (!element.getClientRects().length) return false;
      return paneName(pane) !== 'groups' || element.id === 'groups_controls_UI' ||
        element.id === 'groups_by_other_group_plot' || element.id === 'groups_by_other_group_table';
    }
    function localResultSettled(pane) {
      var phase = window.CerebroDatasetContext && window.CerebroDatasetContext.phase();
      if (paneName(pane) === 'loadData') {
        // A catalog describes the selection without loading it, so Data info
        // also settles while pending. resetPages() clears every delivery when
        // the context changes, so the overview below belongs to this selection.
        if (phase && phase !== 'ready' && phase !== 'pending') return false;
        // Overview includes the initial annotation bars in the same response.
        // A later child-output binding must not hold the entire workspace shut.
        return outputSettled(document.getElementById('load_data_overview'));
      }
      if (phase && phase !== 'ready') return false;
      var controls = pane.querySelector('.cerebro-result-controls, .cerebro-groups-controls');
      if (!outputSettled(controls)) return false;
      if (paneName(pane) === 'groups') {
        // Missing grouping variables is an explicit terminal result.
        if (controls.querySelector('[data-composition-unavailable]')) return true;
        if (!controls.querySelector('select')) return !!controls.textContent.trim();
        var plot = document.getElementById('groups_by_other_group_plot');
        if (!outputSettled(plot)) return false;
        var table = document.getElementById('groups_by_other_group_table');
        return !table || !table.getClientRects().length || outputSettled(table);
      }
      var slot = pane.querySelector('.cerebro-result-slot');
      if (!outputSettled(slot)) return false;
      // A renderUI value is only the start: all mounted, visible child outputs
      // must also finish. In particular a DT value is not a completed draw.
      return Array.from(pane.querySelectorAll('.shiny-bound-output')).every(function (element) {
        return !element.getClientRects().length || outputSettled(element);
      });
    }
    function finish(pane) {
      if (!pane) return;
      cancelScheduledFinish();
      window.CerebroPageLifecycle.setReady(pane);
      pane.classList.remove('cerebro-page-initializing');
      setBusy(pane, false);
      window.dispatchEvent(new CustomEvent("cerebro:page-ready"));
      if (pane !== activePane) return;
      if (announcement) announcement.textContent = '';
      window.clearTimeout(slowTimer);
      window.clearTimeout(showTimer);
      window.clearTimeout(hideTimer);
      loader.classList.remove("is-visible");
      hideTimer = window.setTimeout(function () {
        if (!loader.classList.contains("is-visible")) loader.hidden = true;
      }, 150);
      activePane = null;
    }
    function begin(pane) {
      if (!pane) return;
      cancelScheduledFinish();
      window.clearTimeout(slowTimer);
      window.clearTimeout(showTimer);
      window.clearTimeout(hideTimer);
      if (window.CerebroPageLifecycle.isReady(pane)) {
        if (activePane && activePane !== pane) setBusy(activePane, false);
        activePane = pane;
        finish(pane);
        return;
      }
      if (activePane && activePane !== pane) setBusy(activePane, false);
      activePane = pane;
      if (window.CerebroPageLifecycle.isTerminal(pane)) {
        pane.classList.remove('cerebro-page-initializing');
        if (announcement) announcement.textContent = '';
        setBusy(pane, false);
        loader.classList.remove('is-visible'); loader.hidden = true;
        activePane = null;
        return;
      }
      setBusy(pane, true);
      pane.classList.add('cerebro-page-initializing');
      // Data info only refreshes lightweight metadata. Dataset loading already
      // owns its progress UI; a second page card before/after it adds no signal.
      // Keep busy/settled tracking so returning to the page still works normally.
      if (paneName(pane) === "loadData") {
        if (announcement) announcement.textContent = "";
        loader.classList.remove("is-visible");
        loader.hidden = true;
        return;
      }
      mountLoader(pane);
      loader.hidden = true;
      loader.classList.remove("is-visible");
      showTimer = window.setTimeout(function () {
        if (pane !== activePane) return;
        showDetail("Loading " + pageLabel(pane) + "…");
        loader.hidden = false;
        loader.classList.add("is-visible");
      }, 180);
      slowTimer = window.setTimeout(function () {
        if (pane === activePane) {
          showDetail("Still loading " + pageLabel(pane) + "…");
        }
      }, 15000);
    }
    function scheduleFinish(pane) {
      if (!pane || pane !== currentPane() ||
          canvasTabs.has(paneName(pane))) return;
      if (localCompletion(pane)) {
        if (pane !== activePane || localFinishPending || !localResultSettled(pane)) return;
        localFinishPending = true;
        var localToken = settleToken;
        // Start paint confirmation at the actual primary draw. Unrelated
        // Shiny work and cosmetic DOM mutations must not restart these frames.
        window.requestAnimationFrame(function () {
          window.requestAnimationFrame(function () {
            if (localToken !== settleToken) return;
            localFinishPending = false;
            if (pane === activePane && pane === currentPane() && localResultSettled(pane)) finish(pane);
          });
        });
        return;
      }
      var token = ++settleToken;
      // Shiny can emit several values for one page. Wait for a short quiet
      // period, then cross two browser paint boundaries, so the overlay never
      // disappears between the first small output and the final chart/table.
      window.setTimeout(function () {
        if (token !== settleToken || shinyBusy || pane !== currentPane()) return;
        window.requestAnimationFrame(function () {
          window.requestAnimationFrame(function () {
            if (token !== settleToken || shinyBusy || pane !== currentPane()) return;
            finish(pane);
          });
        });
      }, 180);
    }
    function resetPages() {
      delivered = new Set();
      drawnTables = new WeakSet();
      drawnPlots = new WeakSet();
      controlValues.clear();
      document.querySelectorAll('.tab-pane[id^="shiny-tab-"]').forEach(
        function (pane) {
          window.CerebroPageLifecycle.reset(pane);
          pane.classList.remove('cerebro-page-initializing');
          pane.removeAttribute("aria-busy");
        }
      );
      begin(currentPane());
    }
    function paneFor(element) {
      return element && element.closest
        ? element.closest('.tab-pane[id^="shiny-tab-"]')
        : null;
    }

    window.jQuery(document)
      .on("show.bs.tab.cerebroPageLoader", function (event) {
        var name = event.target && event.target.getAttribute('data-value');
        if (name) begin(document.getElementById('shiny-tab-' + name));
      })
      .on("shown.bs.tab.cerebroPageLoader", function () {
        var pane = currentPane();
        begin(pane);
        if (localCompletion(pane) || !shinyBusy) scheduleFinish(pane);
      })
      .on("shiny:inputchanged.cerebroPageLoader", function (event) {
        // Dataset context owns resets, including optimistic user selections.
        // Initial Selectize binding can arrive after the overview response;
        // resetting again here would discard its completed output forever.
        if (event.name === "crb_file_selector") return;
        var input = event.el || document.getElementById(event.name);
        if (!activePane || !localCompletion(activePane) || !input ||
            !activePane.contains(input) || !input.matches('select, input') ||
            input.closest('.datatables')) return;
        var value = JSON.stringify(event.value);
        var unchanged = controlValues.has(input.id) && controlValues.get(input.id) === value;
        controlValues.set(input.id, value);
        if (unchanged) return;
        var field = input.closest('.form-group');
        if (document.activeElement !== input &&
            !(field && field.contains(document.activeElement))) return;
        // A control edit during first entry must not acknowledge the previous
        // table/plot while waiting for Shiny to invalidate its output. Initial
        // input binding messages are not user edits and can arrive after draw.
        if (paneName(activePane) === 'groups' && !input.closest('#groups_controls_UI')) return;
        cancelScheduledFinish();
        activePane.querySelectorAll('.datatables, .plotly, .shiny-plot-output').forEach(function (output) {
          clearDelivered(output); drawnTables.delete(output); drawnPlots.delete(output);
        });
      })
      .on("shiny:busy.cerebroPageLoader", function () {
        shinyBusy = true;
        if (!localCompletion(currentPane())) cancelScheduledFinish();
      })
      .on("shiny:idle.cerebroPageLoader", function () {
        shinyBusy = false;
        var pane = currentPane();
        if (!pane || canvasTabs.has(paneName(pane))) return;
        scheduleFinish(pane);
      })
      .on("shiny:value.cerebroPageLoader", function (event) {
        markDelivered(event.target);
        drawnTables.delete(event.target);
        drawnPlots.delete(event.target);
        var pane = paneFor(event.target);
        if (pane && pane === currentPane() && !canvasTabs.has(paneName(pane)) && ownsOutput(pane, event.target)) {
          cancelScheduledFinish();
          // Shiny fires value before applying it to the DOM.
          window.setTimeout(function () {
            if (localCompletion(pane) || !shinyBusy) scheduleFinish(pane);
          }, 0);
        }
      })
      .on('shiny:outputinvalidated.cerebroPageLoader', function (event) {
        clearDelivered(event.target);
        drawnTables.delete(event.target);
        drawnPlots.delete(event.target);
        if (paneFor(event.target) === activePane && localCompletion(activePane) && ownsOutput(activePane, event.target)) cancelScheduledFinish();
      })
      .on('draw.dt.cerebroPageLoader', function (event) {
        var output = event.target.closest && event.target.closest('.datatables.shiny-bound-output');
        if (!output) return;
        drawnTables.add(output);
        scheduleFinish(paneFor(output));
      })
      .on('plotly_afterplot.cerebroPageLoader', function (event) {
        // Plotly emits a bubbling jQuery event as well as an emitter callback.
        // Delegation survives newPlot/purge replacing the element's emitter.
        var output = event.target.closest && event.target.closest('.plotly.shiny-bound-output');
        if (!output) return;
        drawnPlots.add(output);
        scheduleFinish(paneFor(output));
      })
      .on("shiny:error.cerebroPageLoader", function (event) {
        // Silent req() conditions are not terminal results.
        if (event.error && event.error.message) markDelivered(event.target);
        var pane = paneFor(event.target);
        if (pane && pane === currentPane() && ownsOutput(pane, event.target)) {
          cancelScheduledFinish();
          window.setTimeout(function () {
            if (localCompletion(pane) || !shinyBusy) scheduleFinish(pane);
          }, 0);
        }
      })
      .on("shiny:connected.cerebroPageLoader", function () {
        begin(currentPane());
      });

    window.addEventListener("cerebro:dataset-context", function (event) {
      var detail = event && event.detail || {};
      if (detail.changed || detail.phase !== "ready") resetPages();
    });

    document.addEventListener('load', function (event) {
      var pane = paneFor(event.target);
      if (localCompletion(pane)) scheduleFinish(pane);
    }, true);

    window.addEventListener("cerebro:cell-view-ready", function (event) {
      if (!event.detail || !event.detail.painted) return;
      if (window.CerebroDatasetContext && !window.CerebroDatasetContext.accepts(event.detail)) return;
      var id = event.detail && event.detail.id;
      var host = id && document.getElementById(id + "_cell_view_host");
      var pane = paneFor(host);
      if (pane && pane === currentPane()) finish(pane);
    });
    window.addEventListener("cerebro:linkedviews-ready", function (event) {
      if (!event.detail || !event.detail.complete || !event.detail.painted) return;
      if (window.CerebroDatasetContext && !window.CerebroDatasetContext.accepts(event.detail)) return;
      var pane = document.getElementById("shiny-tab-coordinated_views");
      if (pane && pane === currentPane()) finish(pane);
    });
    window.addEventListener('cerebro:view-terminal', function (event) {
      var pane = document.getElementById('shiny-tab-' + event.detail.page);
      if (!pane || pane !== activePane) return;
      cancelScheduledFinish();
      window.clearTimeout(slowTimer); window.clearTimeout(hideTimer); window.clearTimeout(showTimer);
      pane.classList.remove('cerebro-page-initializing');
      setBusy(pane, false);
      if (announcement) announcement.textContent = '';
      loader.classList.remove('is-visible'); loader.hidden = true;
      activePane = null;
    });

    var content = document.getElementById("main-content");
    if (content && window.MutationObserver) {
      new MutationObserver(function (records) {
        var pane = activePane;
        if (!pane || (!localCompletion(pane) && shinyBusy) || canvasTabs.has(paneName(pane))) return;
        var changed = records.some(function (record) {
          return pane.contains(record.target) && !loader.contains(record.target);
        });
        if (changed) scheduleFinish(pane);
      }).observe(content, {
        subtree: true,
        childList: true,
        characterData: true,
        attributes: true
      });
    }
  });

  ready(function () {
    var HANDLE = ".cerebro-selection-composition-drag";
    var HEADER = ".cerebro-selection-composition-head";
    var SLOT = ".cerebro-selection-composition-slot";
    var MOBILE = "(max-width: 700px)";
    var EDGE = 8;
    var drag = null;
    var clampPending = false;

    function isMobile() {
      return window.matchMedia(MOBILE).matches;
    }
    function place(slot, left, top) {
      var frame = slot.parentElement;
      if (!frame || !slot.getClientRects().length || isMobile()) return;
      var frameRect = frame.getBoundingClientRect();
      var slotRect = slot.getBoundingClientRect();
      var maxLeft = Math.max(EDGE, frameRect.width - slotRect.width - EDGE);
      var maxTop = Math.max(EDGE, frameRect.height - slotRect.height - EDGE);
      slot.style.left = Math.max(EDGE, Math.min(maxLeft, left)) + "px";
      slot.style.top = Math.max(EDGE, Math.min(maxTop, top)) + "px";
      slot.style.bottom = "auto";
      slot.dataset.dragged = "true";
    }
    function pin(slot) {
      if (slot.dataset.dragged === "true") return;
      var frame = slot.parentElement;
      var frameRect = frame.getBoundingClientRect();
      var slotRect = slot.getBoundingClientRect();
      place(slot, slotRect.left - frameRect.left, slotRect.top - frameRect.top);
    }
    function clamp(slot) {
      if (!slot || slot.dataset.dragged !== "true") return;
      place(slot, parseFloat(slot.style.left) || EDGE,
        parseFloat(slot.style.top) || EDGE);
    }
    function clampAll() {
      Array.prototype.forEach.call(
        document.querySelectorAll(SLOT + '[data-dragged="true"]'),
        clamp
      );
    }
    function scheduleClamp() {
      if (clampPending) return;
      clampPending = true;
      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(function () {
          clampPending = false;
          clampAll();
        });
      });
    }
    function finish(pointerId) {
      pointerId = typeof pointerId === "number"
        ? pointerId
        : pointerId && pointerId.pointerId;
      if (!drag || (pointerId != null && drag.pointerId !== pointerId)) return;
      if (drag.handle.hasPointerCapture &&
          drag.handle.hasPointerCapture(drag.pointerId)) {
        drag.handle.releasePointerCapture(drag.pointerId);
      }
      drag.slot.classList.remove("is-dragging");
      drag = null;
    }

    document.addEventListener("pointerdown", function (event) {
      var header = event.target.closest && event.target.closest(HEADER);
      if (!header || event.button !== 0 || isMobile()) return;
      var slot = header.closest(SLOT);
      if (!slot || !slot.getClientRects().length) return;
      pin(slot);
      var rect = slot.getBoundingClientRect();
      drag = {
        slot: slot,
        handle: header,
        pointerId: event.pointerId,
        offsetX: event.clientX - rect.left,
        offsetY: event.clientY - rect.top
      };
      if (header.setPointerCapture) header.setPointerCapture(event.pointerId);
      slot.classList.add("is-dragging");
      event.preventDefault();
    });
    document.addEventListener("pointermove", function (event) {
      if (!drag || drag.pointerId !== event.pointerId) return;
      var frameRect = drag.slot.parentElement.getBoundingClientRect();
      place(
        drag.slot,
        event.clientX - frameRect.left - drag.offsetX,
        event.clientY - frameRect.top - drag.offsetY
      );
      event.preventDefault();
    });
    ["pointerup", "pointercancel", "lostpointercapture"].forEach(function (name) {
      document.addEventListener(name, finish);
    });
    window.addEventListener("blur", finish);
    document.addEventListener("keydown", function (event) {
      var handle = event.target.closest && event.target.closest(HANDLE);
      if (!handle || isMobile()) return;
      var moves = {
        ArrowLeft: [-1, 0], ArrowRight: [1, 0],
        ArrowUp: [0, -1], ArrowDown: [0, 1]
      };
      var move = moves[event.key];
      if (!move) return;
      var slot = handle.closest(SLOT);
      pin(slot);
      var step = event.shiftKey ? 1 : 10;
      place(
        slot,
        (parseFloat(slot.style.left) || EDGE) + move[0] * step,
        (parseFloat(slot.style.top) || EDGE) + move[1] * step
      );
      event.preventDefault();
      event.stopPropagation();
    });
    window.addEventListener("resize", scheduleClamp);
    if (window.jQuery) {
      window.jQuery(document).on(
        "shiny:value.cerebroCompositionDrag shown.bs.tab.cerebroCompositionDrag",
        scheduleClamp
      );
    }
  });

  ready(function () {
    var overlay = null;
    var startedAt = 0;
    var elapsedTimer = null;
    var hideTimer = null;
    var revealTimer = null;
    var visibleAt = 0;
    var fadingOut = false;
    var installed = false;
    var completion = null;
    var viewTimer = null;
    // A page that never reports readiness must not leave the workspace inert.
    var VIEW_READY_TIMEOUT_MS = 20000;

    function workspaceBusy(busy) {
      var content = document.getElementById("main-content");
      if (content) {
        content.inert = busy;
        if (busy) content.setAttribute("aria-busy", "true");
        else content.removeAttribute("aria-busy");
      }
    }

    function pageReady() {
      var pane = document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
      return !pane || window.CerebroPageLifecycle.isSettled(pane);
    }

    function ensureOverlay() {
      if (overlay) return overlay;
      overlay = document.createElement("div");
      overlay.id = "cerebro-dataset-loading";
      overlay.className = "cerebro-dataset-loading";
      overlay.hidden = true;
      overlay.innerHTML =
        '<div class="cerebro-dataset-loading-card">' +
        '<div role="status" aria-live="polite" aria-atomic="true">' +
        '<div class="cerebro-dataset-loading-kicker">Loading dataset</div>' +
        '<div class="cerebro-dataset-loading-label"></div>' +
        '<div class="cerebro-dataset-loading-detail"></div>' +
        '</div>' +
        '<div class="cerebro-dataset-loading-track" aria-hidden="true">' +
        '<span></span></div>' +
        '<div class="cerebro-dataset-loading-meta">' +
        '<span class="cerebro-dataset-loading-percent">0%</span>' +
        '<span class="cerebro-dataset-loading-elapsed">0.0 s</span>' +
        '</div></div>';
      document.body.appendChild(overlay);
      return overlay;
    }

    function elapsedText() {
      return ((window.performance.now() - startedAt) / 1000).toFixed(1) + " s";
    }

    function begin(label, detail) {
      var element = ensureOverlay();
      window.clearTimeout(hideTimer);
      if (element.hidden) {
        startedAt = window.performance.now();
        element.hidden = false;
        // Avoid a flash for cached or near-instant loads. The transparent layer
        // still blocks interaction with the outgoing dataset while waiting.
        revealTimer = window.setTimeout(function () {
          revealTimer = null;
          visibleAt = window.performance.now();
          element.classList.add("is-visible");
        }, 150);
      } else if (fadingOut) {
        // A second switch during the exit continues the same visual layer.
        element.classList.add("is-visible");
      }
      fadingOut = false;
      document.body.classList.add("cerebro-dataset-is-loading");
      workspaceBusy(true);
      var labelNode = element.querySelector(".cerebro-dataset-loading-label");
      var detailNode = element.querySelector(".cerebro-dataset-loading-detail");
      // Do not re-announce an unchanged stage on every progress update.
      if (labelNode.textContent !== (label || "Dataset")) labelNode.textContent = label || "Dataset";
      if (detailNode.textContent !== (detail || "Preparing dataset")) detailNode.textContent = detail || "Preparing dataset";
      window.clearInterval(elapsedTimer);
      elapsedTimer = window.setInterval(function () {
        if (!overlay || overlay.hidden) return;
        overlay.querySelector(".cerebro-dataset-loading-elapsed").textContent =
          elapsedText();
      }, 100);
    }

    function update(message) {
      message = message || {};
      var context = window.CerebroDatasetContext;
      if (context && !context.acceptsProgress(message)) return;
      begin(message.label, message.detail);
      var awaitingView = message.done && !message.failed &&
        !message.viewTimedOut && !pageReady();
      // File-load stages are not an end-to-end percentage. Reserve the last
      // part for the painted page instead of displaying 100% while waiting.
      var percent = message.done && !awaitingView ? 100 :
        Math.max(0, Math.min(90, (Number(message.percent) || 0) * 0.9));
      overlay.querySelector(".cerebro-dataset-loading-track span").style.width =
        percent + "%";
      overlay.querySelector(".cerebro-dataset-loading-percent").textContent =
        Math.round(percent) + "%";
      overlay.querySelector(".cerebro-dataset-loading-elapsed").textContent =
        elapsedText();
      if (message.done) {
        // The object being loaded is not yet a painted visualization. Let the
        // existing page readiness events own this boundary (including empties).
        if (awaitingView) {
          completion = message;
          overlay.querySelector(".cerebro-dataset-loading-detail").textContent =
            "Preparing the current view…";
          window.clearTimeout(viewTimer);
          viewTimer = window.setTimeout(function () {
            if (completion !== message) return;
            completion = null;
            update(Object.assign({}, message, { viewTimedOut: true }));
          }, VIEW_READY_TIMEOUT_MS);
          return;
        }
        completion = null;
        window.clearTimeout(viewTimer);
        window.clearInterval(elapsedTimer);
        var hide = function () {
          overlay.hidden = true;
          overlay.classList.remove("is-visible");
          fadingOut = false;
          document.body.classList.remove("cerebro-dataset-is-loading");
          workspaceBusy(false);
        };
        if (revealTimer !== null) {
          window.clearTimeout(revealTimer);
          revealTimer = null;
          hide();
          return;
        }
        // Once shown, give the progress card time to read before fading out.
        hideTimer = window.setTimeout(function () {
          fadingOut = true;
          overlay.classList.remove("is-visible");
          var reducedMotion = window.matchMedia &&
            window.matchMedia("(prefers-reduced-motion: reduce)").matches;
          hideTimer = window.setTimeout(hide, reducedMotion ? 0 : 120);
        }, Math.max(0, 240 - (window.performance.now() - visibleAt)));
      }
    }

    function install() {
      if (installed || typeof window.Shiny === "undefined" ||
          !window.Shiny.addCustomMessageHandler) return;
      installed = true;
      window.Shiny.addCustomMessageHandler("cerebro_dataset_load", update);
    }

    if (window.jQuery) {
      window.jQuery(document).on(
        "shiny:connected.cerebroDatasetProgress",
        install
      );
    }
    window.addEventListener("cerebro:dataset-context", function (event) {
      var detail = event.detail || {};
      if (detail.selectionPending) {
        completion = null;
        var input = document.getElementById("crb_file_selector");
        var selected = input && input.options[input.selectedIndex];
        begin(selected ? selected.textContent : "Dataset", "Updating dataset…");
        overlay.querySelector(".cerebro-dataset-loading-track span").style.width = "0%";
        overlay.querySelector(".cerebro-dataset-loading-percent").textContent = "0%";
      } else if (detail.phase === "error") {
        completion = null;
        // This event has already passed the context/selection freshness gate.
        window.clearTimeout(hideTimer);
        window.clearTimeout(revealTimer);
        window.clearInterval(elapsedTimer);
        revealTimer = null;
        if (overlay) { overlay.hidden = true; overlay.classList.remove("is-visible"); }
        document.body.classList.remove("cerebro-dataset-is-loading");
        workspaceBusy(false);
      }
    });
    function finishPage() {
      if (completion) update(completion);
    }
    window.addEventListener("cerebro:page-ready", finishPage);
    window.addEventListener("cerebro:view-terminal", finishPage);
    document.addEventListener("change", function (event) {
      var input = event.target;
      if (!input || (input.id !== "crb_file_selector" && input.id !== "input_file")) {
        return;
      }
      if (input.id === "input_file" && !(input.files && input.files.length)) return;
      // Selectize emits Shiny input events; beginSelection supplies immediate,
      // deduplicated feedback for keyboard, mouse, and programmatic changes.
      if (input.id === "crb_file_selector") return;
      var selected = input.id === "crb_file_selector" &&
        input.options[input.selectedIndex];
      begin(selected ? selected.textContent : "Dataset", "Waiting for the server");
      overlay.querySelector(".cerebro-dataset-loading-track span").style.width = "0%";
      overlay.querySelector(".cerebro-dataset-loading-percent").textContent = "0%";
    });
    install();
  });
}());
