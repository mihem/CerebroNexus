(function () {
  "use strict";

  var MOBILE_QUERY = "(max-width: 767px)";
  function ready(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn, { once: true });
    } else {
      fn();
    }
  }

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
    function finish(element) {
      if (!element || !element.classList) return;
      window.clearTimeout(element.__cerebroWaitTimer);
      element.__cerebroWaitTimer = null;
      element.classList.remove("cerebro-output-waiting");
    }
    window.jQuery(document)
      .on("shiny:outputinvalidated.cerebroMotion", function (event) {
        var element = event.target;
        if (!element || !element.classList ||
            !element.classList.contains("shiny-bound-output")) return;
        if (!element.textContent.trim() && !element.children.length) return;
        finish(element);
        element.__cerebroWaitTimer = window.setTimeout(function () {
          element.classList.add("cerebro-output-waiting");
        }, 120);
      })
      .on("shiny:value.cerebroMotion shiny:error.cerebroMotion", function (event) {
        finish(event.target);
      });
  });
}());
