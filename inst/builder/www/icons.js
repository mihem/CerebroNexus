/* Small inline icon registry for the Builder. No external assets are loaded. */
(function () {
  "use strict";

  // Only the icons that a data-icon attribute names.
  var icons = {
    "check": "<path d='M3 8.5 6.5 12 13 4.5'/>",
    "question": "<circle cx='8' cy='8' r='6'/><path d='M6.5 6a1.7 1.7 0 1 1 2.2 1.6C8 8 8 8.5 8 9M8 12h.01'/>",
    "up": "<path d='m4 9 4-4 4 4M8 5v8'/>",
    "down": "<path d='m4 7 4 4 4-4M8 3v8'/>",
    "error": "<circle cx='8' cy='8' r='6'/><path d='M8 4v5M8 12h.01'/>",
    "recovery": "<path d='M13 5V2l-2 2a5.5 5.5 0 1 0 2 7'/>"
  };

  function svg(name) {
    var body = icons[name];
    if (!body) return "";
    return "<svg class='builder-icon' viewBox='0 0 16 16' " +
      "fill='none' stroke='currentColor' stroke-width='1.5' " +
      "stroke-linecap='round' stroke-linejoin='round' aria-hidden='true'>" +
      body + "</svg>";
  }

  function decorate(root) {
    (root || document).querySelectorAll("[data-icon]").forEach(function (node) {
      if (node.dataset.iconReady === "true") return;
      var holder = document.createElement("span");
      holder.className = "builder-icon-slot";
      holder.setAttribute("aria-hidden", "true");
      holder.innerHTML = svg(node.dataset.icon);
      node.insertBefore(holder, node.firstChild);
      node.dataset.iconReady = "true";
    });
  }

  window.BuilderIcons = { icons: icons, svg: svg, decorate: decorate };
})();
