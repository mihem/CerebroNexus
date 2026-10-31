/* Small inline icon registry for the Builder. No external assets are loaded. */
(function () {
  "use strict";

  var icons = {
    "check": "<path d='M3 8.5 6.5 12 13 4.5'/>",
    "warning": "<path d='M8 2 15 14H1L8 2Z'/><path d='M8 6v3.5M8 12h.01'/>",
    "block": "<circle cx='8' cy='8' r='6'/><path d='m4 4 8 8'/>",
    "spinner": "<path d='M14 8a6 6 0 1 1-2-4.5'/>",
    "reload": "<path d='M13 5V2l-2 2a5.5 5.5 0 1 0 2 7'/>",
    "question": "<circle cx='8' cy='8' r='6'/><path d='M6.5 6a1.7 1.7 0 1 1 2.2 1.6C8 8 8 8.5 8 9M8 12h.01'/>",
    "add": "<path d='M8 3v10M3 8h10'/>",
    "remove": "<path d='M3 8h10'/>",
    "duplicate": "<rect x='5' y='5' width='8' height='8' rx='1'/><path d='M3 11H2V2h9v1'/>",
    "up": "<path d='m4 9 4-4 4 4M8 5v8'/>",
    "down": "<path d='m4 7 4 4 4-4M8 3v8'/>",
    "start": "<circle cx='8' cy='8' r='5'/><circle cx='8' cy='8' r='2'/>",
    "open": "<path d='M2 5h5l1 2h6v6H2V5Zm1-2h4l1 2'/>",
    "copy": "<rect x='5' y='5' width='8' height='8' rx='1'/><path d='M3 11H2V2h9v1'/>",
    "build": "<path d='m3 12 6-6 2 2-6 6H3v-2Zm6-6 2-3 2 2-2 3'/>",
    "empty": "<path d='M2 5h4l1.5 2H14v6H2V5Z'/>",
    "error": "<circle cx='8' cy='8' r='6'/><path d='M8 4v5M8 12h.01'/>",
    "recovery": "<path d='M13 5V2l-2 2a5.5 5.5 0 1 0 2 7'/>",
    "expression": "<path d='M2 11c2-7 4 0 6-5s4 2 6-2'/>",
    "groups": "<circle cx='5' cy='6' r='2'/><circle cx='11' cy='6' r='2'/><path d='M2 13c.5-3 5.5-3 6 0M8 13c.5-3 5.5-3 6 0'/>",
    "projections": "<circle cx='4' cy='11' r='1'/><circle cx='8' cy='5' r='1'/><circle cx='12' cy='10' r='1'/><path d='m4 11 4-6 4 5'/>",
    "spatial": "<path d='M8 14s4-4 4-7a4 4 0 1 0-8 0c0 3 4 7 4 7Z'/><circle cx='8' cy='7' r='1'/>",
    "immune": "<path d='M8 2v12M3 5c3 0 3 6 5 6M13 5c-3 0-3 6-5 6'/>",
    "hla": "<path d='M3 3h10v10H3zM3 8h10M8 3v10'/>",
    "trajectory": "<path d='M2 12c3-8 7 1 12-8'/>",
    "trekker": "<path d='M3 13 6 4l3 5 2-6 2 10'/>",
    "table": "<rect x='2' y='3' width='12' height='10' rx='1'/><path d='M2 7h12M6 3v10'/>",
    "plot": "<path d='M2 13V3M2 13h12M4 10l3-3 2 2 4-5'/>",
    "undo": "<path d='M5 5H2v-3M2 5c2-3 8-4 11 1 2 4-1 7-4 7'/>",
    "reveal": "<path d='M1 8s2.5-4 7-4 7 4 7 4-2.5 4-7 4-7-4-7-4Z'/><circle cx='8' cy='8' r='2'/>",
    "loading": "<path d='M14 8a6 6 0 1 1-2-4.5'/>",
    "failure": "<circle cx='8' cy='8' r='6'/><path d='M8 4v5M8 12h.01'/>",
    "blocking": "<circle cx='8' cy='8' r='6'/><path d='m4 4 8 8'/>",
    "attention": "<path d='M8 2 15 14H1L8 2Z'/><path d='M8 6v3.5M8 12h.01'/>",
    "ready": "<path d='M3 8.5 6.5 12 13 4.5'/>",
    "decision": "<circle cx='8' cy='8' r='6'/><path d='M6.5 6a1.7 1.7 0 1 1 2.2 1.6C8 8 8 8.5 8 9M8 12h.01'/>",
    "building": "<path d='m3 12 6-6 2 2-6 6H3v-2Zm6-6 2-3 2 2-2 3'/>"
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
