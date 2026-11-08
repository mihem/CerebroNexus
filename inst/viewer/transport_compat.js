// Browser half of transport_compat.R: receives a binary frame that the server
// sent as base64 text and hands Shiny the frame it would have received over a
// plain WebSocket (type length, type, payload).
(function () {
  'use strict';
  function register() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return false;
    Shiny.addCustomMessageHandler('cerebro_sockjs_binary', function (message) {
      var type = message.type;
      if (typeof type !== 'string' || !type.length || type.length > 255 ||
          /[^\x20-\x7e]/.test(type)) throw new Error('Invalid binary message type');
      var bytes = atob(message.data);
      var frame = new Uint8Array(1 + type.length + bytes.length);
      frame[0] = type.length;
      for (var i = 0; i < type.length; ++i) frame[i + 1] = type.charCodeAt(i);
      for (var j = 0; j < bytes.length; ++j) frame[1 + type.length + j] = bytes.charCodeAt(j);
      return Shiny.shinyapp.dispatchMessage(frame.buffer);
    });
    return true;
  }
  if (!register()) document.addEventListener('DOMContentLoaded', register, {once:true});
})();
