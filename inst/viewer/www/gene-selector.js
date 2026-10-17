/* Refresh mounted gene searches atomically; late search results cannot restore
   an old selection or inject choices from the previous dataset. */
(function () {
  'use strict';

  function install() {
    if (!window.Shiny || !Shiny.inputBindings) return;
    Shiny.inputBindings.getBindings().forEach(function (entry) {
      var binding = entry.binding;
      if (binding.name !== 'shiny.selectInput' || binding._cerebroGeneChoices) return;
      var receive = binding.receiveMessage;
      binding.receiveMessage = function (element, message) {
        var selectize = element.selectize;
        if (!/^expression_(genes_input|rgb_gene_[rgb])$/.test(element.id) ||
            !selectize || !message.url) {
          return receive.apply(this, arguments);
        }
        var selected = Object.prototype.hasOwnProperty.call(message, 'value')
          ? message.value : selectize.getValue();
        var values = Array.isArray(selected) ? selected.slice() : selected ? [selected] : [];
        var options = values.map(function (value) {
          var option = selectize.options[value];
          if (option) return option;
          option = {};
          option[selectize.settings.valueField] = option[selectize.settings.labelField] = value;
          return option;
        });
        selectize._cerebroGeneChoicesToken = {};
        if (!selectize._cerebroGeneLoad) {
          var load = selectize.load;
          selectize.load = function (fn) {
            var self = this, token = self._cerebroGeneChoicesToken;
            return load.call(self, function (callback) {
              fn.call(self, function (choices) {
                callback(token === self._cerebroGeneChoicesToken ? choices : []);
              });
            });
          };
          selectize._cerebroGeneLoad = true;
        }
        var update = Object.assign({}, message);
        // Shiny reads value again when the asynchronous choices request ends.
        // Use the then-current selection, including a deliberate user clear.
        Object.defineProperty(update, 'value', {
          enumerable: true,
          get: function () { return selectize.getValue(); }
        });
        // Shiny clears synchronously, then starts its asynchronous search. Keep
        // that internal empty value out of both Shiny inputs and feedback UI.
        var clear = selectize.clear, result;
        selectize.clear = function () { return clear.call(this, true); };
        try {
          result = receive.call(this, element, update);
          options.forEach(function (option) { selectize.addOption(option); });
          selectize.setValue(selected, true);
        } finally {
          selectize.clear = clear;
        }
        return result;
      };
      binding._cerebroGeneChoices = true;
    });
  }

  install();
  document.addEventListener('DOMContentLoaded', install);
  if (window.jQuery) window.jQuery(document).on('shiny:connected', install);
}());
