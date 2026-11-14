/* Authoritative Viewer dataset-load context and freshness gate. */

/* No input reaches the server before Shiny's `init`. Shiny sends `init` (the
   values of every input when the page connected) right after the
   shiny:connected handlers return. Inputs that changed before are held back
   until then, but an input sent with priority "event" from one of those
   handlers sends all of them at once, ahead of `init`, which then sets them
   back on the server: a page opened while the App was connecting stayed
   "Loading" for good, because the server still had Data info as the page.
   Updates sent while the socket is open and `init` may not have gone out yet
   wait for the end of that turn of the event loop. */
(function () {
  'use strict';
  var S = window.Shiny;
  if (!S || typeof S !== 'object') return;
  function holdUntilInit(shinyapp) {
    if (!shinyapp || typeof shinyapp.sendInput !== 'function' ||
        shinyapp.cerebroHoldsUntilInit) return;
    shinyapp.cerebroHoldsUntilInit = true;
    var send = shinyapp.sendInput;
    var initialised = null;
    var held = [];
    shinyapp.sendInput = function (values) {
      var socket = shinyapp.$socket;
      if (!socket || socket.readyState !== 1 || socket === initialised) {
        return send.call(shinyapp, values);
      }
      held.push(values);
      if (held.length > 1) return;
      window.setTimeout(function () {
        initialised = socket;
        var pending = held;
        held = [];
        pending.forEach(function (value) { send.call(shinyapp, value); });
      }, 0);
    };
  }
  // Shiny creates its app object when it initialises, after this file ran.
  var current = S.shinyapp;
  try {
    Object.defineProperty(S, 'shinyapp', {
      configurable: true,
      enumerable: true,
      get: function () { return current; },
      set: function (value) { current = value; holdUntilInit(value); }
    });
  } catch (e) {
    // A Shiny object that cannot take the property keeps Shiny's behaviour.
  }
  holdUntilInit(current);
})();

(function () {
  'use strict';

  var currentContext = null;
  var currentPhase = 'idle';
  var shinyBound = false;
  var acceptNewEpoch = true;
  var retiredEpochs = new Set();
  var connectionSequence = 0;
  var selectedValue = null;
  var pendingSelection = null;
  var selectionSequence = 0;

  function scalar(value) {
    if (value == null || (typeof value !== 'string' && typeof value !== 'number')) {
      return '';
    }
    if (typeof value === 'number' && !isFinite(value)) return '';
    return String(value);
  }

  function normalize(value) {
    if (!value || typeof value !== 'object') return null;
    var epoch = scalar(value.epoch);
    var datasetKey = scalar(value.dataset_key);
    var generation = Number(value.generation);
    if (!epoch || !datasetKey || !Number.isSafeInteger(generation) || generation < 1) {
      return null;
    }
    return Object.freeze({
      epoch: epoch,
      dataset_key: datasetKey,
      generation: generation
    });
  }

  function key(value) {
    var context = normalize(value);
    return context
      ? JSON.stringify([context.epoch, context.dataset_key, context.generation])
      : '';
  }

  function same(left, right) {
    var leftKey = key(left);
    return !!leftKey && leftKey === key(right);
  }

  function fromMessage(message) {
    return normalize(message && message.dataset_context);
  }

  function accepts(message) {
    return currentPhase === 'ready' && same(currentContext, fromMessage(message));
  }

  // Progress may arrive during pending, but never from an earlier selection,
  // including A -> B -> A where the dataset key alone cannot identify a load.
  function acceptsProgress(message) {
    return currentPhase !== 'error' && same(currentContext, fromMessage(message)) &&
      (pendingSelection === null || (message.selection === pendingSelection &&
        message.selection_nonce === selectionSequence));
  }

  function current() {
    return currentContext;
  }

  function phase() {
    return currentPhase;
  }

  // The context only once its load is ready, so callers never act on a pending one.
  function readyContext() {
    return currentPhase === 'ready' ? currentContext : null;
  }

  function readyKey() {
    return key(readyContext());
  }

  function snapshot() {
    return Object.freeze({
      phase: currentPhase,
      dataset_context: currentContext,
      key: key(currentContext)
    });
  }

  function receive(message) {
    var next = normalize(message && message.dataset_context);
    var nextPhase = scalar(message && message.phase).toLowerCase();
    if (!next || ['pending', 'ready', 'error'].indexOf(nextPhase) < 0) return false;
    // A server response for an earlier selector choice cannot reopen the gate.
    if (pendingSelection !== null &&
        (message.selection !== pendingSelection ||
          message.selection_nonce !== selectionSequence)) return false;

    var previous = currentContext;
    var previousPhase = currentPhase;
    if (retiredEpochs.has(next.epoch)) return false;
    if (previous) {
      if (next.epoch !== previous.epoch) {
        if (!acceptNewEpoch) return false;
      } else {
        if (next.generation < previous.generation) return false;
        if (next.generation === previous.generation) {
          if (next.dataset_key !== previous.dataset_key) return false;
          var legalPhase = nextPhase === previousPhase ||
            (previousPhase === 'pending' &&
              (nextPhase === 'ready' || nextPhase === 'error'));
          if (!legalPhase) return false;
        }
      }
    }
    if (previous && next.epoch !== previous.epoch) {
      retiredEpochs.add(previous.epoch);
    }
    currentContext = next;
    currentPhase = nextPhase;
    if (typeof message.selection === 'string') selectedValue = message.selection;
    if (nextPhase !== 'pending') pendingSelection = null;
    acceptNewEpoch = false;
    publish(previous, previousPhase);
    return true;
  }

  // Publishes the current context as the saved-view identity and announces it.
  function publish(previous, previousPhase, extra) {
    var next = currentContext;
    var changed = !same(previous, next);
    var identity = Object.assign(
      changed ? {} : (window.cerebroSavedViewDataset || {}),
      {
        dataset_id: next.dataset_key,
        dataset_key: next.dataset_key,
        generation: next.generation,
        epoch: next.epoch
      }
    );
    window.cerebroSavedViewDataset = identity;
    window.dispatchEvent(new CustomEvent('cerebro:dataset-context', {
      detail: Object.assign({
        phase: currentPhase,
        previousPhase: previousPhase,
        dataset_context: next,
        previous_context: previous,
        changed: changed,
        phaseChanged: previousPhase !== currentPhase,
        identity: identity
      }, extra)
    }));
  }

  function bindShiny() {
    if (shinyBound || !window.Shiny || !Shiny.addCustomMessageHandler) return false;
    shinyBound = true;
    Shiny.addCustomMessageHandler('cerebro_dataset_context', receive);
    return true;
  }

  function onConnected() {
    var selector = document.getElementById &&
      document.getElementById('crb_file_selector');
    selectedValue = selector ? selector.value : null;
    pendingSelection = null;
    // A reconnected browser may be attached to a fresh server session whose
    // generation starts at one. Only this connection boundary authorises an
    // unseen epoch; ordinary late control messages cannot switch epochs. Enter
    // pending until the server explicitly replays its current snapshot, because
    // a resumed session does not otherwise invalidate any reactive observer.
    if (currentContext) acceptNewEpoch = true;
    bindShiny();
    if (currentContext && currentPhase !== 'pending') {
      var previousPhase = currentPhase;
      currentPhase = 'pending';
      publish(currentContext, previousPhase, { reconnect: true });
    }
    if (window.Shiny && Shiny.setInputValue) {
      connectionSequence += 1;
      Shiny.setInputValue('viewer_dataset_context_sync', {
        nonce: connectionSequence,
        known_context: currentContext
      }, { priority: 'event' });
    }
  }

  function beginSelection(value) {
    if (typeof value !== 'string' || !value || value === selectedValue) return;
    selectedValue = value;
    if (!currentContext) return;
    pendingSelection = value;
    selectionSequence += 1;
    var previousPhase = currentPhase;
    currentPhase = 'pending';
    window.dispatchEvent(new CustomEvent('cerebro:dataset-context', { detail: {
      phase: 'pending', previousPhase: previousPhase,
      dataset_context: currentContext, previous_context: currentContext,
      changed: false, phaseChanged: previousPhase !== 'pending',
      selectionPending: true
    } }));
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('viewer_dataset_selection', {
        value: value, nonce: selectionSequence
      });
    }
  }

  window.CerebroDatasetContext = Object.freeze({
    normalize: normalize,
    key: key,
    same: same,
    fromMessage: fromMessage,
    accepts: accepts,
    acceptsProgress: acceptsProgress,
    current: current,
    phase: phase,
    readyContext: readyContext,
    readyKey: readyKey,
    snapshot: snapshot,
    receive: receive,
    beginConnection: onConnected,
    beginSelection: beginSelection
  });

  function onInputChanged(event) {
    if (event.name === 'crb_file_selector') beginSelection(event.value);
  }
  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected', onConnected);
    window.jQuery(document).on('shiny:inputchanged', onInputChanged);
  } else {
    document.addEventListener('shiny:connected', onConnected);
    document.addEventListener('shiny:inputchanged', onInputChanged);
  }
  if (!bindShiny()) window.setTimeout(bindShiny, 0);
})();
