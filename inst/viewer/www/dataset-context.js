/* Authoritative Viewer dataset-load context and freshness gate. */
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
    var previousKey = key(previous);
    var nextKey = key(next);
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

    var identity = Object.assign(
      previousKey === nextKey ? (window.cerebroSavedViewDataset || {}) : {},
      {
      dataset_id: next.dataset_key,
      dataset_key: next.dataset_key,
      generation: next.generation,
      epoch: next.epoch
      }
    );
    window.cerebroSavedViewDataset = identity;

    var detail = {
      phase: nextPhase,
      previousPhase: previousPhase,
      dataset_context: next,
      previous_context: previous,
      changed: previousKey !== nextKey,
      phaseChanged: previousPhase !== nextPhase,
      identity: identity
    };
    window.dispatchEvent(new CustomEvent('cerebro:dataset-context', { detail: detail }));
    // Older consumers still use this event to refresh identity-dependent UI.
    // It is informational only; freshness decisions must use dataset_context.
    window.dispatchEvent(new CustomEvent('cerebro:dataset-identity', {
      detail: {
        identity: identity,
        changed: detail.changed,
        first: !previousKey,
        dataset_context: next,
        phase: nextPhase
      }
    }));
    return true;
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
      var identity = Object.assign(
        window.cerebroSavedViewDataset || {},
        {
          dataset_id: currentContext.dataset_key,
          dataset_key: currentContext.dataset_key,
          generation: currentContext.generation,
          epoch: currentContext.epoch
        }
      );
      window.cerebroSavedViewDataset = identity;
      var detail = {
        phase: 'pending',
        previousPhase: previousPhase,
        dataset_context: currentContext,
        previous_context: currentContext,
        changed: false,
        phaseChanged: previousPhase !== 'pending',
        identity: identity,
        reconnect: true
      };
      window.dispatchEvent(new CustomEvent(
        'cerebro:dataset-context',
        { detail: detail }
      ));
      window.dispatchEvent(new CustomEvent('cerebro:dataset-identity', {
        detail: {
          identity: identity,
          changed: false,
          first: false,
          dataset_context: currentContext,
          phase: 'pending'
        }
      }));
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
