/* Cerebro Dataset Builder: semantic client interaction and accessibility. */
(function () {
  "use strict";

  var tableUploadHighlightTimer = null;
  var narrowManager = window.matchMedia("(max-width: 58rem)");
  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  function syncWorkflowProgressHeight() {
    var progress = document.querySelector(".builder-workflow-progress");
    var topbar = document.querySelector(".topbar");
    var height = progress ? Math.ceil(progress.getBoundingClientRect().height) : 0;
    if (topbar) {
      document.documentElement.style.setProperty(
        "--builder-topbar-height",
        Math.ceil(topbar.getBoundingClientRect().height) + "px"
      );
    }
    document.documentElement.style.setProperty(
      "--builder-workflow-progress-height",
      height + "px"
    );
  }
  window.addEventListener("resize", syncWorkflowProgressHeight);

  var statusTimer = null;
  var lastAnnouncement = "";
  var statusSemanticsAnnounced = false;
  var observedPrimaryAction = null;
  var buildDialogHandlerRegistered = false;
  var viewerGroupHandlerRegistered = false;
  var viewerProjectionHandlerRegistered = false;
  var viewerTrajectoryHandlerRegistered = false;
  var spatialSectionHandlerRegistered = false;
  var desiredSpatialSection = null;
  var spatialSectionGeneration = 0;
  var spatialSectionTimers = [];
  var clientUploadSequence = 0;
  var clientImportQueue = [];
  var clientImportFailures = [];
  var activeClientImport = null;
  var uploadConnectionReady = true;
  var datasetPickerOpening = false;
  var datasetPickerTimer = null;
  var importSyncPending = false;
  var serverImportGate = false;
  var clientImportHandlersRegistered = false;
  var viewerDisclosureState = new Map();
  var managerTransitionSequence = 0;
  var datasetMutationsLocked = false;
  var builderConnectionReady = true;
  var builderWorkerReady = false;
  var builderWorkerStatusTimer = null;
  var datasetStartFocusToken = 0;
  var stageFocusToken = 0;
  var configureActionSequence = 0;
  var datasetSwitchState = {
    target: null,
    authoritative: null,
    generation: 0,
    phase: "idle",
    timeout: null,
    removal: null,
  };
  var coordinateResetSliderIds = new Set([
    "enhance-coordinate_rotation",
    "enhance-point_opacity",
    "enhance-point_size",
  ]);
  var coordinateResetMotionTimers = new Map();
  var dynamicContentEnhancementFrame = null;
  var dynamicContentEnhancementRoots = new Set();
  var spatialScrollbarFrame = null;
  var workflowCompactFrame = null;
  var observedStages = new Set();
  var datasetLoadTimeTimer = null;
  var builderActivityState = {
    phase: "none",
    capabilities: {
      select_dataset: true,
      add_dataset: true,
      edit_dataset: true,
      mutate_datasets: true,
      check_dataset: true,
      create_project: false,
      save_project: false,
      open_project: true,
      navigate_workflow: true,
      build: true,
      page_inert: false,
      warn_before_unload: false,
    },
    busy_title: null,
    busy_message: null,
    busy_detail: null,
    busy_detail_items: null,
    busy_phase: null,
    busy_modal: false,
    has_project: false,
    build_cancelable: false,
    open_cancelable: false,
    warn_before_unload: false,
    page_inert: false,
  };

  function scheduleWorkflowCompactState() {
    if (workflowCompactFrame !== null) return;
    workflowCompactFrame = window.requestAnimationFrame(function () {
      workflowCompactFrame = null;
      var workflow = document.getElementById("workflow_progress");
      var topbar = document.querySelector(".topbar");
      if (!workflow || !topbar) return;
      var threshold = topbar.offsetTop + topbar.offsetHeight;
      var compact = workflow.classList.contains("is-workflow-compact");
      var nextCompact = window.scrollY >= (
        compact ? threshold - 48 : threshold
      );
      if (nextCompact === compact) return;
      workflow.classList.toggle("is-workflow-compact", nextCompact);
      syncWorkflowProgressHeight();
    });
  }
  var builderProjectSaveResult = null;
  var builderProjectSaveResultOpen = false;
  var buildStatusScrollPhase = 0;
  var buildStatusFocusToken = 0;
  var buildOperationActive = false;
  var buildOperationFocusHeld = false;
  var buildProgressState = {
    active: false, text: "", phase: null, anchor: null, timer: null,
  };
  var buildOperationRestoreFocus = null;
  var buildOperationFocusToken = 0;
  var projectOpenOperationActive = false;
  var projectOpenRestoreFocus = null;
  var projectOpenCancelPending = false;
  var buildStopPending = false;
  var normalMotionDuration = 180;
  var authEditor = {
    nextId: 1,
    committed: [],
    snapshot: [],
    open: false,
    saving: false,
    rollbackRequireLogin: false,
  };
  var datasetMessageGenerations = new Map();
  var projectSourceLoadStates = new Map();

  function send(name, value) {
    if (!name || !window.Shiny) return false;
    window.Shiny.setInputValue(name, value, { priority: "event" });
    return true;
  }

  function contentActionOwner(control) {
    if (!control || !control.dataset) return null;
    var ownerControl = typeof control.closest === "function"
      ? control.closest(
        "[data-generation][data-project-epoch][data-owner-token]"
      ) || control
      : control;
    var generation = Number(ownerControl.dataset.generation);
    var projectEpoch = Number(ownerControl.dataset.projectEpoch);
    var ownerToken = ownerControl.dataset.ownerToken;
    if (
      !Number.isFinite(generation) ||
      generation < 0 ||
      generation !== Math.floor(generation) ||
      !Number.isFinite(projectEpoch) ||
      projectEpoch < 0 ||
      projectEpoch !== Math.floor(projectEpoch) ||
      typeof ownerToken !== "string" ||
      !ownerToken
    ) return null;
    return {
      generation: generation,
      project_epoch: projectEpoch,
      owner_token: ownerToken,
    };
  }

  function messageValues(value) {
    if (value === null || typeof value === "undefined") return [];
    return Array.isArray(value) ? value : [value];
  }

  function renderedDatasetId() {
    var rendered = document.querySelector(
      "#builder-workspace .builder-rendered-for-input"
    );
    if (rendered && rendered.value) return rendered.value;
    var selected = document.querySelector(
      "#ds_ready_list .builder-pick[aria-current=true]"
    );
    return selected && selected.dataset.ds ? selected.dataset.ds : null;
  }

  function datasetIdForElement(element) {
    var stage = element && element.closest && element.closest(
      ".builder-stage, .builder-enhancement-stack, #builder-workspace"
    );
    var rendered = stage && stage.querySelector(".builder-rendered-for-input");
    return rendered && rendered.value ? rendered.value : renderedDatasetId();
  }

  function applyProjectSourceLoadState() {
    var dataset = renderedDatasetId();
    if (!dataset) return;
    var loadState = projectSourceLoadStates.get(dataset);
    if (!loadState) return;
    var button = document.getElementById("project_resume_current_source");
    var status = document.getElementById("project-source-load-status");
    if (!button || !status) return;

    var phase = loadState.status || "queued";
    var failed = phase === "error";
    var ready = phase === "ready";
    var label = button.querySelector(".builder-project-source-button-label");
    var title = status.querySelector(".builder-project-source-load-title");
    var detail = status.querySelector(".builder-project-source-load-detail");
    var spinner = status.querySelector(".builder-project-source-load-spinner");
    var stage = status.closest(".builder-project-artifact-stage");
    var footer = stage && stage.querySelector(".builder-stage-footer-status");

    status.hidden = false;
    status.classList.toggle("is-error", failed);
    status.classList.toggle("is-ready", ready);
    if (spinner) spinner.hidden = failed;
    if (stage) stage.setAttribute("data-source-load-state", phase);

    if (failed) {
      button.classList.remove("is-loading");
      button.removeAttribute("aria-busy");
      if (activityCapability("edit_dataset")) {
        button.disabled = false;
        button.setAttribute("aria-disabled", "false");
      }
      if (label) label.textContent = "Retry loading source";
      if (title) title.textContent = "Original source was not loaded";
      if (detail) {
        detail.textContent = (loadState.message ||
          "Try loading the source again.") +
          " The cached copy is unchanged and remains Build-ready.";
      }
      if (footer) footer.textContent = "Source loading stopped. You can retry or continue with the cached copy.";
      return;
    }

    button.disabled = true;
    button.setAttribute("aria-disabled", "true");
    button.setAttribute("aria-busy", "true");
    button.classList.add("is-loading");
    if (ready) {
      if (label) label.textContent = "Opening editor…";
      if (title) title.textContent = "Original source is ready";
      if (detail) detail.textContent = "Opening the editable dataset settings…";
      if (footer) footer.textContent = "Source loaded. Opening the editor…";
      return;
    }
    if (label) label.textContent = phase === "queued" ? "Queueing source…" : "Loading source…";
    if (title) {
      title.textContent = phase === "queued"
        ? "Source request received"
        : "Loading original source";
    }
    if (detail) {
      detail.textContent = phase === "queued"
        ? "Waiting for the background worker. The cached copy remains ready for Build."
        : "Large files can take a while. You can switch datasets; the editor will open here when ready.";
    }
    if (footer) footer.textContent = "Loading the original source in the background…";
  }

  function beginProjectSourceLoad(button) {
    var dataset = datasetIdForElement(button);
    if (!dataset) return;
    projectSourceLoadStates.set(dataset, {
      status: "queued",
      message: null,
    });
    applyProjectSourceLoadState();
    scheduleStatusAnnouncement("Original source request received.");
  }

  function updateProjectSourceLoadState(message) {
    if (!message || typeof message.dataset !== "string" || !message.dataset) return;
    var phase = message.status;
    if (["queued", "loading", "ready", "error"].indexOf(phase) < 0) return;
    projectSourceLoadStates.set(message.dataset, {
      status: phase,
      message: typeof message.message === "string" ? message.message : null,
    });
    applyProjectSourceLoadState();
    if (message.dataset !== renderedDatasetId()) return;
    if (phase === "error") {
      scheduleStatusAnnouncement("Original source was not loaded. The cached copy remains ready for Build.");
    } else if (phase === "ready") {
      scheduleStatusAnnouncement("Original source loaded. Opening the editor.");
    }
  }

  function datasetMessageGenerationIsFresh(message) {
    if (!message || typeof message.dataset !== "string" || !message.dataset) {
      return false;
    }
    var generation = Number(message.generation);
    if (!Number.isFinite(generation)) return false;
    var previous = datasetMessageGenerations.get(message.dataset);
    if (Number.isFinite(previous) && generation < previous) return false;
    datasetMessageGenerations.set(
      message.dataset,
      Number.isFinite(previous) ? Math.max(previous, generation) : generation
    );
    return true;
  }

  function datasetMessageIsCurrent(message) {
    if (!message || typeof message.dataset !== "string" || !message.dataset) {
      return false;
    }
    if (
      datasetSwitchState.target &&
      message.dataset !== datasetSwitchState.target
    ) return false;
    var rendered = renderedDatasetId();
    if (!rendered || message.dataset !== rendered) return false;
    return datasetMessageGenerationIsFresh(message);
  }

  function resetDatasetMessageGenerationsForActivity(message) {
    if (
      message && message.phase === "opening" &&
      builderActivityState.phase !== "opening"
    ) {
      // Entry revisions restart when another saved project replaces the
      // workspace. Do not let a same-named dataset inherit the prior
      // project's generation floor.
      datasetMessageGenerations.clear();
      projectSourceLoadStates.clear();
    }
  }

  function applyDatasetActionOwner(message) {
    if (
      !message ||
      typeof message.dataset !== "string" ||
      message.dataset !== renderedDatasetId() ||
      typeof message.owner_token !== "string" ||
      !message.owner_token
    ) return;
    var generation = Number(message.generation);
    var projectEpoch = Number(message.project_epoch);
    if (
      !Number.isFinite(generation) ||
      generation < 0 ||
      generation !== Math.floor(generation) ||
      !Number.isFinite(projectEpoch) ||
      projectEpoch < 0 ||
      projectEpoch !== Math.floor(projectEpoch)
    ) return;
    document.querySelectorAll(
      "[data-generation][data-project-epoch][data-owner-token]"
    ).forEach(function (control) {
      if (control.dataset.ownerToken !== message.owner_token) return;
      var currentProjectEpoch = Number(control.dataset.projectEpoch);
      var currentGeneration = Number(control.dataset.generation);
      if (
        !Number.isFinite(currentProjectEpoch) ||
        projectEpoch < currentProjectEpoch ||
        (projectEpoch === currentProjectEpoch &&
        Number.isFinite(currentGeneration) &&
        generation < currentGeneration
        )
      ) return;
      control.dataset.generation = String(generation);
      control.dataset.projectEpoch = String(projectEpoch);
    });
  }

  function flushPendingTextEdits() {
    if (!window.Shiny) return;
    document.querySelectorAll(
      "#builder-workspace input.shiny-bound-input[type=text]" +
        "[data-builder-dirty=true], " +
        "#builder-workspace textarea.shiny-bound-input" +
        "[data-builder-dirty=true]"
    ).forEach(function (control) {
      if (!control.id || control.disabled || control.readOnly) return;
      window.Shiny.setInputValue(control.id, control.value, {
        priority: "event",
      });
      delete control.dataset.builderDirty;
    });
  }

  function shouldFlushPendingTextEdits(target) {
    if (!target || typeof target.closest !== "function") return false;
    return Boolean(target.closest(
      ".builder-pick, .builder-workflow-stage-link, " +
        "#complete_dataset_check, #continue_to_review, " +
        "#back_to_settings, #confirm_review, #back_to_review, #build, " +
        "#save_builder_project, #open_builder_project"
    ));
  }

  function legacyCopyText(text) {
    var active = document.activeElement;
    var textarea = document.createElement("textarea");
    textarea.value = text;
    textarea.setAttribute("readonly", "");
    textarea.className = "visually-hidden";
    document.body.appendChild(textarea);
    textarea.select();
    var copied = false;
    try {
      copied = document.execCommand("copy");
    } catch (error) {
      copied = false;
    }
    textarea.remove();
    if (canRestoreFocus(active)) active.focus({ preventScroll: true });
    return copied;
  }

  function copyTextToClipboard(message) {
    var value = message && typeof message.text === "string" ? message.text : "";
    if (!value) return;
    function announce(copied) {
      scheduleStatusAnnouncement(
        copied ? "Copied to clipboard." : "Could not copy. Select and copy the value manually."
      );
    }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(value).then(function () {
        announce(true);
      }).catch(function () {
        announce(legacyCopyText(value));
      });
      return;
    }
    announce(legacyCopyText(value));
  }

  function matchingDynamicElements(roots, selector) {
    var matches = new Set();
    (roots && roots.length ? roots : [document]).forEach(function (root) {
      if (root.nodeType === 1 && root.matches(selector)) matches.add(root);
      root.querySelectorAll(selector).forEach(function (node) {
        matches.add(node);
      });
    });
    return Array.from(matches);
  }

  function matchingDynamicContainers(roots, selector) {
    var matches = new Set(matchingDynamicElements(roots, selector));
    (roots && roots.length ? roots : [document]).forEach(function (root) {
      if (root.nodeType !== 1) return;
      var container = root.closest(selector);
      if (container) matches.add(container);
    });
    return Array.from(matches);
  }

  function datasetSwitchCopy() {
    if (datasetSwitchState.phase === "spatial") {
      return "Preparing Spatial preview…";
    }
    if (datasetSwitchState.phase === "slow") {
      return "This is taking longer than expected…";
    }
    return "Switching dataset…";
  }

  function ensureDatasetSwitchVeil() {
    if (!datasetSwitchState.target) return null;
    var workbench = document.getElementById("workbench");
    var pane = document.getElementById("pane");
    if (!workbench || !pane) return null;
    window.clearTimeout(datasetSwitchState.removal);
    datasetSwitchState.removal = null;
    workbench.setAttribute("aria-busy", "true");
    workbench.inert = true;
    var veil = pane.querySelector(":scope > .builder-dataset-switch-veil");
    if (!veil) {
      veil = document.createElement("div");
      veil.className = "builder-dataset-switch-veil";
      veil.setAttribute("role", "status");
      veil.setAttribute("aria-live", "polite");
      var status = document.createElement("div");
      status.className = "builder-dataset-switch-status";
      var spinner = document.createElement("span");
      spinner.className = "spinner";
      spinner.setAttribute("aria-hidden", "true");
      var text = document.createElement("span");
      text.className = "builder-dataset-switch-copy";
      status.append(spinner, text);
      veil.appendChild(status);
      pane.appendChild(veil);
    }
    veil.classList.remove("is-leaving");
    var text = veil.querySelector(".builder-dataset-switch-copy");
    if (text) text.textContent = datasetSwitchCopy();
    return veil;
  }

  function optimisticallySelectDataset(target) {
    document.querySelectorAll("#ds_ready_list .ds[data-ds]").forEach(
      function (row) {
        var selected = row.dataset.ds === target;
        var control = row.querySelector(".builder-pick");
        row.classList.toggle("is-active", selected);
        if (!control) return;
        if (selected) control.setAttribute("aria-current", "true");
        else control.removeAttribute("aria-current");
      }
    );
  }

  function beginDatasetSwitch(target) {
    if (typeof target !== "string" || !target) return false;
    var selected = document.querySelector(
      "#ds_ready_list .builder-pick[aria-current=true]"
    );
    if (
      !datasetSwitchState.target &&
      selected &&
      selected.dataset.ds === target
    ) return false;

    flushPendingTextEdits();
    if (!datasetSwitchState.target) {
      var selectedId = selected ? selected.dataset.ds : null;
      datasetSwitchState.authoritative = selectedId;
    }
    datasetSwitchState.generation += 1;
    datasetSwitchState.target = target;
    datasetSwitchState.phase = "switching";
    window.clearTimeout(datasetSwitchState.timeout);
    var generation = datasetSwitchState.generation;
    optimisticallySelectDataset(target);
    ensureDatasetSwitchVeil();
    datasetSwitchState.timeout = window.setTimeout(function () {
      if (
        datasetSwitchState.target === target &&
        datasetSwitchState.generation === generation
      ) {
        datasetSwitchState.phase = "slow";
        ensureDatasetSwitchVeil();
        datasetSwitchState.timeout = window.setTimeout(function () {
          if (
            datasetSwitchState.target === target &&
            datasetSwitchState.generation === generation
          ) {
            scheduleStatusAnnouncement(
              "The dataset switch did not finish. Try selecting it again."
            );
            settleDatasetSwitch("error");
          }
        }, 22000);
      }
    }, 8000);
    return true;
  }

  function settleDatasetSwitch(outcome) {
    window.clearTimeout(datasetSwitchState.timeout);
    datasetSwitchState.timeout = null;
    if (outcome === "ready") {
      datasetSwitchState.authoritative = datasetSwitchState.target;
    } else if (outcome === "error") {
      optimisticallySelectDataset(datasetSwitchState.authoritative);
    }
    datasetSwitchState.target = null;
    datasetSwitchState.phase = "idle";
    var workbench = document.getElementById("workbench");
    var pane = document.getElementById("pane");
    if (workbench) {
      workbench.removeAttribute("aria-busy");
      workbench.inert = false;
    }
    if (!pane) return;
    var veil = pane.querySelector(":scope > .builder-dataset-switch-veil");
    if (!veil) return;
    window.clearTimeout(datasetSwitchState.removal);
    veil.classList.add("is-leaving");
    datasetSwitchState.removal = window.setTimeout(function () {
      if (veil.isConnected) veil.remove();
      datasetSwitchState.removal = null;
    }, reducedMotion.matches ? 0 : normalMotionDuration);
  }

  function updateDatasetSwitchPhase(message) {
    if (
      !message ||
      message.dataset !== datasetSwitchState.target ||
      Number(message.switch_token) !== datasetSwitchState.generation
    ) return;
    if (message.state === "spatial") {
      datasetSwitchState.phase = "spatial";
      ensureDatasetSwitchVeil();
      return;
    }
    if (message.state === "ready") settleDatasetSwitch("ready");
    if (message.state === "error") settleDatasetSwitch("error");
  }

  function builderOperationElements() {
    var overlay = document.getElementById("builder-operation-overlay");
    return {
      overlay: overlay,
      card: overlay && overlay.querySelector(".builder-operation-overlay-card"),
      title: document.getElementById("builder-operation-overlay-title"),
      message: document.getElementById("builder-operation-overlay-message"),
      detail: document.getElementById("builder-operation-overlay-detail"),
      progress: document.getElementById("builder-build-progress"),
      progressSummary: document.getElementById("builder-build-progress-summary"),
      actions: document.getElementById("builder-operation-overlay-actions"),
    };
  }

  function updateBuildOperationProgress(active, phase) {
    var elements = builderOperationElements();
    if (!elements.progress) return;
    var phases = [
      "preflight",
      "stage",
      "dispatch",
      "datasets",
      "viewer",
      "verify",
      "publish",
    ];
    var activeIndex = phases.indexOf(phase);
    elements.progress.hidden = !active || activeIndex < 0;
    if (elements.progressSummary) {
      elements.progressSummary.hidden = !active || activeIndex < 0;
      elements.progressSummary.textContent = activeIndex < 0
        ? ""
        : "Step " + (activeIndex + 1) + " of " + phases.length;
    }
    elements.progress.querySelectorAll("[data-build-phase]").forEach(function (step) {
      var index = phases.indexOf(step.dataset.buildPhase);
      var current = index === activeIndex;
      step.classList.toggle("is-complete", index < activeIndex);
      step.classList.toggle("is-current", current);
      if (current) step.setAttribute("aria-current", "step");
      else step.removeAttribute("aria-current");
    });
  }

  function setBuilderOperationCopy(title, message, detail) {
    var elements = builderOperationElements();
    if (elements.title) elements.title.textContent = title || "";
    if (elements.message) elements.message.textContent = message || "";
    if (elements.detail) elements.detail.textContent = detail || "";
  }

  function setBuilderOperationActions(buttons) {
    var elements = builderOperationElements();
    if (!elements.actions || !elements.card) return;
    // Progress/heartbeat updates must not replace a focused or pressed button.
    var previous = elements.actions._builderActionSpecs;
    if (previous && previous.length === buttons.length && buttons.every(function (spec, index) {
      return spec.label === previous[index].label &&
        spec.primary === previous[index].primary && spec.action === previous[index].action;
    })) return;
    elements.actions._builderActionSpecs = buttons.slice();
    elements.actions.replaceChildren();
    buttons.forEach(function (specification) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = specification.primary ? "btn btn-primary" : "btn";
      button.textContent = specification.label;
      button.addEventListener("click", specification.action);
      elements.actions.appendChild(button);
    });
    var hasActions = buttons.length > 0;
    elements.card.classList.toggle("has-actions", hasActions);
    elements.actions.setAttribute("aria-hidden", hasActions ? "false" : "true");
  }

  function cancelBuilderProjectOpen() {
    if (builderActivityState.open_cancelable !== true) return;
    projectOpenCancelPending = true;
    var actions = builderOperationElements().actions;
    var button = actions && actions.querySelector("button");
    if (button) {
      button.disabled = true;
      button.setAttribute("aria-busy", "true");
      button.textContent = "Cancelling…";
    }
    var sent = send("cancel_builder_project_open", { nonce: Date.now() });
    if (!sent) {
      projectOpenCancelPending = false;
      if (button) {
        button.disabled = false;
        button.removeAttribute("aria-busy");
        button.textContent = "Cancel";
      }
    }
  }

  function useBuilderPickerPath() {
    send("builder_picker_alternative", { action: "path", nonce: Date.now() });
  }

  function cancelBuilderPicker() {
    send("builder_picker_alternative", { action: "cancel", nonce: Date.now() });
  }

  function stopBuilderRelease() {
    if (builderActivityState.build_cancelable !== true) return;
    buildStopPending = true;
    var actions = builderOperationElements().actions;
    var button = actions && actions.querySelector("button");
    if (button) {
      button.disabled = true;
      button.setAttribute("aria-busy", "true");
      button.textContent = "Stopping…";
    }
    var sent = send("stop_builder_release", { nonce: Date.now() });
    if (!sent) {
      buildStopPending = false;
      if (button) {
        button.disabled = false;
        button.removeAttribute("aria-busy");
        button.textContent = "Stop";
      }
    }
  }

  function closeBuilderProjectSaveResult() {
    builderProjectSaveBackground = false;
    builderProjectSaveResultOpen = false;
    builderProjectSaveResult = null;
    document.body.classList.remove("builder-project-result-open");
    var elements = builderOperationElements();
    if (elements.overlay) {
      elements.overlay.removeEventListener("keydown", trapDialogKeydown);
      elements.overlay.setAttribute("role", "status");
      elements.overlay.removeAttribute("aria-modal");
    }
    if (elements.card) {
      elements.card.classList.remove("is-result", "is-success", "is-error", "has-actions");
    }
    setBuilderOperationActions([]);
    applyBuilderActivityState();
    updateDialogLock();
    if (elements.overlay) restoreFocus(elements.overlay);
  }

  function showBuilderProjectSaveCompletion(status) {
    if (!builderProjectSaveResultOpen || !builderProjectSaveResult) return;
    var elements = builderOperationElements();
    if (!elements.card) return;
    var checked = Number(builderProjectSaveResult.checked || 0);
    var reusable = Number(builderProjectSaveResult.reusable || 0);
    var remaining = Math.max(0, checked - reusable);
    if (status !== "failed") {
      var message = "Project saved." + (remaining > 0 ?
        " " + remaining + " dataset cache" + (remaining === 1 ? " is" : "s are") +
        " preparing. Keep this page open." :
        "");
      closeBuilderProjectSaveResult();
      document.querySelectorAll(".builder-display-status").forEach(function (status) {
        status.textContent = "Viewer appearance saved.";
      });
      // A brief flash beside the project status, not a pop-up.
      showStatusFlash({ type: "success", text: message, hide_after: 5 });
      return;
    }
    elements.card.classList.add("is-result");
    elements.card.classList.remove("is-success");
    elements.card.classList.add("is-error");
    setBuilderOperationCopy(
      "Project save failed",
      "Some source files could not be saved.",
      "Your previous save is kept. Retry to finish saving."
    );
    var buttons = [{ label: "Done", action: closeBuilderProjectSaveResult }];
    if (status === "failed") {
      buttons.push({
        label: "Retry save",
        primary: true,
        action: function () {
          closeBuilderProjectSaveResult();
          send("save_builder_project", { nonce: Date.now() });
        },
      });
    }
    setBuilderOperationActions(buttons);
    var firstButton = elements.actions && elements.actions.querySelector("button");
    if (firstButton) firstButton.focus({ preventScroll: true });
  }

  function updateBuilderProjectSourceProgress(message) {
    if (builderProjectSaveBackground) {
      if (message && (message.status === "ready" || message.status === "failed")) {
        builderProjectSaveBackground = false;
        if (message.status === "failed") {
          openBuilderProjectSaveResult();
        } else {
          builderProjectSaveResultOpen = true;
        }
        showBuilderProjectSaveCompletion(message.status);
      }
      return;
    }
    if (!builderProjectSaveResultOpen || !builderProjectSaveResult) return;
    var status = message && message.status;
    if (status === "ready" || status === "failed") {
      showBuilderProjectSaveCompletion(status);
      return;
    }
    if (status === "cancelling") {
      var stoppingElements = builderOperationElements();
      if (stoppingElements.card) {
        stoppingElements.card.classList.add("is-result");
        stoppingElements.card.classList.remove("is-success", "is-error", "has-actions");
      }
      setBuilderOperationActions([]);
      setBuilderOperationCopy(
        "Stopping source copy",
        "Waiting for the current copy to stop…",
        (message && message.error) || "Keep this page open."
      );
      return;
    }
    if (status !== "syncing") return;
    var completed = Number(message.completed || 0);
    var total = Number(message.total || 0);
    var bytesDone = Number(message.bytes_done || 0);
    var bytesTotal = Number(message.bytes_total || 0);
    var elements = builderOperationElements();
    if (elements.card) {
      elements.card.classList.add("is-result");
      elements.card.classList.remove("is-success", "is-error", "has-actions");
    }
    setBuilderOperationActions([]);
    var detail = "Keep this page open.";
    if (bytesTotal > 0) {
      detail = formatBuilderProjectBytes(Math.min(bytesDone, bytesTotal)) +
        " of " + formatBuilderProjectBytes(bytesTotal) + " saved";
      if (total > 1) detail += " · file " + Math.min(completed + 1, total) + " of " + total;
    } else if (total > 0) {
      detail = completed + " of " + total + " files saved";
    }
    setBuilderOperationCopy(
      "Saving dataset sources",
      "Keep this page open until saving finishes.",
      detail
    );
  }

  function formatBuilderProjectBytes(bytes) {
    var units = ["bytes", "KB", "MB", "GB", "TB"];
    var value = Math.max(0, Number(bytes) || 0);
    var unit = 0;
    while (value >= 1024 && unit < units.length - 1) {
      value /= 1024;
      unit += 1;
    }
    return (unit === 0 ? Math.round(value) : value.toFixed(1)) + " " + units[unit];
  }

  // ---- In-place feedback (builder_notify in notify.R) ----------------------
  // Messages go where they belong: a field, a section, a dataset row, the
  // Build card, the project status, or one notice bar under the stepper.
  // Contextual notices are re-placed when Shiny re-renders their section and
  // dropped once their section is gone; a target that never appears falls
  // back to the notice bar.
  var builderNotices = [];
  var builderNoticeObserver = null;
  var builderNoticePlaceTimer = null;
  var builderNoticeGlyphs = { success: "✓", info: "i", warning: "!", error: "!" };

  function noticePlainText(message) {
    if (typeof message.text === "string") return message.text;
    var scratch = document.createElement("div");
    scratch.innerHTML = message.html || "";
    return (scratch.textContent || "").replace(/\s+/g, " ").trim();
  }

  function noticeBind(node) {
    if (window.Shiny && typeof window.Shiny.bindAll === "function") {
      window.Shiny.bindAll(node);
    }
  }

  function noticeRemoveNode(node) {
    if (!node || !node.isConnected) return;
    if (window.Shiny && typeof window.Shiny.unbindAll === "function") {
      window.Shiny.unbindAll(node);
    }
    node.classList.add("is-leaving");
    window.setTimeout(function () { node.remove(); }, reducedMotion.matches ? 0 : 180);
  }

  function buildNotice(message, variant) {
    var node = document.createElement("div");
    node.className = "builder-notice builder-notice--" + variant + " is-" + message.type;
    node.setAttribute("role", message.type === "error" ? "alert" : "status");
    node.setAttribute("aria-live", message.type === "error" ? "assertive" : "polite");
    if (message.id) node.dataset.noticeId = message.id;
    var icon = document.createElement("span");
    icon.className = "builder-notice-icon";
    icon.setAttribute("aria-hidden", "true");
    icon.textContent = builderNoticeGlyphs[message.type] || "i";
    var text = document.createElement("div");
    text.className = "builder-notice-text";
    if (typeof message.html === "string") text.innerHTML = message.html;
    else text.textContent = noticePlainText(message);
    node.appendChild(icon);
    node.appendChild(text);
    if (message.action && message.action.label && message.action.click) {
      var action = document.createElement("button");
      action.type = "button";
      action.className = "builder-notice-action";
      action.textContent = message.action.label;
      action.addEventListener("click", function () {
        var trigger = document.getElementById(message.action.click);
        if (trigger) trigger.click();
        dismissNotice(message);
      });
      node.appendChild(action);
    }
    var dismiss = document.createElement("button");
    dismiss.type = "button";
    dismiss.className = "builder-notice-dismiss";
    dismiss.setAttribute("aria-label", "Dismiss");
    dismiss.textContent = "×";
    dismiss.addEventListener("click", function () { dismissNotice(message); });
    node.appendChild(dismiss);
    return node;
  }

  function dismissNotice(message) {
    builderNotices = builderNotices.filter(function (item) { return item !== message; });
    window.clearTimeout(message._hideTimer);
    window.clearTimeout(message._fallbackTimer);
    (message._nodes || []).forEach(noticeRemoveNode);
    message._nodes = [];
    if (message._field) clearFieldNote(message._field);
  }

  function clearNoticesById(id) {
    if (!id) return;
    builderNotices.filter(function (item) { return item.id === id; }).forEach(dismissNotice);
    document.querySelectorAll('[data-notice-id="' + CSS.escape(id) + '"]').forEach(noticeRemoveNode);
  }

  function scheduleNoticeHide(message) {
    window.clearTimeout(message._hideTimer);
    var seconds = Number(message.hide_after);
    if (!(seconds > 0)) return;
    message._hideTimer = window.setTimeout(function () { dismissNotice(message); }, seconds * 1000);
  }

  function showGlobalNotice(message) {
    var bar = document.getElementById("builder-notice-bar");
    if (!bar) return;
    // The same message twice reads as one notice, refreshed.
    builderNotices
      .filter(function (item) {
        return item._kind === "global" && item.html === message.html && item.type === message.type;
      })
      .forEach(dismissNotice);
    message._kind = "global";
    var node = buildNotice(message, "bar");
    bar.appendChild(node);
    noticeBind(node);
    message._nodes = [node];
    builderNotices.push(message);
    scheduleNoticeHide(message);
  }

  function showStatusFlash(message) {
    // A Save with nothing to write answers only with this flash, without a
    // new activity state; the click's "Preparing save…" ends here too.
    var pendingFeedback = document.getElementById("builder-project-pending-feedback");
    if (pendingFeedback) pendingFeedback.remove();
    var toolbar = document.querySelector(".builder-project-toolbar");
    if (!toolbar) return showGlobalNotice(message);
    var prior = toolbar.querySelector(".builder-status-flash");
    if (prior) prior.remove();
    var text = noticePlainText(message);
    var flash = document.createElement("span");
    flash.className = "builder-status-flash is-" + message.type;
    flash.setAttribute("role", "status");
    flash.title = text;
    flash.textContent = (message.type === "success" ? "✓ " : "") + text;
    var status = document.getElementById("project_status");
    if (status && status.parentNode === toolbar) status.insertAdjacentElement("afterend", flash);
    else toolbar.insertBefore(flash, toolbar.firstChild);
    var seconds = Number(message.hide_after) > 0 ? Number(message.hide_after) : 4;
    window.setTimeout(function () { noticeRemoveNode(flash); }, seconds * 1000);
  }

  function clearFieldNote(field) {
    if (!field) return;
    field.classList.remove("is-invalid");
    field.removeAttribute("aria-invalid");
    var note = document.getElementById(field.id + "-builder-note");
    if (note) note.remove();
    var described = (field.getAttribute("aria-describedby") || "")
      .split(/\s+/)
      .filter(function (token) { return token && token !== field.id + "-builder-note"; });
    if (described.length) field.setAttribute("aria-describedby", described.join(" "));
    else field.removeAttribute("aria-describedby");
  }

  function showFieldNote(message, field) {
    clearFieldNote(field);
    var isInput = field.matches("input, select, textarea");
    var problem = message.type === "error" || message.type === "warning";
    var note = document.createElement("p");
    note.id = field.id + "-builder-note";
    note.className = "builder-field-note is-" + message.type;
    note.setAttribute("role", message.type === "error" ? "alert" : "status");
    if (typeof message.html === "string") note.innerHTML = message.html;
    else note.textContent = noticePlainText(message);
    var anchor = field.closest(".shiny-input-container") || field;
    anchor.insertAdjacentElement("afterend", note);
    noticeBind(note);
    if (isInput) {
      field.setAttribute(
        "aria-describedby",
        ((field.getAttribute("aria-describedby") || "") + " " + note.id).trim()
      );
      if (problem) {
        field.classList.add("is-invalid");
        field.setAttribute("aria-invalid", "true");
      }
      // Editing the value is the answer to the message.
      field.addEventListener("input", function () { dismissNotice(message); }, { once: true });
    }
    message._kind = "field";
    message._field = field;
    message._nodes = [note];
    builderNotices.push(message);
    scheduleNoticeHide(message);
  }

  function noticeSectionPlace(message) {
    var target = message.target;
    if (target === "build") {
      var status = document.getElementById("build-stage-status");
      return status ? { anchor: status, where: "beforebegin" } : null;
    }
    var selectors = [];
    if (target === "review") selectors = ['[data-workflow-stage="review"]'];
    else if (target && target.selector) selectors = [].concat(target.selector);
    for (var index = 0; index < selectors.length; index += 1) {
      var section = document.querySelector(selectors[index]);
      if (!section) continue;
      // A message inside a closed disclosure would never be seen.
      var disclosure = section.closest("details");
      if (disclosure && !disclosure.open) disclosure.open = true;
      var header = section.querySelector(
        ":scope > .builder-stage-header, :scope > [class$='-header']"
      );
      return header
        ? { anchor: header, where: "afterend" }
        : { anchor: section, where: "afterbegin" };
    }
    return null;
  }

  function noticeDatasetPlaces(message) {
    var id = message.target.dataset;
    var places = [];
    var row = document.querySelector(
      '.rail .ds[data-ds="' + CSS.escape(id) + '"], .rail .ds[data-import-id="' + CSS.escape(id) + '"]'
    );
    var body = row && row.querySelector(".ds-body");
    if (body) places.push({ anchor: body, where: "beforeend", variant: "row" });
    var active = document.querySelector(".rail .ds.is-active");
    var activeId = active && (active.dataset.ds || active.dataset.importId);
    if (activeId === id) {
      var stage = document.querySelector(".builder-stage-configure, .builder-project-artifact-stage");
      var header = stage && stage.querySelector(":scope > .builder-stage-header");
      if (header) places.push({ anchor: header, where: "afterend", variant: "inline" });
    }
    return places;
  }

  function placeContextualNotices() {
    builderNoticePlaceTimer = null;
    builderNotices.slice().forEach(function (message) {
      if (message._kind === "section") {
        var live = (message._nodes || []).filter(function (node) { return node.isConnected; });
        if (live.length) return;
        var place = noticeSectionPlace(message);
        if (!place) {
          // Navigated away from the section it was about.
          if (message._placed) dismissNotice(message);
          return;
        }
        var node = buildNotice(message, "inline");
        place.anchor.insertAdjacentElement(place.where, node);
        noticeBind(node);
        message._nodes = [node];
        if (!message._placed) {
          message._placed = true;
          window.clearTimeout(message._fallbackTimer);
          scheduleNoticeHide(message);
        }
      } else if (message._kind === "dataset") {
        message._nodes = (message._nodes || []).filter(function (node) { return node.isConnected; });
        noticeDatasetPlaces(message).forEach(function (place) {
          var present = message._nodes.some(function (node) {
            return place.anchor.contains(node) || place.anchor.nextElementSibling === node;
          });
          if (present) return;
          var node = buildNotice(message, place.variant);
          place.anchor.insertAdjacentElement(place.where, node);
          noticeBind(node);
          message._nodes.push(node);
        });
        if (!message._placed && message._nodes.length) {
          message._placed = true;
          window.clearTimeout(message._fallbackTimer);
          scheduleNoticeHide(message);
        }
      }
    });
  }

  function scheduleNoticePlacement() {
    if (builderNoticePlaceTimer) return;
    builderNoticePlaceTimer = window.setTimeout(placeContextualNotices, 60);
  }

  function watchNoticeTargets() {
    if (builderNoticeObserver || typeof MutationObserver !== "function") return;
    builderNoticeObserver = new MutationObserver(function (records) {
      var relevant = records.some(function (record) {
        return !(record.target.closest && record.target.closest(".builder-notice, #builder-notice-bar"));
      });
      if (relevant && builderNotices.some(function (item) {
        return item._kind === "section" || item._kind === "dataset";
      })) {
        scheduleNoticePlacement();
      }
    });
    builderNoticeObserver.observe(document.body, { childList: true, subtree: true });
  }

  function showContextualNotice(message, kind) {
    message._kind = kind;
    message._nodes = [];
    builderNotices.push(message);
    watchNoticeTargets();
    placeContextualNotices();
    if (!message._placed) {
      // The section may still be rendering; give it a moment, then fall back.
      message._fallbackTimer = window.setTimeout(function () {
        if (message._placed) return;
        dismissNotice(message);
        showGlobalNotice(Object.assign({}, message, { _nodes: [], _kind: null }));
      }, 1500);
    }
  }

  function showDatasetFlash(message) {
    var id = message.target.dataset;
    var row = document.querySelector('.rail .ds[data-ds="' + CSS.escape(id) + '"]');
    var body = row && row.querySelector(".ds-body");
    if (!body) return showStatusFlash(message);
    var prior = body.querySelector(".ds-flash");
    if (prior) prior.remove();
    var flash = document.createElement("span");
    flash.className = "ds-flash is-" + message.type;
    flash.setAttribute("role", "status");
    flash.textContent = "✓ " + noticePlainText(message);
    body.appendChild(flash);
    var seconds = Number(message.hide_after) > 0 ? Number(message.hide_after) : 4;
    window.setTimeout(function () { noticeRemoveNode(flash); }, seconds * 1000);
  }

  function builderNotify(message) {
    if (!message || typeof message !== "object") return;
    if (message.clear) {
      clearNoticesById(message.clear);
      return;
    }
    if (!builderNoticeGlyphs[message.type]) message.type = "info";
    if (message.type === "error") {
      // A failed action ends its "Preparing…"/"Opening…" feedback too.
      var failedFeedback = document.getElementById("builder-project-pending-feedback");
      if (failedFeedback) failedFeedback.remove();
    }
    clearNoticesById(message.id);
    var target = message.target;
    if (target === "status") {
      if (message.type === "success" || message.type === "info") showStatusFlash(message);
      else showGlobalNotice(message);
      return;
    }
    if (target && target.field) {
      var ids = [].concat(target.field);
      for (var index = 0; index < ids.length; index += 1) {
        var field = document.getElementById(ids[index]);
        if (field && field.getClientRects().length) {
          showFieldNote(message, field);
          return;
        }
      }
      showGlobalNotice(message);
      return;
    }
    if (target && target.dataset) {
      if (message.type === "success") showDatasetFlash(message);
      else showContextualNotice(message, "dataset");
      return;
    }
    if (target === "build" || target === "review" || (target && target.selector)) {
      showContextualNotice(message, "section");
      return;
    }
    showGlobalNotice(message);
  }

  var builderProjectSaveBackground = false;
  function showBuilderProjectSaveResult(message) {
    builderProjectSaveResult = message || {};
    builderProjectSaveBackground = message && message.source_syncing === true;
    if (builderProjectSaveBackground) return;
    openBuilderProjectSaveResult();
    showBuilderProjectSaveCompletion("ready");
  }

  function openBuilderProjectSaveResult() {
    builderProjectSaveResultOpen = true;
    document.body.classList.add("builder-project-result-open");
    var elements = builderOperationElements();
    if (elements.overlay) {
      elements.overlay.setAttribute("aria-hidden", "false");
      elements.overlay.removeEventListener("keydown", trapDialogKeydown);
      prepareDialog(
        elements.overlay,
        document.getElementById("save_builder_project"),
        function () {
          if (elements.card && elements.card.classList.contains("has-actions")) {
            closeBuilderProjectSaveResult();
          }
        },
        function () {
          return document.getElementById("save_builder_project");
        }
      );
    }
    if (elements.card) elements.card.classList.add("is-result");
    var shell = document.querySelector(".builder-shell");
    if (shell) shell.inert = true;
  }

  function applyDatasetMutationLock(roots) {
    var activityLocked = !builderConnectionReady || !builderWorkerReady ||
      builderActivityState.capabilities.mutate_datasets === false;
    var selectors = [
      "#dataset_files",
      ".builder-add-datasets",
      ".enhance-table-add-button",
      ".enhance-tissue-file-button",
      ".builder-file-trigger",
      ".example-btn",
      ".builder-reorder",
      ".builder-drop",
      ".builder-retry-import",
      ".builder-remove-import",
      "#undo_remove",
      "#undo_remove_notice",
    ].join(", ");
    matchingDynamicElements(roots, selectors).forEach(function (control) {
      var restoreControl = control.matches(
        ".builder-retry-import, .builder-remove-import"
      );
      var retryQueueLocked =
        control.matches(".builder-retry-import") && clientImportQueue.length > 0;
      var pickerLocked = datasetPickerOpening && control.matches(".builder-add-datasets");
      // The first row cannot move up and the last cannot move down; R marks
      // those buttons and this lock must not re-enable them.
      var edgeLocked = control.matches(".builder-reorder") &&
        control.getAttribute("data-at-edge") === "true";
      var controlLocked = datasetMutationsLocked ||
        (activityLocked && !restoreControl) || retryQueueLocked || pickerLocked ||
        edgeLocked;
      if ("disabled" in control) control.disabled = controlLocked;
      control.setAttribute(
        "aria-disabled",
        controlLocked ? "true" : "false"
      );
      control.classList.toggle(
        "is-dataset-mutation-locked",
        controlLocked
      );
      if (controlLocked) control.setAttribute("tabindex", "-1");
      else if (control.classList.contains("builder-file-trigger")) {
        control.setAttribute("tabindex", "0");
      } else {
        control.removeAttribute("tabindex");
      }
    });
  }

  function activityCapability(name) {
    var importSensitive = [
      "save_project",
      "open_project",
      "build",
    ].indexOf(name) >= 0;
    var workerSensitive = [
      "add_dataset",
      "edit_dataset",
      "mutate_datasets",
      "check_dataset",
      "open_project",
      "navigate_workflow",
      "build",
    ].indexOf(name) >= 0;
    return builderConnectionReady &&
      (!workerSensitive || builderWorkerReady) &&
      (!importSensitive || clientImportQueue.length === 0) &&
      builderActivityState.capabilities[name] !== false;
  }

  function updateBuilderWorkerStatus(message) {
    if (!message || typeof message !== "object") return;
    var state = message.state || "starting";
    var status = document.getElementById("builder-worker-status");
    var title = document.getElementById("builder-worker-status-title");
    var detail = document.getElementById("builder-worker-status-detail");
    builderWorkerReady = state === "ready";
    if (status) {
      if (builderWorkerStatusTimer !== null) {
        window.clearTimeout(builderWorkerStatusTimer);
        builderWorkerStatusTimer = null;
      }
      status.classList.remove("is-dismissed");
      status.setAttribute("aria-hidden", "false");
      status.classList.toggle("is-starting", state === "starting");
      status.classList.toggle("is-ready", state === "ready");
      status.classList.toggle("is-error", state === "error");
      status.setAttribute("data-worker-state", state);
      if (state === "ready") {
        builderWorkerStatusTimer = window.setTimeout(function () {
          status.classList.add("is-dismissed");
          status.setAttribute("aria-hidden", "true");
          builderWorkerStatusTimer = null;
        }, 1500);
      }
    }
    if (title && message.title) title.textContent = message.title;
    if (detail) {
      detail.textContent = message.detail || "";
      detail.hidden = state === "ready" || !message.detail;
    }
    applyDatasetMutationLock();
    applyBuilderActivityState();
    if (message.title) scheduleStatusAnnouncement(message.title);
  }

  function setActivityDisabled(control, disabled) {
    if (!control || !("disabled" in control)) return;
    // Selectize owns its generated text input. Locking that input separately
    // records the parent's temporary disabled state as a permanent one, then
    // disables it again immediately after selectize.enable() on Save/Open.
    if (control.closest(".selectize-control")) return;
    if (disabled) {
      if (!control.hasAttribute("data-builder-activity-lock")) {
        control.setAttribute(
          "data-builder-activity-was-disabled",
          control.disabled ? "true" : "false"
        );
      }
      control.setAttribute("data-builder-activity-lock", "true");
      control.disabled = true;
      if (control.selectize) control.selectize.disable();
      control.setAttribute("aria-disabled", "true");
      return;
    }
    if (!control.hasAttribute("data-builder-activity-lock")) return;
    var wasDisabled = control.getAttribute(
      "data-builder-activity-was-disabled"
    ) === "true";
    control.removeAttribute("data-builder-activity-lock");
    control.removeAttribute("data-builder-activity-was-disabled");
    control.disabled = wasDisabled;
    if (control.selectize && !wasDisabled) control.selectize.enable();
    control.setAttribute("aria-disabled", wasDisabled ? "true" : "false");
  }

  function renderBuilderOperationDetail(node, text, items) {
    var parts = Array.isArray(items)
      ? items.filter(function (item) {
        return typeof item === "string" && item.length > 0;
      })
      : [];
    if (!parts.length) {
      node.classList.remove("is-metric-list");
      node.removeAttribute("aria-label");
      if (node.textContent !== text) node.textContent = text;
      return;
    }
    node.textContent = "";
    node.classList.add("is-metric-list");
    node.setAttribute("aria-label", text);
    parts.forEach(function (part, index) {
      var metric = document.createElement("span");
      metric.className = index === 0
        ? "builder-operation-detail-progress"
        : "builder-operation-detail-metric";
      metric.textContent = part;
      node.appendChild(metric);
    });
  }

  // Build waits for Save, autosave and dataset-cache registration. Say why
  // beside the Build actions instead of leaving a dead button.
  function applyBuildBlockedReason() {
    var action = document.getElementById("build");
    var actions = action && action.closest(".builder-stage-footer-actions");
    var reason = builderActivityState.build_reason;
    var blocked = Boolean(actions) && builderConnectionReady &&
      !activityCapability("build") &&
      typeof reason === "string" && reason.length > 0;
    document.querySelectorAll(".builder-build-blocked-reason").forEach(function (node) {
      if (!blocked || node.parentElement !== actions) node.remove();
    });
    if (!blocked) return;
    var note = actions.querySelector(":scope > .builder-build-blocked-reason");
    if (!note) {
      note = document.createElement("span");
      note.className = "builder-build-blocked-reason";
      note.setAttribute("role", "status");
      actions.prepend(note);
    }
    if (note.textContent !== reason) note.textContent = reason;
  }

  // Workspace controls follow edit_dataset, but one with its own capability
  // (Build, Build again) must not be unlocked by it: dataset-cache
  // registration keeps the workspace editable while Build waits.
  function workspaceControlLocked(control, projectControls, workspaceLocked) {
    if (workspaceLocked) return true;
    return Object.prototype.hasOwnProperty.call(projectControls, control.id) &&
      !projectControls[control.id];
  }

  function applyBuilderActivityState(roots) {
    var capabilities = builderActivityState.capabilities || {};
    var projectControls = {
      open_builder_project: activityCapability("open_project"),
      save_builder_project: activityCapability("save_project") ||
        activityCapability("create_project"),
      confirm_builder_project_open: activityCapability("open_project") ||
        (builderActivityState.has_project === true &&
          activityCapability("edit_dataset")),
      choose_saved_project_datasets: activityCapability("edit_dataset"),
      project_resume_current_source: activityCapability("edit_dataset"),
      complete_dataset_check: activityCapability("check_dataset"),
      continue_to_review: activityCapability("navigate_workflow"),
      back_to_settings: activityCapability("navigate_workflow"),
      confirm_review: activityCapability("navigate_workflow"),
      back_to_review: activityCapability("navigate_workflow"),
      build: activityCapability("build"),
      build_same_folder: activityCapability("build"),
    };
    Object.keys(projectControls).forEach(function (id) {
      setActivityDisabled(
        document.getElementById(id),
        !projectControls[id]
      );
    });
    applyBuildBlockedReason();
    matchingDynamicElements(roots, ".builder-pick").forEach(function (control) {
      setActivityDisabled(control, !activityCapability("select_dataset"));
    });

    var workspaceLocked = !activityCapability("edit_dataset");
    matchingDynamicElements(
      roots,
      "#builder-workspace input, #builder-workspace select, " +
      "#builder-workspace textarea, #builder-workspace button"
    ).forEach(function (control) {
      setActivityDisabled(
        control,
        workspaceControlLocked(control, projectControls, workspaceLocked)
      );
    });
    matchingDynamicElements(roots, ".builder-workflow-stage-link").forEach(function (link) {
      var locked = !activityCapability("navigate_workflow");
      link.classList.toggle("is-activity-locked", locked);
      link.setAttribute("aria-disabled", locked ? "true" : "false");
      if (locked) link.setAttribute("tabindex", "-1");
      else link.removeAttribute("tabindex");
    });

    var pageInert = builderConnectionReady &&
      builderActivityState.page_inert === true;
    var restoring = builderConnectionReady && builderActivityState.phase === "restoring";
    var restoreStatus = document.getElementById("builder-restore-status");
    if (restoring && !restoreStatus) {
      restoreStatus = document.createElement("div");
      restoreStatus.id = "builder-restore-status";
      restoreStatus.className = "builder-restore-status";
      var restoreCopy = document.createElement("span");
      restoreCopy.setAttribute("role", "status");
      var cancelRestore = document.createElement("button");
      cancelRestore.type = "button";
      cancelRestore.className = "btn";
      cancelRestore.textContent = "Cancel restoration";
      cancelRestore.addEventListener("click", function () {
        cancelRestore.disabled = true;
        cancelRestore.textContent = "Cancelling…";
        send("cancel_builder_project_restore_loads", { nonce: Date.now() });
      });
      restoreStatus.append(restoreCopy, cancelRestore);
      var restoreShell = document.querySelector(".builder-shell");
      if (restoreShell) restoreShell.before(restoreStatus);
    }
    if (restoreStatus) {
      if (!restoring) restoreStatus.remove();
      else restoreStatus.firstChild.textContent = [
        builderActivityState.busy_title,
        builderActivityState.busy_message,
        builderActivityState.busy_detail
      ].filter(Boolean).join(" · ");
    }
    // A system picker opened for Build does not make the page inert, but its
    // overlay is the only place offering "Enter path" and "Cancel" while the
    // picker window may be hidden behind the browser.
    var pickerWaiting = builderConnectionReady &&
      builderActivityState.picker_waiting === true;
    var operationOverlayOpen = (pageInert || pickerWaiting) &&
      builderActivityState.busy_modal !== true;
    // The Build progress panel does not depend on page_inert: when the
    // server stops making the page inert during a Build, the same panel
    // stays up as a non-modal status card and the workspace stays usable.
    var nextBuildOperationActive = builderConnectionReady &&
      builderActivityState.busy_title === "Building output";
    var buildPanelNonModal = nextBuildOperationActive && !pageInert &&
      !builderProjectSaveResultOpen;
    var nextProjectOpenOperationActive = operationOverlayOpen &&
      (builderActivityState.phase === "opening" || builderActivityState.phase === "restoring") &&
      !builderProjectSaveResultOpen;
    var projectOpenPriorFocus = !projectOpenOperationActive &&
      nextProjectOpenOperationActive
      ? document.activeElement
      : null;
    var overlay = document.getElementById("builder-operation-overlay");
    document.body.classList.toggle("builder-page-inert", pageInert);
    if (overlay) {
      var overlayOpen = operationOverlayOpen || builderProjectSaveResultOpen;
      var modalOpen = overlayOpen && !buildPanelNonModal;
      overlay.setAttribute(
        "aria-hidden",
        overlayOpen || buildPanelNonModal ? "false" : "true"
      );
      overlay.classList.toggle("is-nonmodal", buildPanelNonModal);
      overlay.setAttribute("role", modalOpen ? "dialog" : "status");
      if (modalOpen) overlay.setAttribute("aria-modal", "true");
      else overlay.removeAttribute("aria-modal");
    }
    if (nextBuildOperationActive && !buildOperationFocusHeld && !buildPanelNonModal) {
      buildOperationFocusHeld = true;
      beginBuildOperationFocus(overlay);
    }
    var shell = document.querySelector(".builder-shell");
    if (shell) shell.inert = pageInert || pickerWaiting || builderProjectSaveResultOpen;
    document.body.classList.toggle(
      "builder-connection-lost",
      !builderConnectionReady
    );
    var title = document.getElementById("builder-operation-overlay-title");
    var message = document.getElementById("builder-operation-overlay-message");
    var detail = document.getElementById("builder-operation-overlay-detail");
    if (
      title &&
      !builderProjectSaveResultOpen &&
      builderActivityState.busy_title &&
      title.textContent !== builderActivityState.busy_title
    ) {
      title.textContent = builderActivityState.busy_title;
    }
    if (
      message &&
      !builderProjectSaveResultOpen &&
      builderActivityState.busy_message &&
      message.textContent !== builderActivityState.busy_message
    ) {
      message.textContent = builderActivityState.busy_message;
    }
    if (detail && !builderProjectSaveResultOpen) {
      var busyDetail = builderActivityState.busy_detail || "";
      renderBuilderOperationDetail(
        detail,
        busyDetail,
        builderActivityState.busy_detail_items
      );
    }
    if (!builderProjectSaveResultOpen) {
      updateBuildOperationProgress(
        nextBuildOperationActive,
        builderActivityState.busy_phase
      );
    }
    renderBuildProgressDetail();
    if (!builderProjectSaveResultOpen) {
      if (nextProjectOpenOperationActive && !projectOpenOperationActive) {
        projectOpenCancelPending = false;
      }
      var operationActions = [];
      if (builderActivityState.picker_waiting === true) {
        operationActions.push({ label: "Enter path", action: useBuilderPickerPath });
        operationActions.push({ label: "Cancel", action: cancelBuilderPicker });
      } else if (builderActivityState.build_cancelable === true) {
        operationActions.push({ label: "Stop", action: stopBuilderRelease });
      } else if (builderActivityState.open_cancelable === true) {
        operationActions.push({ label: "Cancel", action: cancelBuilderProjectOpen });
      }
      setBuilderOperationActions(operationActions);
      if (nextProjectOpenOperationActive && projectOpenCancelPending) {
        var pendingCancel = builderOperationElements().actions;
        pendingCancel = pendingCancel && pendingCancel.querySelector("button");
        if (pendingCancel) {
          pendingCancel.disabled = true;
          pendingCancel.setAttribute("aria-busy", "true");
          pendingCancel.textContent = "Cancelling…";
        }
      }
      if (nextBuildOperationActive && buildStopPending) {
        var pendingStop = builderOperationElements().actions;
        pendingStop = pendingStop && pendingStop.querySelector("button");
        if (pendingStop) {
          pendingStop.disabled = true;
          pendingStop.setAttribute("aria-busy", "true");
          pendingStop.textContent = "Stopping…";
        }
      }
    }
    if (nextProjectOpenOperationActive && !projectOpenOperationActive) {
      projectOpenRestoreFocus = canRestoreFocus(projectOpenPriorFocus)
        ? projectOpenPriorFocus
        : document.getElementById("open_builder_project");
      var cancelOpen = builderOperationElements().actions;
      cancelOpen = cancelOpen && cancelOpen.querySelector("button");
      if (cancelOpen) cancelOpen.focus({ preventScroll: true });
    } else if (
      projectOpenOperationActive &&
      !nextProjectOpenOperationActive &&
      !builderProjectSaveResultOpen
    ) {
      var openRestoreTarget = projectOpenRestoreFocus;
      projectOpenRestoreFocus = null;
      projectOpenCancelPending = false;
      window.setTimeout(function () {
        if (document.querySelector('.modal.show, [aria-modal="true"]')) return;
        if (canRestoreFocus(openRestoreTarget)) {
          openRestoreTarget.focus({ preventScroll: true });
        }
      }, 100);
    }
    projectOpenOperationActive = nextProjectOpenOperationActive;
    var priorBuildOperationActive = buildOperationActive;
    buildOperationActive = nextBuildOperationActive;
    setBuildProgressClock(buildOperationActive);
    if (!buildOperationActive) buildStopPending = false;
    if (priorBuildOperationActive && !buildOperationActive) {
      // A non-modal panel never took focus, so it gives nothing back.
      if (buildOperationFocusHeld) restoreBuildOperationFocus();
      buildOperationFocusHeld = false;
    }
    applyDatasetMutationLock(roots);
    applyProjectSourceLoadState();
  }

  function formatBuildElapsed(milliseconds) {
    var total = Math.max(0, Math.floor(milliseconds / 1000));
    var hours = Math.floor(total / 3600);
    var minutes = Math.floor((total % 3600) / 60);
    var seconds = total % 60;
    function pad(value) { return (value < 10 ? "0" : "") + value; }
    return (hours ? hours + ":" + pad(minutes) : String(minutes)) +
      ":" + pad(seconds) + " elapsed";
  }

  // "Dataset i/N · name · step" from the server plus a clock the browser runs
  // itself, so the panel moves every second without a server round trip.
  function renderBuildProgressDetail() {
    var running = buildProgressState.active && buildProgressState.anchor !== null;
    var elapsed = running
      ? formatBuildElapsed(Date.now() - buildProgressState.anchor)
      : "";
    // The step header and this detail line arrive in separate messages. When
    // the header has already moved on (Writing CRB -> Packaging Viewer), the
    // previous step's dataset line is stale; show the clock alone until the
    // matching detail arrives.
    var headerPhase = builderActivityState.busy_phase;
    var stale = typeof headerPhase === "string" && headerPhase.length > 0 &&
      typeof buildProgressState.phase === "string" &&
      buildProgressState.phase !== headerPhase;
    var text = buildProgressState.active && !stale ? buildProgressState.text : "";
    document.querySelectorAll("[data-build-progress-detail]").forEach(function (node) {
      var label = node.querySelector(".builder-build-progress-text");
      var clock = node.querySelector(".builder-build-progress-elapsed");
      node.hidden = !(text || elapsed);
      if (label && label.textContent !== text) label.textContent = text;
      if (clock && clock.textContent !== elapsed) clock.textContent = elapsed;
    });
  }

  function setBuildProgressClock(active) {
    active = Boolean(active);
    if (active && buildProgressState.anchor === null) {
      buildProgressState.anchor = Date.now();
    }
    if (!active) {
      buildProgressState.anchor = null;
      buildProgressState.text = "";
      buildProgressState.phase = null;
    }
    buildProgressState.active = active;
    if (active && buildProgressState.timer === null) {
      buildProgressState.timer = window.setInterval(renderBuildProgressDetail, 1000);
    } else if (!active && buildProgressState.timer !== null) {
      window.clearInterval(buildProgressState.timer);
      buildProgressState.timer = null;
    }
    renderBuildProgressDetail();
  }

  function handleBuildProgress(message) {
    if (!message || message.active !== true) {
      buildProgressState.text = "";
      buildProgressState.phase = null;
      renderBuildProgressDetail();
      return;
    }
    buildProgressState.text = typeof message.text === "string" ? message.text : "";
    buildProgressState.phase = typeof message.phase === "string" ? message.phase : null;
    var elapsed = Number(message.elapsed_seconds);
    if (Number.isFinite(elapsed) && elapsed >= 0) {
      // After a reload the worker's start time is earlier than this page's.
      var implied = Date.now() - elapsed * 1000;
      if (buildProgressState.anchor === null || implied < buildProgressState.anchor) {
        buildProgressState.anchor = implied;
      }
    }
    renderBuildProgressDetail();
  }

  function beginBuildOperationFocus(overlay) {
    buildOperationFocusToken += 1;
    var active = document.activeElement;
    if (
      !buildOperationRestoreFocus &&
      canRestoreFocus(active) &&
      active !== overlay
    ) {
      buildOperationRestoreFocus = active;
    } else if (!buildOperationRestoreFocus) {
      buildOperationRestoreFocus = document.getElementById("build");
    }
    if (overlay) overlay.focus({ preventScroll: true });
  }

  function restoreBuildOperationFocus() {
    buildOperationFocusToken += 1;
    var token = buildOperationFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== buildOperationFocusToken) return;
      var heading = document.querySelector(".result-card h3");
      var action = document.querySelector(
        ".result-card .builder-result-actions button, " +
          ".result-card .builder-recovery-action button"
      );
      var target = heading || action;
      if (heading) heading.setAttribute("tabindex", "-1");
      if (!canRestoreFocus(target)) target = buildOperationRestoreFocus;
      if (!canRestoreFocus(target)) target = document.getElementById("build");
      if (canRestoreFocus(target)) {
        target.focus({ preventScroll: true });
        buildOperationRestoreFocus = null;
        return;
      }
      attempts += 1;
      if (attempts >= 12) {
        buildOperationRestoreFocus = null;
        return;
      }
      window.setTimeout(apply, 50);
    }
    window.setTimeout(apply, 0);
  }

  function focusBuildStatus() {
    var host = document.getElementById("build-stage-status");
    if (!host || !host.isConnected) {
      return false;
    }
    var topbar = document.querySelector(".topbar");
    var topbarBottom = topbar ? topbar.getBoundingClientRect().bottom : 0;
    host.style.scrollMarginTop = Math.max(0, topbarBottom + 12) + "px";
    host.setAttribute("tabindex", "-1");
    host.scrollIntoView({
      block: "start",
      behavior: reducedMotion.matches ? "auto" : "smooth",
    });
    host.focus({ preventScroll: true });
    return true;
  }

  function scheduleBuildStatusFocus() {
    buildStatusFocusToken += 1;
    var token = buildStatusFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== buildStatusFocusToken) return;
      if (focusBuildStatus()) return;
      attempts += 1;
      if (attempts < 12) window.setTimeout(apply, 50);
    }
    apply();
  }

  function showImmediateBuildStatus() {
    var host = document.getElementById("build-stage-status");
    var output = document.getElementById("build_stage_status_content");
    if (!host || !output) return;
    buildOperationRestoreFocus = document.activeElement ||
      document.getElementById("build");

    var previous = host.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    if (previous) previous.remove();

    var section = document.createElement("section");
    section.className = [
      "builder-stage-section",
      "builder-build-status-section",
      "is-client-build-status",
    ].join(" ");
    var heading = document.createElement("h3");
    heading.textContent = "Build status";
    var waiting = document.createElement("div");
    waiting.className = "builder-build-waiting";
    var spinner = document.createElement("span");
    spinner.className = "spinner";
    spinner.setAttribute("aria-hidden", "true");
    var label = document.createElement("span");
    var statusText = "Preparing build…";
    label.textContent = statusText;
    waiting.append(spinner, label);
    section.append(heading, waiting);
    host.insertBefore(section, output);

    buildStatusScrollPhase = 1;
    scheduleBuildStatusFocus();
    buildStatusScrollPhase = 2;
    scheduleStatusAnnouncement(statusText);
  }

  function authCopy(accounts) {
    return accounts.map(function (account) {
      return { id: account.id, username: account.username, password: account.password };
    });
  }

  function authAccountRow(account) {
    var row = document.createElement("div");
    row.className = "builder-auth-row";
    row.dataset.authId = account.id;
    var safeId = String(account.id || authEditor.nextId)
      .replace(/[^a-zA-Z0-9_-]/g, "-");
    [["Username", "text", "builder-auth-username", "username", account.username],
      ["Password", "password", "builder-auth-password", "new-password", account.password]
    ].forEach(function (spec) {
      var label = document.createElement("label");
      label.textContent = spec[0];
      var input = document.createElement("input");
      input.type = spec[1];
      input.className = spec[2];
      input.id = "builder-auth-" + safeId + "-" + spec[3];
      input.name = spec[3] + "-" + safeId;
      input.autocomplete = spec[3];
      if (spec[3] === "username") input.spellcheck = false;
      if (spec[3] === "new-password") {
        input.setAttribute("aria-describedby", "builder-auth-password-note");
      }
      input.value = spec[4];
      label.htmlFor = input.id;
      label.appendChild(input);
      row.appendChild(label);
    });
    var remove = document.createElement("button");
    remove.type = "button";
    remove.className = "btn builder-auth-remove";
    remove.setAttribute(
      "aria-label",
      "Remove account" + (account.username ? " " + account.username : "")
    );
    remove.title = "Remove account";
    remove.textContent = "×";
    row.appendChild(remove);
    return row;
  }

  function authRows() {
    return Array.from(document.querySelectorAll(".builder-auth-row")).map(function (row) {
      return {
        id: row.dataset.authId,
        username: row.querySelector(".builder-auth-username").value,
        password: row.querySelector(".builder-auth-password").value,
      };
    });
  }

  function authRender(accounts) {
    var root = document.querySelector("[data-auth-rows]");
    if (!root) return;
    root.replaceChildren();
    accounts.forEach(function (account) { root.appendChild(authAccountRow(account)); });
  }

  function authNewAccount() {
    return { id: "auth-account-" + authEditor.nextId++, username: "", password: "" };
  }

  function clearAuthLiveInputs() {
    document.querySelectorAll(".builder-auth-row input").forEach(function (input) {
      input.value = "";
    });
  }

  function clearAuthSecrets() {
    clearAuthLiveInputs();
    authEditor.committed = [];
    authEditor.snapshot = [];
  }

  function clearAuthError() {
    var error = document.getElementById("builder-auth-error");
    if (!error) return;
    error.textContent = "";
    error.hidden = true;
    document.querySelectorAll(".builder-auth-row input").forEach(function (input) {
      input.removeAttribute("aria-invalid");
      var descriptions = (input.getAttribute("aria-describedby") || "")
        .split(/\s+/)
        .filter(function (id) { return id && id !== "builder-auth-error"; });
      if (descriptions.length) input.setAttribute("aria-describedby", descriptions.join(" "));
      else input.removeAttribute("aria-describedby");
    });
  }

  function markAuthInvalid(input) {
    if (!input) return;
    input.setAttribute("aria-invalid", "true");
    var descriptions = (input.getAttribute("aria-describedby") || "")
      .split(/\s+/)
      .filter(Boolean);
    if (!descriptions.includes("builder-auth-error")) {
      descriptions.push("builder-auth-error");
    }
    input.setAttribute("aria-describedby", descriptions.join(" "));
  }

  function restoreAuthSnapshot() {
    authEditor.committed = authCopy(authEditor.snapshot);
    authRender(authEditor.snapshot);
  }

  function rollbackRequireLogin() {
    var shouldRollback = authEditor.rollbackRequireLogin;
    authEditor.rollbackRequireLogin = false;
    if (!shouldRollback) return;
    var input = document.getElementById("build_require_login");
    if (!input || !input.checked) return;
    input.checked = false;
    input.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function cancelAuthDialog() {
    if (authEditor.saving) return;
    restoreAuthSnapshot();
    rollbackRequireLogin();
    closeAuthDialog(true);
  }

  function authOpenFocusFallback() {
    var trigger = document.querySelector(".builder-auth-open");
    var options = trigger && trigger.closest("details");
    if (options) options.open = true;
    return trigger;
  }

  function setAuthSaving(pendingNonce) {
    var saving = typeof pendingNonce === "number" && Number.isFinite(pendingNonce);
    authEditor.saving = saving ? pendingNonce : false;
    var dialog = document.getElementById("builder-auth-dialog");
    if (!dialog) return;
    dialog.querySelectorAll("input, button").forEach(function (control) {
      control.disabled = saving;
    });
  }

  function closeAuthDialog(restore) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    authEditor.open = false;
    dialog.removeEventListener("keydown", trapDialogKeydown);
    backdrop.classList.remove("is-visible");
    dialog.classList.remove("is-visible");
    backdrop.hidden = true;
    document.body.classList.remove("builder-dialog-open");
    if (restore) restoreFocus(dialog);
  }

  function openAuthDialog(trigger, rollbackRequireLoginOnCancel) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    clearAuthError();
    setAuthSaving(false);
    authEditor.snapshot = authCopy(authEditor.committed);
    if (!authEditor.committed.length) authRender([authNewAccount()]);
    else authRender(authEditor.committed);
    authEditor.open = true;
    authEditor.rollbackRequireLogin = Boolean(rollbackRequireLoginOnCancel);
    backdrop.hidden = false;
    prepareDialog(
      dialog,
      trigger,
      cancelAuthDialog,
      authOpenFocusFallback
    );
    backdrop.onclick = function (event) {
      if (event.target === backdrop) cancelAuthDialog();
    };
    showTransientLayer(backdrop, dialog);
    window.setTimeout(function () {
      var username = dialog.querySelector(".builder-auth-username");
      if (authEditor.open && username) username.focus({ preventScroll: true });
    }, 0);
  }

  function focusableElements(root) {
    return Array.from(
      root.querySelectorAll(
        "button:not([disabled]), [href], input:not([disabled]), " +
          "select:not([disabled]), textarea:not([disabled]), " +
          '[tabindex]:not([tabindex="-1"])'
      )
    ).filter(function (element) {
      return element.getClientRects().length > 0;
    });
  }

  function canRestoreFocus(target) {
    return Boolean(
      target &&
      document.contains(target) &&
      !target.disabled &&
      target.getClientRects().length > 0
    );
  }

  function restoreFocus(dialog) {
    var target = dialog && dialog.__builderRestoreFocus;
    var fallback = dialog && dialog.__builderRestoreFocusFallback;
    var restoreToken = dialog ?
      (dialog.__builderFocusRestoreToken || 0) + 1 : 0;
    if (dialog) dialog.__builderFocusRestoreToken = restoreToken;
    var lastTarget = null;
    var attempts = 0;
    function attempt() {
      if (dialog && dialog.__builderFocusRestoreToken !== restoreToken) return;
      var otherModal = Array.from(document.querySelectorAll('[aria-modal="true"]'))
        .find(function (candidate) {
          return candidate !== dialog && !candidate.closest("[hidden]") &&
            candidate.getClientRects().length > 0;
        });
      var active = document.activeElement;
      var focusWasLost = !active || active === document.body ||
        active === document.documentElement ||
        (dialog && dialog.contains(active)) || active === lastTarget;
      // Do not steal focus if the user has already moved it elsewhere while a
      // Shiny redraw is settling. Re-check the intended target briefly because
      // a redraw can replace the button after the first successful focus,
      // leaving document.body active again.
      if (!focusWasLost) return;
      var nextTarget = canRestoreFocus(target) ? target : null;
      if (nextTarget && otherModal && !otherModal.contains(nextTarget)) {
        nextTarget = null;
      }
      if (!nextTarget && typeof fallback === "function") {
        nextTarget = fallback();
        if (nextTarget && otherModal && !otherModal.contains(nextTarget)) {
          nextTarget = null;
        }
      }
      if (canRestoreFocus(nextTarget)) {
        nextTarget.focus();
        lastTarget = nextTarget;
        target = nextTarget;
      }
      attempts++;
      if (attempts < 12) {
        window.setTimeout(attempt, attempts === 1 ? 50 : 250);
      }
    }
    window.setTimeout(attempt, 0);
  }

  function removeDatasetFocusFallback() {
    var rail = document.querySelector(".rail");
    if (
      narrowManager.matches &&
      rail &&
      rail.classList.contains("is-manager-open")
    ) {
      var close = rail.querySelector(".rail-manager-close");
      if (canRestoreFocus(close)) return close;
      var managerItems = focusableElements(rail);
      if (managerItems.length) return managerItems[0];
      if (canRestoreFocus(rail)) return rail;
    }
    var fileTrigger = document.querySelector(".builder-file-trigger");
    if (canRestoreFocus(fileTrigger)) return fileTrigger;
    var railItems = rail ? focusableElements(rail) : [];
    return railItems.length ? railItems[0] : null;
  }

  function trapDialogKeydown(event) {
    var dialog = event.currentTarget;
    if (event.key === "Escape") {
      event.preventDefault();
      event.stopPropagation();
      if (dialog.__builderClose) dialog.__builderClose();
      return;
    }
    if (event.key !== "Tab") return;
    var items = focusableElements(dialog);
    if (!items.length) {
      event.preventDefault();
      dialog.focus();
      return;
    }
    var first = items[0];
    var last = items[items.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function prepareDialog(dialog, trigger, close, restoreFallback) {
    dialog.__builderFocusRestoreToken =
      (dialog.__builderFocusRestoreToken || 0) + 1;
    dialog.setAttribute("role", "dialog");
    dialog.setAttribute("aria-modal", "true");
    dialog.setAttribute("tabindex", "-1");
    dialog.__builderRestoreFocus = trigger || document.activeElement;
    dialog.__builderRestoreFocusFallback = restoreFallback;
    dialog.__builderClose = close;
    dialog.addEventListener("keydown", trapDialogKeydown);
    document.body.classList.add("builder-dialog-open");
    window.setTimeout(function () {
      var items = focusableElements(dialog);
      (items[0] || dialog).focus();
    }, 0);
  }

  function showTransientLayer(backdrop, dialog, visibleClass) {
    var previous = backdrop && backdrop.__builderTransientState;
    if (previous && previous.cancel) previous.cancel();
    var state = { closing: false };
    if (backdrop) backdrop.__builderTransientState = state;
    window.requestAnimationFrame(function () {
      if (
        state.closing ||
        !backdrop ||
        backdrop.__builderTransientState !== state ||
        !backdrop.isConnected ||
        !dialog ||
        !dialog.isConnected
      ) return;
      backdrop.classList.add("is-visible");
      dialog.classList.add(visibleClass || "is-visible");
    });
  }

  function removeTransientLayer(
    backdrop,
    dialog,
    visibleClass,
    complete,
    removeBackdrop
  ) {
    var state = backdrop && backdrop.__builderTransientState;
    if (!state) {
      state = { closing: false };
      if (backdrop) backdrop.__builderTransientState = state;
    }
    state.closing = true;
    if (backdrop) backdrop.classList.remove("is-visible");
    if (dialog) dialog.classList.remove(visibleClass || "is-visible");

    var finished = false;
    var timeout = null;
    function cleanup() {
      if (timeout !== null) window.clearTimeout(timeout);
      if (dialog) dialog.removeEventListener("transitionend", onTransitionEnd);
    }
    function finish() {
      if (finished) return;
      finished = true;
      cleanup();
      if (
        backdrop &&
        backdrop.__builderTransientState === state
      ) {
        delete backdrop.__builderTransientState;
      }
      if (removeBackdrop !== false && backdrop) backdrop.remove();
      if (complete) complete();
    }
    function onTransitionEnd(event) {
      if (event.target !== dialog) return;
      finish();
    }
    state.cancel = function () {
      if (finished) return;
      finished = true;
      cleanup();
    };

    if (reducedMotion.matches || window.__builderMotionDuration === 0) {
      finish();
      return;
    }
    if (dialog) dialog.addEventListener("transitionend", onTransitionEnd);
    timeout = window.setTimeout(
      finish,
      window.__builderMotionDuration + 60
    );
  }

  function pageShortcutBlocked() {
    if (
      document.body.classList.contains("modal-open") ||
      document.body.classList.contains("builder-dialog-open")
    ) return true;
    return Array.from(
      document.querySelectorAll('[aria-modal="true"]')
    ).some(function (dialog) {
      return !dialog.closest("[hidden]") && dialog.getClientRects().length > 0;
    });
  }

  function isTextInput(target) {
    return Boolean(target && target.closest(
      "input, textarea, select, [contenteditable='true'], .selectize-input"
    ));
  }

  function openDatasetPicker(message) {
    if (datasetMutationsLocked || !activityCapability("add_dataset")) return;
    var picker = document.createElement("input");
    picker.type = "file";
    picker.multiple = true;
    var transport = document.getElementById("dataset_files");
    picker.accept = transport ? transport.accept : ".rds,.qs,.qs2";
    picker.addEventListener("change", function () {
      enqueueClientFiles(picker.files);
    }, { once: true });
    picker.click();
  }

  // The native dialog is a separate process that can take seconds to appear;
  // keep the add buttons busy until the server reports it closed.
  function setDatasetPickerOpening(active) {
    datasetPickerOpening = Boolean(active);
    window.clearTimeout(datasetPickerTimer);
    datasetPickerTimer = null;
    if (datasetPickerOpening) {
      datasetPickerTimer = window.setTimeout(function () {
        setDatasetPickerOpening(false);
      }, 300000);
    }
    document.querySelectorAll(".builder-add-datasets").forEach(function (control) {
      control.setAttribute("aria-busy", datasetPickerOpening ? "true" : "false");
      control.classList.toggle("is-picker-opening", datasetPickerOpening);
      var label = control.querySelector(".builder-add-label");
      if (!label) return;
      if (datasetPickerOpening) {
        if (!label.dataset.idleLabel) label.dataset.idleLabel = label.textContent;
        label.textContent = "Opening file dialog…";
      } else if (label.dataset.idleLabel) {
        label.textContent = label.dataset.idleLabel;
        delete label.dataset.idleLabel;
      }
    });
    applyDatasetMutationLock();
  }

  function addDatasetFiles() {
    if (datasetMutationsLocked || datasetPickerOpening) return;
    if (!activityCapability("add_dataset")) return;
    setDatasetPickerOpening(true);
    scheduleStatusAnnouncement("Opening file dialog…");
    send("builder_choose_dataset_files", { nonce: Date.now() });
  }

  function addTableFiles() {
    if (datasetMutationsLocked || !activityCapability("edit_dataset")) return;
    var picker = document.getElementById("enhance-table_files");
    if (!picker) return;
    // A browser does not emit `change` when the same file is chosen twice.
    // Clear only the transport value; retained attachment state lives in R.
    picker.value = "";
    picker.click();
  }

  // Files dropped anywhere in the window are added as datasets. Drags that
  // carry no files (text, links, rail rows) are left alone, and a Shiny
  // file input keeps its own drops (tables, images).
  var windowFileDrag = { active: false, timer: null, overlay: null };

  function dragCarriesFiles(event) {
    var types = event.dataTransfer && event.dataTransfer.types;
    return Boolean(types) && Array.prototype.indexOf.call(types, "Files") >= 0;
  }

  function fileInputOwnsDrop(target) {
    var container = target && target.closest
      ? target.closest(".shiny-input-container")
      : null;
    return Boolean(container && container.querySelector("input[type=file]"));
  }

  function windowFileDropAllowed() {
    return activityCapability("add_dataset") && !datasetMutationsLocked &&
      !document.querySelector(".modal.show");
  }

  function windowFileDropOverlay() {
    if (windowFileDrag.overlay && windowFileDrag.overlay.isConnected) {
      return windowFileDrag.overlay;
    }
    var overlay = document.createElement("div");
    overlay.className = "builder-window-drop";
    overlay.setAttribute("aria-hidden", "true");
    overlay.hidden = true;
    var label = document.createElement("div");
    label.className = "builder-window-drop-label";
    label.textContent = "Drop files to add datasets";
    overlay.appendChild(label);
    document.body.appendChild(overlay);
    windowFileDrag.overlay = overlay;
    return overlay;
  }

  function setWindowFileDragActive(active) {
    window.clearTimeout(windowFileDrag.timer);
    windowFileDrag.timer = null;
    if (windowFileDrag.active === active) return;
    windowFileDrag.active = active;
    windowFileDropOverlay().hidden = !active;
    document.documentElement.classList.toggle("builder-file-drag-active", active);
    document.querySelectorAll(".builder-dataset-dropzone").forEach(function (zone) {
      zone.classList.toggle("is-drag-over", active);
    });
  }

  function handleWindowFileDrag(event) {
    if (!dragCarriesFiles(event)) return;
    if (fileInputOwnsDrop(event.target)) {
      setWindowFileDragActive(false);
      return;
    }
    // Always claim a file drag so the browser never opens the file in place
    // of the Builder; refuse it visibly when datasets cannot be added.
    event.preventDefault();
    var allowed = windowFileDropAllowed();
    if (event.dataTransfer) event.dataTransfer.dropEffect = allowed ? "copy" : "none";
    setWindowFileDragActive(allowed);
    // dragover repeats while the pointer is over the page; when it stops the
    // drag has left the window or been cancelled.
    if (allowed) {
      windowFileDrag.timer = window.setTimeout(function () {
        setWindowFileDragActive(false);
      }, 400);
    }
  }

  function handleWindowFileDrop(event) {
    if (!dragCarriesFiles(event)) return;
    setWindowFileDragActive(false);
    if (fileInputOwnsDrop(event.target)) return;
    event.preventDefault();
    if (!windowFileDropAllowed()) return;
    enqueueClientFiles(event.dataTransfer && event.dataTransfer.files);
  }

  function setupWindowFileDrop() {
    if (windowFileDrag.bound) return;
    windowFileDrag.bound = true;
    document.addEventListener("dragenter", handleWindowFileDrag);
    document.addEventListener("dragover", handleWindowFileDrag);
    document.addEventListener("drop", handleWindowFileDrop);
  }

  function clientQueueStatus(entry, index) {
    if (entry.state === "paused") return "Connection lost · Waiting to restore the import state…";
    if (entry.state === "unknown") return "Import state could not be restored";
    if (entry.state === "awaiting_dispatch") return "Preparing upload…";
    if (entry.state === "awaiting_upload") return "Waiting to upload…";
    var uploadPercent = typeof entry.uploadPercent === "number" ? entry.uploadPercent : null;
    if (
      entry.state === "dispatching" ||
      entry.state === "uploading" ||
      (entry.state === "awaiting_accept" && uploadPercent !== null && uploadPercent < 100)
    ) {
      return uploadPercent === null ? "Uploading…" : "Uploading… " + String(uploadPercent) + "%";
    }
    if (entry.state === "awaiting_accept") return "Waiting for the server…";
    if (entry.state === "error" || entry.state === "rejected") {
      return entry.error || "Could not start this import";
    }
    return "Waiting · " + String(index + 1) + " in queue" +
      (entry.duplicateHint ? " · Possible duplicate" : "");
  }

  // Shiny reports transfer progress by resizing the hidden
  // `#dataset_files_progress .progress-bar`; mirror it into the queue row.
  function clientUploadPercent(bar) {
    var width = bar && bar.style ? String(bar.style.width || "") : "";
    var match = /^(\d+(?:\.\d+)?)%$/.exec(width.trim());
    if (!match) return null;
    return Math.max(0, Math.min(100, Math.round(Number(match[1]))));
  }

  function handleClientUploadProgress(percent) {
    var entry = activeClientImport;
    if (!entry || entry.kind !== "file" || entry.serverId) return;
    if (!["uploading", "awaiting_accept"].includes(entry.state)) return;
    if (percent === null || entry.uploadPercent === percent) return;
    entry.uploadPercent = percent;
    var container = document.getElementById("ds_client_import_queue");
    if (!container) return;
    var rows = container.querySelectorAll(".ds--client-upload");
    for (var index = 0; index < rows.length; index += 1) {
      if (rows[index].dataset.clientImportId !== entry.clientId) continue;
      var status = rows[index].querySelector(".builder-import-status");
      if (status) {
        status.textContent = clientQueueStatus(entry, clientImportQueue.indexOf(entry));
      }
    }
  }

  function observeClientUploadProgress() {
    var bar = document.querySelector("#dataset_files_progress .progress-bar");
    if (!bar || bar.dataset.uploadProgressBound === "true") return;
    if (typeof MutationObserver !== "function") return;
    bar.dataset.uploadProgressBound = "true";
    new MutationObserver(function () {
      handleClientUploadProgress(clientUploadPercent(bar));
    }).observe(bar, { attributes: true, attributeFilter: ["style"] });
  }

  function applyClientImportQueueLock(roots) {
    var locked = clientImportQueue.length > 0;
    matchingDynamicElements(roots, ".builder-retry-import").forEach(function (control) {
      control.disabled = locked;
      control.setAttribute("aria-disabled", locked ? "true" : "false");
    });
  }

  function reportClientImportQueueState() {
    send("builder_client_import_state", {
      nonce: Date.now(),
      pending: clientImportQueue.length,
    });
  }

  function clientQueueRow(entry, index) {
    var row = document.createElement("div");
    row.className = "ds ds--import ds--client-upload";
    row.dataset.clientImportId = entry.clientId;
    row.dataset.loadState = entry.state;
    var ordinal = document.createElement("span");
    ordinal.className = "ds-idx";
    var body = document.createElement("span");
    body.className = "ds-body";
    var name = document.createElement("span");
    name.className = "nm";
    name.textContent = entry.name;
    var status = document.createElement("span");
    status.className = "builder-import-status";
    status.textContent = clientQueueStatus(entry, index);
    body.appendChild(name);
    body.appendChild(status);
    row.appendChild(ordinal);
    row.appendChild(body);
    var dot = document.createElement("span");
    dot.className = "ds-state-dot";
    dot.setAttribute("aria-hidden", "true");
    row.appendChild(dot);
    var actions = "none";
    if (
      entry !== activeClientImport &&
      !entry.serverId &&
      !["error", "rejected"].includes(entry.state)
    ) {
      actions = "cancel";
      var cancel = document.createElement("button");
      cancel.type = "button";
      cancel.className = "btn btn-remove-soft builder-cancel-client-import";
      cancel.dataset.clientImportId = entry.clientId;
      cancel.setAttribute("aria-label", "Cancel queued import " + entry.name);
      cancel.textContent = "Cancel";
      row.appendChild(cancel);
    } else if (clientImportFailures.includes(entry)) {
      actions = "remove";
      if (entry.outcome === "error") {
        actions = "retry-remove";
        var retry = document.createElement("button");
        retry.type = "button";
        retry.className = "btn builder-retry-client-import";
        retry.dataset.clientImportId = entry.clientId;
        retry.textContent = "Retry";
        row.appendChild(retry);
      }
      var remove = document.createElement("button");
      remove.type = "button";
      remove.className = "btn btn-remove-soft builder-remove-client-failure";
      remove.dataset.clientImportId = entry.clientId;
      remove.textContent = "Remove";
      row.appendChild(remove);
    }
    // Everything but the status line and its state; see patchRailRowInPlace.
    row.dataset.clientStructure = JSON.stringify([entry.name, actions]);
    return row;
  }

  var clientQueueRowContract = {
    structure: "clientStructure",
    live: [{ selector: ".builder-import-status", text: true }],
  };

  function renderClientImportQueue() {
    reportClientImportQueueState();
    applyBuilderActivityState();
    var container = document.getElementById("ds_client_import_queue");
    if (!container) return;
    var existing = new Map();
    container.querySelectorAll(":scope > .ds--client-upload").forEach(function (row) {
      existing.set(row.dataset.clientImportId, row);
    });
    var rows = [];
    clientImportQueue.concat(clientImportFailures).forEach(function (entry, index) {
      if (entry.serverId && entry === activeClientImport) return;
      var fresh = clientQueueRow(entry, index);
      var row = existing.get(entry.clientId);
      if (!row || !patchRailRowInPlace(row, fresh, clientQueueRowContract)) {
        if (row) row.replaceWith(fresh);
        row = fresh;
      }
      existing.delete(entry.clientId);
      rows.push(row);
    });
    existing.forEach(function (row) { row.remove(); });
    Array.from(container.children).forEach(function (child) {
      if (!rows.includes(child)) child.remove();
    });
    rows.forEach(function (row, position) {
      placeRailRow(container, row, position);
    });
    renumberClientImportQueue();
    applyClientImportQueueLock();
  }

  // Waiting browser imports continue the numbering of the rows above them.
  // Those rows change on their own (an import starts or finishes), so number
  // again whenever either rail changes instead of only when the queue does.
  function renumberClientImportQueue() {
    var container = document.getElementById("ds_client_import_queue");
    if (!container) return;
    var sequence = document.querySelectorAll(
      "#ds_ready_list > .ds, #ds_import_list .ds"
    ).length;
    container.querySelectorAll(":scope > .ds > .ds-idx").forEach(function (ordinal) {
      sequence += 1;
      var label = String(sequence);
      if (ordinal.textContent !== label) ordinal.textContent = label;
    });
  }

  function failClientDispatch(entry, message) {
    entry.state = "error";
    entry.outcome = "error";
    entry.error = message;
    scheduleStatusAnnouncement(entry.name + ". " + message);
    clientImportQueue.shift();
    activeClientImport = null;
    clientImportFailures.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function startFileTransport(entry) {
    var transport = document.getElementById("dataset_files");
    if (!transport) {
      failClientDispatch(entry, "The upload transport is unavailable.");
      return;
    }
    entry.state = "uploading";
    entry.uploadPercent = 0;
    try {
      var transfer = new DataTransfer();
      transfer.items.add(entry.file);
      transport.files = transfer.files;
    } catch (error) {
      failClientDispatch(entry, "This browser could not prepare the upload.");
      return;
    }
    if (
      !transport.files ||
      transport.files.length !== 1 ||
      transport.files[0].name !== entry.file.name ||
      transport.files[0].size !== entry.file.size
    ) {
      failClientDispatch(entry, "This browser could not prepare the selected file.");
      return;
    }
    renderClientImportQueue();
    transport.dispatchEvent(new Event("change", { bubbles: true }));
    entry.state = "awaiting_accept";
    renderClientImportQueue();
  }

  function dispatchFileImport(entry) {
    entry.state = "awaiting_dispatch";
    renderClientImportQueue();
    send("builder_client_import_dispatch", {
      client_id: entry.clientId,
      name: entry.name,
      size: entry.size,
      nonce: Date.now(),
    });
  }

  function handleClientImportDispatchReady(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.kind !== "file" || activeClientImport.serverId) return;
    if (!["awaiting_dispatch", "awaiting_upload"].includes(activeClientImport.state)) return;
    startFileTransport(activeClientImport);
  }

  function dispatchExampleImport(entry) {
    entry.state = "awaiting_accept";
    renderClientImportQueue();
    send("builder_import_example", {
      example: entry.exampleId,
      client_id: entry.clientId,
      nonce: Date.now(),
    });
  }

  function dispatchNextClientImport() {
    if (!uploadConnectionReady) return;
    if (importSyncPending) return;
    if (serverImportGate) return;
    if (activeClientImport) return;
    if (!clientImportQueue.length) return;
    activeClientImport = clientImportQueue[0];
    if (activeClientImport.kind === "file") dispatchFileImport(activeClientImport);
    else dispatchExampleImport(activeClientImport);
  }

  function enqueueClientFiles(fileList) {
    if (!activityCapability("add_dataset")) return;
    var files = Array.from(fileList || []);
    files.forEach(function (file) {
      var duplicateHint = clientImportQueue.some(function (queued) {
        return queued.kind === "file" &&
          queued.name === file.name &&
          queued.size === file.size &&
          queued.lastModified === file.lastModified;
      });
      clientUploadSequence += 1;
      clientImportQueue.push({
        clientId: "client-import-" + clientUploadSequence,
        kind: "file",
        file: file,
        name: file.name,
        size: file.size,
        lastModified: file.lastModified,
        exampleId: null,
        serverId: null,
        state: "queued",
        outcome: null,
        error: null,
        duplicateHint: duplicateHint,
      });
    });
    if (!files.length) return;
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function enqueueExample(example) {
    if (!activityCapability("add_dataset")) return;
    clientUploadSequence += 1;
    clientImportQueue.push({
      clientId: "client-import-" + clientUploadSequence,
      kind: "example",
      file: null,
      name: example.name,
      size: null,
      lastModified: null,
      exampleId: example.exampleId,
      serverId: null,
      state: "queued",
      outcome: null,
      error: null,
    });
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function cancelClientImport(clientId) {
    var index = clientImportQueue.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportQueue[index];
    if (entry === activeClientImport) {
      if (!entry.serverId) return;
      send("cancel_pending_upload", { id: entry.serverId, nonce: Date.now() });
      return;
    }
    clientImportQueue.splice(index, 1);
    renderClientImportQueue();
  }

  function removeClientImportFailure(clientId) {
    clientImportFailures = clientImportFailures.filter(function (entry) {
      return entry.clientId !== clientId;
    });
    renderClientImportQueue();
  }

  function retryClientImportFailure(clientId) {
    var index = clientImportFailures.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportFailures.splice(index, 1)[0];
    clientUploadSequence += 1;
    entry.clientId = "client-import-" + clientUploadSequence;
    entry.state = "queued";
    entry.outcome = null;
    entry.error = null;
    entry.serverId = null;
    clientImportQueue.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportAccepted(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    activeClientImport.serverId = message.server_id || null;
    activeClientImport.state = "reading";
    renderClientImportQueue();
  }

  function handleClientImportTransportRelease(message) {
    if (!activeClientImport || !message) return;
    if (activeClientImport.kind !== "file") return;
    if (message.client_id !== activeClientImport.clientId) return;
    if (!activeClientImport.serverId || !message.server_id) return;
    if (message.server_id !== activeClientImport.serverId) return;
    if (clientImportQueue[0] !== activeClientImport) return;
    clientImportQueue.shift();
    activeClientImport = null;
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportRelease(message) {
    if (!activeClientImport || !message) return;
    if (message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.serverId && message.server_id && message.server_id !== activeClientImport.serverId) return;
    var releasedEntry = activeClientImport;
    activeClientImport.outcome = message.outcome;
    activeClientImport.error = message.message || null;
    if (["error", "rejected"].includes(message.outcome)) {
      scheduleStatusAnnouncement(
        activeClientImport.name + ". " +
          (activeClientImport.error || "The import could not be completed.")
      );
    }
    clientImportQueue.shift();
    activeClientImport = null;
    if (["error", "rejected"].includes(message.outcome) && !message.server_id) {
      clientImportFailures.push(releasedEntry);
    }
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function failClientImportReconciliation(entry, message) {
    entry.state = "error";
    entry.outcome = "error";
    entry.error = message;
    entry.serverId = null;
    var index = clientImportQueue.indexOf(entry);
    if (index >= 0) clientImportQueue.splice(index, 1);
    if (activeClientImport === entry) activeClientImport = null;
    importSyncPending = false;
    if (!clientImportFailures.includes(entry)) clientImportFailures.push(entry);
    scheduleStatusAnnouncement(entry.name + ". " + message);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportSync(message) {
    var records = Array.isArray(message && message.imports) ? message.imports : [];
    serverImportGate = Boolean(message && message.server_busy);
    if (!activeClientImport) {
      importSyncPending = false;
      dispatchNextClientImport();
      return;
    }
    var entry = activeClientImport;
    var record = records.find(function (item) {
      return item && item.client_id === entry.clientId;
    });
    if (!record) {
      if (!entry.serverId && ["uploading", "awaiting_accept", "paused"].includes(entry.state)) {
        entry.serverId = null;
        entry.state = "queued";
        activeClientImport = null;
        importSyncPending = false;
        renderClientImportQueue();
        dispatchNextClientImport();
        return;
      }
      failClientImportReconciliation(entry, "The server could not match this import after reconnecting.");
      return;
    }
    if (entry.serverId && record.server_id !== entry.serverId) {
      failClientImportReconciliation(entry, "The restored import identity did not match.");
      return;
    }
    entry.serverId = record.server_id || entry.serverId;
    if (["ready", "error", "cancelled", "rejected"].includes(record.state)) {
      importSyncPending = false;
      handleClientImportRelease({
        client_id: entry.clientId,
        server_id: entry.serverId,
        outcome: record.state,
        message: record.message || null,
      });
      return;
    }
    if (record.transport_retained === true) {
      importSyncPending = false;
      handleClientImportTransportRelease({
        client_id: entry.clientId,
        server_id: entry.serverId,
      });
      return;
    }
    if (record.state === "awaiting_upload") {
      var uploadWasStarted = ["uploading", "awaiting_accept"].includes(entry.stateBeforePause);
      entry.state = uploadWasStarted ? "awaiting_accept" : "awaiting_upload";
      importSyncPending = false;
      renderClientImportQueue();
      if (!uploadWasStarted) startFileTransport(entry);
      return;
    }
    entry.state = record.state || entry.stateBeforePause || "unknown";
    importSyncPending = false;
    renderClientImportQueue();
  }

  function updateDialogLock() {
    var hasVisibleModal = Array.from(
      document.querySelectorAll('[aria-modal="true"]')
    ).some(function (dialog) {
      return !dialog.closest("[hidden]");
    });
    document.body.classList.toggle(
      "builder-dialog-open",
      hasVisibleModal
    );
  }

  function setRailDesktopSemantics(rail) {
    rail.setAttribute("role", "complementary");
    rail.setAttribute("aria-label", "Datasets");
    rail.removeAttribute("aria-modal");
    rail.removeAttribute("aria-hidden");
  }

  function updateRailSummary() {
    var summary = document.querySelector(".rail-summary");
    var rail = document.querySelector(".rail");
    if (!summary || !rail) return;
    var current = rail.querySelector(".ds.is-active");
    var name = current && current.querySelector(".nm");
    var readiness = current && current.querySelector(".rail-readiness-status");
    var nextName = name ? name.textContent.trim() : "No dataset selected";
    var nextState = [
      readiness ? readiness.textContent.trim() : "",
    ]
      .filter(Boolean)
      .join(" · ");
    var nameOutput = summary.querySelector(".rail-summary-name");
    var stateOutput = summary.querySelector(".rail-summary-state");
    if (nameOutput.textContent !== nextName) nameOutput.textContent = nextName;
    if (stateOutput.textContent !== nextState) stateOutput.textContent = nextState;
    if (nextState) scheduleStatusAnnouncement(nextName + ". " + nextState + ".");
  }

  function closeDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !rail.classList.contains("is-manager-open")) return;
    managerTransitionSequence += 1;
    var closeSequence = managerTransitionSequence;
    if (summary) summary.setAttribute("aria-expanded", "false");
    rail.removeEventListener("keydown", trapDialogKeydown);
    setRailDesktopSemantics(rail);
    if (narrowManager.matches) rail.setAttribute("aria-hidden", "true");
    restoreFocus(rail);
    removeTransientLayer(
      backdrop,
      rail,
      "is-manager-visible",
      function () {
        if (managerTransitionSequence !== closeSequence) return;
        rail.classList.remove("is-manager-open");
        if (backdrop) backdrop.classList.remove("is-open");
        if (!narrowManager.matches) setRailDesktopSemantics(rail);
        updateDialogLock();
      },
      false
    );
  }

  function openDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !summary || !narrowManager.matches) return;
    managerTransitionSequence += 1;
    rail.classList.add("is-manager-open");
    if (backdrop) backdrop.classList.add("is-open");
    rail.removeAttribute("aria-hidden");
    summary.setAttribute("aria-expanded", "true");
    prepareDialog(rail, summary, closeDatasetManager);
    showTransientLayer(backdrop, rail, "is-manager-visible");
  }

  function applyRailMode() {
    var rail = document.querySelector(".rail");
    if (!rail) return;
    if (narrowManager.matches) {
      if (!rail.classList.contains("is-manager-open")) {
        rail.setAttribute("aria-hidden", "true");
      }
    } else {
      managerTransitionSequence += 1;
      rail.classList.remove("is-manager-open");
      rail.classList.remove("is-manager-visible");
      var backdrop = document.querySelector(".rail-manager-backdrop");
      if (backdrop) {
        var state = backdrop.__builderTransientState;
        if (state && state.cancel) state.cancel();
        delete backdrop.__builderTransientState;
        backdrop.classList.remove("is-open", "is-visible");
      }
      var summary = document.querySelector(".rail-summary");
      if (summary) summary.setAttribute("aria-expanded", "false");
      rail.removeEventListener("keydown", trapDialogKeydown);
      setRailDesktopSemantics(rail);
      updateDialogLock();
    }
  }

  function setupRail() {
    var rail = document.querySelector(".rail");
    if (!rail || rail.dataset.builderManager === "true") return;
    rail.dataset.builderManager = "true";
    rail.id = rail.id || "builder-dataset-manager";
    setRailDesktopSemantics(rail);

    var summary = document.createElement("button");
    summary.type = "button";
    summary.className = "rail-summary";
    summary.setAttribute("aria-controls", rail.id);
    summary.setAttribute("aria-expanded", "false");
    var text = document.createElement("span");
    text.className = "rail-summary-copy";
    var name = document.createElement("span");
    name.className = "rail-summary-name";
    var state = document.createElement("span");
    state.className = "rail-summary-state";
    text.appendChild(name);
    text.appendChild(state);
    var affordance = document.createElement("span");
    affordance.className = "rail-summary-action";
    affordance.textContent = "Dataset Manager";
    summary.appendChild(text);
    summary.appendChild(affordance);
    rail.parentNode.insertBefore(summary, rail);

    var backdrop = document.createElement("button");
    backdrop.type = "button";
    backdrop.className = "rail-manager-backdrop";
    backdrop.setAttribute("aria-label", "Close Dataset Manager");
    rail.parentNode.insertBefore(backdrop, rail);

    var close = document.createElement("button");
    close.type = "button";
    close.className = "rail-manager-close";
    close.setAttribute("aria-label", "Close Dataset Manager");
    close.textContent = "Close";
    rail.querySelector(".rail-head").appendChild(close);
    updateRailSummary();
    applyRailMode();
  }

  function showRemoveConfirmation(removeDataset) {
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog";
    var kicker = document.createElement("span");
    kicker.className = "builder-project-modal-kicker";
    kicker.textContent = "Dataset";
    dialog.appendChild(kicker);
    var title = document.createElement("h2");
    title.id = "builder-remove-title";
    title.textContent = "Remove dataset setup?";
    var message = document.createElement("p");
    message.textContent = "You can undo the most recent removal in this session.";
    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions";
    var cancel = document.createElement("button");
    cancel.type = "button";
    cancel.className = "btn btn-default";
    cancel.textContent = "Keep dataset";
    var confirm = document.createElement("button");
    confirm.type = "button";
    confirm.className = "btn btn-danger";
    confirm.textContent = "Remove dataset";
    actions.appendChild(cancel);
    actions.appendChild(confirm);
    dialog.appendChild(title);
    dialog.appendChild(message);
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);

    var closed = false;
    function close(commitRemoval) {
      if (closed) return;
      closed = true;
      if (commitRemoval) {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    cancel.addEventListener("click", function () { close(false); });
    confirm.addEventListener("click", function () {
      close(true);
    });
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, removeDataset, close, removeDatasetFocusFallback);
    showTransientLayer(backdrop, dialog);
  }

  function projectRestoreDatasetLabel(count) {
    return count === 1 ? "dataset" : "datasets";
  }

  function syncProjectRestoreDialog(modal) {
    var dialog = modal && modal.querySelector(".builder-project-restore-dialog");
    if (!dialog) return;
    var rows = Array.from(
      dialog.querySelectorAll(".builder-project-restore-row")
    );
    var counts = rows.reduce(function (summary, row) {
      var select = row.querySelector("select");
      var action = select ? select.value : "skip";
      row.dataset.currentAction = action;
      summary.total += 1;
      if (action === "skip") {
        summary.skipped += 1;
      } else {
        summary.selected += 1;
      }
      if (action === "reuse") summary.reused += 1;
      if (action === "resume" || action === "reload") summary.sourced += 1;
      if (action === "reload") summary.reloaded += 1;
      return summary;
    }, {
      total: 0,
      selected: 0,
      skipped: 0,
      reused: 0,
      sourced: 0,
      reloaded: 0
    });

    ["reuse", "source", "skipped"].forEach(function (key) {
      var target = dialog.querySelector(
        '[data-project-restore-count="' + key + '"]'
      );
      if (!target) return;
      target.textContent = key === "reuse"
        ? counts.reused
        : (key === "source" ? counts.sourced : counts.skipped);
    });
    var footerSelected = modal.querySelector(
      "[data-project-restore-footer-selected]"
    );
    if (footerSelected) footerSelected.textContent = counts.selected;

    var notice = dialog.querySelector(".builder-project-restore-notice");
    var noticeText = dialog.querySelector("[data-project-restore-notice]");
    if (!notice || !noticeText) return;
    var messages = [];
    if (counts.skipped > 0) {
      messages.push(
        counts.skipped + " " + projectRestoreDatasetLabel(counts.skipped) +
          (counts.skipped === 1 ? " is" : " are") +
          " set to Skip and will stay unloaded in this session."
      );
    }
    if (counts.reloaded > 0) {
      messages.push(
        counts.reloaded + " " + projectRestoreDatasetLabel(counts.reloaded) +
          " will reload from source and reset saved settings."
      );
    }
    if (!messages.length) {
      messages.push(counts.total > 0
        ? counts.reused + " " + projectRestoreDatasetLabel(counts.reused) +
          " will open from cache; " +
          counts.sourced + " " + projectRestoreDatasetLabel(counts.sourced) +
          (counts.sourced === 1
            ? " will load its original source."
            : " will load their original sources.")
        : "This project does not contain saved datasets.");
    }
    noticeText.textContent = messages.join(" ");
    // Success only when something actually opens from the cache.
    notice.dataset.tone = counts.skipped > 0
      ? "warning"
      : (counts.reloaded > 0 ? "caution" : (counts.reused > 0 ? "ready" : "neutral"));
    var icon = notice.querySelector(".builder-project-restore-notice-icon i");
    if (icon) {
      icon.className = notice.dataset.tone === "ready"
        ? "fas fa-check"
        : (notice.dataset.tone === "neutral"
          ? "fas fa-info"
          : "fas fa-exclamation");
    }
  }

  function setupProjectRestoreEvents() {
    if (document.documentElement.dataset.builderProjectRestoreEvents === "true") {
      return;
    }
    document.documentElement.dataset.builderProjectRestoreEvents = "true";
    document.addEventListener("change", function (event) {
      if (!event.target.matches(".builder-project-restore-row select")) return;
      syncProjectRestoreDialog(event.target.closest(".modal"));
    });
  }

  function setupBootstrapModalEvents() {
    if (!window.jQuery || document.documentElement.dataset.builderModalEvents === "true") {
      return;
    }
    document.documentElement.dataset.builderModalEvents = "true";
    window.jQuery(document)
      .on("show.bs.modal.builderA11y", ".modal", function () {
        this.__builderRestoreFocus = document.activeElement;
      })
      .on("shown.bs.modal.builderA11y", ".modal", function () {
        var modal = this;
        syncProjectRestoreDialog(modal);
        modal.setAttribute("role", "dialog");
        modal.setAttribute("aria-modal", "true");
        modal.setAttribute("tabindex", "-1");
        var title = modal.querySelector(".modal-title");
        if (title) {
          title.id = title.id || "builder-modal-title-" + Date.now().toString(36);
          modal.setAttribute("aria-labelledby", title.id);
        } else if (!modal.hasAttribute("aria-label")) {
          modal.setAttribute("aria-label", "Builder dialog");
        }
        var preferred = modal.querySelector(
          ".modal-body input:not([type=hidden]):not([disabled]), " +
            ".modal-body select:not([disabled]), .modal-body textarea:not([disabled]), " +
            ".modal-footer .btn-primary:not([disabled]), .modal-footer button:not([disabled])"
        );
        (preferred || modal).focus({ preventScroll: true });
      })
      .on("hidden.bs.modal.builderA11y", ".modal", function () {
        this.removeAttribute("aria-modal");
        restoreFocus(this);
      });
  }

  function showAttachmentRemoveConfirmation(control, kind) {
    if (document.querySelector(".builder-attachment-remove-backdrop")) return;
    var actionOwner = contentActionOwner(control);
    if (!actionOwner) return;
    var row = control.closest(".enhance-sheet-item, .enhance-workbook-item");
    var fallbackRow = row && (row.nextElementSibling || row.previousElementSibling);
    var label = row && row.querySelector(".enhance-attachment-name");
    var displayName = label ? label.textContent.trim() :
      (kind === "workbook" ? "this workbook" : "this table");
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop builder-attachment-remove-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog";
    var title = document.createElement("h2");
    title.id = "builder-attachment-remove-title";
    title.textContent = kind === "workbook" ? "Remove workbook?" : "Remove table?";
    var message = document.createElement("p");
    message.id = "builder-attachment-remove-description";
    message.textContent = "Remove “" + displayName + "” from Extra material?";
    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions";
    var cancel = document.createElement("button");
    cancel.type = "button";
    cancel.className = "btn";
    cancel.textContent = "Keep " + kind;
    var confirm = document.createElement("button");
    confirm.type = "button";
    confirm.className = "btn btn-remove-soft";
    confirm.textContent = "Remove " + kind;
    actions.append(cancel, confirm);
    dialog.append(title, message, actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    dialog.setAttribute("aria-describedby", message.id);

    var closed = false;
    function close(remove) {
      if (closed) return;
      closed = true;
      if (remove) {
        send("enhance-table_action", {
          action: kind === "workbook" ? "remove_workbook" : "remove",
          key: kind === "workbook"
            ? control.dataset.workbookKey
            : control.dataset.tableKey,
          dataset: datasetIdForElement(control),
          generation: actionOwner.generation,
          project_epoch: actionOwner.project_epoch,
          owner_token: actionOwner.owner_token,
          nonce: Date.now(),
        });
      }
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    cancel.addEventListener("click", function () { close(false); });
    confirm.addEventListener("click", function () { close(true); });
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close(false);
    });
    prepareDialog(
      dialog,
      control,
      function () { close(false); },
      function () {
        if (fallbackRow && fallbackRow.isConnected) {
          var adjacent = focusableElements(fallbackRow);
          if (adjacent.length) return adjacent[0];
        }
        var addTables = document.querySelector(".enhance-table-add-button");
        if (canRestoreFocus(addTables)) return addTables;
        var stage = document.querySelector(".builder-stage");
        var stageControls = stage ? focusableElements(stage) : [];
        return stageControls.length ? stageControls[0] : null;
      }
    );
    showTransientLayer(backdrop, dialog);
  }

  var datasetRailContract = {
    row: ".ds.ds--ready[data-ds]",
    id: "ds",
    fingerprint: "railFingerprint",
    actions: {
      pick: ".builder-pick",
      reorder: ".builder-reorder",
      drop: ".builder-drop",
    },
  };
  var importRailContract = {
    row: ".ds.ds--import[data-import-id]",
    id: "importId",
    fingerprint: "importFingerprint",
    structure: "importStructure",
    // Parts that change while an import runs. Their attributes (and text,
    // where `text` is set) are copied onto the live row; the timer text is
    // left to updateDatasetLoadTimes so it does not jump back to 0.0s.
    live: [
      { selector: ":scope > .ds-pick" },
      { selector: ".ds-idx", text: true },
      { selector: ".builder-import-status", text: true },
      { selector: ".builder-load-time" },
    ],
    actions: {
      pick: ".builder-pick-import",
      retry: ".builder-retry-import",
      remove: ".builder-remove-import",
    },
  };

  function syncElementAttributes(target, source) {
    Array.from(target.attributes).forEach(function (attribute) {
      if (!source.hasAttribute(attribute.name)) {
        target.removeAttribute(attribute.name);
      }
    });
    Array.from(source.attributes).forEach(function (attribute) {
      if (target.getAttribute(attribute.name) !== attribute.value) {
        target.setAttribute(attribute.name, attribute.value);
      }
    });
  }

  // Bring an existing row up to date from freshly rendered markup without
  // replacing it, so hover, focus and a click in progress stay on the same
  // buttons. Returns false when the row's elements differ and it must be
  // replaced instead.
  function patchRailRowInPlace(row, fresh, contract) {
    if (!contract.structure || !contract.live) return false;
    var structure = row.dataset[contract.structure];
    if (!structure || structure !== fresh.dataset[contract.structure]) {
      return false;
    }
    var pairs = [];
    for (var index = 0; index < contract.live.length; index++) {
      var part = contract.live[index];
      var current = row.querySelector(part.selector);
      var next = fresh.querySelector(part.selector);
      if (!current !== !next) return false;
      if (current) pairs.push({ current: current, next: next, text: part.text });
    }
    syncElementAttributes(row, fresh);
    pairs.forEach(function (pair) {
      syncElementAttributes(pair.current, pair.next);
      if (pair.text && pair.current.textContent !== pair.next.textContent) {
        pair.current.textContent = pair.next.textContent;
      }
    });
    return true;
  }

  // Put `row` at `position` in `parent`, moving it only when it is out of
  // place: moving a focused or pressed button drops focus and the click.
  function placeRailRow(parent, row, position) {
    var occupant = parent.children[position] || null;
    if (occupant !== row) parent.insertBefore(row, occupant);
  }

  function railFocusIdentity(element, contract) {
    if (!element || !element.closest) return null;
    var row = element.closest(contract.row);
    var selectors = Object.values(contract.actions).join(", ");
    var control = element.closest(selectors);
    if (!row || !control) return null;
    var action = Object.keys(contract.actions).find(function (name) {
      return control.matches(contract.actions[name]);
    });
    if (!action) return null;
    return {
      id: row.dataset[contract.id],
      action: action,
      direction: control.dataset.direction || null,
    };
  }

  function railFocusTarget(rail, identity, contract) {
    if (!identity) return null;
    var row = Array.from(rail.querySelectorAll(contract.row)).find(function (item) {
      return item.dataset[contract.id] === identity.id;
    });
    if (!row) return null;
    var selector = contract.actions[identity.action];
    if (!selector) return null;
    var controls = Array.from(row.querySelectorAll(selector));
    if (!identity.direction) return controls[0] || null;
    return controls.find(function (item) {
      return item.dataset.direction === identity.direction;
    }) || null;
  }

  function parseRailElement(html, expectedId, expectedFingerprint, contract) {
    if (typeof html !== "string" || !html.trim()) return null;
    var template = document.createElement("template");
    template.innerHTML = html.trim();
    if (template.content.childElementCount !== 1) return null;
    var row = template.content.firstElementChild;
    if (
      !row.matches(contract.row) ||
      row.dataset[contract.id] !== expectedId ||
      row.dataset[contract.fingerprint] !== expectedFingerprint
    ) return null;
    return row;
  }

  function railSnapshot(rows, existing, contract, label, recover) {
    var ids = new Set();
    var target = [];
    for (var index = 0; index < rows.length; index++) {
      var record = rows[index];
      if (
        !record || typeof record.id !== "string" || !record.id ||
        typeof record.fingerprint !== "string" || ids.has(record.id)
      ) {
        console.error("Builder rejected an invalid " + label + " rail snapshot.");
        return null;
      }
      var current = existing.get(record.id);
      var needsMarkup = !current ||
        current.dataset[contract.fingerprint] !== record.fingerprint;
      var parsed = needsMarkup ? parseRailElement(
        record.html, record.id, record.fingerprint, contract
      ) : null;
      if (needsMarkup && !parsed) {
        console.error("Builder rejected invalid " + label + " rail row markup.");
        if (recover) recover();
        return null;
      }
      ids.add(record.id);
      target.push({ record: record, parsed: parsed });
    }
    return target;
  }

  function removeReadyImportOverlap() {
    var readyIds = new Set(Array.from(
      document.querySelectorAll("#ds_ready_list > .ds[data-ds]"),
      function (row) { return row.dataset.ds; }
    ));
    var host = document.getElementById("ds_import_list");
    if (!host || !readyIds.size) return;
    host.querySelectorAll(".ds[data-import-id]").forEach(function (row) {
      if (readyIds.has(row.dataset.importId)) row.remove();
    });
    var list = host.querySelector(":scope > .builder-import-list");
    if (list && !list.querySelector(".ds[data-import-id]")) {
      host.replaceChildren();
    }
  }

  function reconcileDatasetRail(message) {
    var rail = document.getElementById("ds_ready_list");
    var rows = message && message.rows;
    if (!rail || !Array.isArray(rows) || typeof message.empty_html !== "string") return;

    var existing = new Map();
    rail.querySelectorAll(":scope > .ds[data-ds]").forEach(function (row) {
      existing.set(row.dataset.ds, row);
    });
    var target = railSnapshot(
      rows, existing, datasetRailContract, "dataset", function () {
        send("builder_dataset_rail_sync", { nonce: Date.now() });
      }
    );
    if (!target) return;

    var empty = null;
    if (!target.length) {
      var emptyTemplate = document.createElement("template");
      emptyTemplate.innerHTML = message.empty_html.trim();
      empty = emptyTemplate.content.childElementCount === 1 ?
        emptyTemplate.content.firstElementChild : null;
      if (!empty || !empty.matches(".rail-empty")) {
        console.error("Builder rejected invalid empty dataset rail markup.");
        return;
      }
    }

    var focusedElement = document.activeElement;
    var focusIdentity = railFocusIdentity(focusedElement, datasetRailContract);

    if (!target.length) {
      rail.replaceChildren(empty);
    } else {
      rail.querySelectorAll(":scope > .rail-empty").forEach(function (node) {
        node.remove();
      });
      existing.forEach(function (row, id) {
        if (!target.some(function (item) { return item.record.id === id; })) {
          row.remove();
        }
      });
      target.forEach(function (item, position) {
        var row = existing.get(item.record.id);
        if (!row || row.dataset.railFingerprint !== item.record.fingerprint) {
          if (row) row.replaceWith(item.parsed);
          row = item.parsed;
        }
        placeRailRow(rail, row, position);
      });
    }

    removeReadyImportOverlap();
    renumberClientImportQueue();

    var nextFocus = railFocusTarget(rail, focusIdentity, datasetRailContract);
    if (nextFocus && focusedElement !== nextFocus) nextFocus.focus();
    document.querySelectorAll(".builder-confirm-dialog").forEach(function (dialog) {
      var identity = railFocusIdentity(
        dialog.__builderRestoreFocus, datasetRailContract
      );
      var replacement = railFocusTarget(rail, identity, datasetRailContract);
      if (replacement) dialog.__builderRestoreFocus = replacement;
    });
    updateRailSummary();
    if (window.BuilderIcons) window.BuilderIcons.decorate(rail);
    applyDatasetMutationLock([rail]);
    rail.querySelectorAll(".builder-pick").forEach(function (control) {
      setActivityDisabled(control, !activityCapability("select_dataset"));
    });
    updateDatasetLoadTimes([rail]);
    scheduleDatasetLoadTimeUpdates();
    if (datasetSwitchState.target) {
      var authoritative = rail.querySelector(
        ".builder-pick[aria-current=true]"
      );
      if (
        authoritative &&
        authoritative.dataset.ds !== datasetSwitchState.target
      ) {
        datasetSwitchState.authoritative = authoritative.dataset.ds;
        settleDatasetSwitch("error");
      } else {
        ensureDatasetSwitchVeil();
      }
    }
  }

  function reconcileImportRail(message) {
    var host = document.getElementById("ds_import_list");
    var rows = message && message.rows;
    if (!host || !Array.isArray(rows)) return;

    var focused = document.activeElement;
    var focusIdentity = railFocusIdentity(focused, importRailContract);
    var list = host.querySelector(":scope > .builder-import-list");
    var existing = new Map();
    if (list) {
      list.querySelectorAll(":scope > .ds[data-import-id]").forEach(function (row) {
        existing.set(row.dataset.importId, row);
      });
    }
    var target = railSnapshot(
      rows, existing, importRailContract, "import", null
    );
    if (!target) return;
    if (!target.length) {
      host.replaceChildren();
      renumberClientImportQueue();
      updateRailSummary();
      return;
    }
    if (!list) {
      list = document.createElement("div");
      list.className = "builder-import-list";
      list.setAttribute("aria-label", "Datasets being added");
      host.replaceChildren(list);
    }
    existing.forEach(function (row, id) {
      if (!target.some(function (item) { return item.record.id === id; })) {
        row.remove();
      }
    });
    target.forEach(function (item, position) {
      var row = existing.get(item.record.id);
      if (
        row &&
        row.dataset.importFingerprint !== item.record.fingerprint &&
        !patchRailRowInPlace(row, item.parsed, importRailContract)
      ) {
        row.replaceWith(item.parsed);
        row = item.parsed;
      }
      if (!row) row = item.parsed;
      placeRailRow(list, row, position);
    });
    removeReadyImportOverlap();
    renumberClientImportQueue();
    var nextFocus = railFocusTarget(list, focusIdentity, importRailContract);
    if (nextFocus && nextFocus !== focused) nextFocus.focus();
    if (window.BuilderIcons) window.BuilderIcons.decorate(host);
    applyDatasetMutationLock([host]);
    updateDatasetLoadTimes([host]);
    scheduleDatasetLoadTimeUpdates();
    updateRailSummary();
  }

  function showAnalysisInfo(infoButton) {
    if (document.querySelector(".builder-analysis-info-backdrop")) return;

    var backdrop = document.createElement("div");
    backdrop.className = "builder-analysis-info-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-analysis-info-dialog";
    var header = document.createElement("div");
    header.className = "builder-analysis-info-header";
    var title = document.createElement("h2");
    title.id = "builder-analysis-info-title";
    title.textContent = infoButton.dataset.title || "Analysis details";
    var closeButton = document.createElement("button");
    closeButton.type = "button";
    closeButton.className = "builder-analysis-info-close";
    closeButton.setAttribute("aria-label", "Close analysis information");
    closeButton.textContent = "Close";
    header.appendChild(title);
    header.appendChild(closeButton);

    var description = document.createElement("p");
    description.id = "builder-analysis-info-description";
    description.className = "builder-analysis-info-description";
    description.textContent = infoButton.dataset.description || "";

    var facts = document.createElement("dl");
    facts.className = "builder-analysis-info-facts";
    [
      { label: "Available in", value: infoButton.dataset.pages },
      { label: "Typical time", value: infoButton.dataset.cost },
      { label: "Requires", value: infoButton.dataset.prerequisite },
      { label: "Network", value: infoButton.dataset.network },
      {
        label: "If already present",
        value: infoButton.dataset.replacement,
        wide: true,
      },
      { label: "If skipped", value: infoButton.dataset.skip, wide: true },
    ].forEach(function (fact) {
      var item = document.createElement("div");
      item.className = "builder-analysis-info-fact" +
        (fact.wide ? " is-wide" : "");
      var label = document.createElement("dt");
      label.textContent = fact.label;
      var value = document.createElement("dd");
      value.textContent = fact.value || "Not specified.";
      item.appendChild(label);
      item.appendChild(value);
      facts.appendChild(item);
    });

    dialog.appendChild(header);
    dialog.appendChild(description);
    dialog.appendChild(facts);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    dialog.setAttribute("aria-describedby", description.id);

    var closed = false;
    function close() {
      if (closed) return;
      closed = true;
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    closeButton.addEventListener("click", close);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, infoButton, close);
    showTransientLayer(backdrop, dialog);
  }

  function setMarkerDialog(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var backdrop = document.getElementById("builder-marker-dialog-backdrop");
    var dialog = document.getElementById("builder-marker-dialog");
    var title = document.getElementById("builder-marker-dialog-title");
    var closeButton = document.getElementById("builder-marker-dialog-close");
    if (!backdrop || !dialog || !closeButton) return;

    function close() {
      if (backdrop.hidden) return;
      delete dialog.dataset.builderMarkerNeedsBodyFocus;
      dialog.removeEventListener("keydown", trapDialogKeydown);
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        backdrop.hidden = true;
        updateDialogLock();
        restoreFocus(dialog);
      }, false);
    }

    if (!message || message.action === "close") {
      close();
      return;
    }
    if (title && message.title) title.textContent = message.title;
    backdrop.hidden = false;
    dialog.dataset.builderMarkerNeedsBodyFocus = "true";
    closeButton.onclick = close;
    backdrop.onclick = function (event) {
      if (event.target === backdrop) close();
    };
    dialog.removeEventListener("keydown", trapDialogKeydown);
    prepareDialog(
      dialog,
      document.querySelector(".marker-genes-action"),
      close
    );
    showTransientLayer(backdrop, dialog);
    window.setTimeout(focusMarkerDialogBody, 0);
  }

  function focusMarkerDialogBody() {
    var backdrop = document.getElementById("builder-marker-dialog-backdrop");
    var dialog = document.getElementById("builder-marker-dialog");
    if (!backdrop || backdrop.hidden || !dialog) return;
    var needsInitialFocus = dialog.dataset.builderMarkerNeedsBodyFocus === "true";
    var active = document.activeElement;
    if (
      !needsInitialFocus && active && active !== document.body &&
      active !== dialog && dialog.contains(active)
    ) {
      return;
    }
    var body = document.getElementById("enhance-marker_dialog_body");
    var target = body && body.querySelector(
      "input:not([type=hidden]):not([disabled]), select:not([disabled]), " +
        "textarea:not([disabled]), button:not([disabled])"
    );
    if (target) {
      delete dialog.dataset.builderMarkerNeedsBodyFocus;
      target.focus({ preventScroll: true });
    }
  }

  function showBuildDialog(message) {
    var existing = document.querySelector(".builder-build-dialog-backdrop");
    if (message && message.action === "close") {
      if (existing) existing.remove();
      var pending = document.querySelector(".is-client-build-status");
      if (pending) pending.remove();
      updateDialogLock();
      return;
    }
    if (!message || message.type !== "conflict") return;
    if (existing) return;
    var trigger = document.getElementById("build");
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop builder-build-dialog-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog builder-build-dialog";
    var title = document.createElement("h2");
    title.id = "builder-build-dialog-title";
    title.textContent = message.title || "Files already exist";
    dialog.appendChild(title);

    var description = document.createElement("p");
    description.textContent = "Some outputs already exist in this folder:";
    dialog.appendChild(description);

    var list = document.createElement("ul");
    list.className = "builder-build-dialog-list";
    var values = Array.isArray(message.files)
      ? message.files
      : (typeof message.files === "string" && message.files.length
        ? [message.files]
        : []);
    var shown = values.slice(0, 4);
    shown.forEach(function (name) {
      var item = document.createElement("li");
      item.textContent = name;
      list.appendChild(item);
    });
    if (values.length > shown.length) {
      var more = document.createElement("li");
      more.textContent = "…and " + (values.length - shown.length) + " more";
      list.appendChild(more);
    }
    dialog.appendChild(list);
    var question = document.createElement("p");
    question.textContent = "What would you like to do?";
    dialog.appendChild(question);

    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions builder-build-dialog-actions";
    var buttons = [
      { label: "Cancel", action: "cancel", className: "btn" },
      { label: "Replace existing files", action: "replace", className: "btn btn-replace" },
      { label: "Choose another folder", action: "choose_another", className: "btn btn-action" },
    ];
    var closed = false;
    function close(action) {
      if (closed) return;
      closed = true;
      send("builder_build_dialog", {
        action: action || "cancel",
        nonce: message.nonce,
      });
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    buttons.forEach(function (definition) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = definition.className;
      button.textContent = definition.label;
      button.addEventListener("click", function () { close(definition.action); });
      actions.appendChild(button);
    });
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close("cancel");
    });
    prepareDialog(dialog, trigger, close);
    showTransientLayer(backdrop, dialog);
  }

  function setCurrentStage(stage) {
    document.querySelectorAll(".builder-stage").forEach(function (candidate) {
      delete candidate.dataset.currentStage;
    });
    if (stage) stage.dataset.currentStage = "true";
  }

  var stageObserver = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.target.isConnected) return;
        stageObserver.unobserve(entry.target);
        observedStages.delete(entry.target);
      });
      var visible = entries
        .filter(function (entry) {
          return entry.target.isConnected && entry.isIntersecting;
        })
        .sort(function (left, right) {
          return right.intersectionRatio - left.intersectionRatio;
        });
      if (visible.length) setCurrentStage(visible[0].target);
    },
    { rootMargin: "-15% 0px -55% 0px", threshold: [0.15, 0.5, 0.85] }
  );

  function registerStages(roots) {
    observedStages.forEach(function (stage) {
      if (stage.isConnected) return;
      stageObserver.unobserve(stage);
      observedStages.delete(stage);
    });
    var stages = matchingDynamicElements(roots, ".builder-stage");
    stages.forEach(function (stage) {
      if (stage.dataset.builderStage === "true") return;
      stage.dataset.builderStage = "true";
      stage.setAttribute("role", "region");
      var heading = stage.querySelector("h2");
      if (heading) {
        heading.id = heading.id || stage.id + "-heading";
        stage.setAttribute("aria-labelledby", heading.id);
      }
      stage.addEventListener("focusin", function () {
        setCurrentStage(stage);
      });
      stageObserver.observe(stage);
      observedStages.add(stage);
    });
    if (stages.length && !document.querySelector('[data-current-stage="true"]')) {
      setCurrentStage(stages[0]);
    }
  }

  function ensureLiveRegion() {
    var live = document.getElementById("builder-live-status");
    if (live) return live;
    live = document.createElement("div");
    live.id = "builder-live-status";
    live.className = "visually-hidden";
    live.setAttribute("role", "status");
    live.setAttribute("aria-live", "polite");
    live.setAttribute("aria-atomic", "true");
    document.body.appendChild(live);
    return live;
  }

  function scheduleStatusAnnouncement(text) {
    var next = String(text || "").replace(/\s+/g, " ").trim();
    if (!next) {
      window.clearTimeout(statusTimer);
      ensureLiveRegion().textContent = "";
      lastAnnouncement = "";
      return;
    }
    if (next === lastAnnouncement) return;
    window.clearTimeout(statusTimer);
    statusTimer = window.setTimeout(function () {
      ensureLiveRegion().textContent = next;
      lastAnnouncement = next;
    }, 350);
  }

  function updateStatusSemantics() {
    var status = ["#busy", ".result-card", "#review_action_summary"]
      .map(function (selector) {
        var node = document.querySelector(selector);
        if (node && selector === ".result-card") {
          // Announce the outcome, not collapsed diagnostics and filesystem paths.
          return Array.from(node.querySelectorAll(":scope > h3, :scope > p"))
            .map(function (item) { return item.textContent; }).join(". ");
        }
        return node ? node.textContent : "";
      })
      .filter(Boolean)
      .join(". ");
    // An empty status clears only what this function announced, not an
    // announcement another part of the page has just queued.
    if (status) {
      statusSemanticsAnnounced = true;
      scheduleStatusAnnouncement(status);
    } else if (statusSemanticsAnnounced) {
      statusSemanticsAnnounced = false;
      scheduleStatusAnnouncement("");
    }
  }

  function updatePipelines(roots) {
    var order = ["queued", "building", "complete"];
    matchingDynamicElements(roots, ".pipeline[data-pipeline-state]").forEach(function (pipeline) {
      var state = pipeline.dataset.pipelineState;
      var activeIndex = order.indexOf(state);
      pipeline.querySelectorAll(".pipeline-step").forEach(function (step) {
        var index = order.indexOf(step.dataset.step);
        var terminal = state === "complete" && step.dataset.step === "complete";
        var current = step.dataset.step === state && !terminal;
        var complete = state !== "failure" && index >= 0 &&
          (index < activeIndex || terminal);
        step.classList.toggle("is-complete", complete);
        step.classList.toggle("is-current", current);
        step.classList.toggle("is-terminal", terminal);
        if (current) {
          step.setAttribute("aria-current", "step");
        } else {
          step.removeAttribute("aria-current");
        }
      });
    });
  }

  var primaryObserver = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.target === observedPrimaryAction) {
          window.__builderPrimaryActionVisible =
            entry.isIntersecting && entry.intersectionRatio >= 0.99;
        }
      });
    },
    { threshold: [0, 0.99, 1] }
  );

  function registerPrimaryAction() {
    var action = document.getElementById("build");
    if (action === observedPrimaryAction) return;
    if (observedPrimaryAction) primaryObserver.unobserve(observedPrimaryAction);
    observedPrimaryAction = action;
    window.__builderPrimaryActionVisible = false;
    if (action) primaryObserver.observe(action);
  }

  function updateMotionDuration() {
    if (reducedMotion.matches) {
      window.__builderMotionDuration = 0;
      return;
    }
    var duration = getComputedStyle(document.documentElement)
      .getPropertyValue("--duration-normal")
      .trim();
    var match = duration.match(/^((?:\d+(?:\.\d+)?|\.\d+))(ms|s)$/);
    if (!match) {
      window.__builderMotionDuration = normalMotionDuration;
      return;
    }
    var value = Number(match[1]);
    var milliseconds = match[2] === "ms" ? value : value * 1000;
    window.__builderMotionDuration = Number.isFinite(milliseconds) ?
      Math.round(milliseconds) : normalMotionDuration;
  }


  function colourLabel(input) {
    var swatch = input.closest(".swatch");
    var name = swatch && swatch.querySelector(".swatch-name");
    var level = name ? name.textContent.trim() : input.dataset.level || "Colour";
    return level + ": " + input.value.toUpperCase();
  }

  function enhanceColour(input) {
    if (!input.hasAttribute("aria-label")) {
      input.setAttribute("aria-label", colourLabel(input));
    }
  }

  function updateGroupColor(input) {
    var item = input.closest(".group-color-item");
    var editor = input.closest(".builder-group-colors");
    var normalized = input.value.toUpperCase();
    var hex = item && item.querySelector(".group-color-hex");
    var status = editor && editor.querySelector(".group-color-status");
    var label = item && item.querySelector(".group-color-name");
    if (hex) hex.textContent = normalized;
    if (status) {
      status.textContent = "Color for " +
        (label ? label.textContent.trim() : input.dataset.level) +
        " changed to " + normalized + ".";
    }
    var projection = document.querySelector(".viewer-projection-workspace");
    if (projection && projection.dataset.previewGroup === input.dataset.group) {
      projection.querySelectorAll(".viewer-scatter-point[data-group]").forEach(
        function (point) {
          if (point.dataset.group === input.dataset.level) {
            point.setAttribute("fill", normalized);
          }
        }
      );
    }
    send(input.dataset.inputId, {
      group: input.dataset.group,
      level: input.dataset.level,
      color: normalized,
      nonce: Date.now(),
    });
  }

  function filterGroupColors(search) {
    var editor = search.closest(".builder-group-colors");
    if (!editor) return;
    var grid = editor.querySelector(".group-color-grid");
    var query = search.value.trim().toLowerCase();
    grid.querySelectorAll(".group-color-item").forEach(function (item) {
      item.hidden = query.length > 0 &&
        !item.dataset.search.includes(query);
    });
  }

  function viewerGroupRows(root) {
    return Array.from(root.querySelectorAll(".viewer-group-row"));
  }

  function updateViewerMetadataSelection(root, emit) {
    if (!root) return;
    var retained = [];
    viewerGroupRows(root).forEach(function (row) {
      var metadata = row.querySelector(".viewer-metadata-retain");
      var group = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      if (metadata && metadata.checked) retained.push(row.dataset.group);
      if (metadata && !metadata.checked) {
        if (group) group.checked = false;
        if (radio) radio.checked = false;
      }
    });
    updateViewerGroupSelection(root, false, true);
    if (emit && root.dataset.metadataInputId) {
      send(root.dataset.metadataInputId, {
        action: "set-retention",
        retained: retained,
        nonce: Date.now(),
      });
    }
  }

  function selectViewerMetadata(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-metadata-retain");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all-supported" ||
        row.dataset.recommendedRetained === "true";
    });
    updateViewerMetadataSelection(root, true);
  }

  function updateDefaultCopy(root, selector) {
    root.querySelectorAll(selector).forEach(function (input) {
      var label = input.closest("label");
      var copy = label && label.querySelector(".viewer-default-copy");
      if (copy) {
        copy.textContent = input.checked
          ? "Default"
          : selector === ".viewer-group-default"
            ? "Set as default"
            : "Set default";
      }
    });
  }

  function updateDisclosureSummary(output, text) {
    if (!output || output.textContent === text) return;
    output.textContent = text;
    output.classList.remove("is-updating");
    void output.offsetWidth;
    output.classList.add("is-updating");
  }

  function updateViewerGroupCount(root) {
    var count = root.querySelectorAll(".viewer-group-include:checked").length;
    var card = root.closest(".builder-viewer-card");
    var output = card && card.querySelector("[data-viewer-group-count]");
    var selected = root.querySelector(".viewer-group-default:checked");
    var row = selected && selected.closest(".viewer-group-row");
    var label = row && row.querySelector(".viewer-group-name");
    updateDisclosureSummary(
      output,
      count + " included · Default: " +
        (label ? label.textContent.trim() : "None")
    );
  }

  function updateViewerGroupSelection(root, emit, allowEmpty) {
    if (!root) return;
    var rows = viewerGroupRows(root);
    var included = rows
      .filter(function (row) {
        var checkbox = row.querySelector(".viewer-group-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (row) { return row.dataset.group; });
    if (!included.length && !allowEmpty) {
      var first = rows.find(function (row) {
        return row.dataset.eligible === "true";
      });
      var firstCheckbox = first && first.querySelector(".viewer-group-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.group];
      }
    }
    var currentDefault = root.querySelector(".viewer-group-default:checked");
    var defaultGroup = currentDefault && included.includes(currentDefault.value)
      ? currentDefault.value
      : included[0] || null;
    rows.forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      var isIncluded = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      row.classList.toggle("is-included", isIncluded);
      if (radio) {
        radio.disabled = row.dataset.eligible !== "true";
        radio.checked = isIncluded && row.dataset.group === defaultGroup;
      }
    });
    updateDefaultCopy(root, ".viewer-group-default");
    updateViewerGroupCount(root);
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set-groups",
        included: included,
        default: defaultGroup,
        nonce: Date.now(),
      });
    }
  }

  function focusViewerGroup(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    viewerGroupRows(root).forEach(function (row) {
      var focus = row.querySelector(".viewer-group-focus");
      var selected = row.dataset.group === button.dataset.group;
      row.classList.toggle("is-focused", selected);
      if (focus) focus.setAttribute("aria-pressed", selected ? "true" : "false");
    });
    if (root.dataset.focusInputId) {
      send(root.dataset.focusInputId, {
        group: button.dataset.group,
        nonce: Date.now(),
      });
    }
  }

  function filterViewerGroups(search) {
    var root = search.closest(".viewer-group-workspace");
    if (!root) return;
    var query = search.value.trim().toLowerCase();
    var rows = viewerGroupRows(root);
    var unavailable = root.querySelector(".viewer-group-unavailable");
    if (unavailable && query.length > 0 &&
        unavailable.dataset.searching !== "true") {
      unavailable.dataset.openBeforeSearch = unavailable.open ? "true" : "false";
      unavailable.dataset.searching = "true";
    }
    var visibleCount = 0;
    var unavailableMatches = 0;
    rows.forEach(function (row) {
      var visible = query.length === 0 ||
        (row.dataset.search || "").includes(query);
      row.hidden = !visible;
      if (visible) visibleCount += 1;
      if (visible && unavailable && unavailable.contains(row)) {
        unavailableMatches += 1;
      }
    });
    if (unavailable) {
      unavailable.hidden = query.length > 0 && unavailableMatches === 0;
      if (query.length > 0 && unavailableMatches > 0) {
        unavailable.open = true;
      } else if (query.length === 0 &&
          unavailable.dataset.searching === "true") {
        unavailable.open = unavailable.dataset.openBeforeSearch === "true";
        delete unavailable.dataset.openBeforeSearch;
        delete unavailable.dataset.searching;
      }
    }
    var empty = root.querySelector(".viewer-group-empty");
    if (empty) empty.hidden = visibleCount > 0;
  }

  function selectViewerGroups(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all" || row.dataset.suggested === "true";
    });
    updateViewerGroupSelection(root, true);
  }

  function setupViewerGroupCatalogs(roots) {
    matchingDynamicElements(roots, ".viewer-group-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderGroups === "true") return;
      workspace.dataset.builderGroups = "true";
      updateViewerGroupSelection(workspace, false);
      var initial = workspace.querySelector(".viewer-group-default:checked");
      var row = initial && initial.closest(".viewer-group-row");
      var focus = row && row.querySelector(".viewer-group-focus");
      if (focus) {
        row.classList.add("is-focused");
        focus.setAttribute("aria-pressed", "true");
      }
    });
  }

  function applyViewerGroupState(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var root = document.querySelector(".viewer-group-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(row.dataset.group);
      if (radio) radio.checked = row.dataset.group === (message && message.default);
    });
    updateViewerGroupSelection(root, false);
    var status = root.querySelector(".viewer-group-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function projectionCards(root) {
    return Array.from(root.querySelectorAll(".viewer-projection-card"));
  }

  function updateProjectionSelection(root, emit) {
    if (!root) return;
    var cards = projectionCards(root);
    var included = cards
      .filter(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (card) { return card.dataset.projection; });
    if (!included.length) {
      var first = cards.find(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && !checkbox.disabled;
      });
      var firstCheckbox = first && first.querySelector(".viewer-projection-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.projection];
      }
    }
    var selectedDefault = root.querySelector(".viewer-projection-default:checked");
    var defaultProjection = selectedDefault && included.includes(selectedDefault.value)
      ? selectedDefault.value
      : included[0] || null;
    var initialProjections = cards
      .filter(function (card) {
        var initial = card.querySelector(".viewer-projection-initial");
        return initial && initial.checked && included.includes(card.dataset.projection);
      })
      .map(function (card) { return card.dataset.projection; });
    if (!initialProjections.length && defaultProjection) {
      initialProjections = [defaultProjection];
    } else if (
      defaultProjection &&
      !initialProjections.includes(defaultProjection)
    ) {
      initialProjections = [defaultProjection].concat(initialProjections);
    }
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var initial = card.querySelector(".viewer-projection-initial");
      var radio = card.querySelector(".viewer-projection-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (initial) {
        initial.checked = selected && initialProjections.includes(card.dataset.projection);
        initial.disabled = Boolean(
          !checkbox || checkbox.disabled || card.dataset.projection === defaultProjection
        );
      }
      if (radio) {
        radio.disabled = Boolean(!checkbox || checkbox.disabled);
        radio.checked = selected && card.dataset.projection === defaultProjection;
      }
    });
    updateDefaultCopy(root, ".viewer-projection-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-projection-count]");
    var defaultCard = cards.find(function (card) {
      return card.dataset.projection === defaultProjection;
    });
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    updateDisclosureSummary(
      countOutput,
      included.length + " included · " + initialProjections.length +
        " shown initially · Default: " +
        (defaultLabel ? defaultLabel.textContent.trim() : "None")
    );
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: included,
        initial: initialProjections,
        default: defaultProjection,
        nonce: Date.now(),
      });
    }
  }

  function updateProjectionPointSize(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var radius = Math.max(0, Math.min(4.5, value * 0.34));
    var output = root.querySelector(".viewer-point-size-value");
    if (output) output.textContent = String(value);
    root.querySelectorAll(".viewer-projection-preview").forEach(function (svg) {
      svg.dataset.pointSize = String(value);
      svg.querySelectorAll(".viewer-scatter-point").forEach(function (point) {
        point.setAttribute("r", radius.toFixed(2));
      });
    });
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function updateProjectionPointOpacity(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var output = root.querySelector(".viewer-point-opacity-value");
    if (output) output.textContent = String(value);
    root.querySelectorAll(".viewer-scatter-point").forEach(function (point) {
      point.setAttribute("fill-opacity", String(value));
    });
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function updateProjectionCellPercentage(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var output = root.querySelector(".viewer-cell-percentage-value");
    if (output) output.textContent = String(value) + "%";
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function trajectoryCards(root) {
    return Array.from(root.querySelectorAll(".viewer-trajectory-card"));
  }

  function trajectoryRecord(card) {
    return { method: card.dataset.method, name: card.dataset.trajectory };
  }

  function updateTrajectorySelection(root, emit) {
    if (!root) return;
    var cards = trajectoryCards(root);
    var includedCards = cards.filter(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      return checkbox && checkbox.checked && !checkbox.disabled;
    });
    var selectedDefault = root.querySelector(".viewer-trajectory-default:checked");
    var defaultCard = selectedDefault && selectedDefault.closest(".viewer-trajectory-card");
    if (!defaultCard || !includedCards.includes(defaultCard)) {
      defaultCard = includedCards[0] || null;
    }
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (radio) {
        radio.disabled = !selected;
        radio.checked = selected && card === defaultCard;
      }
    });
    updateDefaultCopy(root, ".viewer-trajectory-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-trajectory-count]");
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    updateDisclosureSummary(
      countOutput,
      includedCards.length + " included" +
        (defaultLabel ? " · Default: " + defaultLabel.textContent.trim() : "")
    );
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: includedCards.map(trajectoryRecord),
        default: defaultCard ? trajectoryRecord(defaultCard) : null,
        nonce: Date.now(),
      });
    }
  }

  function setupViewerContentCatalogs(roots) {
    matchingDynamicElements(roots, ".viewer-projection-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderProjections === "true") return;
      workspace.dataset.builderProjections = "true";
      updateProjectionSelection(workspace, false);
      var pointSize = workspace.querySelector(".viewer-point-size-input");
      if (pointSize) updateProjectionPointSize(pointSize, false);
      var pointOpacity = workspace.querySelector(".viewer-point-opacity-input");
      if (pointOpacity) updateProjectionPointOpacity(pointOpacity, false);
      var cellPercentage = workspace.querySelector(".viewer-cell-percentage-input");
      if (cellPercentage) updateProjectionCellPercentage(cellPercentage, false);
    });
    matchingDynamicElements(roots, ".viewer-trajectory-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderTrajectories === "true") return;
      workspace.dataset.builderTrajectories = "true";
      updateTrajectorySelection(workspace, false);
    });
  }

  function disclosureStateKey(details) {
    var stage = details.closest(".builder-stage-core");
    var dataset = stage && stage.querySelector(".builder-rendered-for-input");
    var datasetId = dataset && dataset.value ? dataset.value : "builder";
    return datasetId + "::" + details.dataset.disclosureKey;
  }

  function setupPersistentDisclosures(roots) {
    matchingDynamicElements(roots, "details[data-disclosure-key]").forEach(function (details) {
      if (details.dataset.builderDisclosure === "true") return;
      var key = disclosureStateKey(details);
      if (viewerDisclosureState.has(key)) {
        details.open = viewerDisclosureState.get(key);
      }
      details.dataset.builderDisclosure = "true";
      details.addEventListener("toggle", function () {
        viewerDisclosureState.set(key, details.open);
      });
    });
  }

  function applyViewerProjectionState(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var root = document.querySelector(".viewer-projection-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    var initial = new Set(messageValues(message && message.initial));
    projectionCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var initialCheckbox = card.querySelector(".viewer-projection-initial");
      var radio = card.querySelector(".viewer-projection-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.projection);
      if (initialCheckbox) initialCheckbox.checked = initial.has(card.dataset.projection);
      if (radio) radio.checked = card.dataset.projection === (message && message.default);
    });
    var pointSize = root.querySelector(".viewer-point-size-input");
    if (pointSize && message && Number.isFinite(Number(message.point_size))) {
      pointSize.value = String(message.point_size);
      updateProjectionPointSize(pointSize, false);
    }
    var pointOpacity = root.querySelector(".viewer-point-opacity-input");
    if (pointOpacity && message && Number.isFinite(Number(message.point_opacity))) {
      pointOpacity.value = String(message.point_opacity);
      updateProjectionPointOpacity(pointOpacity, false);
    }
    var cellPercentage = root.querySelector(".viewer-cell-percentage-input");
    if (
      cellPercentage &&
      message &&
      Number.isFinite(Number(message.percentage_cells_to_show))
    ) {
      cellPercentage.value = String(message.percentage_cells_to_show);
      updateProjectionCellPercentage(cellPercentage, false);
    }
    updateProjectionSelection(root, false);
    var status = root.querySelector(".viewer-projection-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function applyViewerTrajectoryState(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var root = document.querySelector(".viewer-trajectory-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included).map(function (record) {
      return record.method + "::" + record.name;
    }));
    var defaultKey = message && message.default
      ? message.default.method + "::" + message.default.name
      : null;
    trajectoryCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.trajectoryKey);
      if (radio) radio.checked = card.dataset.trajectoryKey === defaultKey;
    });
    updateTrajectorySelection(root, false);
    var status = root.querySelector(".viewer-trajectory-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function normalizeCreatableSelectValue(value) {
    return String(value || "").trim().replace(/\s+/g, " ");
  }

  function setupCreatableSelect(root) {
    if (!root || root.dataset.builderCreatableSelectReady === "true") return;
    var select = root.querySelector("select");
    var selectize = select && select.selectize;
    var dropdown = selectize && selectize.$dropdown && selectize.$dropdown[0];
    if (!selectize || !dropdown) return;

    var inputLabel = root.dataset.builderCreateInputLabel || "Custom value";
    var actionLabel = root.dataset.builderCreateActionLabel || "Add custom value";
    var maximumLength = parseInt(root.dataset.builderCreateMaxlength, 10) || 80;
    var row = document.createElement("div");
    row.className = "builder-creatable-select-row";
    var input = document.createElement("input");
    input.type = "text";
    input.className = "builder-creatable-select-input";
    input.placeholder = root.dataset.builderCreatePlaceholder || "Type another value";
    input.setAttribute("aria-label", inputLabel);
    input.setAttribute("autocomplete", "off");
    var add = document.createElement("button");
    add.type = "button";
    add.className = "builder-creatable-select-add";
    add.textContent = "Add";
    add.setAttribute("aria-label", actionLabel);
    add.disabled = true;
    var error = document.createElement("p");
    error.className = "builder-creatable-select-error";
    error.setAttribute("aria-live", "polite");
    error.hidden = true;
    row.append(input, add, error);
    dropdown.appendChild(row);

    function updateState() {
      var value = normalizeCreatableSelectValue(input.value);
      var tooLong = value.length > maximumLength;
      add.disabled = !value || tooLong;
      error.hidden = !tooLong;
      error.textContent = tooLong ?
        "Use " + maximumLength + " characters or fewer." : "";
      input.setAttribute("aria-invalid", tooLong ? "true" : "false");
    }

    function matchingOption(value) {
      var comparison = value.toLocaleLowerCase();
      return Object.keys(selectize.options).find(function (key) {
        var option = selectize.options[key] || {};
        return [
          key,
          option[selectize.settings.valueField],
          option[selectize.settings.labelField],
        ].some(function (candidate) {
          return normalizeCreatableSelectValue(candidate).toLocaleLowerCase() === comparison;
        });
      });
    }

    function commitValue() {
      var value = normalizeCreatableSelectValue(input.value);
      if (!value || value.length > maximumLength) {
        updateState();
        return;
      }
      var existing = matchingOption(value);
      var selectedValue = existing || value;
      if (!existing) {
        var option = {};
        option[selectize.settings.valueField] = value;
        option[selectize.settings.labelField] = value;
        selectize.addOption(option);
      }
      selectize.setValue(selectedValue);
      selectize.close();
      input.value = "";
      updateState();
      window.setTimeout(function () {
        var controlInput = selectize.$control_input && selectize.$control_input[0];
        if (canRestoreFocus(controlInput)) controlInput.focus();
      }, 0);
    }

    row.addEventListener("mousedown", function (event) {
      event.stopPropagation();
    });
    input.addEventListener("mousedown", function () {
      selectize.ignoreFocus = true;
    });
    input.addEventListener("focus", function () {
      selectize.ignoreFocus = false;
      selectize.isFocused = true;
      selectize.isBlurring = false;
      selectize.$control.addClass("focus");
    });
    input.addEventListener("input", updateState);
    input.addEventListener("keydown", function (event) {
      if (event.key !== "Enter") return;
      event.preventDefault();
      event.stopPropagation();
      commitValue();
    });
    add.addEventListener("click", commitValue);
    root.dataset.builderCreatableSelectReady = "true";
  }

  function setupCreatableSelects(roots) {
    matchingDynamicElements(roots, "[data-builder-creatable-select='true']").forEach(
      setupCreatableSelect
    );
  }

  var multiSelectPlaceholder = "Select…";

  function minimumMultiSelectEmptyWidth() {
    var more = document.getElementById("cv-more-btn");
    return more ? Math.ceil(more.getBoundingClientRect().width) : 140;
  }

  function multiSelectEmptyWidth(select, instance) {
    var control = instance.$control && instance.$control[0];
    var style = window.getComputedStyle(control || select);
    var canvas = multiSelectEmptyWidth.canvas ||
      (multiSelectEmptyWidth.canvas = document.createElement("canvas"));
    var context = canvas.getContext("2d");
    context.font = style.font || [style.fontSize, style.fontFamily].join(" ");
    var longest = Array.prototype.reduce.call(select.options, function (width, option) {
      return Math.max(width, context.measureText(option.textContent || "").width);
    }, 0);
    return Math.min(
      window.innerWidth - 32,
      Math.max(minimumMultiSelectEmptyWidth(), Math.ceil(longest + 42))
    );
  }

  function sizeEmptyMultiSelect(select, instance) {
    var empty = !instance.items.length;
    instance.$wrapper.toggleClass("cerebro-multiselect-empty", empty);
    instance.$wrapper.css(
      "width",
      empty ? multiSelectEmptyWidth(select, instance) + "px" : ""
    );
  }

  function enhanceMultiSelect(select) {
    if (!select || !select.multiple) return;
    select.setAttribute("data-placeholder", multiSelectPlaceholder);
    if (!select.selectize) return;
    select.selectize.settings.placeholder = multiSelectPlaceholder;
    select.selectize.$control_input.attr("placeholder", multiSelectPlaceholder);
    select.selectize.updatePlaceholder();
    select.selectize.$wrapper.addClass("cerebro-multiselect");
    sizeEmptyMultiSelect(select, select.selectize);
    if (!select.dataset.cerebroMultiSelectReady) {
      select.selectize.on("change", function () {
        sizeEmptyMultiSelect(select, select.selectize);
      });
      select.dataset.cerebroMultiSelectReady = "true";
    }
  }

  function spatialAlignmentHasImage(sidebar) {
    var imageControls = sidebar.querySelector(
      ".spatial-alignment-sidebar-scroll > .shiny-panel-conditional"
    );
    return Boolean(
      imageControls && window.getComputedStyle(imageControls).display !== "none"
    );
  }

  function syncSpatialAlignmentScrollbar(sidebar) {
    var scrollbar = sidebar.querySelector(".spatial-alignment-persistent-scrollbar");
    if (!scrollbar) {
      scrollbar = document.createElement("div");
      scrollbar.className = "spatial-alignment-persistent-scrollbar";
      scrollbar.setAttribute("aria-hidden", "true");
      scrollbar.innerHTML = '<div class="spatial-alignment-persistent-scrollbar-thumb"></div>';
      sidebar.appendChild(scrollbar);

      scrollbar.addEventListener("pointerdown", function (event) {
        event.preventDefault();
        var start = event.clientY;
        var initialScroll = sidebar.scrollTop;
        var available = Math.max(1, sidebar.scrollHeight - sidebar.clientHeight);
        var thumb = scrollbar.firstElementChild;
        var track = Math.max(1, scrollbar.clientHeight - thumb.clientHeight);
        function drag(moveEvent) {
          sidebar.scrollTop = initialScroll +
            ((moveEvent.clientY - start) / track) * available;
        }
        function stop() {
          document.removeEventListener("pointermove", drag);
          document.removeEventListener("pointerup", stop);
        }
        document.addEventListener("pointermove", drag);
        document.addEventListener("pointerup", stop, { once: true });
      });
      sidebar.addEventListener("scroll", scheduleSpatialAlignmentScrollbars, {
        passive: true,
      });
    }

    var visible = spatialAlignmentHasImage(sidebar);
    scrollbar.hidden = !visible;
    if (!visible) return;

    var rect = sidebar.getBoundingClientRect();
    var trackHeight = Math.max(0, rect.height);
    var range = Math.max(0, sidebar.scrollHeight - sidebar.clientHeight);
    var thumbHeight = range > 0
      ? Math.max(28, trackHeight * (sidebar.clientHeight / sidebar.scrollHeight))
      : trackHeight;
    var travel = Math.max(0, trackHeight - thumbHeight);
    var thumbTop = range > 0 ? travel * (sidebar.scrollTop / range) : 0;
    var thumb = scrollbar.firstElementChild;

    scrollbar.style.top = rect.top + "px";
    scrollbar.style.left = (rect.right - 10) + "px";
    scrollbar.style.height = trackHeight + "px";
    thumb.style.height = thumbHeight + "px";
    thumb.style.transform = "translateY(" + thumbTop + "px)";
  }

  function syncSpatialAlignmentScrollbars(roots) {
    matchingDynamicElements(roots, ".spatial-alignment-sidebar").forEach(
      syncSpatialAlignmentScrollbar
    );
  }

  function scheduleSpatialAlignmentScrollbars() {
    if (spatialScrollbarFrame !== null) return;
    spatialScrollbarFrame = window.requestAnimationFrame(function () {
      spatialScrollbarFrame = null;
      syncSpatialAlignmentScrollbars();
    });
  }

  function updateOptionalAnalysisCount(roots) {
    matchingDynamicContainers(
      roots,
      ".builder-viewer-optional-analyses"
    ).forEach(function (disclosure) {
      var output = disclosure.querySelector("[data-analysis-count]");
      if (!output) return;
      var selected = disclosure.querySelectorAll(
        ".enhance-module-checkbox:checked, " +
        ".marker-genes-action[aria-pressed=\"true\"]"
      ).length;
      updateDisclosureSummary(output, selected + " included");
    });
  }

  function updateExtraMaterialCount(roots) {
    matchingDynamicContainers(
      roots,
      ".builder-viewer-extra-material"
    ).forEach(function (disclosure) {
      var output = disclosure.querySelector("[data-extra-material-count]");
      if (!output) return;
      updateDisclosureSummary(
        output,
        disclosure.querySelectorAll(".enhance-sheet-item").length + " included"
      );
    });
  }

  function enhanceDynamicContent(roots) {
    syncWorkflowProgressHeight();
    syncSpatialAlignmentScrollbars(roots);
    updateOptionalAnalysisCount(roots);
    updateExtraMaterialCount(roots);
    if (window.BuilderIcons) {
      (roots && roots.length ? roots : [document]).forEach(function (root) {
        window.BuilderIcons.decorate(root);
      });
    }
    setupRail();
    updateRailSummary();
    registerStages(roots);
    registerPrimaryAction();
    updateStatusSemantics();
    updatePipelines(roots);
    ensureDatasetSwitchVeil();
    var buildStatusHost = document.getElementById("build-stage-status");
    var clientStatus = buildStatusHost && buildStatusHost.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    var serverStatus = document.querySelector(
      "#build_stage_status_content .builder-build-status-section"
    );
    if (clientStatus && (serverStatus || document.getElementById("shiny-modal"))) clientStatus.remove();
    if (
      buildStatusScrollPhase === 2 &&
      document.querySelector(
        "#build-stage-status .builder-build-status-section:not(.is-client-build-status)"
      )
    ) {
      focusBuildStatus();
      buildStatusScrollPhase = 0;
    }
    updateDialogLock();
    setupPersistentDisclosures(roots);
    focusMarkerDialogBody();
    setupViewerGroupCatalogs(roots);
    setupViewerContentCatalogs(roots);
    setupCreatableSelects(roots);
    applyDesiredSpatialSection(document.getElementById("enhance-active_section"));
    applyBuilderActivityState(roots);
    applyProjectSourceLoadState();
    matchingDynamicElements(roots, 'input[type="color"]').forEach(enhanceColour);
    matchingDynamicElements(roots, "select[multiple]").forEach(enhanceMultiSelect);
  }

  function scheduleDynamicContentEnhancement(root) {
    dynamicContentEnhancementRoots.add(
      root && root.querySelectorAll ? root : document
    );
    if (dynamicContentEnhancementFrame !== null) return;
    dynamicContentEnhancementFrame = window.requestAnimationFrame(function () {
      dynamicContentEnhancementFrame = null;
      var roots = Array.from(dynamicContentEnhancementRoots);
      dynamicContentEnhancementRoots.clear();
      enhanceDynamicContent(roots);
    });
  }

  function destroyRemovedSelectizeControls(root) {
    if (!root || root.nodeType !== 1) return;
    var selects = [];
    if (root.matches("select.selectized")) selects.push(root);
    selects = selects.concat(
      Array.from(root.querySelectorAll("select.selectized"))
    );
    selects.forEach(function (select) {
      var selectize = select.selectize;
      if (!selectize || typeof selectize.destroy !== "function") return;
      selectize.destroy();
    });
  }

  function handleDynamicContentMutations(mutations) {
    mutations.forEach(function (mutation) {
      Array.from(mutation.removedNodes || []).forEach(
        destroyRemovedSelectizeControls
      );
    });
    var isSelfManagedRailMutation = mutations.length > 0 && mutations.every(
      function (mutation) {
        var target = mutation.target.nodeType === 1
          ? mutation.target
          : mutation.target.parentElement;
        return target && target.closest(
          "#ds_ready_list, #ds_import_list, #ds_client_import_queue, " +
          ".builder-load-time"
        );
      }
    );
    if (isSelfManagedRailMutation) return;
    // The canvas tooltip changes on every pointer move and the build clock every
    // second; neither needs the document re-enhanced.
    var ignoredSelector =
      ".selectize-control, .builder-icon-slot, .shiny-notification-panel, " +
      ".builder-spatial-canvas-tooltip, .builder-spatial-canvas-summary, " +
      "[data-build-progress-detail]";
    var affectedSelector =
      ".shiny-html-output, .modal, .builder-dialog, " +
      "[data-workflow-stage], #build-stage-status, #builder-operation-overlay";
    var relevantTargets = new Set();
    mutations.forEach(function (mutation) {
      var target = mutation.target.nodeType === 1
        ? mutation.target
        : mutation.target.parentElement;
      if (!target || target.closest(ignoredSelector)) return;
      var affected = target.closest(affectedSelector);
      if (affected) {
        relevantTargets.add(affected);
        return;
      }
      Array.from(mutation.addedNodes || []).forEach(function (node) {
        if (
          node.nodeType === 1 &&
          !node.closest(ignoredSelector) &&
          (node.matches(affectedSelector) || node.querySelector(affectedSelector))
        ) {
          relevantTargets.add(node);
        }
      });
    });
    if (!relevantTargets.size) return;
    var roots = Array.from(relevantTargets);
    roots.forEach(scheduleDynamicContentEnhancement);
    updateDatasetLoadTimes(roots);
    scheduleDatasetLoadTimeUpdates();
  }

  function setAttachmentEditing(row, editing) {
    var name = row.querySelector(".enhance-attachment-name");
    var editor = row.querySelector(".enhance-attachment-editor");
    var edit = row.querySelector(".enhance-attachment-edit");
    var save = row.querySelector(".enhance-attachment-save");
    var cancel = row.querySelector(".enhance-attachment-cancel");
    var input = editor && editor.querySelector("input");
    if (!name || !editor || !edit || !save || !cancel || !input) return;
    if (editing) {
      input.dataset.originalValue = input.value;
      name.hidden = true;
      editor.hidden = false;
      edit.hidden = true;
      save.hidden = false;
      cancel.hidden = false;
      input.focus();
    } else {
      input.removeAttribute("aria-invalid");
      input.removeAttribute("aria-describedby");
      var error = row.querySelector(".enhance-attachment-error");
      if (error) error.remove();
      name.hidden = false;
      editor.hidden = true;
      edit.hidden = false;
      save.hidden = true;
      cancel.hidden = true;
      edit.focus({ preventScroll: true });
    }
  }

  function announceAddedTables(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var workbooks = message && Array.isArray(message.workbooks)
      ? message.workbooks.filter(function (workbook) {
        return workbook && workbook.key && Number(workbook.count) > 0;
      })
      : [];
    if (!workbooks.length) return;
    var cards = [];
    document.querySelectorAll(".enhance-workbook-item[data-workbook-key]").forEach(
      function (card) {
        if (!workbooks.some(function (workbook) {
          return workbook.key === card.dataset.workbookKey;
        })) return;
        card.open = false;
        card.classList.add("enhance-workbook-item--new");
        cards.push(card);
      }
    );
    var latest = cards[cards.length - 1];
    window.clearTimeout(tableUploadHighlightTimer);
    tableUploadHighlightTimer = window.setTimeout(function () {
      document.querySelectorAll(".enhance-workbook-item--new").forEach(function (card) {
        card.classList.remove("enhance-workbook-item--new");
      });
    }, 5000);
    if (!latest) return;
    var bounds = latest.getBoundingClientRect();
    if (bounds.top < 0 || bounds.bottom > window.innerHeight) {
      latest.scrollIntoView({
        block: "nearest",
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
    }
  }

  function commitAttachmentName(row) {
    var input = row.querySelector(".enhance-attachment-editor input");
    if (!input) return;
    var name = input.value.trim();
    var originalName = String(input.dataset.originalValue || "").trim();
    if (name && name === originalName) {
      input.value = input.dataset.originalValue;
      setAttachmentEditing(row, false);
      return;
    }
    var isWorkbook = Boolean(input.dataset.workbookKey);
    var error = row.querySelector(".enhance-attachment-error");
    if (!error) {
      error = document.createElement("span");
      error.className = "enhance-attachment-error";
      error.id = "enhance-attachment-error-" + Date.now().toString(36);
      error.setAttribute("role", "alert");
      input.insertAdjacentElement("afterend", error);
    }
    if (!name) {
      error.textContent = "Enter a non-empty Viewer name.";
      input.setAttribute("aria-invalid", "true");
      input.setAttribute("aria-describedby", error.id);
      input.focus({ preventScroll: true });
      return;
    }
    var actionOwner = contentActionOwner(row);
    if (!actionOwner) return;
    if (isWorkbook) {
      var duplicate = Array.from(document.querySelectorAll(
        ".enhance-workbook-display-name"
      )).some(function (candidate) {
        return candidate !== input && candidate.value.trim() === name;
      });
      if (duplicate) {
        error.textContent = "Use a unique Viewer workbook name.";
        input.setAttribute("aria-invalid", "true");
        input.setAttribute("aria-describedby", error.id);
        input.focus({ preventScroll: true });
        return;
      }
    }
    error.textContent = "";
    input.removeAttribute("aria-invalid");
    input.removeAttribute("aria-describedby");
    send("enhance-table_action", {
      action: isWorkbook ? "rename_workbook" : "rename",
      key: isWorkbook ? input.dataset.workbookKey : input.dataset.tableKey,
      dataset: datasetIdForElement(row),
      name: name,
      generation: actionOwner.generation,
      project_epoch: actionOwner.project_epoch,
      owner_token: actionOwner.owner_token,
      nonce: Date.now(),
    });
  }

  function applyAttachmentSaved(message) {
    if (!datasetMessageIsCurrent(message)) return;
    if (!message || !message.kind || !message.key || !message.name) return;
    var selector = message.kind === "workbook"
      ? ".enhance-workbook-item[data-workbook-key]"
      : ".enhance-sheet-item";
    var row = Array.from(document.querySelectorAll(selector)).find(function (item) {
      var input = item.querySelector(".enhance-attachment-editor input");
      return input && (message.kind === "workbook"
        ? input.dataset.workbookKey === message.key
        : input.dataset.tableKey === message.key);
    });
    if (!row) return;
    var input = row.querySelector(".enhance-attachment-editor input");
    var name = row.querySelector(".enhance-attachment-name");
    input.value = message.name;
    input.dataset.originalValue = message.name;
    if (name) name.textContent = message.name;
    var labels = message.kind === "workbook"
      ? [
        [".enhance-attachment-edit", "Edit Viewer workbook name for "],
        [".enhance-attachment-save", "Save Viewer workbook name for "],
        [".enhance-attachment-cancel", "Cancel editing Viewer workbook name for "],
        [".enhance-workbook-remove", "Remove workbook "],
      ]
      : [
        [".enhance-attachment-edit", "Edit Viewer table name for "],
        [".enhance-attachment-save", "Save Viewer table name for "],
        [".enhance-attachment-cancel", "Cancel editing Viewer table name for "],
        [".enhance-table-remove", "Remove table "],
      ];
    labels.forEach(function (specification) {
      var control = row.querySelector(specification[0]);
      if (control) control.setAttribute("aria-label", specification[1] + message.name);
    });
    setAttachmentEditing(row, false);
  }

  function reopenWorkbookAfterRename(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var key = message && message.key;
    if (!key) return;
    document.querySelectorAll(".enhance-workbook-item[data-workbook-key]").forEach(
      function (workbook) {
        if (workbook.dataset.workbookKey === key) workbook.open = true;
      }
    );
  }

  document.addEventListener("click", function (event) {
    if (shouldFlushPendingTextEdits(event.target)) {
      flushPendingTextEdits();
    }
  }, true);

  document.addEventListener("click", function (event) {
    var target = event.target;
    var undoNotice = target.closest("#undo_remove_notice");
    if (undoNotice) {
      event.preventDefault();
      var undoControl = document.getElementById("undo_remove");
      if (undoControl && undoControl.getAttribute("aria-disabled") !== "true") {
        undoControl.click();
      }
      return;
    }
    var acknowledgeContent = target.closest("[data-builder-acknowledge]");
    if (acknowledgeContent) {
      event.preventDefault();
      var acknowledgeOwner = contentActionOwner(acknowledgeContent);
      if (!acknowledgeOwner) return;
      send(acknowledgeContent.dataset.inputId, {
        dataset: acknowledgeContent.dataset.dataset,
        capability: acknowledgeContent.dataset.capability,
        token: acknowledgeContent.dataset.token,
        generation: acknowledgeOwner.generation,
        project_epoch: acknowledgeOwner.project_epoch,
        owner_token: acknowledgeOwner.owner_token,
        nonce: Date.now(),
      });
      return;
    }
    var contentDisposition = target.closest("[data-builder-content-disposition]");
    if (contentDisposition) {
      event.preventDefault();
      var dispositionOwner = contentActionOwner(contentDisposition);
      if (!dispositionOwner) return;
      send(contentDisposition.dataset.inputId, {
        dataset: contentDisposition.dataset.dataset,
        capability: contentDisposition.dataset.capability,
        disposition: contentDisposition.dataset.disposition,
        generation: dispositionOwner.generation,
        project_epoch: dispositionOwner.project_epoch,
        owner_token: dispositionOwner.owner_token,
        nonce: Date.now(),
      });
      return;
    }
    var spatialImageTrigger = target.closest(".enhance-tissue-file-button");
    if (spatialImageTrigger) {
      var spatialImageInput = document.getElementById(
        spatialImageTrigger.getAttribute("for")
      );
      if (spatialImageInput) spatialImageInput.value = "";
    }
    if (target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
    var configureAction = target.closest("[data-builder-configure-action]");
    if (configureAction) {
      event.preventDefault();
      if (configureAction.disabled) return;
      configureActionSequence += 1;
      send(configureAction.dataset.builderConfigureAction, {
        sequence: configureActionSequence,
        nonce: Date.now(),
      });
      return;
    }
    var sourceLoad = target.closest("#project_resume_current_source");
    if (sourceLoad && !sourceLoad.disabled) beginProjectSourceLoad(sourceLoad);
    if (target.closest("#build")) showImmediateBuildStatus();
    var authOpen = target.closest(".builder-auth-open");
    if (authOpen) {
      event.preventDefault();
      openAuthDialog(authOpen);
      return;
    }
    if (target.closest(".builder-auth-add")) {
      event.preventDefault();
      var rows = authRows();
      rows.push(authNewAccount());
      authRender(rows);
      var lastUsername = document.querySelector(".builder-auth-row:last-child .builder-auth-username");
      if (lastUsername) lastUsername.focus();
      return;
    }
    var authRemove = target.closest(".builder-auth-remove");
    if (authRemove) {
      event.preventDefault();
      var removedRow = authRemove.closest(".builder-auth-row");
      var nextFocus = removedRow && (removedRow.nextElementSibling || removedRow.previousElementSibling);
      if (removedRow) removedRow.remove();
      var nextUsername = nextFocus && nextFocus.querySelector(".builder-auth-username");
      var addAccount = document.querySelector(".builder-auth-add");
      if (nextUsername || addAccount) {
        (nextUsername || addAccount).focus({ preventScroll: true });
      }
      clearAuthError();
      return;
    }
    if (target.closest(".builder-auth-cancel")) {
      event.preventDefault();
      cancelAuthDialog();
      return;
    }
    if (target.closest(".builder-auth-save")) {
      event.preventDefault();
      if (authEditor.saving) return;
      var accounts = authRows();
      var users = accounts.map(function (account) { return account.username.trim(); });
      var authError = document.getElementById("builder-auth-error");
      clearAuthError();
      var rows = Array.from(document.querySelectorAll(".builder-auth-row"));
      var seenUsers = new Map();
      var firstInvalid = null;
      rows.forEach(function (row, index) {
        var username = row.querySelector(".builder-auth-username");
        var password = row.querySelector(".builder-auth-password");
        var normalized = users[index];
        var usernameInvalid = !normalized || seenUsers.has(normalized);
        if (normalized && !seenUsers.has(normalized)) seenUsers.set(normalized, index);
        if (usernameInvalid) {
          markAuthInvalid(username);
          if (!firstInvalid) firstInvalid = username;
          if (normalized && seenUsers.has(normalized)) {
            var firstRow = rows[seenUsers.get(normalized)];
            markAuthInvalid(firstRow && firstRow.querySelector(".builder-auth-username"));
          }
        }
        if (!password || password.value.length < 8) {
          markAuthInvalid(password);
          if (!firstInvalid) firstInvalid = password;
        }
      });
      if (!accounts.length || firstInvalid) {
        if (authError) {
          authError.textContent = "Add unique usernames and passwords of at least 8 characters.";
          authError.hidden = false;
        }
        if (firstInvalid) firstInvalid.focus({ preventScroll: true });
        else {
          var addButton = document.querySelector(".builder-auth-add");
          if (addButton) addButton.focus({ preventScroll: true });
        }
        return;
      }
      accounts.forEach(function (account, index) { account.username = users[index]; });
      authEditor.committed = authCopy(accounts);
      var nonce = Date.now();
      setAuthSaving(nonce);
      send("builder_auth_accounts", { enabled: true, accounts: accounts, nonce: nonce });
      return;
    }
    if (target.matches("#build_require_login")) {
      if (target.checked) {
        openAuthDialog(target, true);
      } else {
        clearAuthSecrets();
        authRender([]);
      }
      return;
    }
    var viewerGroupFocus = target.closest(".viewer-group-focus");
    if (viewerGroupFocus) {
      event.preventDefault();
      focusViewerGroup(viewerGroupFocus);
      return;
    }
    var viewerGroupSelect = target.closest(".viewer-group-select");
    if (viewerGroupSelect) {
      event.preventDefault();
      selectViewerGroups(viewerGroupSelect);
      return;
    }
    var viewerMetadataSelect = target.closest(".viewer-metadata-select");
    if (viewerMetadataSelect) {
      event.preventDefault();
      selectViewerMetadata(viewerMetadataSelect);
      return;
    }
    var attachmentEdit = target.closest(".enhance-attachment-edit");
    if (attachmentEdit) {
      event.preventDefault();
      event.stopPropagation();
      setAttachmentEditing(
        attachmentEdit.closest(".enhance-sheet-item, .enhance-workbook-item"),
        true
      );
      return;
    }
    var attachmentCancel = target.closest(".enhance-attachment-cancel");
    if (attachmentCancel) {
      event.preventDefault();
      event.stopPropagation();
      var cancelRow = attachmentCancel.closest(
        ".enhance-sheet-item, .enhance-workbook-item"
      );
      var cancelInput = cancelRow.querySelector(".enhance-attachment-editor input");
      if (cancelInput) cancelInput.value = cancelInput.dataset.originalValue || cancelInput.value;
      setAttachmentEditing(cancelRow, false);
      return;
    }
    var attachmentSave = target.closest(".enhance-attachment-save");
    if (attachmentSave) {
      event.preventDefault();
      event.stopPropagation();
      commitAttachmentName(attachmentSave.closest(
        ".enhance-sheet-item, .enhance-workbook-item"
      ));
      return;
    }
    var removeWorkbook = target.closest(".enhance-workbook-remove");
    if (removeWorkbook) {
      event.preventDefault();
      event.stopPropagation();
      showAttachmentRemoveConfirmation(removeWorkbook, "workbook");
      return;
    }
    var removeTable = target.closest(".enhance-table-remove");
    if (removeTable) {
      event.preventDefault();
      event.stopPropagation();
      showAttachmentRemoveConfirmation(removeTable, "table");
      return;
    }
    var confirmMarkerSource = target.closest(".marker-source-confirm");
    if (confirmMarkerSource) {
      event.preventDefault();
      send("enhance-marker_source_confirm", {
        id: confirmMarkerSource.dataset.sourceId,
        nonce: Date.now(),
      });
      return;
    }
    var infoButton = target.closest(".enhance-info-button");
    if (infoButton) {
      event.preventDefault();
      event.stopPropagation();
      showAnalysisInfo(infoButton);
      return;
    }
    var managerSummary = target.closest(".rail-summary");
    if (managerSummary) {
      openDatasetManager();
      return;
    }
    if (
      target.closest(".rail-manager-close") ||
      target.closest(".rail-manager-backdrop")
    ) {
      closeDatasetManager();
      return;
    }

    var removeDataset = target.closest(".builder-drop");
    if (removeDataset) {
      event.preventDefault();
      event.stopPropagation();
      if (removeDataset.dataset.confirm === "true") {
        showRemoveConfirmation(removeDataset);
      } else {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      return;
    }

    var pickImport = target.closest(".builder-pick-import");
    if (pickImport) {
      send("pick_import", {
        id: pickImport.dataset.importId,
        nonce: Date.now(),
      });
      if (narrowManager.matches) closeDatasetManager();
      return;
    }

    var retryImport = target.closest(".builder-retry-import");
    if (retryImport) {
      event.preventDefault();
      event.stopPropagation();
      if (clientImportQueue.length) return;
      send("retry_import", {
        id: retryImport.dataset.importId,
        nonce: Date.now(),
      });
      return;
    }

    var removeImport = target.closest(".builder-remove-import");
    if (removeImport) {
      event.preventDefault();
      event.stopPropagation();
      send("remove_import", {
        id: removeImport.dataset.importId,
        nonce: Date.now(),
      });
      return;
    }

    var cancelClientImportButton = target.closest(".builder-cancel-client-import");
    if (cancelClientImportButton) {
      cancelClientImport(cancelClientImportButton.dataset.clientImportId);
      return;
    }

    var removeClientFailure = target.closest(".builder-remove-client-failure");
    if (removeClientFailure) {
      removeClientImportFailure(removeClientFailure.dataset.clientImportId);
      return;
    }

    var retryClientFailure = target.closest(".builder-retry-client-import");
    if (retryClientFailure) {
      retryClientImportFailure(retryClientFailure.dataset.clientImportId);
      return;
    }

    var addDatasets = target.closest(".builder-add-datasets");
    if (addDatasets) {
      addDatasetFiles(addDatasets);
      return;
    }

    var addTables = target.closest(".enhance-table-add-button");
    if (addTables) {
      event.preventDefault();
      addTableFiles(addTables);
      return;
    }

    var reorderDataset = target.closest(".builder-reorder");
    if (reorderDataset) {
      send("reorder_ds", {
        id: reorderDataset.dataset.ds,
        direction: reorderDataset.dataset.direction,
      });
      return;
    }
    var pick = target.closest(".builder-pick");
    if (pick) {
      if (beginDatasetSwitch(pick.dataset.ds)) {
        send("pick", {
          id: pick.dataset.ds,
          switch_token: datasetSwitchState.generation,
        });
      }
      // On the Data step, opening a loaded dataset means configuring it.
      if (
        document.querySelector('#workbench [data-workflow-stage="upload"]') &&
        activityCapability("navigate_workflow")
      ) {
        send("workflow_stage_configure", Date.now());
      }
      if (narrowManager.matches) closeDatasetManager();
      return;
    }
    var example = target.closest(".example-btn");
    if (example) {
      var members = [];
      try {
        members = JSON.parse(example.dataset.examples || "[]");
      } catch (error) {
        members = [];
      }
      if (!Array.isArray(members) || !members.length) {
        members = [{
          id: example.dataset.ex,
          label: example.dataset.label || "Selected example",
        }];
      }
      members.forEach(function (member) {
        enqueueExample({
          exampleId: member.id,
          name: member.label,
        });
      });
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      return;
    }
    if (event.target.matches(".enhance-attachment-editor input")) {
      if (event.key === "Enter") {
        event.preventDefault();
        commitAttachmentName(event.target.closest(
          ".enhance-sheet-item, .enhance-workbook-item"
        ));
      } else if (event.key === "Escape") {
        event.preventDefault();
        var editRow = event.target.closest(
          ".enhance-sheet-item, .enhance-workbook-item"
        );
        event.target.value = event.target.dataset.originalValue || event.target.value;
        setAttachmentEditing(editRow, false);
      }
      return;
    }
    var fileTrigger = event.target.closest(
      ".builder-file-trigger, .enhance-tissue-file-button"
    );
    if (
      fileTrigger &&
      (event.key === "Enter" || event.key === " ")
    ) {
      event.preventDefault();
      var targetId = fileTrigger.getAttribute("for");
      var fileInput = targetId ? document.getElementById(targetId) : null;
      if (fileInput) fileInput.click();
      return;
    }
    var enhanceCheckbox = event.target.closest(".enhance-module-checkbox");
    if (
      enhanceCheckbox &&
      event.key === "Enter" &&
      !enhanceCheckbox.disabled
    ) {
      event.preventDefault();
      enhanceCheckbox.click();
      return;
    }
    if (!isTextInput(event.target)) {
      var modifier = event.ctrlKey || event.metaKey;
      if (modifier && pageShortcutBlocked()) return;
      if (modifier && event.key === "Enter") {
        event.preventDefault();
        var build = document.getElementById("build");
        if (build && !build.disabled) build.click();
        return;
      }
      if (modifier && event.code === "KeyO") {
        event.preventDefault();
        addDatasetFiles();
        return;
      }
      if (modifier && event.code === "KeyZ") {
        var undo = document.getElementById("undo_remove");
        if (undo) {
          event.preventDefault();
          undo.click();
        }
        return;
      }
    }
    var pick = event.target.closest(".builder-pick");
    if (!pick || !event.altKey) return;
    var direction = null;
    if (event.key === "ArrowUp") direction = "up";
    if (event.key === "ArrowDown") direction = "down";
    if (!direction) return;
    event.preventDefault();
    send("reorder_ds", { id: pick.dataset.ds, direction: direction });
  });

  document.addEventListener("input", function (event) {
    if (event.target.matches(
      "#builder-workspace input.shiny-bound-input[type=text], " +
        "#builder-workspace textarea.shiny-bound-input"
    )) {
      event.target.dataset.builderDirty = "true";
    }
    if (event.target.matches(".enhance-attachment-editor input")) {
      event.target.removeAttribute("aria-invalid");
      event.target.removeAttribute("aria-describedby");
      var attachmentRow = event.target.closest(
        ".enhance-sheet-item, .enhance-workbook-item"
      );
      var attachmentError = attachmentRow && attachmentRow.querySelector(
        ".enhance-attachment-error"
      );
      if (attachmentError) attachmentError.remove();
    }
    if (event.target.matches(".builder-auth-row input")) {
      event.target.removeAttribute("aria-invalid");
      var describedBy = (event.target.getAttribute("aria-describedby") || "")
        .split(/\s+/)
        .filter(function (id) { return id && id !== "builder-auth-error"; });
      if (describedBy.length) {
        event.target.setAttribute("aria-describedby", describedBy.join(" "));
      } else {
        event.target.removeAttribute("aria-describedby");
      }
      var accountRow = event.target.closest(".builder-auth-row");
      var removeAccount = accountRow && accountRow.querySelector(".builder-auth-remove");
      var accountUsername = accountRow && accountRow.querySelector(".builder-auth-username");
      if (removeAccount && accountUsername) {
        removeAccount.setAttribute(
          "aria-label",
          "Remove account" + (accountUsername.value.trim() ? " " + accountUsername.value.trim() : "")
        );
      }
    }
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-point-opacity-input")) {
      updateProjectionPointOpacity(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-group-search")) {
      filterViewerGroups(event.target);
      return;
    }
    if (event.target.matches(".group-color-search")) {
      filterGroupColors(event.target);
      return;
    }
    if (event.target.matches('input[type="color"]')) {
      enhanceColour(event.target);
    }
  });

  document.addEventListener("change", function (event) {
    if (event.target.id === "enhance-active_section") {
      desiredSpatialSection = event.target.value;
    }
    if (event.target.id.indexOf("enhance-marker_source_mode_") === 0) {
      send("enhance-marker_source_mode", {
        id: event.target.id.replace("enhance-marker_source_mode_", ""),
        mode: event.target.value,
        nonce: Date.now(),
      });
      return;
    }
    if (event.target.matches("#dataset_files")) return;
    if (event.target.matches(".enhance-module-checkbox")) {
      updateOptionalAnalysisCount();
    }
    if (event.target.matches(".viewer-group-include")) {
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-metadata-retain")) {
      updateViewerMetadataSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-group-default")) {
      var groupRow = event.target.closest(".viewer-group-row");
      var groupCheckbox = groupRow &&
        groupRow.querySelector(".viewer-group-include");
      if (groupCheckbox && !groupCheckbox.disabled) groupCheckbox.checked = true;
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-projection-initial")) {
      var initialCard = event.target.closest(".viewer-projection-card");
      var initialInclude = initialCard &&
        initialCard.querySelector(".viewer-projection-include");
      if (event.target.checked && initialInclude && !initialInclude.disabled) {
        initialInclude.checked = true;
      }
      updateProjectionSelection(
        event.target.closest(".viewer-projection-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-projection-default")) {
      var defaultCard = event.target.closest(".viewer-projection-card");
      var defaultInclude = defaultCard &&
        defaultCard.querySelector(".viewer-projection-include");
      var defaultInitial = defaultCard &&
        defaultCard.querySelector(".viewer-projection-initial");
      if (defaultInclude && !defaultInclude.disabled) defaultInclude.checked = true;
      if (defaultInitial) defaultInitial.checked = true;
      updateProjectionSelection(
        event.target.closest(".viewer-projection-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-projection-include")) {
      updateProjectionSelection(
        event.target.closest(".viewer-projection-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-point-opacity-input")) {
      updateProjectionPointOpacity(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-trajectory-include, .viewer-trajectory-default")) {
      updateTrajectorySelection(
        event.target.closest(".viewer-trajectory-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".group-color-input")) {
      updateGroupColor(event.target);
      return;
    }
  });

  function focusDatasetStart(message) {
    var dataset = message && message.dataset;
    if (
      typeof dataset !== "string" || !dataset ||
      !datasetMessageGenerationIsFresh(message)
    ) return;
    datasetStartFocusToken += 1;
    var token = datasetStartFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== datasetStartFocusToken) return;
      var selected = document.querySelector(
        "#ds_ready_list .ds[data-ds] .builder-pick[aria-current=true]"
      );
      var row = selected && selected.closest(".ds[data-ds]");
      var stage = document.querySelector('[data-workflow-stage="configure"]');
      var heading = stage && stage.querySelector("h2");
      if (
        !row || row.dataset.ds !== dataset ||
        renderedDatasetId() !== dataset || !heading
      ) {
        attempts += 1;
        if (attempts < 40) window.setTimeout(apply, 50);
        return;
      }
      window.scrollTo({
        top: 0,
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
      heading.setAttribute("tabindex", "-1");
      heading.focus({ preventScroll: true });
    }
    apply();
  }

  function focusIncompleteSetting(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var selectors = {
      settings_organism: "#core-organism",
      settings_groups: "#core-groups",
      settings_reductions: ".builder-viewer-projections",
      settings_layer: "#core-layer",
      settings_nUMI: "#core-nUMI",
      settings_nGene: "#core-nGene",
      metadata: "#core-groups",
      metadata_policy: "#core-groups",
      groups: "#core-groups",
      projection: ".builder-viewer-projections",
      trajectory: ".builder-viewer-trajectories",
      spatial: ".builder-viewer-spatial-alignment",
    };
    datasetStartFocusToken += 1;
    stageFocusToken += 1;
    var selector = selectors[message && message.blocker] ||
      ".builder-configure-inspect";
    var node = document.querySelector(selector);
    if (!node) return;
    var details = node.matches("details") ? node : node.closest("details");
    if (details) details.open = true;
    var target = node.closest(".shiny-input-container") || node;
    target.scrollIntoView({
      block: "center",
      behavior: reducedMotion.matches ? "auto" : "smooth",
    });
    var focus = target.querySelector(".selectize-input input") ||
      target.querySelector(
        "summary, input:not([type=hidden]), select:not([tabindex='-1']), button"
      ) || target;
    if (typeof focus.focus === "function") {
      focus.focus({ preventScroll: true });
    }
  }

  function animateCoordinateResetSliders(message) {
    if (!datasetMessageIsCurrent(message)) return;
    var ids = message && Array.isArray(message.ids) ? message.ids : [];
    ids.forEach(function (id) {
      if (!coordinateResetSliderIds.has(id)) return;
      var input = document.getElementById(id);
      var container = input && input.closest(".shiny-input-container");
      if (!container) return;
      var timer = coordinateResetMotionTimers.get(id);
      if (timer) window.clearTimeout(timer);
      container.classList.remove("builder-slider-reset-motion");
      void container.offsetWidth;
      container.classList.add("builder-slider-reset-motion");
      coordinateResetMotionTimers.set(id, window.setTimeout(function () {
        container.classList.remove("builder-slider-reset-motion");
        coordinateResetMotionTimers.delete(id);
      }, 380));
    });
  }

  function registerBuildDialogHandler() {
    if (buildDialogHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("builder_copy_text", copyTextToClipboard);
    window.Shiny.addCustomMessageHandler("builder_build_dialog", showBuildDialog);
    window.Shiny.addCustomMessageHandler("builder_build_progress", handleBuildProgress);
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_rail_patch",
      reconcileDatasetRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_rail_patch",
      reconcileImportRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_mutation_lock",
      function (message) {
        datasetMutationsLocked = Boolean(message && message.locked === true);
        applyDatasetMutationLock();
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_activity_state",
      function (message) {
        if (!message || typeof message !== "object") return;
        // Feedback inside a dialog stays until that dialog closes; the
        // activity state changes while the dialog's action is still running.
        var pendingFeedback = document.getElementById("builder-project-pending-feedback");
        if (pendingFeedback && pendingFeedback.dataset.inDialog !== "true") {
          pendingFeedback.remove();
        }
        resetDatasetMessageGenerationsForActivity(message);
        builderActivityState = {
          phase: message.phase || "none",
          capabilities: message.capabilities || {},
          busy_title: message.busy_title || null,
          busy_message: message.busy_message || null,
          busy_detail: message.busy_detail || null,
          busy_detail_items: Array.isArray(message.busy_detail_items)
            ? message.busy_detail_items
            : null,
          busy_phase: message.busy_phase || null,
          busy_modal: message.busy_modal === true,
          has_project: message.has_project === true,
          picker_waiting: message.picker_waiting === true,
          build_cancelable: message.build_cancelable === true,
          open_cancelable: message.open_cancelable === true,
          warn_before_unload: message.warn_before_unload === true,
          page_inert: message.page_inert === true,
          build_reason: typeof message.build_reason === "string"
            ? message.build_reason
            : null,
        };
        applyBuilderActivityState();
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_action_owner",
      applyDatasetActionOwner
    );
    window.Shiny.addCustomMessageHandler(
      "builder_worker_status",
      updateBuilderWorkerStatus
    );
    window.Shiny.addCustomMessageHandler("builder_notify", builderNotify);
    window.Shiny.addCustomMessageHandler(
      "builder_project_save_result",
      showBuilderProjectSaveResult
    );
    window.Shiny.addCustomMessageHandler(
      "builder_project_source_progress",
      updateBuilderProjectSourceProgress
    );
    window.Shiny.addCustomMessageHandler(
      "builder_project_source_load_state",
      updateProjectSourceLoadState
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_switch_state",
      updateDatasetSwitchPhase
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_tables_added",
      announceAddedTables
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_attachment_saved",
      applyAttachmentSaved
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_workbook_reopen",
      reopenWorkbookAfterRename
    );

    window.Shiny.addCustomMessageHandler("builder_marker_dialog", setMarkerDialog);
    window.Shiny.addCustomMessageHandler(
      "builder_focus_dataset_start",
      focusDatasetStart
    );
    window.Shiny.addCustomMessageHandler(
      "builder_focus_incomplete_setting",
      focusIncompleteSetting
    );
    window.Shiny.addCustomMessageHandler(
      "builder_coordinate_reset_motion",
      animateCoordinateResetSliders
    );
    window.Shiny.addCustomMessageHandler(
      "builder_focus_build_status",
      function (message) {
        if (buildOperationActive) {
          beginBuildOperationFocus(
            document.getElementById("builder-operation-overlay")
          );
          return;
        }
        scheduleBuildStatusFocus();
      }
    );
    window.Shiny.addCustomMessageHandler("builder_focus_stage", function (message) {
      var id = message && message.id;
      if (["upload", "configure", "review", "build"].indexOf(id) < 0) return;
      stageFocusToken += 1;
      var token = stageFocusToken;
      var attempts = 0;
      function apply() {
        if (token !== stageFocusToken) return;
        var stage = document.querySelector('[data-workflow-stage="' + id + '"]');
        var heading = stage && stage.querySelector("h2");
        if (!heading) {
          attempts += 1;
          if (attempts < 12) window.setTimeout(apply, 50);
          return;
        }
        heading.setAttribute("tabindex", "-1");
        if (id === "build") {
          window.scrollTo({
            top: 0,
            behavior: reducedMotion.matches ? "auto" : "smooth",
          });
        } else {
          var topbar = document.querySelector(".topbar");
          var workflowBar = document.querySelector("#workflow_progress");
          var stickyBottom = Math.max(
            topbar ? topbar.getBoundingClientRect().bottom : 0,
            workflowBar ? workflowBar.getBoundingClientRect().bottom : 0
          );
          heading.style.scrollMarginTop = Math.max(0, stickyBottom + 12) + "px";
          heading.scrollIntoView({
            block: "start",
            behavior: reducedMotion.matches ? "auto" : "smooth",
          });
        }
        heading.focus({ preventScroll: true });
        scheduleStatusAnnouncement("Opened " + id + " step.");
      }
      apply();
    });
    window.Shiny.addCustomMessageHandler("builder_import_status", function (message) {
      if (message && message.text) scheduleStatusAnnouncement(message.text);
    });
    function handleAuthStatus(message) {
      if (
        !message ||
        typeof authEditor.saving !== "number" ||
        message.nonce !== authEditor.saving
      ) return;
      var error = document.getElementById("builder-auth-error");
      setAuthSaving(false);
      if (message.ok) {
        if (error) { error.textContent = ""; error.hidden = true; }
        clearAuthLiveInputs();
        authRender([]);
        authEditor.snapshot = [];
        send("builder_auth_accounts", null);
        authEditor.rollbackRequireLogin = false;
        closeAuthDialog(true);
      } else if (error) {
        error.textContent = "Login accounts could not be saved. Review the account details and try again.";
        error.hidden = false;
        error.focus({ preventScroll: true });
      }
    }
    window.__builderHandleAuthStatus = handleAuthStatus;
    window.Shiny.addCustomMessageHandler("builder_auth_status", handleAuthStatus);
    // Build and Retry ask for login accounts in place instead of sending the
    // user back to Review.
    window.Shiny.addCustomMessageHandler("builder_open_auth_dialog", function (message) {
      if (!message || message.open !== true || authEditor.open) return;
      openAuthDialog(document.getElementById("build"));
    });
    window.Shiny.addCustomMessageHandler("builder_auth_reset", function (message) {
      if (!message || message.reset !== true) return;
      setAuthSaving(false);
      clearAuthSecrets();
      clearAuthError();
      authRender([]);
      send("builder_auth_accounts", null);
    });
    buildDialogHandlerRegistered = true;
  }

  function updateDatasetLoadTimes(roots) {
    var now = Date.now();
    var running = false;
    matchingDynamicElements(roots, ".builder-load-time").forEach(function (node) {
      var fixed = Number(node.dataset.elapsedMs);
      var started = Number(node.dataset.startedAtMs);
      if (!Number.isFinite(fixed) && Number.isFinite(started)) running = true;
      var elapsed = Number.isFinite(fixed)
        ? fixed
        : Number.isFinite(started)
          ? Math.max(0, now - started)
          : NaN;
      if (!Number.isFinite(elapsed)) return;
      var text = (elapsed / 1000).toFixed(1) + "s";
      if (node.textContent !== text) node.textContent = text;
    });
    if (!roots && !running && datasetLoadTimeTimer !== null) {
      window.clearTimeout(datasetLoadTimeTimer);
      datasetLoadTimeTimer = null;
    }
    return running;
  }

  function scheduleDatasetLoadTimeUpdates() {
    if (datasetLoadTimeTimer !== null) return;
    if (!document.querySelector(".builder-load-time[data-started-at-ms]")) return;
    datasetLoadTimeTimer = window.setTimeout(function () {
      datasetLoadTimeTimer = null;
      if (updateDatasetLoadTimes()) scheduleDatasetLoadTimeUpdates();
    }, 100);
  }

  function registerClientImportHandlers() {
    if (clientImportHandlersRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_dispatch_ready",
      handleClientImportDispatchReady
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_accepted",
      handleClientImportAccepted
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_transport_release",
      handleClientImportTransportRelease
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_release",
      handleClientImportRelease
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_sync",
      handleClientImportSync
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_picker_fallback",
      function (message) {
        setDatasetPickerOpening(false);
        openDatasetPicker(message);
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_picker_state",
      function (message) {
        setDatasetPickerOpening(Boolean(message && message.active));
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_scheduler_state",
      function (message) {
        serverImportGate = Boolean(message && message.active);
        if (!serverImportGate) dispatchNextClientImport();
      }
    );
    clientImportHandlersRegistered = true;
  }

  // Re-apply the server's spatial section after Configure redraws. The
  // remembered section may belong to the dataset shown before; never force a
  // section this select does not offer (it would show an empty choice).
  function applyDesiredSpatialSection(select) {
    if (!select || !desiredSpatialSection) return;
    if (select.value === desiredSpatialSection) return;
    var offered = select.selectize
      ? Object.prototype.hasOwnProperty.call(
          select.selectize.options,
          desiredSpatialSection
        )
      : Array.prototype.some.call(select.options, function (option) {
          return option.value === desiredSpatialSection;
        });
    if (!offered) return;
    if (select.selectize) {
      select.selectize.setValue(desiredSpatialSection, true);
    } else {
      select.value = desiredSpatialSection;
    }
  }

  function registerViewerGroupHandler() {
    if (viewerGroupHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_group_state",
      applyViewerGroupState
    );
    viewerGroupHandlerRegistered = true;
  }

  function registerViewerContentHandlers() {
    if (!window.Shiny) return;
    if (!viewerProjectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_projection_state",
        applyViewerProjectionState
      );
      viewerProjectionHandlerRegistered = true;
    }
    if (!viewerTrajectoryHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_trajectory_state",
        applyViewerTrajectoryState
      );
      viewerTrajectoryHandlerRegistered = true;
    }
    if (!spatialSectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_spatial_section_state",
        function (message) {
          if (!datasetMessageIsCurrent(message) || !message.value) return;
          spatialSectionGeneration += 1;
          var generation = spatialSectionGeneration;
          spatialSectionTimers.forEach(window.clearTimeout);
          spatialSectionTimers = [];
          desiredSpatialSection = message.value;
          [0, 50, 200, 500, 1000, 2000, 5000].forEach(function (delay) {
            var timer = window.setTimeout(function () {
              if (generation !== spatialSectionGeneration) return;
              applyDesiredSpatialSection(
                document.getElementById("enhance-active_section")
              );
            }, delay);
            spatialSectionTimers.push(timer);
          });
        }
      );
      spatialSectionHandlerRegistered = true;
    }
  }

  function requestClientImportSync() {
    uploadConnectionReady = true;
    importSyncPending = true;
    registerBuildDialogHandler();
    registerClientImportHandlers();
    registerViewerGroupHandler();
    registerViewerContentHandlers();
    send("builder_dataset_rail_sync", { nonce: Date.now() });
    send("builder_import_rail_sync", { nonce: Date.now() });
    send("builder_import_sync_request", { nonce: Date.now() });
    if (document.body) scheduleDynamicContentEnhancement();
  }

  document.addEventListener("shiny:connected", function () {
    builderConnectionReady = true;
    requestClientImportSync();
    reportClientImportQueueState();
    send("builder_client_connection", {
      status: "connected",
      nonce: Date.now(),
    });
    applyBuilderActivityState();
  });
  document.addEventListener("shiny:disconnected", function () {
    var feedback = document.getElementById("builder-project-pending-feedback");
    if (feedback) feedback.remove();
    builderConnectionReady = false;
    uploadConnectionReady = false;
    setDatasetPickerOpening(false);
    importSyncPending = true;
    clientImportQueue.forEach(function (entry) {
      entry.stateBeforePause = entry.state;
      entry.state = "paused";
    });
    renderClientImportQueue();
    scheduleStatusAnnouncement(
      "Connection lost. Waiting to restore the import state."
    );
    applyBuilderActivityState();
  });
  document.addEventListener("shiny:sessioninitialized", function () {
    requestClientImportSync();
  });
  document.addEventListener("shiny:conditional", function () {
    scheduleSpatialAlignmentScrollbars();
  });
  window.addEventListener("beforeunload", function (event) {
    if (
      builderActivityState.warn_before_unload !== true &&
      builderConnectionReady
    ) return;
    event.preventDefault();
    event.returnValue = "";
  });
  function initializeBuilder() {
    setupBootstrapModalEvents();
    setupProjectRestoreEvents();
    registerBuildDialogHandler();
    registerClientImportHandlers();
    observeClientUploadProgress();
    setupWindowFileDrop();
    registerViewerGroupHandler();
    registerViewerContentHandlers();

    new MutationObserver(handleDynamicContentMutations).observe(document.documentElement, {
      childList: true,
      subtree: true,
    });

    if (narrowManager.addEventListener) {
      narrowManager.addEventListener("change", applyRailMode);
      reducedMotion.addEventListener("change", updateMotionDuration);
    } else {
      narrowManager.addListener(applyRailMode);
      reducedMotion.addListener(updateMotionDuration);
    }
    updateMotionDuration();
    window.addEventListener("resize", scheduleSpatialAlignmentScrollbars, { passive: true });
    window.addEventListener("scroll", scheduleSpatialAlignmentScrollbars, { passive: true });
    window.addEventListener("resize", scheduleWorkflowCompactState, { passive: true });
    window.addEventListener("scroll", scheduleWorkflowCompactState, { passive: true });
    ensureLiveRegion();
    enhanceDynamicContent();
    updateDatasetLoadTimes();
    scheduleDatasetLoadTimeUpdates();
    scheduleWorkflowCompactState();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeBuilder, { once: true });
  } else {
    initializeBuilder();
  }

  function flushViewerDisplay(root) {
    if (!root) return true;
    var values = {};
    var invalid = null;
    root.querySelectorAll("[data-display-field]").forEach(function (field) {
      if (!field.checkValidity() && !invalid) invalid = field;
      var key = field.dataset.displayField;
      values[key] = field.type === "checkbox" ? field.checked :
        (field.type === "number" || key === "color_clip" ? Number(field.value) : field.value);
    });
    if (invalid) { invalid.reportValidity(); return false; }
    var previous = null;
    try { previous = JSON.parse(root.dataset.displayValues || "null"); } catch (error) {}
    var keys = Object.keys(values);
    if (previous && Object.keys(previous).length === keys.length &&
        keys.every(function (key) { return values[key] === previous[key]; })) return true;
    if (send(root.dataset.inputId, {id: root.dataset.datasetId,
      epoch: Number(root.dataset.projectEpoch), values: values, nonce: Date.now()})) {
      root.dataset.displayValues = JSON.stringify(values);
      root.querySelector(".builder-display-status").textContent =
        "Viewer appearance updated. Save to keep these settings.";
    }
    return true;
  }

  document.addEventListener("change", function (event) {
    var control = event.target.closest("[data-display-field]");
    flushViewerDisplay(control && control.closest(".builder-viewer-display"));
  });

  // Native number inputs may not emit change before the action button's
  // delegated handler. Send the visible snapshot before Save/Review/Check.
  document.addEventListener("click", function (event) {
    var button = event.target.closest("#save_builder_project, #continue_to_review, #complete_dataset_check");
    if (!button || button.disabled) return;
    if (!flushViewerDisplay(document.querySelector(".builder-viewer-display"))) {
      event.preventDefault();
      event.stopImmediatePropagation();
    }
  }, true);

  document.addEventListener("click", function (event) {
    var button = event.target.closest("#open_builder_project, #save_builder_project, #confirm_builder_project_open, #continue_to_review, #complete_dataset_check");
    if (!button || button.disabled) return;
    var initialModal = document.querySelector("#shiny-modal");
    var prior = document.getElementById("builder-project-pending-feedback");
    if (prior) prior.remove();
    var feedback = document.createElement("span");
    feedback.id = "builder-project-pending-feedback";
    feedback.className = "builder-project-pending-feedback";
    feedback.setAttribute("role", "status");
    var messages = {open_builder_project: "Opening…", save_builder_project: "Preparing save…",
      confirm_builder_project_open: "Opening selected datasets…",
      continue_to_review: "Preparing review…", complete_dataset_check: "Checking settings…"};
    feedback.textContent = messages[button.id];
    if (button.closest("#shiny-modal")) feedback.dataset.inDialog = "true";
    button.insertAdjacentElement("beforebegin", feedback);
    // Some clicks open a dialog (such as the discard question before Open)
    // without changing the project phase.
    var observer = new MutationObserver(function () {
      if (!feedback.isConnected || document.querySelector("#shiny-modal") !== initialModal) {
        feedback.remove(); observer.disconnect();
      }
    });
    observer.observe(document.body, {childList: true, subtree: true});
  });

  // Text bindings may still be debouncing when a pasted path is confirmed.
  // Flush the visible value before Shiny's delegated action-button handler.
  document.addEventListener("click", function (event) {
    var button = event.target.closest("button");
    var paths = {
      use_builder_project_server_manifest: "builder_project_server_manifest",
      use_builder_project_server_folder: "builder_project_server_folder",
      use_builder_build_server_folder: "builder_build_server_folder"
    };
    var id = button && paths[button.id];
    var field = id && document.getElementById(id);
    if (field && window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue(id, field.value, {priority: "event"});
    }
  }, true);
})();
