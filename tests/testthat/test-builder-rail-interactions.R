## Dataset rail and import interactions, checked against a real browser DOM
## where the behaviour lives in builder.js.

builder_rail_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  runtime
}

builder_rail_js <- function() {
  paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

## The balanced `{ ... }` block that starts at `marker`.
builder_rail_js_block <- function(js, marker, suffix = "") {
  start <- regexpr(marker, js, fixed = TRUE)[[1L]]
  if (start < 1L) {
    stop("JavaScript definition not found: ", marker, call. = FALSE)
  }
  candidate <- substring(js, start)
  chars <- strsplit(candidate, "", fixed = TRUE)[[1L]]
  open <- regexpr("{", candidate, fixed = TRUE)[[1L]]
  depth <- 0L
  for (index in seq.int(open, length(chars))) {
    if (identical(chars[[index]], "{")) {
      depth <- depth + 1L
    }
    if (identical(chars[[index]], "}")) {
      depth <- depth - 1L
    }
    if (depth == 0L) {
      return(paste0(paste0(chars[seq_len(index)], collapse = ""), suffix))
    }
  }
  stop("JavaScript definition is incomplete: ", marker, call. = FALSE)
}

builder_rail_js_functions <- function(
  js,
  functions = character(),
  vars = character()
) {
  c(
    vapply(
      vars,
      function(name) {
        builder_rail_js_block(js, paste0("var ", name, " = "), ";")
      },
      character(1)
    ),
    vapply(
      functions,
      function(name) builder_rail_js_block(js, paste0("function ", name, "(")),
      character(1)
    )
  )
}

## Run `body` (statements ending in `return <value>;`) in a blank Chrome page
## and return its JSON-compatible value.
builder_rail_dom_run <- function(definitions, body) {
  testthat::skip_if_not_installed("chromote")
  session <- tryCatch(
    chromote::ChromoteSession$new(),
    error = function(error) NULL
  )
  testthat::skip_if(is.null(session), "Chrome is not available")
  on.exit(try(session$close(), silent = TRUE), add = TRUE)
  script <- paste(
    c("(() => {", "'use strict';", definitions, body, "})()"),
    collapse = "\n"
  )
  result <- session$Runtime$evaluate(script, returnByValue = TRUE)
  if (!is.null(result$exceptionDetails)) {
    stop(
      result$exceptionDetails$exception$description %||%
        result$exceptionDetails$text,
      call. = FALSE
    )
  }
  result$result$value
}

builder_rail_import_model <- function(...) {
  utils::modifyList(
    list(
      index = 1L,
      id = "import-a",
      label = "A rather long dataset name for the rail",
      detail = "dataset.qs2 · QS2",
      load_state = "reading",
      progress_label = "Reading dataset…",
      progress_percent = 10L,
      cancelling = FALSE,
      error = NULL,
      started_at_ms = 1000,
      import_elapsed_ms = NA_real_,
      selected = FALSE
    ),
    list(...),
    keep.null = TRUE
  )
}

builder_rail_import_message <- function(runtime, models) {
  as.character(jsonlite::toJSON(
    list(
      rows = lapply(models, function(model) {
        list(
          id = model$id,
          fingerprint = runtime$builder_import_rail_row_fingerprint(model),
          html = htmltools::renderTags(
            runtime$builder_import_rail_row_ui(model)
          )$html
        )
      })
    ),
    auto_unbox = TRUE
  ))
}

test_that("import progress keeps the row structure fingerprint", {
  withr::local_package("shiny")
  runtime <- builder_rail_runtime()
  structure <- function(...) {
    runtime$builder_import_rail_row_structure(builder_rail_import_model(...))
  }
  base <- structure()

  expect_identical(structure(progress_percent = 42L), base)
  expect_identical(structure(progress_label = "Checking cells…"), base)
  expect_identical(structure(load_state = "inspecting"), base)
  expect_identical(structure(index = 3L, selected = TRUE), base)
  expect_identical(structure(started_at_ms = 2000), base)
  expect_false(identical(structure(cancelling = TRUE), base))
  expect_false(identical(structure(load_state = "queued"), base))
  expect_false(identical(
    structure(load_state = "error", error = "Unreadable"),
    base
  ))
  expect_false(identical(
    structure(started_at_ms = NA_real_, import_elapsed_ms = NA_real_),
    base
  ))
  html <- htmltools::renderTags(
    runtime$builder_import_rail_row_ui(builder_rail_import_model())
  )$html
  expect_match(html, "data-import-structure=", fixed = TRUE)
})

test_that("import progress ticks update the row in place", {
  withr::local_package("shiny")
  runtime <- builder_rail_runtime()
  js <- builder_rail_js()
  definitions <- builder_rail_js_functions(
    js,
    functions = c(
      "syncElementAttributes",
      "patchRailRowInPlace",
      "placeRailRow",
      "railFocusIdentity",
      "railFocusTarget",
      "parseRailElement",
      "railSnapshot",
      "removeReadyImportOverlap",
      "reconcileImportRail"
    ),
    vars = "importRailContract"
  )
  first <- builder_rail_import_model()
  second <- builder_rail_import_model(id = "import-b", index = 2L, label = "B")
  messages <- c(
    start = builder_rail_import_message(runtime, list(first, second)),
    tick = builder_rail_import_message(
      runtime,
      list(
        builder_rail_import_model(progress_percent = 42L),
        second
      )
    ),
    phase = builder_rail_import_message(
      runtime,
      list(
        builder_rail_import_model(
          load_state = "inspecting",
          progress_label = "Checking cells, genes and metadata…",
          progress_percent = NULL
        ),
        second
      )
    ),
    cancelling = builder_rail_import_message(
      runtime,
      list(
        builder_rail_import_model(
          load_state = "inspecting",
          cancelling = TRUE,
          progress_label = "Cancelling…",
          progress_percent = NULL
        ),
        second
      )
    ),
    finished = builder_rail_import_message(
      runtime,
      list(
        utils::modifyList(second, list(index = 1L))
      )
    )
  )
  out <- builder_rail_dom_run(
    c(
      "document.body.innerHTML = '<div id=\"ds_ready_list\"></div><div id=\"ds_import_list\"></div>';",
      "function renumberClientImportQueue() {}",
      "function applyDatasetMutationLock() {}",
      "function updateRailSummary() {}",
      "function updateDatasetLoadTimes() {}",
      "function scheduleDatasetLoadTimeUpdates() {}",
      definitions
    ),
    c(
      sprintf(
        "const messages = {%s};",
        paste0(names(messages), ": ", messages, collapse = ", ")
      ),
      "const host = document.getElementById('ds_import_list');",
      "const rowOf = id => host.querySelector('.ds[data-import-id=\"' + id + '\"]');",
      "reconcileImportRail(messages.start);",
      "const row = rowOf('import-a');",
      "const cancel = row.querySelector('.builder-remove-import');",
      "const other = rowOf('import-b');",
      "cancel.focus();",
      "const out = {};",
      "reconcileImportRail(messages.tick);",
      "out.tickSameRow = rowOf('import-a') === row;",
      "out.tickSameButton = row.querySelector('.builder-remove-import') === cancel;",
      "out.tickFocus = document.activeElement === cancel;",
      "out.tickStatus = row.querySelector('.builder-import-status').textContent;",
      "out.tickValue = row.querySelector('.builder-import-status').getAttribute('aria-valuenow');",
      "out.tickFingerprint = row.dataset.importFingerprint === messages.tick.rows[0].fingerprint;",
      "reconcileImportRail(messages.phase);",
      "out.phaseSameButton = rowOf('import-a').querySelector('.builder-remove-import') === cancel;",
      "out.phaseState = rowOf('import-a').dataset.loadState;",
      "out.phaseStatus = row.querySelector('.builder-import-status').textContent;",
      "out.phaseRole = row.querySelector('.builder-import-status').hasAttribute('role');",
      "reconcileImportRail(messages.cancelling);",
      "out.cancelReplaced = rowOf('import-a') !== row;",
      "out.cancelDisabled = rowOf('import-a').querySelector('.builder-remove-import').disabled;",
      "out.otherKept = rowOf('import-b') === other;",
      "reconcileImportRail(messages.finished);",
      "out.finishedRows = host.querySelectorAll('.ds').length;",
      "out.finishedKept = rowOf('import-b') === other;",
      "out.finishedIndex = other.querySelector('.ds-idx').textContent;",
      "return out;"
    )
  )

  expect_true(out$tickSameRow)
  expect_true(out$tickSameButton)
  expect_true(out$tickFocus)
  expect_identical(out$tickStatus, "Reading dataset… 42%")
  expect_identical(out$tickValue, "42")
  expect_true(out$tickFingerprint)
  expect_true(out$phaseSameButton)
  expect_identical(out$phaseState, "inspecting")
  expect_identical(out$phaseStatus, "Checking cells, genes and metadata…")
  expect_false(out$phaseRole)
  expect_true(out$cancelReplaced)
  expect_true(out$cancelDisabled)
  expect_true(out$otherKept)
  expect_identical(out$finishedRows, 1L)
  expect_true(out$finishedKept)
  expect_identical(out$finishedIndex, "1")
})

test_that("waiting browser imports keep their rows while the queue changes", {
  js <- builder_rail_js()
  definitions <- builder_rail_js_functions(
    js,
    functions = c(
      "syncElementAttributes",
      "patchRailRowInPlace",
      "placeRailRow",
      "clientQueueStatus",
      "clientQueueRow",
      "renderClientImportQueue",
      "renumberClientImportQueue"
    ),
    vars = "clientQueueRowContract"
  )
  out <- builder_rail_dom_run(
    c(
      "document.body.innerHTML = '<div id=\"ds_ready_list\"></div><div id=\"ds_import_list\"></div><div id=\"ds_client_import_queue\"></div>';",
      "let clientImportQueue = [];",
      "let clientImportFailures = [];",
      "let activeClientImport = null;",
      "function reportClientImportQueueState() {}",
      "function applyBuilderActivityState() {}",
      "function applyClientImportQueueLock() {}",
      definitions
    ),
    c(
      "const entry = (id, state) => ({ clientId: id, kind: 'file', name: id + '.qs2', state: state, serverId: null });",
      "const a = entry('a', 'uploading'), b = entry('b', 'queued'), c = entry('c', 'queued');",
      "clientImportQueue = [a, b, c]; activeClientImport = a;",
      "const host = document.getElementById('ds_client_import_queue');",
      "const rowOf = id => host.querySelector('[data-client-import-id=\"' + id + '\"]');",
      "renderClientImportQueue();",
      "const rowB = rowOf('b'), cancelB = rowB.querySelector('button'), rowC = rowOf('c');",
      "cancelB.focus();",
      "const out = {};",
      "a.uploadPercent = 60;",
      "renderClientImportQueue();",
      "out.sameRow = rowOf('b') === rowB;",
      "out.sameButton = rowB.querySelector('button') === cancelB;",
      "out.focus = document.activeElement === cancelB;",
      "out.statusA = rowOf('a').querySelector('.builder-import-status').textContent;",
      "clientImportQueue = [b, c]; activeClientImport = b; b.state = 'awaiting_dispatch';",
      "renderClientImportQueue();",
      "out.rows = host.children.length;",
      "out.activeRowReplaced = rowOf('b') !== rowB;",
      "out.keptC = rowOf('c') === rowC;",
      "out.statusC = rowC.querySelector('.builder-import-status').textContent;",
      "out.order = Array.from(host.children, row => row.dataset.clientImportId);",
      "return out;"
    )
  )

  expect_true(out$sameRow)
  expect_true(out$sameButton)
  expect_true(out$focus)
  expect_identical(out$statusA, "Uploading… 60%")
  expect_identical(out$rows, 2L)
  expect_true(out$activeRowReplaced)
  expect_true(out$keptC)
  expect_identical(out$statusC, "Waiting · 2 in queue")
  expect_identical(out$order, list("b", "c"))
})

test_that("the import timer leaves room for the dataset name", {
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.components.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  start <- regexpr(
    ".ds--import:has(.builder-load-time) .nm {",
    css,
    fixed = TRUE
  )[[1L]]
  expect_gt(start, 0L)
  rule <- substring(css, start)
  rule <- substring(rule, 1L, regexpr("}", rule, fixed = TRUE)[[1L]])

  expect_match(rule, "padding-right: 3rem;", fixed = TRUE)
  expect_match(
    css,
    ".ds--import .builder-load-time {\n  position: absolute;",
    fixed = TRUE
  )
})

test_that("rail move buttons are named icons and Remove keeps its label", {
  withr::local_package("shiny")
  runtime <- builder_rail_runtime()
  html <- htmltools::renderTags(runtime$builder_dataset_rail_row_ui(list(
    index = 2L,
    id = "dataset-b",
    label = "Dataset B",
    cells = 180L,
    format = "qs2",
    import_elapsed_ms = 1200,
    readiness_label = "Ready",
    checked = FALSE,
    selected = TRUE,
    confirm = FALSE,
    can_up = TRUE,
    can_down = TRUE
  )))$html
  buttons <- regmatches(
    html,
    gregexpr(
      "<button class=\"ds-(move|del)[^>]*>[\\s\\S]*?</button>",
      html,
      perl = TRUE
    )
  )[[1L]]

  expect_length(buttons, 3L)
  expect_match(buttons[[1L]], 'title="Move up"', fixed = TRUE)
  expect_match(buttons[[1L]], 'aria-label="Move Dataset B up"', fixed = TRUE)
  expect_match(buttons[[1L]], 'data-icon="up"', fixed = TRUE)
  expect_match(buttons[[2L]], 'title="Move down"', fixed = TRUE)
  expect_match(buttons[[2L]], 'aria-label="Move Dataset B down"', fixed = TRUE)
  expect_false(any(grepl("ds-action-label", buttons[1:2], fixed = TRUE)))
  expect_false(grepl(">Move", html, fixed = TRUE))
  expect_match(
    buttons[[3L]],
    '<span class="ds-action-label">Remove</span>',
    fixed = TRUE
  )
})

test_that("files dropped anywhere in the window are added as datasets", {
  js <- builder_rail_js()
  definitions <- builder_rail_js_functions(
    js,
    functions = c(
      "dragCarriesFiles",
      "fileInputOwnsDrop",
      "windowFileDropAllowed",
      "windowFileDropOverlay",
      "setWindowFileDragActive",
      "handleWindowFileDrag",
      "handleWindowFileDrop",
      "setupWindowFileDrop"
    ),
    vars = "windowFileDrag"
  )
  out <- builder_rail_dom_run(
    c(
      paste0(
        "document.body.innerHTML = '",
        "<button class=\"builder-add-datasets\" id=\"add\">Add files</button>",
        "<section id=\"configure\"><p id=\"text\">Configure</p></section>",
        "<div class=\"shiny-input-container\" id=\"tables\">",
        "<input type=\"file\"><span id=\"tables-zone\">Drop tables</span></div>",
        "<button class=\"builder-dataset-dropzone\" id=\"zone\"></button>';"
      ),
      "let allowed = true;",
      "let datasetMutationsLocked = false;",
      "const added = [];",
      "function activityCapability(name) { return name === 'add_dataset' && allowed; }",
      "function enqueueClientFiles(files) { added.push(...Array.from(files, file => file.name)); }",
      definitions
    ),
    c(
      "setupWindowFileDrop();",
      "const files = name => { const data = new DataTransfer(); data.items.add(new File(['x'], name)); return data; };",
      "const fire = (id, type, data) => { const event = new DragEvent(type, { bubbles: true, cancelable: true, dataTransfer: data }); document.getElementById(id).dispatchEvent(event); return event.defaultPrevented; };",
      "const overlay = () => { const node = document.querySelector('.builder-window-drop'); return Boolean(node && !node.hidden); };",
      "const zone = () => document.getElementById('zone').classList.contains('is-drag-over');",
      "const out = {};",
      "out.overClaimed = fire('text', 'dragover', files('a.qs2'));",
      "out.overlayShown = overlay();",
      "out.zoneHighlighted = zone();",
      "out.dropClaimed = fire('text', 'drop', files('a.qs2'));",
      "out.overlayAfterDrop = overlay();",
      "out.zoneAfterDrop = zone();",
      "fire('add', 'drop', files('b.rds'));",
      "const text = new DataTransfer(); text.setData('text/plain', 'hello');",
      "out.textClaimed = fire('text', 'dragover', text) || fire('text', 'drop', text);",
      "out.textOverlay = overlay();",
      "out.inputClaimed = fire('tables-zone', 'dragover', files('t.csv')) || fire('tables-zone', 'drop', files('t.csv'));",
      "allowed = false;",
      "out.lockedClaimed = fire('text', 'dragover', files('c.qs2')) && fire('text', 'drop', files('c.qs2'));",
      "out.lockedOverlay = overlay();",
      "out.added = added;",
      "return out;"
    )
  )

  expect_true(out$overClaimed)
  expect_true(out$overlayShown)
  expect_true(out$zoneHighlighted)
  expect_true(out$dropClaimed)
  expect_false(out$overlayAfterDrop)
  expect_false(out$zoneAfterDrop)
  expect_false(out$textClaimed)
  expect_false(out$textOverlay)
  expect_false(out$inputClaimed)
  expect_true(out$lockedClaimed)
  expect_false(out$lockedOverlay)
  expect_identical(out$added, list("a.qs2", "b.rds"))
  expect_false(grepl("function setupDatasetDropzones(", js, fixed = TRUE))
})
