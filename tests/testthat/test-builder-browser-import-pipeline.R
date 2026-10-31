builder_browser_pipeline_text <- function(...) {
  paste(
    readLines(builder_profile_inst_path("builder", ...), warn = FALSE),
    collapse = "\n"
  )
}

builder_browser_pipeline_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_transform.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "worker.R", "extras.R", "project.R", "build.R")) {
    sys.source(
      builder_test_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_browser_pipeline_js_function <- function(js, name) {
  marker <- paste0("function ", name, "(")
  start <- regexpr(marker, js, fixed = TRUE)[[1L]]
  if (start < 1L) {
    stop("JavaScript function not found: ", name, call. = FALSE)
  }
  candidate <- substring(js, start)
  open <- regexpr("{", candidate, fixed = TRUE)[[1L]]
  chars <- strsplit(candidate, "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (index in seq.int(open, length(chars))) {
    if (identical(chars[[index]], "{")) {
      depth <- depth + 1L
    }
    if (identical(chars[[index]], "}")) {
      depth <- depth - 1L
    }
    if (depth == 0L) {
      return(paste0(chars[seq_len(index)], collapse = ""))
    }
  }
  stop("JavaScript function is incomplete: ", name, call. = FALSE)
}

builder_browser_pipeline_run_node <- function(functions, body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c("'use strict';", functions, body), runner, useBytes = TRUE)
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("browser picker fallback satisfies the Shiny handler contract", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  picker <- builder_browser_pipeline_js_function(js, "openDatasetPicker")
  out <- builder_browser_pipeline_run_node(
    picker,
    c(
      "const assert = require('assert');",
      "assert.strictEqual(openDatasetPicker.length, 1);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  expect_match(js, '"builder_dataset_picker_fallback"', fixed = TRUE)
})

test_that("opening another project resets same-id dataset message generations", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  reset <- builder_browser_pipeline_js_function(
    js,
    "resetDatasetMessageGenerationsForActivity"
  )
  freshness <- builder_browser_pipeline_js_function(
    js,
    "datasetMessageGenerationIsFresh"
  )
  out <- builder_browser_pipeline_run_node(
    c(reset, freshness),
    c(
      "const assert = require('assert');",
      "const datasetMessageGenerations = new Map();",
      "const projectSourceLoadStates = new Map([['ds1', {status: 'loading'}]]);",
      "let builderActivityState = {phase: 'none'};",
      "assert.strictEqual(datasetMessageGenerationIsFresh({dataset: 'ds1', generation: 12}), true);",
      "assert.strictEqual(datasetMessageGenerationIsFresh({dataset: 'ds1', generation: 2}), false);",
      "resetDatasetMessageGenerationsForActivity({phase: 'opening'});",
      "assert.strictEqual(projectSourceLoadStates.size, 0);",
      "assert.strictEqual(datasetMessageGenerationIsFresh({dataset: 'ds1', generation: 2}), true);",
      "builderActivityState = {phase: 'opening'};",
      "assert.strictEqual(datasetMessageGenerationIsFresh({dataset: 'ds1', generation: 3}), true);",
      "resetDatasetMessageGenerationsForActivity({phase: 'opening'});",
      "assert.strictEqual(datasetMessageGenerationIsFresh({dataset: 'ds1', generation: 2}), false);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("top workflow navigation flushes pending text edits", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  predicate <- builder_browser_pipeline_js_function(
    js,
    "shouldFlushPendingTextEdits"
  )
  out <- builder_browser_pipeline_run_node(
    predicate,
    c(
      "const assert = require('assert');",
      "function target(selector) {",
      "  return {closest: function(list) {",
      "    return list.split(',').map(function(value) { return value.trim(); }).includes(selector) ? {} : null;",
      "  }};",
      "}",
      "assert.strictEqual(shouldFlushPendingTextEdits(target('.builder-workflow-stage-link')), true);",
      "assert.strictEqual(shouldFlushPendingTextEdits(target('.builder-pick')), true);",
      "assert.strictEqual(shouldFlushPendingTextEdits(target('#confirm_review')), true);",
      "assert.strictEqual(shouldFlushPendingTextEdits(target('.unrelated-control')), false);",
      "assert.strictEqual(shouldFlushPendingTextEdits(null), false);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  expect_match(
    js,
    "if (shouldFlushPendingTextEdits(event.target))",
    fixed = TRUE
  )
})

test_that("content actions carry a complete dataset owner", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  owner <- builder_browser_pipeline_js_function(js, "contentActionOwner")
  out <- builder_browser_pipeline_run_node(
    owner,
    c(
      "const assert = require('assert');",
      "function control(generation, projectEpoch, ownerToken) {",
      "  return {dataset: {generation, projectEpoch, ownerToken}};",
      "}",
      "assert.deepStrictEqual(contentActionOwner(control('4', '7', 'owner-a')), {",
      "  generation: 4, project_epoch: 7, owner_token: 'owner-a'",
      "});",
      "assert.strictEqual(contentActionOwner(control('4.5', '7', 'owner-a')), null);",
      "assert.strictEqual(contentActionOwner(control('4', '-1', 'owner-a')), null);",
      "assert.strictEqual(contentActionOwner(control('4', '7', '')), null);",
      "assert.strictEqual(contentActionOwner({dataset: {generation: '4'}}), null);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  expect_match(js, "generation: acknowledgeOwner.generation", fixed = TRUE)
  expect_match(
    js,
    "project_epoch: acknowledgeOwner.project_epoch",
    fixed = TRUE
  )
  expect_match(js, "owner_token: acknowledgeOwner.owner_token", fixed = TRUE)
  expect_match(js, "generation: dispositionOwner.generation", fixed = TRUE)
  expect_match(
    js,
    "project_epoch: dispositionOwner.project_epoch",
    fixed = TRUE
  )
  expect_match(js, "owner_token: dispositionOwner.owner_token", fixed = TRUE)
})

test_that("dataset revisions refresh only matching action owners", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  apply_owner <- builder_browser_pipeline_js_function(
    js,
    "applyDatasetActionOwner"
  )
  out <- builder_browser_pipeline_run_node(
    apply_owner,
    c(
      "const assert = require('assert');",
      "function owned(generation, projectEpoch, ownerToken) {",
      "  return {dataset: {generation, projectEpoch, ownerToken}};",
      "}",
      "const current = owned('4', '7', 'owner-a');",
      "const otherProject = owned('4', '8', 'owner-a');",
      "const otherSnapshot = owned('4', '7', 'owner-b');",
      "const newer = owned('9', '7', 'owner-a');",
      "function renderedDatasetId() { return 'ds1'; }",
      "const document = {querySelectorAll: function() {",
      "  return [current, otherProject, otherSnapshot, newer];",
      "}};",
      "applyDatasetActionOwner({",
      "  dataset: 'ds1', generation: 5, project_epoch: 7, owner_token: 'owner-a'",
      "});",
      "assert.strictEqual(current.dataset.generation, '5');",
      "assert.strictEqual(otherProject.dataset.generation, '4');",
      "assert.strictEqual(otherSnapshot.dataset.generation, '4');",
      "assert.strictEqual(newer.dataset.generation, '9');",
      "applyDatasetActionOwner({",
      "  dataset: 'ds1', generation: 6, project_epoch: 8, owner_token: 'owner-a'",
      "});",
      "assert.strictEqual(current.dataset.projectEpoch, '8');",
      "assert.strictEqual(current.dataset.generation, '6');",
      "assert.strictEqual(otherProject.dataset.generation, '6');",
      "assert.strictEqual(otherSnapshot.dataset.generation, '4');",
      "applyDatasetActionOwner({",
      "  dataset: 'ds2', generation: 7, project_epoch: 8, owner_token: 'owner-a'",
      "});",
      "assert.strictEqual(current.dataset.generation, '6');",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  expect_match(js, '"builder_dataset_action_owner"', fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      js,
      gregexpr("generation: actionOwner.generation", js, fixed = TRUE)
    )),
    2L
  )
})

test_that("saving an unchanged attachment name closes without a server write", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  commit <- builder_browser_pipeline_js_function(js, "commitAttachmentName")
  out <- builder_browser_pipeline_run_node(
    commit,
    c(
      "const assert = require('assert');",
      "const input = {value: '  Clinical data  ', dataset: {originalValue: 'Clinical data'}};",
      "const row = {querySelector: function(selector) {",
      "  assert.strictEqual(selector, '.enhance-attachment-editor input');",
      "  return input;",
      "}};",
      "let editing = null;",
      "let writes = 0;",
      "function setAttachmentEditing(target, value) { assert.strictEqual(target, row); editing = value; }",
      "function send() { writes += 1; }",
      "commitAttachmentName(row);",
      "assert.strictEqual(input.value, 'Clinical data');",
      "assert.strictEqual(editing, false);",
      "assert.strictEqual(writes, 0);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("retained loader publishes its marker before registration", {
  runtime <- builder_browser_pipeline_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "shiny-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  marker <- file.path(root, ".transport-retained-ds1.rds")
  bytes <- charToRaw("browser-upload-bytes")
  writeBin(bytes, source)
  events <- character()

  loaded <- runtime$builder_project_load_retained_source(
    "ds1",
    list(
      source = source,
      retained_path = retained,
      transport_retained_path = marker
    ),
    progress = NULL,
    .copy = function(from, to) {
      events <<- c(events, "copy")
      expect_identical(dirname(to), dirname(retained))
      expect_match(basename(to), "[.]part$")
      file.copy(from, to, overwrite = TRUE, copy.mode = TRUE)
    },
    .transport_writer = function(path, id, retained_path) {
      events <<- c(events, "marker")
      expect_true(file.exists(retained_path))
      expect_identical(readBin(retained_path, "raw", n = 100L), bytes)
      runtime$builder_project_transport_retained_write(
        path,
        id,
        retained_path
      )
    },
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) {
      events <<- c(events, "register")
      expect_true(file.exists(marker))
      expect_identical(
        runtime$builder_project_transport_retained_read(marker, id),
        list(
          id = id,
          retained_path = normalizePath(retained, winslash = "/")
        )
      )
      list(adapter = adapter, id = id)
    }
  )

  expect_identical(events, c("copy", "marker", "register"))
  expect_identical(
    loaded$retained_path,
    normalizePath(retained, winslash = "/")
  )
  expect_identical(readBin(retained, "raw", n = 100L), bytes)
  expect_true(file.exists(source))
  expect_identical(readBin(source, "raw", n = 100L), bytes)
  expect_true(runtime$builder_project_transport_retained_remove(marker))
  expect_null(runtime$builder_project_transport_retained_read(marker, "ds1"))
  expect_true(runtime$builder_project_transport_retained_remove(marker))
})

test_that("sources without a browser transport path publish no marker", {
  runtime <- builder_browser_pipeline_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "local-source.qs2")
  retained <- file.path(root, "session-sources", "ds1", "local-source.qs2")
  writeBin(charToRaw("local-source"), source)
  marker_writes <- 0L

  runtime$builder_project_load_retained_source(
    "ds1",
    list(
      source = source,
      retained_path = retained,
      source_origin = "local"
    ),
    progress = NULL,
    .copy = function(from, to) file.copy(from, to, overwrite = TRUE),
    .transport_writer = function(...) {
      marker_writes <<- marker_writes + 1L
      FALSE
    },
    .adapter = identity,
    .register = function(adapter, id, progress) list(path = adapter, id = id)
  )

  expect_identical(marker_writes, 0L)
  expect_true(file.exists(source))
  expect_true(file.exists(retained))
})

test_that("browser uploads expose a distinct retained transport signal", {
  foundation <- builder_browser_pipeline_text("server", "foundation.R")
  imports <- builder_browser_pipeline_text("server", "imports.R")

  expect_match(
    foundation,
    "builder_import_queue(max_active = 1L)",
    fixed = TRUE
  )
  expect_match(
    foundation,
    '"builder_client_import_transport_release"',
    fixed = TRUE
  )
  expect_match(imports, "consume_transport_retained", fixed = TRUE)
  expect_match(imports, "transport_retained_path", fixed = TRUE)
  expect_match(imports, "builder_project_transport_retained_read", fixed = TRUE)
  expect_match(
    imports,
    "builder_project_transport_retained_remove",
    fixed = TRUE
  )
})

test_that("completed imports persist the recommended expression backend", {
  imports <- builder_browser_pipeline_text("server", "imports.R")

  expect_match(imports, "builder_recommend_backend(", fixed = TRUE)
  expect_match(imports, "value$matrix_summary", fixed = TRUE)
  expect_match(
    imports,
    "settings$expression_backend <- recommendations$backend$value",
    fixed = TRUE
  )
})

test_that("browser transport handoff is identity checked and advances the queue", {
  js <- builder_browser_pipeline_text("www", "builder.js")

  expect_match(js, "function handleClientImportTransportRelease", fixed = TRUE)
  expect_match(
    js,
    "message.client_id !== activeClientImport.clientId",
    fixed = TRUE
  )
  expect_match(
    js,
    "message.server_id !== activeClientImport.serverId",
    fixed = TRUE
  )
  expect_match(js, "clientImportQueue.shift();", fixed = TRUE)
  expect_match(js, "dispatchNextClientImport();", fixed = TRUE)
  expect_match(
    js,
    '"builder_client_import_transport_release",',
    fixed = TRUE
  )
  expect_match(js, "handleClientImportTransportRelease", fixed = TRUE)
})

test_that("transport release gates three browser files by exact identity", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handler <- builder_browser_pipeline_js_function(
    js,
    "handleClientImportTransportRelease"
  )
  out <- builder_browser_pipeline_run_node(
    handler,
    c(
      "const assert = require('assert');",
      "function entry(client, server) {",
      "  return {clientId: client, serverId: server, kind: 'file', outcome: null};",
      "}",
      "const a = entry('A', 'server-A');",
      "const b = entry('B', 'server-B');",
      "const c = entry('C', 'server-C');",
      "let clientImportQueue = [a, b, c];",
      "let activeClientImport = a;",
      "let renderCount = 0;",
      "const dispatched = [];",
      "function renderClientImportQueue() { renderCount += 1; }",
      "function dispatchNextClientImport() {",
      "  if (activeClientImport || !clientImportQueue.length) return;",
      "  activeClientImport = clientImportQueue[0];",
      "  dispatched.push(activeClientImport.clientId);",
      "}",
      "handleClientImportTransportRelease({client_id: 'wrong', server_id: 'server-A'});",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'wrong'});",
      "assert.deepStrictEqual(clientImportQueue, [a, b, c]);",
      "assert.strictEqual(activeClientImport, a);",
      "const realActive = activeClientImport;",
      "activeClientImport = entry('stale', 'server-stale');",
      "handleClientImportTransportRelease({client_id: 'stale', server_id: 'server-stale'});",
      "assert.deepStrictEqual(clientImportQueue, [a, b, c]);",
      "activeClientImport = realActive;",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "assert.deepStrictEqual(clientImportQueue, [b, c]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "handleClientImportTransportRelease({client_id: 'C', server_id: 'server-C'});",
      "assert.deepStrictEqual(clientImportQueue, [b, c]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "handleClientImportTransportRelease({client_id: 'B', server_id: 'server-B'});",
      "assert.deepStrictEqual(clientImportQueue, [c]);",
      "assert.strictEqual(activeClientImport, c);",
      "assert.deepStrictEqual(dispatched, ['B', 'C']);",
      "assert.strictEqual(renderCount, 2);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("terminal and transport handlers apply different state transitions", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handlers <- c(
    builder_browser_pipeline_js_function(
      js,
      "handleClientImportTransportRelease"
    ),
    builder_browser_pipeline_js_function(js, "handleClientImportRelease")
  )
  out <- builder_browser_pipeline_run_node(
    handlers,
    c(
      "const assert = require('assert');",
      "function entry(client, server) {",
      "  return {clientId: client, serverId: server, kind: 'file', outcome: null, error: null, name: client};",
      "}",
      "let clientImportFailures = [];",
      "let clientImportQueue = [entry('A', 'server-A'), entry('B', 'server-B')];",
      "let activeClientImport = clientImportQueue[0];",
      "const transported = activeClientImport;",
      "function renderClientImportQueue() {}",
      "function scheduleStatusAnnouncement() {}",
      "function dispatchNextClientImport() {",
      "  if (!activeClientImport && clientImportQueue.length) activeClientImport = clientImportQueue[0];",
      "}",
      "handleClientImportTransportRelease({client_id: 'A', server_id: 'server-A'});",
      "assert.strictEqual(transported.outcome, null);",
      "assert.strictEqual(clientImportFailures.length, 0);",
      "assert.strictEqual(activeClientImport.clientId, 'B');",
      "const terminal = entry('X', 'server-X');",
      "clientImportQueue = [terminal, entry('Y', 'server-Y')];",
      "activeClientImport = terminal;",
      "handleClientImportRelease({client_id: 'X', server_id: 'server-X', outcome: 'error', message: 'failed'});",
      "assert.strictEqual(terminal.outcome, 'error');",
      "assert.strictEqual(terminal.error, 'failed');",
      "assert.strictEqual(activeClientImport.clientId, 'Y');",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("sync replay of retained transport advances the browser queue", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  handlers <- c(
    builder_browser_pipeline_js_function(
      js,
      "handleClientImportTransportRelease"
    ),
    builder_browser_pipeline_js_function(js, "handleClientImportSync")
  )
  out <- builder_browser_pipeline_run_node(
    handlers,
    c(
      "const assert = require('assert');",
      "const a = {clientId: 'A', serverId: null, kind: 'file', state: 'paused'};",
      "const b = {clientId: 'B', serverId: null, kind: 'file', state: 'queued'};",
      "let clientImportQueue = [a, b];",
      "let activeClientImport = a;",
      "let importSyncPending = true;",
      "let serverImportGate = false;",
      "const dispatched = [];",
      "function renderClientImportQueue() {}",
      "function dispatchNextClientImport() {",
      "  if (activeClientImport || !clientImportQueue.length) return;",
      "  activeClientImport = clientImportQueue[0];",
      "  dispatched.push(activeClientImport.clientId);",
      "}",
      "function failClientImportReconciliation() { throw new Error('unexpected reconciliation failure'); }",
      "function handleClientImportRelease() { throw new Error('unexpected terminal release'); }",
      "function startFileTransport() { throw new Error('unexpected upload restart'); }",
      "handleClientImportSync({imports: [{",
      "  client_id: 'A', server_id: 'server-A', state: 'reading', transport_retained: true",
      "}], server_busy: false});",
      "assert.strictEqual(importSyncPending, false);",
      "assert.deepStrictEqual(clientImportQueue, [b]);",
      "assert.strictEqual(activeClientImport, b);",
      "assert.deepStrictEqual(dispatched, ['B']);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
})

test_that("terminal and transport releases remain separate browser contracts", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  transport <- regmatches(
    js,
    regexpr(
      "function handleClientImportTransportRelease[\\s\\S]*?\\n  }",
      js,
      perl = TRUE
    )
  )
  terminal <- regmatches(
    js,
    regexpr(
      "function handleClientImportRelease[\\s\\S]*?\\n  }",
      js,
      perl = TRUE
    )
  )

  expect_match(transport, "dispatchNextClientImport", fixed = TRUE)
  expect_false(grepl("message.outcome", transport, fixed = TRUE))
  expect_match(terminal, "message.outcome", fixed = TRUE)
})

test_that("retained loader uses clone-aware copy with an atomic part file", {
  project <- builder_browser_pipeline_text("project.R")
  loader <- regmatches(
    project,
    regexpr(
      "builder_project_load_retained_source <- function[\\s\\S]*?\\n}",
      project,
      perl = TRUE
    )
  )

  expect_match(loader, ".copy = builder_project_copy_file", fixed = TRUE)
  expect_match(loader, 'fileext = ".part"', fixed = TRUE)
  expect_match(loader, "file.rename(part, target)", fixed = TRUE)
  # Only a Builder-owned browser upload may be moved instead of copied.
  expect_match(
    loader,
    paste0(
      "moved <- isTRUE(source$transport_owned) &&
",
      "      isTRUE(suppressWarnings(file.rename(source$source, target)))"
    ),
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(
      loader,
      gregexpr("file.rename(source$source", loader, fixed = TRUE)
    )),
    1L
  )
  expect_false(grepl("unlink(source$source", loader, fixed = TRUE))

  imports <- builder_browser_pipeline_text("server", "imports.R")
  expect_match(
    imports,
    "transport_owned = builder_upload_transport_owned(path)",
    fixed = TRUE
  )
  expect_match(
    imports,
    paste0(
      "transport_owned <- isTRUE(retain_in_worker) &&
",
      "    isTRUE(file_meta$transport_owned)"
    ),
    fixed = TRUE
  )
})

test_that("local sources do not opt into browser transport retention", {
  imports <- builder_browser_pipeline_text("server", "imports.R")

  expect_match(
    imports,
    '!identical(source_origin, "local")',
    fixed = TRUE
  )
  expect_match(
    imports,
    "if (isTRUE(retain_in_worker) && builder_has_text(progress_path))",
    fixed = TRUE
  )
})

## A minimal DOM for the rail and Build helpers below: elements keep children,
## classes and text, and `document` answers exactly the selectors they use.
builder_browser_pipeline_fake_dom <- c(
  "class El {",
  "  constructor(tag, className = '', text = '') {",
  "    this.tagName = tag; this.className = className; this.textContent = text;",
  "    this.children = []; this.parentElement = null; this.attributes = {};",
  "    this.hidden = false; this.disabled = false; this.id = '';",
  "  }",
  "  appendChild(child) { child.remove(); child.parentElement = this; this.children.push(child); return child; }",
  "  prepend(child) { child.remove(); child.parentElement = this; this.children.unshift(child); }",
  "  remove() { const p = this.parentElement; if (p) { p.children = p.children.filter(c => c !== this); this.parentElement = null; } }",
  "  setAttribute(name, value) { this.attributes[name] = String(value); }",
  "  hasClass(name) { return this.className.split(/\\s+/).includes(name); }",
  "  closest(selector) { let node = this; while (node) { if (node.hasClass(selector.slice(1))) return node; node = node.parentElement; } return null; }",
  "  querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }",
  "  querySelectorAll(selector) {",
  "    const scoped = selector.match(/^:scope > \\.([\\w-]+)$/);",
  "    if (scoped) return this.children.filter(c => c.hasClass(scoped[1]));",
  "    if (selector === ':scope > .ds > .ds-idx') return this.children.filter(c => c.hasClass('ds')).flatMap(c => c.children.filter(i => i.hasClass('ds-idx')));",
  "    const plain = selector.match(/^\\.([\\w-]+)$/);",
  "    if (plain) return descendants(this).filter(c => c.hasClass(plain[1]));",
  "    throw new Error('Unsupported selector: ' + selector);",
  "  }",
  "}",
  "function descendants(node) { return node.children.flatMap(c => [c, ...descendants(c)]); }",
  "const body = new El('body');",
  "function byId(id) { return descendants(body).find(n => n.id === id) || null; }",
  "function withId(node, id) { node.id = id; return node; }",
  "const document = {",
  "  getElementById: byId,",
  "  createElement: tag => new El(tag),",
  "  querySelectorAll(selector) {",
  "    if (selector === '#ds_ready_list > .ds, #ds_import_list .ds') {",
  "      const ready = byId('ds_ready_list'); const queue = byId('ds_import_list');",
  "      return [...(ready ? ready.children.filter(c => c.hasClass('ds')) : []),",
  "              ...(queue ? descendants(queue).filter(c => c.hasClass('ds')) : [])];",
  "    }",
  "    if (selector === '[data-build-progress-detail]') return descendants(body).filter(n => n.attributes['data-build-progress-detail'] !== undefined);",
  "    return body.querySelectorAll(selector);",
  "  },",
  "};",
  "function row(label) { const r = new El('div', 'ds'); r.appendChild(new El('span', 'ds-idx', label)); return r; }"
)

test_that("waiting browser imports keep numbering after the rows above change", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  renumber <- builder_browser_pipeline_js_function(
    js,
    "renumberClientImportQueue"
  )
  out <- builder_browser_pipeline_run_node(
    c(builder_browser_pipeline_fake_dom, renumber),
    c(
      "const assert = require('assert');",
      "const ready = body.appendChild(withId(new El('div'), 'ds_ready_list'));",
      "const imports = body.appendChild(withId(new El('div'), 'ds_import_list'));",
      "const waiting = body.appendChild(withId(new El('div'), 'ds_client_import_queue'));",
      "ready.appendChild(row('1'));",
      "waiting.appendChild(row('2')); waiting.appendChild(row('3'));",
      "const labels = () => waiting.children.map(r => r.children[0].textContent);",
      "renumberClientImportQueue();",
      "assert.deepStrictEqual(labels(), ['2', '3']);",
      "// The first waiting file started loading: its server row now sits above.",
      "const list = imports.appendChild(new El('div', 'builder-import-list'));",
      "list.appendChild(row('2'));",
      "waiting.children[0].remove();",
      "renumberClientImportQueue();",
      "assert.deepStrictEqual(labels(), ['3']);",
      "// An example started while two dropped files wait behind it.",
      "waiting.appendChild(row('?'));",
      "list.appendChild(row('3'));",
      "renumberClientImportQueue();",
      "assert.deepStrictEqual(labels(), ['4', '5']);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  for (caller in c(
    "reconcileDatasetRail",
    "reconcileImportRail",
    "renderClientImportQueue"
  )) {
    expect_match(
      builder_browser_pipeline_js_function(js, caller),
      "renumberClientImportQueue();",
      fixed = TRUE,
      info = caller
    )
  }
})

test_that("Build actions say why they wait for a Project write", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  reason <- builder_browser_pipeline_js_function(js, "applyBuildBlockedReason")
  out <- builder_browser_pipeline_run_node(
    c(builder_browser_pipeline_fake_dom, reason),
    c(
      "const assert = require('assert');",
      "let builderConnectionReady = true;",
      "let capabilities = { build: false };",
      "function activityCapability(name) { return capabilities[name] === true; }",
      "let builderActivityState = { build_reason: 'Wait for the dataset cache to finish saving.' };",
      "const footer = body.appendChild(new El('footer', 'builder-stage-footer'));",
      "const actions = footer.appendChild(new El('div', 'builder-stage-footer-actions'));",
      "actions.appendChild(withId(new El('button'), 'build_same_folder'));",
      "actions.appendChild(withId(new El('button'), 'build'));",
      "const notes = () => body.querySelectorAll('.builder-build-blocked-reason');",
      "applyBuildBlockedReason();",
      "assert.strictEqual(notes().length, 1);",
      "assert.strictEqual(actions.children[0], notes()[0]);",
      "assert.strictEqual(notes()[0].textContent, 'Wait for the dataset cache to finish saving.');",
      "assert.strictEqual(notes()[0].attributes.role, 'status');",
      "applyBuildBlockedReason();",
      "assert.strictEqual(notes().length, 1);",
      "capabilities.build = true;",
      "applyBuildBlockedReason();",
      "assert.strictEqual(notes().length, 0);",
      "// No Build actions on the page: nothing to annotate.",
      "capabilities.build = false; footer.remove();",
      "applyBuildBlockedReason();",
      "assert.strictEqual(notes().length, 0);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  apply <- builder_browser_pipeline_js_function(js, "applyBuilderActivityState")
  expect_match(
    apply,
    'build_same_folder: activityCapability("build")',
    fixed = TRUE
  )
  expect_match(apply, "applyBuildBlockedReason();", fixed = TRUE)
  expect_match(js, "build_reason: typeof message.build_reason", fixed = TRUE)
})

test_that("Build actions stay disabled while the workspace stays editable", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  lock <- builder_browser_pipeline_js_function(js, "setActivityDisabled")
  locked <- builder_browser_pipeline_js_function(js, "workspaceControlLocked")
  out <- builder_browser_pipeline_run_node(
    c(builder_browser_pipeline_fake_dom, lock, locked),
    c(
      "const assert = require('assert');",
      "function control(id) {",
      "  const node = withId(new El('button'), id);",
      "  node.disabled = false;",
      "  node.hasAttribute = name => name in node.attributes;",
      "  node.getAttribute = name => node.attributes[name] ?? null;",
      "  node.removeAttribute = name => { delete node.attributes[name]; };",
      "  return node;",
      "}",
      "const build = control('build');",
      "const again = control('build_same_folder');",
      "const field = control('core-name');",
      "// One activity pass during dataset-cache registration: edit_dataset is",
      "// still allowed, build is not.",
      "function apply(capabilities) {",
      "  const projectControls = {",
      "    build: capabilities.build, build_same_folder: capabilities.build",
      "  };",
      "  [build, again].forEach(c => setActivityDisabled(c, !projectControls[c.id]));",
      "  const workspaceLocked = !capabilities.edit_dataset;",
      "  [build, again, field].forEach(c => setActivityDisabled(",
      "    c, workspaceControlLocked(c, projectControls, workspaceLocked)",
      "  ));",
      "}",
      "apply({ build: false, edit_dataset: true });",
      "for (const c of [build, again]) {",
      "  assert.strictEqual(c.disabled, true, c.id);",
      "  assert.strictEqual(c.attributes['aria-disabled'], 'true', c.id);",
      "}",
      "assert.strictEqual(field.disabled, false);",
      "// Registration ended: both Build actions come back.",
      "apply({ build: true, edit_dataset: true });",
      "for (const c of [build, again]) {",
      "  assert.strictEqual(c.disabled, false, c.id);",
      "  assert.strictEqual(c.attributes['aria-disabled'], 'false', c.id);",
      "}",
      "// A locked workspace still locks everything.",
      "apply({ build: true, edit_dataset: false });",
      "assert.strictEqual(build.disabled, true);",
      "assert.strictEqual(field.disabled, true);",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  apply <- builder_browser_pipeline_js_function(js, "applyBuilderActivityState")
  expect_match(
    apply,
    "workspaceControlLocked(control, projectControls, workspaceLocked)",
    fixed = TRUE
  )
})

test_that("the Build card hides the previous step's dataset line", {
  js <- builder_browser_pipeline_text("www", "builder.js")
  render <- builder_browser_pipeline_js_function(
    js,
    "renderBuildProgressDetail"
  )
  out <- builder_browser_pipeline_run_node(
    c(builder_browser_pipeline_fake_dom, render),
    c(
      "const assert = require('assert');",
      "function formatBuildElapsed() { return '0:05'; }",
      "const buildProgressState = { active: true, anchor: 0, phase: 'datasets',",
      "  text: 'Dataset 4/4 · Spatial · Writing CRB' };",
      "let builderActivityState = { busy_phase: 'datasets' };",
      "const detail = body.appendChild(new El('div'));",
      "detail.setAttribute('data-build-progress-detail', '');",
      "const label = detail.appendChild(new El('span', 'builder-build-progress-text'));",
      "const clock = detail.appendChild(new El('span', 'builder-build-progress-elapsed'));",
      "renderBuildProgressDetail();",
      "assert.strictEqual(label.textContent, 'Dataset 4/4 · Spatial · Writing CRB');",
      "// The header moved to Packaging Viewer before the matching detail arrived.",
      "builderActivityState.busy_phase = 'viewer';",
      "renderBuildProgressDetail();",
      "assert.strictEqual(label.textContent, '');",
      "assert.strictEqual(clock.textContent, '0:05');",
      "assert.strictEqual(detail.hidden, false);",
      "buildProgressState.phase = 'viewer'; buildProgressState.text = 'Packaging Viewer';",
      "renderBuildProgressDetail();",
      "assert.strictEqual(label.textContent, 'Packaging Viewer');",
      "console.log('ok');"
    )
  )

  expect_equal(attr(out, "status"), NULL)
  expect_identical(out, "ok")
  handler <- builder_browser_pipeline_js_function(js, "handleBuildProgress")
  expect_match(
    handler,
    "buildProgressState.phase = typeof message.phase",
    fixed = TRUE
  )
})
