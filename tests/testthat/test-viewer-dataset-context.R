dataset_context_javascript <- viewer_test_path("www", "dataset-context.js")

test_that("the context gate loads before every consumer", {
  ui <- paste(
    readLines(viewer_test_path("shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  gate_at <- regexpr('cerebro_js("dataset-context.js")', ui, fixed = TRUE)[1L]
  cell_views_at <- regexpr('cerebro_js("cell_views.js")', ui, fixed = TRUE)[1L]
  specialist_at <- regexpr(
    'cerebro_js("specialist-view-state.js"',
    ui,
    fixed = TRUE
  )[1L]

  expect_true(gate_at > 0L)
  expect_true(cell_views_at > gate_at)
  expect_true(specialist_at > gate_at)
})

run_dataset_context_node <- function(body) {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_true(
    file.exists(dataset_context_javascript),
    info = "dataset-context.js not found"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const events = []; const handlers = {}; const inputs = [];",
      "global.window = global;",
      "global.document = {addEventListener: () => {}};",
      "global.CustomEvent = function(type, options) {",
      "  this.type = type; this.detail = options && options.detail;",
      "};",
      "window.dispatchEvent = event => { events.push(event); return true; };",
      "window.setTimeout = () => 0;",
      "global.Shiny = window.Shiny = {",
      "  addCustomMessageHandler: (name, handler) => { handlers[name] = handler; },",
      "  setInputValue: (name, value) => { inputs.push({name:name,value:value}); }",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(dataset_context_javascript, quote = "\"")
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("dataset context freshness is epoch/key/generation exact", {
  output <- run_dataset_context_node(c(
    "const C = window.CerebroDatasetContext;",
    "const a1 = {epoch:'session-1',dataset_key:'A',generation:1};",
    "const b2 = {epoch:'session-1',dataset_key:'B',generation:2};",
    "const a3 = {epoch:'session-1',dataset_key:'A',generation:3};",
    "const nextEpoch = {epoch:'session-2',dataset_key:'A',generation:1};",
    "const message = context => ({dataset_context:context});",
    "const result = {};",
    "result.pendingAccepted = C.receive({phase:'pending',dataset_context:a1});",
    "result.pendingIsFresh = C.accepts(message(a1));",
    "result.readyAccepted = C.receive({phase:'ready',dataset_context:a1});",
    "result.a1Fresh = C.accepts(message(a1));",
    "result.missingFresh = C.accepts({});",
    "C.receive({phase:'ready',dataset_context:b2});",
    "result.a1AfterB = C.accepts(message(a1));",
    "result.b2Fresh = C.accepts(message(b2));",
    "C.receive({phase:'ready',dataset_context:a3});",
    "result.a1AfterA3 = C.accepts(message(a1));",
    "result.b2AfterA3 = C.accepts(message(b2));",
    "result.a3Fresh = C.accepts(message(a3));",
    "result.oldControlRejected = !C.receive({phase:'ready',dataset_context:b2});",
    "C.beginConnection();",
    "result.reconnectPending = C.phase() === 'pending';",
    "result.reconnectBlocksOld = !C.accepts(message(a3));",
    "result.syncRequested = inputs.some(item => item.name === 'viewer_dataset_context_sync');",
    "C.receive({phase:'ready',dataset_context:nextEpoch});",
    "result.oldEpochFresh = C.accepts(message(a3));",
    "result.nextEpochFresh = C.accepts(message(nextEpoch));",
    "result.retiredEpochRejected = !C.receive({phase:'ready',dataset_context:{epoch:'session-1',dataset_key:'A',generation:4}});",
    "const beforeInvalid = C.snapshot().key;",
    "result.invalidReceive = C.receive({phase:'ready'});",
    "result.invalidPreserved = beforeInvalid === C.snapshot().key;",
    "console.log(JSON.stringify(result));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      pendingAccepted = TRUE,
      pendingIsFresh = FALSE,
      readyAccepted = TRUE,
      a1Fresh = TRUE,
      missingFresh = FALSE,
      a1AfterB = FALSE,
      b2Fresh = TRUE,
      a1AfterA3 = FALSE,
      b2AfterA3 = FALSE,
      a3Fresh = TRUE,
      oldControlRejected = TRUE,
      reconnectPending = TRUE,
      reconnectBlocksOld = TRUE,
      syncRequested = TRUE,
      oldEpochFresh = FALSE,
      nextEpochFresh = TRUE,
      retiredEpochRejected = TRUE,
      invalidReceive = FALSE,
      invalidPreserved = TRUE
    )
  )
})

test_that("every asynchronous specialist channel fails closed on stale context", {
  output <- run_dataset_context_node(c(
    "const C = window.CerebroDatasetContext;",
    "const a1 = {epoch:'session',dataset_key:'A',generation:1};",
    "const b2 = {epoch:'session',dataset_key:'B',generation:2};",
    "C.receive({phase:'ready',dataset_context:b2});",
    "const channels = ['json','binary','aux','background','image'];",
    "const stale = {}; const missing = {}; const current = {};",
    "channels.forEach(channel => {",
    "  stale[channel] = C.accepts({channel:channel,dataset_context:a1});",
    "  missing[channel] = C.accepts({channel:channel});",
    "  current[channel] = C.accepts({channel:channel,dataset_context:b2});",
    "});",
    "let imageCommits = 0;",
    "const guardedImageCommit = captured => {",
    "  if (C.accepts({dataset_context:captured})) imageCommits += 1;",
    "};",
    "guardedImageCommit(a1); guardedImageCommit(null); guardedImageCommit(b2);",
    "console.log(JSON.stringify({stale,missing,current,imageCommits}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  result <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_false(any(unlist(result$stale, use.names = FALSE)))
  expect_false(any(unlist(result$missing, use.names = FALSE)))
  expect_true(all(unlist(result$current, use.names = FALSE)))
  expect_identical(result$imageCommits, 1L)
})

test_that("every specialist transport is wired through the context gate", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    engine,
    "function renderSingle\\(message\\)[\\s\\S]+?datasetMessageFresh\\(message\\)",
    perl = TRUE
  )
  expect_match(
    engine,
    "function onSingleBinary\\(buffer\\)[\\s\\S]+?renderSingle\\(message\\)",
    perl = TRUE
  )
  expect_match(
    engine,
    "function onSingleAuxBinary\\(buffer\\)[\\s\\S]+?datasetMessageFresh\\(message\\)",
    perl = TRUE
  )
  expect_match(
    engine,
    "function updateSingleBackground\\(message\\)[\\s\\S]+?datasetMessageFresh\\(message\\)",
    perl = TRUE
  )
  expect_match(
    engine,
    "var stillCurrent = function \\(\\)[\\s\\S]+?sameDatasetContext\\(imageDatasetContext, currentDatasetContext\\(\\)\\)",
    perl = TRUE
  )
})

test_that("R dataset contexts validate and compare the full identity", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)

  a1 <- runtime$viewerDatasetContext("session-1", "A", 1L)
  expect_identical(
    a1,
    list(epoch = "session-1", dataset_key = "A", generation = 1)
  )
  expect_true(runtime$viewerDatasetContextEqual(a1, a1))
  expect_false(runtime$viewerDatasetContextEqual(
    a1,
    runtime$viewerDatasetContext("session-1", "B", 1L)
  ))
  expect_false(runtime$viewerDatasetContextEqual(
    a1,
    runtime$viewerDatasetContext("session-1", "A", 2L)
  ))
  expect_false(runtime$viewerDatasetContextEqual(
    a1,
    runtime$viewerDatasetContext("session-2", "A", 1L)
  ))
  expect_false(runtime$viewerDatasetContextEqual(a1, NULL))

  expect_error(runtime$viewerDatasetContext("", "A", 1L), "Invalid")
  expect_error(runtime$viewerDatasetContext("session", "", 1L), "Invalid")
  expect_error(runtime$viewerDatasetContext("session", "A", 0L), "Invalid")
  expect_error(runtime$viewerDatasetContext("session", "A", 1.5), "Invalid")
})

test_that("dataset context tokens are unambiguous", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)

  contexts <- list(
    runtime$viewerDatasetContext("a", "bc", 1L),
    runtime$viewerDatasetContext("ab", "c", 1L),
    runtime$viewerDatasetContext("a", "bc", 2L),
    runtime$viewerDatasetContext("other", "bc", 1L)
  )
  tokens <- vapply(contexts, runtime$viewerDatasetContextToken, character(1))
  expect_identical(anyDuplicated(tokens), 0L)
})

test_that("JSON renders keep the captured context after the selector changes", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  sent <- new.env(parent = emptyenv())
  sent$messages <- list()
  runtime$input <- list(coordviews_wire_supported = FALSE)
  runtime$session <- list(sendCustomMessage = function(type, message) {
    sent$messages[[length(sent$messages) + 1L]] <- list(
      type = type,
      message = message
    )
  })

  captured_a1 <- runtime$viewerDatasetContext("session", "dataset-a", 1L)
  private_paths <- c(
    `dataset-a` = "C:/private/patient-a.crb",
    `dataset-b` = "C:/private/patient-b.crb"
  )
  # Reproduce the old TOCTOU window: the computation captured A, but by the
  # time the message is sent the live selector already points at B.
  runtime$available_crb_files <- list(
    files = private_paths,
    selected = "dataset-b"
  )
  runtime$cerebroCellViewRender(
    "spatial_projection",
    meta = list(color_type = "continuous"),
    data = list(
      x = c(1, 2),
      y = c(3, 4),
      selection_key = c("cell-1", "cell-2")
    ),
    dataset_context = captured_a1
  )

  expect_length(sent$messages, 1L)
  expect_identical(sent$messages[[1L]]$type, "cell_view_render")
  message <- sent$messages[[1L]]$message
  expect_identical(message$dataset_context, captured_a1)
  expect_identical(message$meta$dataset_id, "dataset-a")
  expect_equal(message$meta$dataset_generation, 1)
  serialized <- paste(capture.output(dput(message)), collapse = "\n")
  expect_false(any(vapply(
    unname(private_paths),
    grepl,
    logical(1),
    x = serialized,
    fixed = TRUE
  )))
})

test_that("binary and progressive auxiliary renders carry one exact context", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  sent <- new.env(parent = emptyenv())
  sent$messages <- list()
  runtime$input <- list(coordviews_wire_supported = TRUE)
  runtime$cv_wire_pack_message <- identity
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent$messages[[length(sent$messages) + 1L]] <- list(
      type = type,
      payload = payload
    )
  })
  context <- runtime$viewerDatasetContext("session", "dataset-a", 7L)

  runtime$cerebroCellViewRender(
    "overview_projection",
    meta = list(color_type = "continuous"),
    data = list(x = 1, y = 2, selection_key = "cell-1"),
    dataset_context = context
  )
  expect_identical(sent$messages[[1L]]$type, "cell_view_binary")
  expect_identical(sent$messages[[1L]]$payload$dataset_context, context)

  keys <- sprintf("cell-%04d", seq_len(4096L))
  runtime$cerebroCellViewRender(
    "spatial_projection",
    meta = list(color_type = "continuous"),
    data = list(
      x = as.numeric(seq_along(keys)),
      y = as.numeric(seq_along(keys)),
      selection_key = keys
    ),
    hover = list(text = keys, hoverinfo = "text"),
    dataset_context = context
  )
  first_frame <- sent$messages[[2L]]$payload
  expect_identical(first_frame$dataset_context, context)
  expect_null(first_frame$data$selection_key)
  pending_keys <- ls(
    envir = runtime$.cerebro_cell_view_aux_pending,
    all.names = TRUE
  )
  expect_length(pending_keys, 1L)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[pending_keys[[1L]]]]
  expect_identical(auxiliary$dataset_context, context)
  expect_identical(auxiliary$render_token, first_frame$render_token)
  expect_identical(
    as.character(unlist(auxiliary$selection_key, use.names = FALSE)),
    keys
  )
})

test_that("background messages carry context and missing context fails closed", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  sent <- new.env(parent = emptyenv())
  sent$messages <- list()
  runtime$input <- list(coordviews_wire_supported = FALSE)
  runtime$session <- list(sendCustomMessage = function(type, message) {
    sent$messages[[length(sent$messages) + 1L]] <- list(
      type = type,
      message = message
    )
  })
  context <- runtime$viewerDatasetContext("session", "dataset-b", 2L)
  identity <- list(spatial = "sample-1", image = "hires")

  runtime$cerebroCellViewBackground(
    "spatial_projection",
    values = list(opacity = 0.7),
    dataset_context = context,
    background_identity = identity
  )
  expect_identical(sent$messages[[1L]]$type, "cell_view_background")
  expect_identical(sent$messages[[1L]]$message$dataset_context, context)
  expect_identical(sent$messages[[1L]]$message$background_identity, identity)

  expect_error(
    runtime$cerebroCellViewRender(
      "spatial_projection",
      meta = list(),
      data = list(),
      dataset_context = NULL
    ),
    "Invalid Viewer data-set context"
  )
  expect_error(
    runtime$cerebroCellViewBackground(
      "spatial_projection",
      values = list(),
      dataset_context = NULL
    ),
    "Invalid Viewer data-set context"
  )
  expect_length(sent$messages, 1L)
})

test_that("server rejects stale auxiliary requests before consuming payloads", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  context <- runtime$viewerDatasetContext("session", "B", 2L)
  old <- runtime$viewerDatasetContext("session", "A", 1L)
  runtime$viewer_loaded_dataset_context <- function() context
  runtime$cv_wire_pack_message <- identity
  sent <- list()
  runtime$session <- list(sendBinaryMessage = function(type, value) {
    sent[[length(sent) + 1L]] <<- value
  })
  message <- list(
    id = "overview_projection",
    wire_token = 1L,
    dataset_context = context,
    render_token = 1,
    selection_key = c("B-1", "B-2"),
    hover = list()
  )
  runtime$.cerebro_cell_view_aux_pending[["overview_projection:1"]] <- message
  expect_false(runtime$cerebroCellViewAuxRequest(list(
    id = "overview_projection",
    wire_token = 1L,
    dataset_context = old
  )))
  expect_length(sent, 0L)
  expect_identical(
    runtime$.cerebro_cell_view_aux_pending[["overview_projection:1"]],
    message
  )
  expect_true(runtime$cerebroCellViewAuxRequest(list(
    id = "overview_projection",
    wire_token = 1L,
    dataset_context = context
  )))
  expect_length(sent, 1L)
  expect_identical(sent[[1]]$dataset_context, context)
  expect_null(runtime$.cerebro_cell_view_aux_pending[["overview_projection:1"]])
})

test_that("browser-visible dataset keys are opaque and never file paths", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  private_paths <- c(
    tumor = "C:/Users/private/subject-001.crb",
    control = "D:/restricted/subject-002.crb"
  )
  labels <- c(tumor = "Tumour", control = "Control")

  expect_identical(
    runtime$viewerDatasetName(private_paths, private_paths[["control"]]),
    "control"
  )
  choices <- runtime$viewerDatasetChoices(private_paths, labels)
  expect_identical(unname(choices), c("tumor", "control"))
  expect_identical(names(choices), unname(labels))
  expect_false(any(unname(private_paths) %in% c(names(choices), choices)))

  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(server, 'dataset_key <- "__single__"', fixed = TRUE)
  expect_match(server, 'dataset_key <- "__example__"', fixed = TRUE)
  expect_match(
    server,
    'dataset_key <- paste0("__upload_", viewer_upload_serial, "__")',
    fixed = TRUE
  )
  expect_match(
    server,
    "dataset_key <- viewerDatasetName(",
    fixed = TRUE
  )
  expect_no_match(server, "dataset_key <- dataset_target", fixed = TRUE)
  expect_no_match(server, "dataset_key <- file_to_load", fixed = TRUE)
})

test_that("spatial rendering carries the computation context end to end", {
  projection <- paste(
    readLines(
      viewer_test_path("spatial", "obj_projection_data_to_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  event <- paste(
    readLines(
      viewer_test_path("spatial", "event_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    projection,
    "dataset_context <- viewer_loaded_dataset_context()",
    fixed = TRUE
  )
  expect_match(
    projection,
    "dataset_context = dataset_context",
    fixed = TRUE
  )
  expect_match(
    event,
    "viewerDatasetContextEqual(",
    fixed = TRUE
  )
  expect_match(
    event,
    "viewerDatasetContextToken(current_context)",
    fixed = TRUE
  )
  expect_match(
    event,
    "viewerDatasetContextEqual(data$dataset_context, current_context)",
    fixed = TRUE
  )
})

test_that("reconnects fail closed until the server replays its snapshot", {
  client <- paste(
    readLines(viewer_test_path("www", "dataset-context.js"), warn = FALSE),
    collapse = "\n"
  )
  views <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(client, "currentPhase = 'pending'", fixed = TRUE)
  expect_match(client, "viewer_dataset_context_sync", fixed = TRUE)
  expect_match(server, 'input[["viewer_dataset_context_sync"]]', fixed = TRUE)
  expect_match(
    server,
    "snapshot <- isolate(viewer_dataset_snapshot())",
    fixed = TRUE
  )
  expect_match(
    server,
    'if (isTRUE(snapshot$ok)) "ready" else "error"',
    fixed = TRUE
  )
  expect_match(
    views,
    "imgChoice = {};\n    geneWanted = null;\n    pendingColorPatch = null;",
    fixed = TRUE
  )
})

test_that("portable-view requests and responses use one captured context", {
  server <- paste(
    readLines(
      viewer_test_path("coordinated_views", "server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    server,
    'c("nonce", "action", "dataset_context", "config")',
    fixed = TRUE
  )
  expect_match(
    server,
    paste0(
      '"nonce",\n            "action",\n            "dataset_context",',
      '\n            "name"'
    ),
    fixed = TRUE
  )
  expect_gte(
    sum(
      gregexpr(
        "viewerDatasetContextEqual(request_context, current_context)",
        server,
        fixed = TRUE
      )[[1L]] >
        0
    ),
    2L
  )
  expect_match(server, "dataset_context = dataset_context", fixed = TRUE)
  # Restrict this contract to portable-view handlers and their response helper.
  # The synchronous expression-feedback helper legitimately captures live context.
  config_expressions <- Filter(
    function(expr) {
      if (!is.call(expr)) {
        return(FALSE)
      }
      if (identical(expr[[1L]], as.name("<-"))) {
        return(identical(expr[[2L]], as.name("cv_config_send_result")))
      }
      identical(expr[[1L]], as.name("observeEvent")) &&
        any(vapply(
          c("coordviews_config_request", "coordviews_config_upload_request"),
          function(id) {
            grepl(id, paste(deparse(expr[[2L]]), collapse = ""), fixed = TRUE)
          },
          logical(1)
        ))
    },
    as.list(parse(text = server))
  )
  expect_length(config_expressions, 3L)
  expect_no_match(
    paste(
      vapply(
        config_expressions,
        function(expr) paste(deparse(expr), collapse = "\n"),
        character(1)
      ),
      collapse = "\n"
    ),
    "dataset_context = viewer_loaded_dataset_context()",
    fixed = TRUE
  )
})

test_that("all specialist state inputs fail closed on context", {
  client <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    client,
    "dataset_context: activeView && activeView.dataset_context",
    fixed = TRUE
  )
  expect_match(
    client,
    "singleActive + '_hidden_groups', {",
    fixed = TRUE
  )
  expect_no_match(client, "pendingStableSelection ? null", fixed = TRUE)

  for (path in list(
    c("overview", "obj_projection_selected_cells.R"),
    c("spatial", "obj_projection_selected_cells.R"),
    c("trajectory", "projection_plot.R")
  )) {
    source <- paste(
      readLines(do.call(viewer_test_path, as.list(path)), warn = FALSE),
      collapse = "\n"
    )
    expect_match(source, "hidden_request", fixed = TRUE)
    expect_match(source, "viewerDatasetContextEqual(", fixed = TRUE)
  }

  coordinated <- paste(
    readLines(viewer_test_path("coordinated_views", "server.R")),
    collapse = "\n"
  )
  hla <- paste(
    readLines(viewer_test_path("hla_tcr_motifs", "network_table.R")),
    collapse = "\n"
  )
  expect_match(coordinated, "is.list(request)", fixed = TRUE)
  expect_no_match(coordinated, "bc <- detail_request\n  } else", fixed = TRUE)
  expect_match(hla, "is.list(request)", fixed = TRUE)
  expect_no_match(
    hla,
    'hla_selection_values(input[["hla_motif_selected_keys"]])',
    fixed = TRUE
  )
})

test_that("same-context renders are monotonically ordered", {
  client <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(client, "Number.isSafeInteger(renderToken)", fixed = TRUE)
  expect_match(client, "renderToken <= previous.render_token", fixed = TRUE)
  expect_match(
    client,
    "extra.progressive_token !== D.progressive_token",
    fixed = TRUE
  )
  expect_match(
    client,
    "Number(view.aux_render_token) !== Number(message.render_token)",
    fixed = TRUE
  )
})

test_that("a rapid selector A/B/A sequence rejects stale ready messages", {
  output <- run_dataset_context_node(c(
    "const C = window.CerebroDatasetContext;",
    "const a1={epoch:'s',dataset_key:'A',generation:1};",
    "const b2={epoch:'s',dataset_key:'B',generation:2};",
    "const a3={epoch:'s',dataset_key:'A',generation:3};",
    "const msg=c=>({dataset_context:c});",
    "C.receive({phase:'ready',dataset_context:a1,selection:'A',selection_nonce:0});",
    "C.beginSelection('A'); C.beginSelection('B');",
    "const blockedImmediately=!C.accepts(msg(a1));",
    "const staleRejected=!C.receive({phase:'ready',dataset_context:b2,selection:'B',selection_nonce:0});",
    "C.beginSelection('A');",
    "const staleARejected=!C.receive({phase:'ready',dataset_context:a1,selection:'A',selection_nonce:0});",
    "const finalAccepted=C.receive({phase:'ready',dataset_context:a3,selection:'A',selection_nonce:2});",
    "console.log(JSON.stringify({blockedImmediately,staleRejected,staleARejected,finalAccepted,fresh:C.accepts(msg(a3))}));"
  ))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_identical(
    jsonlite::fromJSON(output),
    list(
      blockedImmediately = TRUE,
      staleRejected = TRUE,
      staleARejected = TRUE,
      finalAccepted = TRUE,
      fresh = TRUE
    )
  )
})
