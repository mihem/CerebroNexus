dataset_context_javascript <- viewer_test_path("www", "dataset-context.js")

run_dataset_context_node <- function(body) {
  skip_if_no_node()
  expect_true(
    file.exists(dataset_context_javascript),
    info = "dataset-context.js not found"
  )
  run_node_script(c(
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
    js_eval_file(dataset_context_javascript),
    body
  ))
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
})

test_that("dataset switches invalidate old generations and reconnects replay the loaded snapshot", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      Cerebro.options <- list()
      sent <- list()
      session$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
      }
      available_crb_files <- reactiveValues(selected = NULL)
      viewer_dataset_epoch <- "session"
      viewer_dataset_generation <- 0
      viewer_dataset_request <- reactiveVal(NULL)
      viewer_clear_cell_view_aux <- function() NULL
      ## Requests decide whether to load; without a catalog they always do.
      sys.source(viewer_test_path("core", "dataset_overview.R"), scope)
      dataset_load_requested <- reactiveVal(FALSE)
      initial_tab <- NULL
      initial_page_applied <- reactiveVal(TRUE)
      viewer_dataset_snapshot <- reactive({
        request <- viewer_dataset_request()
        if (is.null(request)) {
          return(NULL)
        }
        list(
          ok = !isTRUE(input$load_failed),
          dataset_context = request$dataset_context
        )
      })
      viewer_server_load(
        viewer_server_statements("shiny_server.R", server = TRUE),
        scope,
        names = c(
          "viewer_send_dataset_phase",
          "viewer_catalog_info",
          "viewer_dataset_load_required",
          "viewer_request_dataset"
        ),
        events = "viewer_dataset_context_sync"
      )
    },
    {
      session$flushReact()
      session$setInputs(viewer_dataset_context_sync = list(nonce = 1))
      expect_length(scope$sent, 0L)
      contexts <- list()
      for (selection in c("A", "B", "A")) {
        request <- viewer_request_dataset(
          selection,
          paste0("/private/", selection, ".crb"),
          selection,
          "configured"
        )
        contexts[[length(contexts) + 1L]] <- request$dataset_context
        pending <- tail(scope$sent, 1)[[1]]
        expect_identical(pending$type, "cerebro_dataset_context")
        expect_identical(pending$message$phase, "pending")
        expect_identical(
          pending$message$dataset_context,
          request$dataset_context
        )
        expect_identical(pending$message$selection, selection)
        session$setInputs(
          viewer_dataset_context_sync = list(nonce = length(contexts) + 1)
        )
        ready <- tail(scope$sent, 1)[[1]]$message
        expect_identical(ready$phase, "ready")
        expect_identical(ready$dataset_context, request$dataset_context)
        expect_false(any(grepl("/private/", unlist(ready), fixed = TRUE)))
      }
      expect_identical(
        vapply(contexts, `[[`, numeric(1), "generation"),
        c(1, 2, 3)
      )
      expect_false(viewerDatasetContextEqual(contexts[[1]], contexts[[3]]))
      before <- length(scope$sent)
      viewer_request_dataset("A", "/private/A.crb", "A", "configured")
      expect_length(scope$sent, before)
      session$setInputs(
        load_failed = TRUE,
        viewer_dataset_context_sync = list(nonce = 5)
      )
      expect_identical(tail(scope$sent, 1)[[1]]$message$phase, "error")
      expect_identical(
        tail(scope$sent, 1)[[1]]$message$dataset_context,
        contexts[[3]]
      )
    }
  )
})

test_that("portable-view requests and responses use one captured context", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(viewer_test_path("coordinated_views", "config.R"), scope)
      context <- list(epoch = "session", dataset_key = "A", generation = 1)
      current <- context
      viewer_loaded_dataset_context <- function() current
      reads <- 0L
      cv_saved_view_dataset <- function() {
        reads <<- reads + 1L
        list(
          cells = c("c1", "c2"),
          fingerprint = "fp",
          dataset_context = current
        )
      }
      messages <- list()
      session$sendCustomMessage <- function(type, payload) {
        messages[[length(messages) + 1L]] <<- payload
      }
      normalized <- list(
        schema = CV_CONFIG_SCHEMA,
        selection = list(cells = "c2")
      )
      cv_config_materialize_selection <- function(config, cells) config
      cv_config_prepare <- function(config, cells, fingerprint) {
        current <<- modifyList(current, list(generation = 2))
        list(json = "prepared", config = normalized)
      }
      cv_config_decode <- function(text, cells, fingerprint) {
        current <<- modifyList(current, list(generation = 2))
        normalized
      }
      cv_config_validate_genes <- function(...) NULL
      cv_config_json_document <- identity
      viewer_server_load(
        viewer_server_statements("coordinated_views", "server.R"),
        scope,
        names = c("cv_config_send_result", "cv_config_log_failure"),
        events = c(
          "coordviews_config_request",
          "coordviews_config_upload_request"
        )
      )
    },
    {
      session$flushReact()
      for (upload in c(FALSE, TRUE)) {
        scope$current <- context
        request <- if (upload) {
          list(
            nonce = "upload",
            action = "apply",
            name = "view.json",
            size = 2L,
            text = "{}"
          )
        } else {
          list(nonce = "prepare", action = "prepare", config = list())
        }
        id <- if (upload) {
          "coordviews_config_upload_request"
        } else {
          "coordviews_config_request"
        }
        request$dataset_context <- modifyList(context, list(generation = 0))
        before <- length(scope$messages)
        reads <- scope$reads
        do.call(session$setInputs, setNames(list(request), id))
        expect_length(scope$messages, before)
        expect_equal(scope$reads, reads)
        request$dataset_context <- context
        do.call(session$setInputs, setNames(list(request), id))
        response <- tail(scope$messages, 1)[[1]]
        expect_true(response$ok)
        expect_identical(response$dataset_context, context)
        expect_equal(scope$current$generation, 2)
        expect_equal(response$selected_cells, 1)
        if (upload) {
          expect_equal(as.integer(response$selection_indices), 1L)
        } else {
          expect_identical(response$json, "prepared")
        }
      }
    }
  )
})

test_that("all specialist state inputs fail closed on context", {
  for (page in c("overview", "spatial")) {
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        sys.source(viewer_test_path("utility_functions.R"), scope)
        context <- list(epoch = "session", dataset_key = "A", generation = 1)
        viewer_loaded_dataset_context <- function() context
        getProjection <- function(...) data.frame(x = c(1, 2), y = c(3, 4))
        getMetaData <- function() {
          data.frame(cell_barcode = c("c1", "c2"), group = c("A", "B"))
        }
        overview_projection_data_to_plot <- function() TRUE
        spatial_projection_data_to_plot <- function() {
          list(
            coordinates = getProjection(),
            cells_df = data.frame(cell_index = 1:2)
          )
        }
        id <- paste0(page, "_projection")
        viewer_server_load(
          viewer_server_statements(page, "obj_projection_selected_cells.R"),
          scope,
          names = c(
            if (page == "spatial") "spatial_projection_interaction_data",
            paste0(id, "_selected_cells")
          )
        )
        selected <- get(paste0(id, "_selected_cells"), scope)
      },
      {
        selection <- list(
          x = c(1, 2),
          y = c(3, 4),
          ids = c("c1", "c2"),
          dataset_context = context
        )
        values <- setNames(
          list(selection, "group", "umap"),
          paste0(id, c("_persistent_selection", "_point_color", "_to_display"))
        )
        do.call(session$setInputs, values)
        expect_identical(selected()$selection_key, c("c1", "c2"))
        hidden <- list(groups = "A", dataset_context = context)
        do.call(
          session$setInputs,
          setNames(list(hidden), paste0(id, "_hidden_groups"))
        )
        expect_identical(selected()$selection_key, "c2")
        hidden$dataset_context$generation <- 0
        do.call(
          session$setInputs,
          setNames(list(hidden), paste0(id, "_hidden_groups"))
        )
        expect_identical(selected()$selection_key, c("c1", "c2"))
        selection$dataset_context$generation <- 0
        do.call(
          session$setInputs,
          setNames(list(selection), paste0(id, "_persistent_selection"))
        )
        expect_null(selected())
      }
    )
  }
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

test_that("progress rejects old generations and repeated-choice responses", {
  result <- run_dataset_context_node(c(
    "const gate = window.CerebroDatasetContext;",
    "const a = {epoch:'test',dataset_key:'a',generation:1};",
    "const b = {epoch:'test',dataset_key:'b',generation:2};",
    "gate.receive({phase:'ready',dataset_context:a,selection:'a',selection_nonce:0});",
    "gate.beginSelection('b');",
    "if(gate.acceptsProgress({dataset_context:a,selection:'a',selection_nonce:0})) throw Error('Old A progress');",
    "gate.receive({phase:'pending',dataset_context:b,selection:'b',selection_nonce:1});",
    "if(!gate.acceptsProgress({dataset_context:b,selection:'b',selection_nonce:1})) throw Error('Current B rejected');",
    "gate.beginSelection('a');",
    "if(gate.acceptsProgress({dataset_context:b,selection:'b',selection_nonce:1})) throw Error('Old B progress');",
    "if(gate.acceptsProgress({dataset_context:a,selection:'a',selection_nonce:0})) throw Error('Earlier A accepted');"
  ))
  expect_null(attr(result, "status"))
})
