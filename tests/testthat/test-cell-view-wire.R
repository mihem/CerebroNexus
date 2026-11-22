wire_file <- viewer_test_path("www", "cell_views_wire.js")
bundle_file <- viewer_test_path("coordinated_views", "bundle.R")

wire_header <- function(payload) {
  header_length <- sum(as.integer(payload[seq_len(4L)]) * 256^(0:3))
  jsonlite::fromJSON(
    rawToChar(payload[4L + seq_len(header_length)]),
    simplifyVector = FALSE
  )
}

## Decodes a packed envelope with the browser's cell_views_wire.js.
unpack_in_node <- function(packed) {
  payload <- withr::local_tempfile(fileext = ".bin")
  writeBin(packed, payload)
  output <- expect_node_script(c(
    "const fs = require('fs');",
    "global.window = global;",
    js_eval_file(wire_file),
    sprintf(
      "const input = fs.readFileSync(%s);",
      encodeString(payload, quote = "\"")
    ),
    "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
    "console.log(JSON.stringify(window.CBViewWire.unpack(buffer), (_key, value) => ArrayBuffer.isView(value) ? Array.from(value) : value));"
  ))
  jsonlite::fromJSON(output, simplifyVector = FALSE)
}

test_that("the browser restores compact linked-view vectors", {
  skip_if_no_node()
  skip_if_not_installed("base64enc")
  skip_if_not_installed("jsonlite")
  expect_true(file.exists(wire_file), info = "cell_views_wire.js not found")
  if (!file.exists(wire_file)) {
    return(invisible(NULL))
  }

  helpers <- viewer_runtime_env()
  sys.source(bundle_file, envir = helpers)
  packed <- helpers$cv_wire_pack_bundle(
    list(
      cells = I(c("cell-1", "cell-2")),
      groups = list(
        cluster = helpers$cv_group(c(0L, NA_integer_), "A", "#fff")
      ),
      cat_extra = list(
        batch = helpers$cv_group(
          c(0L, 1L),
          c("A", "B"),
          c("#fff", "#000")
        )
      ),
      fields = list(score = helpers$cv_field("Score", c(0L, 1000L), 0, 1)),
      projections = list(
        umap = list(x = I(c(1.25, NA_real_)), y = I(c(-2.5, 3.75)), ndim = 2L)
      ),
      spaces = list(helpers$cv_space("spatial", "Spatial", c(4, 5), c(6, 7)))
    ),
    min_length = 1L
  )
  header <- wire_header(packed)
  expect_identical(header$groups$cluster$values$`__cv_wire__`, "i32")
  expect_identical(header$cat_extra$batch$values$`__cv_wire__`, "i8")
  expect_identical(header$fields$score$v$`__cv_wire__`, "i16")
  restored <- unpack_in_node(packed)

  expect_identical(restored$wire_format, "binary-v1")
  expect_identical(unlist(restored$cells), c("cell-1", "cell-2"))
  expect_identical(restored$groups$cluster$values[[1L]], 0L)
  expect_null(restored$groups$cluster$values[[2L]])
  expect_identical(unlist(restored$cat_extra$batch$values), c(0L, 1L))
  expect_identical(unlist(restored$fields$score$v), c(0L, 1000L))
  expect_identical(restored$projections$umap$x[[1L]], 1.25)
  expect_null(restored$projections$umap$x[[2L]])
  expect_equal(unlist(restored$projections$umap$y), c(-2.5, 3.75))
})

test_that("hover code remapping preserves observed order and missing labels", {
  helpers <- viewer_runtime_env()
  for (values in list(
    factor(c("b", NA, "a", "NA", "b"), levels = c("unused", "NA", "a", "b")),
    ordered(c("b", "a", NA), levels = c("a", "b")),
    factor(c("b", NA, "a"), exclude = NULL)
  )) {
    for (registered in list(c("a", "b", "NA"), "a", character())) {
      actual <- helpers$cerebroProjectionHoverColumns(
        data.frame(group = values),
        "group",
        function(group) registered
      )
      expected <- helpers$cerebroProjectionHoverColumns(
        data.frame(group = as.character(values)),
        "group",
        function(group) registered
      )
      expect_identical(actual, expected)
      labels <- as.character(values)
      labels[is.na(labels)] <- "NA"
      expect_identical(actual[[1L]]$levels, unique(labels))
      expect_identical(actual[[1L]]$values, match(labels, unique(labels)) - 1L)
    }
  }
})

test_that("binary envelopes also preserve messages without packed vectors", {
  helpers <- viewer_runtime_env()
  message <- list(id = "empty", data = list(n = 0L), labels = c("a", "b"))
  packed <- helpers$cv_wire_pack_message(message)
  expect_type(packed, "raw")
  header <- wire_header(packed)
  expect_identical(header$id, "empty")
  expect_identical(header$data$n, 0L)
  expect_identical(unlist(header$labels), c("a", "b"))
  expect_identical(length(packed) %% 4L, 0L)
})

test_that("specialist cell views use the same binary envelope", {
  skip_if_no_node()
  skip_if_not_installed("jsonlite")

  helpers <- viewer_runtime_env()
  sys.source(bundle_file, envir = helpers)
  packed <- helpers$cv_wire_pack_message(
    list(
      id = "overview_projection",
      data = list(
        x = I(c(1.25, NA_real_)),
        group = I(c(1L, NA_integer_)),
        selection_key = I(c("cell-1", "cell-2"))
      )
    ),
    min_length = 1L
  )
  restored <- unpack_in_node(packed)
  expect_identical(restored$id, "overview_projection")
  expect_equal(restored$data$x[[1L]], 1.25)
  expect_null(restored$data$x[[2L]])
  expect_identical(restored$data$group[[1L]], 1L)
  expect_null(restored$data$group[[2L]])
  expect_identical(unlist(restored$data$selection_key), c("cell-1", "cell-2"))
})

test_that("large specialist views send their first frame before hover data", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()
  sent <- list()
  runtime$viewerDatasetIdentity <- function() {
    list(
      cell_count = 4096L,
      fingerprint = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      order_fingerprint = "md5-cell-order-v1:fedcba9876543210fedcba9876543210"
    )
  }
  runtime$session <- list(
    sendBinaryMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    },
    sendCustomMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    }
  )
  keys <- sprintf("cell-%04d", seq_len(4096L))
  dataset_context <- runtime$viewerDatasetContext(
    "wire-test-session",
    "dataset-a",
    1L
  )

  runtime$cerebroCellViewRender(
    "overview_projection",
    meta = list(color_type = "categorical", traces = "A"),
    data = list(
      x = list(I(as.numeric(seq_along(keys)))),
      y = list(I(as.numeric(seq_along(keys)))),
      selection_key = list(I(keys)),
      color = list("#123456")
    ),
    hover = list(text = list(I(keys)), hoverinfo = "text"),
    dataset_context = dataset_context
  )

  expect_identical(vapply(sent, `[[`, character(1), "type"), "cell_view_binary")
  first <- wire_header(sent[[1L]]$payload)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[
    paste("overview_projection", first$data$wire_token, sep = ":")
  ]]
  expect_identical(first$dataset_context$epoch, dataset_context$epoch)
  expect_identical(
    first$dataset_context$dataset_key,
    dataset_context$dataset_key
  )
  expect_equal(first$dataset_context$generation, dataset_context$generation)
  expect_identical(first$data$n, 4096L)
  expect_identical(
    first$dataset_identity,
    list(
      cell_count = 4096L,
      cell_fingerprint = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      cell_order_fingerprint = "md5-cell-order-v1:fedcba9876543210fedcba9876543210",
      pack_dataset_fingerprint = ""
    )
  )
  expect_null(first$data$selection_key)
  expect_identical(first$data$x[[1L]]$`__cv_wire__`, "f32")
  expect_identical(first$hover$hoverinfo, "skip")
  expect_identical(auxiliary$id, "overview_projection")
  expect_identical(auxiliary$dataset_context, dataset_context)
  expect_identical(unlist(auxiliary$selection_key, use.names = FALSE), keys)
  auxiliary_response <- wire_header(
    runtime$cv_wire_pack_message(auxiliary, min_length = 1L)
  )
  expect_identical(
    auxiliary_response$dataset_context$epoch,
    dataset_context$epoch
  )
  expect_identical(
    auxiliary_response$dataset_context$dataset_key,
    dataset_context$dataset_key
  )
  expect_equal(
    auxiliary_response$dataset_context$generation,
    dataset_context$generation
  )
})

test_that("deferred specialist IDs can declare trace lengths without placeholders", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()
  sent <- list()
  runtime$viewerDatasetIdentity <- function() {
    list(
      cell_count = 4096L,
      fingerprint = "dataset-a",
      order_fingerprint = "order-a"
    )
  }
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  trace_lengths <- c(2500L, 1596L)
  keys <- sprintf("cell-%04d", seq_len(sum(trace_lengths)))

  runtime$cerebroCellViewRender(
    "ir_clonalUMAP_projection",
    meta = list(color_type = "categorical", traces = c("Other", "Single")),
    data = list(
      x = list(seq_len(trace_lengths[[1L]]), seq_len(trace_lengths[[2L]])),
      y = list(seq_len(trace_lengths[[1L]]), seq_len(trace_lengths[[2L]])),
      deferred_selection_lengths = trace_lengths,
      color = list("#d9d9d9", "#123456")
    ),
    deferred_aux = function() {
      list(
        selection_key = list(
          keys[seq_len(trace_lengths[[1L]])],
          keys[trace_lengths[[1L]] + seq_len(trace_lengths[[2L]])]
        ),
        hover = list(hoverinfo = "skip")
      )
    },
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    )
  )

  expect_identical(length(sent), 1L)
  first <- wire_header(sent[[1L]]$payload)
  expect_identical(first$data$n, 4096L)
  expect_null(first$data$selection_key)
  expect_null(first$data$deferred_selection_lengths)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[
    paste(
      "ir_clonalUMAP_projection",
      first$data$wire_token,
      sep = ":"
    )
  ]]
  expect_true(is.function(auxiliary$build))
  expect_identical(
    unlist(auxiliary$build()$selection_key, use.names = FALSE),
    keys
  )
})

test_that("specialist views can reference validated shared coordinates", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()

  has_contract <- all(
    c(
      "viewerSharedProjectionName",
      "cerebroCellViewRender"
    ) %in%
      ls(runtime)
  )
  expect_true(has_contract)
  if (!has_contract) {
    return(invisible(NULL))
  }

  identity <- list(
    cell_count = 4096L,
    fingerprint = "dataset-a",
    order_fingerprint = "order-a"
  )
  shared <- list(
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    ),
    dataset_fingerprint = "dataset-a",
    cell_count = 4096L,
    canonical_order_id = "order-a",
    projection = "umap"
  )
  expect_identical(
    runtime$viewerSharedProjectionName(
      "umap",
      4096L,
      shared,
      identity,
      shared$dataset_context
    ),
    "umap"
  )
  expect_null(runtime$viewerSharedProjectionName(
    "umap",
    4096L,
    shared,
    identity,
    list(epoch = "wire-test", dataset_key = "A", generation = 2)
  ))
  expect_null(runtime$viewerSharedProjectionName(
    "umap",
    4096L,
    shared,
    identity,
    list(epoch = "new-epoch", dataset_key = "A", generation = 1)
  ))
  expect_null(runtime$viewerSharedProjectionName(
    "umap",
    4096L,
    within(shared, dataset_fingerprint <- "dataset-b"),
    identity,
    shared$dataset_context
  ))

  sent <- list()
  runtime$input <- list(coordviews_shared_base = shared)
  runtime$viewerDatasetIdentity <- function() identity
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  keys <- sprintf("cell-%04d", seq_len(4096L))
  runtime$cerebroCellViewRender(
    "expression_projection",
    meta = list(color_type = "continuous", space_label = "umap"),
    data = list(
      x = seq_len(4096L),
      y = rev(seq_len(4096L)),
      color = rep(0, 4096L),
      selection_key = keys,
      shared_zero_color = TRUE
    ),
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    )
  )

  first <- wire_header(sent[[1L]]$payload)
  expect_identical(first$shared_projection, "umap")
  expect_null(first$data$x)
  expect_null(first$data$y)
  expect_null(first$data$color)
  expect_true(first$data$zero_color)
  expect_identical(first$data$n, 4096L)

  sent <- list()
  runtime$input$coordviews_shared_base <- NULL
  runtime$cerebroCellViewRender(
    "expression_projection",
    meta = list(color_type = "continuous", space_label = "umap"),
    data = list(
      x = seq_len(4096L),
      y = rev(seq_len(4096L)),
      color = numeric(),
      selection_key = keys,
      shared_zero_color = TRUE
    ),
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    )
  )
  unshared <- wire_header(sent[[1L]]$payload)
  expect_null(unshared$shared_projection)
  expect_false(is.null(unshared$data$x))
  expect_false(is.null(unshared$data$y))
  expect_null(unshared$data$color)
  expect_true(unshared$data$zero_color)
  expect_identical(unshared$data$n, 4096L)

  ## Per-group panels hold one group's cells each, so they never take the
  ## dataset-wide coordinates of a projection with the same label.
  sent <- list()
  runtime$input$coordviews_shared_base <- shared
  half <- seq_len(2048L)
  runtime$cerebroCellViewRender(
    "ir_clonalUMAP_projection",
    meta = list(color_type = "categorical", space_label = "umap"),
    data = list(
      selection_key = keys,
      group = rep("Other cells", 4096L),
      panels = list(
        list(id = "group1", selection_key = keys[half], x = half, y = half),
        list(id = "group2", selection_key = keys[-half], x = half, y = half)
      )
    ),
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    )
  )
  grouped <- wire_header(sent[[1L]]$payload)
  expect_null(grouped$shared_projection)
})

test_that("core auxiliary identities precede full metadata and reject duplicate stages", {
  env <- viewer_runtime_env()
  sent <- list()
  core_builds <- 0L
  full_builds <- 0L
  env$cv_wire_pack_message <- identity
  env$session <- list(sendBinaryMessage = function(type, value) {
    sent[[length(sent) + 1L]] <<- value
  })
  context <- env$viewerDatasetContext("wire-test", "A", 1)
  env$viewer_loaded_dataset_context <- function() context
  env$.cerebro_cell_view_aux_pending[["gene:7"]] <- list(
    dataset_context = context,
    render_token = 1,
    id = "gene",
    wire_token = 7L,
    core_builder = function() {
      core_builds <<- core_builds + 1L
      list(
        selection_key = c("a", "b"),
        hover = list(hoverinfo = "text", pending = TRUE)
      )
    },
    build = function() {
      full_builds <<- full_builds + 1L
      list(
        selection_key = c("a", "b"),
        hover = list(columns = list("metadata"))
      )
    }
  )
  request <- list(id = "gene", wire_token = 7L, dataset_context = context)
  expect_true(env$cerebroCellViewAuxRequest(request))
  expect_identical(core_builds, 1L)
  expect_identical(full_builds, 0L)
  expect_identical(sent[[1L]]$selection_key, c("a", "b"))
  expect_true(sent[[1L]]$hover$pending)
  expect_false(env$cerebroCellViewAuxRequest(request))
  request$stage <- "metadata"
  expect_true(env$cerebroCellViewAuxRequest(request))
  expect_identical(full_builds, 1L)
  expect_null(sent[[2L]]$selection_key)
  expect_identical(sent[[2L]]$hover$columns, list("metadata"))
  expect_false(env$cerebroCellViewAuxRequest(request))
  request$wire_token <- 6L
  expect_false(env$cerebroCellViewAuxRequest(request))
})

test_that("renders with delivered cells name the barcodes the browser holds", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()
  sent <- list()
  runtime$session <- list(
    sendBinaryMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    },
    sendCustomMessage = function(type, payload) NULL
  )
  context <- runtime$viewerDatasetContext("wire-test", "dataset-a", 1L)
  runtime$viewer_loaded_dataset_context <- function() context
  keys <- sprintf("cell-%04d", seq_len(4096L))
  core_builds <- 0L
  render <- function(color, cells) {
    runtime$cerebroCellViewRender(
      "overview_projection",
      meta = list(color_type = "categorical", traces = "A"),
      data = list(
        x = list(I(as.numeric(seq_along(keys)))),
        y = list(I(as.numeric(seq_along(keys)))),
        color = list(color),
        deferred_selection_lengths = length(keys)
      ),
      deferred_aux = function() list(selection_key = keys),
      core_aux = function() {
        core_builds <<- core_builds + 1L
        list(selection_key = keys, hover = list(pending = FALSE))
      },
      dataset_context = context,
      aux_key = list(cell_indices = cells, hover = FALSE)
    )
    header <- wire_header(tail(sent, 1L)[[1L]]$payload)
    header$data
  }
  first <- render("#123456", seq_along(keys))
  expect_null(first$aux_reuse)
  ## Nothing was delivered yet, so a recolour cannot reuse anything.
  expect_null(render("#654321", seq_along(keys))$aux_reuse)
  second_token <- runtime$.cerebro_cell_view_wire_serial
  expect_true(runtime$cerebroCellViewAuxRequest(list(
    id = "overview_projection",
    wire_token = second_token,
    dataset_context = context
  )))
  expect_identical(core_builds, 1L)
  expect_identical(render("#abcdef", seq_along(keys))$aux_reuse, second_token)
  ## A different cell set or dataset generation must download its barcodes.
  expect_null(render("#abcdef", rev(seq_along(keys)))$aux_reuse)
  context <- runtime$viewerDatasetContext("wire-test", "dataset-a", 2L)
  expect_null(render("#abcdef", seq_along(keys))$aux_reuse)
})

test_that("auxiliary data delivered at once is named by renders of the same cells", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()
  sent <- list()
  runtime$session <- list(
    sendBinaryMessage = function(type, payload) {
      sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
    },
    sendCustomMessage = function(type, payload) NULL
  )
  context <- runtime$viewerDatasetContext("wire-test", "dataset-a", 1L)
  runtime$viewer_loaded_dataset_context <- function() context
  n <- 4096L
  render <- function(color) {
    runtime$cerebroCellViewRender(
      "spatial_projection",
      meta = list(color_type = "categorical", traces = "A"),
      data = list(
        x = list(I(as.numeric(seq_len(n)))),
        y = list(I(as.numeric(seq_len(n)))),
        color = list(color),
        deferred_selection_lengths = n
      ),
      ## Spatial sends canonical rows named through the cell-name resource.
      deferred_aux = function() {
        list(
          selection_index = seq_len(n) - 1L,
          cell_names_resource = list(url = "names"),
          hover = list(hoverinfo = "skip", columns = list())
        )
      },
      dataset_context = context,
      aux_key = list(cells = seq_len(n), hover = FALSE)
    )
    wire_header(tail(sent, 1L)[[1L]]$payload)$data
  }
  expect_null(render("#123456")$aux_reuse)
  token <- runtime$.cerebro_cell_view_wire_serial
  expect_true(runtime$cerebroCellViewAuxRequest(list(
    id = "spatial_projection",
    wire_token = token,
    dataset_context = context
  )))
  ## The key stays on the server: the browser never reads it.
  expect_null(wire_header(tail(sent, 1L)[[1L]]$payload)$aux_key)
  expect_identical(render("#abcdef")$aux_reuse, token)
})

test_that("a frame sent at once keeps the cell rows and the cell-name file", {
  skip_if_not_installed("jsonlite")
  runtime <- viewer_runtime_env()
  sent <- list()
  runtime$viewerDatasetIdentity <- function() NULL
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  resource <- list(
    protocol = "canonical-cell-names-v1",
    url = "cell-names/names.bin",
    cells = 5L
  )
  rows <- c(1L, 3L)
  ## Split panels are never progressive, so the deferred auxiliary data are
  ## built at once; with the cell-name file they name rows, not barcodes.
  runtime$cerebroCellViewRender(
    "spatial_projection",
    meta = list(color_type = "categorical", traces = c("A", "B")),
    data = list(
      selection_key = seq_along(rows),
      group = c("A", "B"),
      panels = list(
        list(id = "split-1", label = "A", selection_key = "c-1", x = 1, y = 1),
        list(id = "split-2", label = "B", selection_key = "c-3", x = 2, y = 2)
      )
    ),
    deferred_aux = function() {
      runtime$cerebroCellViewDeferredAux(
        selection_rows = seq_along(rows),
        cell_barcodes = NULL,
        names_resource = resource,
        cell_rows = rows
      )
    },
    dataset_context = list(
      epoch = "wire-test",
      dataset_key = "A",
      generation = 1
    )
  )
  frame <- wire_header(sent[[1L]]$payload)
  expect_null(frame$data$selection_key)
  expect_identical(unlist(frame$data$selection_index), rows - 1L)
  expect_identical(frame$data$cell_names_resource$url, resource$url)
  expect_length(frame$data$panels, 2L)
})
