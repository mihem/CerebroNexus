wire_file <- viewer_test_path("www", "cell_views_wire.js")
bundle_file <- viewer_test_path("coordinated_views", "bundle.R")
utility_file <- viewer_test_path("utility_functions.R")

wire_header <- function(payload) {
  header_length <- sum(as.integer(payload[seq_len(4L)]) * 256^(0:3))
  jsonlite::fromJSON(
    rawToChar(payload[4L + seq_len(header_length)]),
    simplifyVector = FALSE
  )
}

test_that("the browser restores compact linked-view vectors", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("base64enc")
  skip_if_not_installed("jsonlite")
  expect_true(file.exists(wire_file), info = "cell_views_wire.js not found")
  if (!file.exists(wire_file)) {
    return(invisible(NULL))
  }

  helpers <- new.env(parent = globalenv())
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
  payload <- tempfile(fileext = ".json")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(packed, payload)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpack(buffer), (_key, value) => ArrayBuffer.isView(value) ? Array.from(value) : value));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)

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

test_that("cell identities travel separately from the first frame", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")

  helpers <- new.env(parent = globalenv())
  sys.source(bundle_file, envir = helpers)
  payload <- tempfile(fileext = ".bin")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(
    helpers$cv_wire_pack_cells("dataset-1", c("cell-1", "cell-2")),
    payload
  )
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpackCells(buffer)));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_identical(restored$dataset_id, "dataset-1")
  expect_identical(unlist(restored$cells), c("cell-1", "cell-2"))
})

test_that("specialist cell views use the same binary envelope", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")

  helpers <- new.env(parent = globalenv())
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
  payload <- tempfile(fileext = ".bin")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(c(payload, runner)), add = TRUE)
  writeBin(packed, payload)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = global;",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(wire_file, quote = "\"")
      ),
      sprintf(
        "const input = fs.readFileSync(%s);",
        encodeString(payload, quote = "\"")
      ),
      "const buffer = input.buffer.slice(input.byteOffset, input.byteOffset + input.byteLength);",
      "console.log(JSON.stringify(window.CBViewWire.unpack(buffer), (_key, value) => ArrayBuffer.isView(value) ? Array.from(value) : value));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  restored <- jsonlite::fromJSON(output, simplifyVector = FALSE)
  expect_identical(restored$id, "overview_projection")
  expect_equal(restored$data$x[[1L]], 1.25)
  expect_null(restored$data$x[[2L]])
  expect_identical(restored$data$group[[1L]], 1L)
  expect_null(restored$data$group[[2L]])
  expect_identical(unlist(restored$data$selection_key), c("cell-1", "cell-2"))
})

test_that("large specialist views send their first frame before hover data", {
  skip_if_not_installed("jsonlite")
  runtime <- new.env(parent = globalenv())
  sys.source(utility_file, envir = runtime)
  sys.source(bundle_file, envir = runtime)
  sent <- list()
  runtime$input <- list(coordviews_wire_supported = TRUE)
  runtime$session <- list(sendBinaryMessage = function(type, payload) {
    sent[[length(sent) + 1L]] <<- list(type = type, payload = payload)
  })
  keys <- sprintf("cell-%04d", seq_len(4096L))

  runtime$cerebroCellViewRender(
    "overview_projection",
    meta = list(color_type = "categorical", traces = "A"),
    data = list(
      x = list(I(as.numeric(seq_along(keys)))),
      y = list(I(as.numeric(seq_along(keys)))),
      selection_key = list(I(keys)),
      color = list("#123456")
    ),
    hover = list(text = list(I(keys)), hoverinfo = "text")
  )

  expect_identical(vapply(sent, `[[`, character(1), "type"), "cell_view_binary")
  first <- wire_header(sent[[1L]]$payload)
  auxiliary <- runtime$.cerebro_cell_view_aux_pending[[
    paste("overview_projection", first$data$wire_token, sep = ":")
  ]]
  expect_identical(first$data$n, 4096L)
  expect_null(first$data$selection_key)
  expect_identical(first$data$x[[1L]]$`__cv_wire__`, "f32")
  expect_identical(first$hover$hoverinfo, "skip")
  expect_identical(auxiliary$id, "overview_projection")
  expect_identical(unlist(auxiliary$selection_key, use.names = FALSE), keys)
})
