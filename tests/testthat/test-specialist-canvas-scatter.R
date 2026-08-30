inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "app.R"))][1]
if (is.na(inst_dir)) {
  inst_dir <- system.file(package = "CerebroNexus")
}

viewer_dir <- file.path(inst_dir, "viewer")

run_geom_node <- function(body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(
          file.path(viewer_dir, "www", "cv-geom.js"),
          quote = "\""
        )
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("shared viewport and gesture math is reversible", {
  out <- run_geom_node(paste0(
    "const G = window.CBGeom;",
    "const zoom = G.zoomView(null, 0.75, [0.5, 0.5]);",
    "const pan = G.panView(zoom, 0.1, -0.2);",
    "const frame = {x:10,y:20,width:200,height:100};",
    "const unit = [[0.2,0.3],[0.7,0.8]];",
    "console.log(JSON.stringify({",
    "bounds:G.viewBounds(pan),",
    "fit:G.fitView(0.2,0.6,0.3,0.5,1.25,0.02),",
    "roundTrip:G.screenToUnit(pan,frame,G.unitToScreen(pan,frame,unit)),",
    "select:G.dragKind('lasso', false, 0, false),",
    "pan:G.dragKind('lasso', false, 0, true),",
    "orbit:G.dragKind('lasso', true, 0, false)",
    "}));"
  ))

  expect_equal(attr(out, "status"), NULL)
  expect_equal(
    jsonlite::fromJSON(out),
    list(
      bounds = list(x0 = 0.05, x1 = 0.8, y0 = -0.025, y1 = 0.725),
      fit = list(cx = 0.4, cy = 0.4, span = 0.5),
      roundTrip = rbind(c(0.2, 0.3), c(0.7, 0.8)),
      select = "select",
      pan = "pan",
      orbit = "orbit"
    )
  )
})

test_that("selection composition shows count, top groups, and Other", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(viewer_dir, "utility_functions.R"), envir = env)
  metadata <- data.frame(
    cell_barcode = paste0("cell", 1:8),
    sample = rep(c("s1", "s2"), 4),
    cell_type = c(
      "T cells",
      "T cells",
      "B cells",
      "Monocytes",
      "NK",
      "DC",
      "Platelets",
      "Erythroid"
    ),
    stringsAsFactors = FALSE
  )
  selection <- data.frame(
    selection_key = paste0("cell", 1:8),
    stringsAsFactors = FALSE
  )

  card <- as.character(env$cerebroSelectionSummary(
    selection,
    source = "umap",
    metadata = metadata,
    groups = c("sample", "cell_type"),
    color_variable = "sample",
    composition = TRUE
  ))

  expect_match(card, "Selected 8 / 8 cells", fixed = TRUE)
  expect_match(card, "by cell_type", fixed = TRUE)
  expect_match(card, "T cells", fixed = TRUE)
  expect_match(card, "Other", fixed = TRUE)
})

test_that("selection summary reports source and escapes labels", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(viewer_dir, "utility_functions.R"), envir = env)
  metadata <- data.frame(
    cell_barcode = paste0("cell", 1:4),
    sample = c("sample_1", "sample_2", "sample_1", "sample_1"),
    cell_type = c("T cells", "T cells", "B cells", "T cells"),
    stringsAsFactors = FALSE
  )
  selection <- data.frame(
    selection_key = c("cell1", "cell2", "cell3"),
    stringsAsFactors = FALSE
  )

  summary <- as.character(env$cerebroSelectionSummary(
    selection,
    source = "umap",
    metadata = metadata,
    groups = c("sample", "cell_type"),
    color_variable = "sample"
  ))
  expect_match(
    gsub("[[:space:]]+", " ", summary),
    "Selected <b>3</b> / 4 cells"
  )
  expect_match(summary, "T cells · 67%", fixed = TRUE)
  expect_match(summary, "Selected in umap", fixed = TRUE)

  metadata$cell_type[1:3] <- "<img src=x onerror=alert(1)>"
  escaped <- as.character(env$cerebroSelectionSummary(
    selection,
    source = "<script>alert(1)</script>",
    metadata = metadata,
    groups = "cell_type"
  ))
  expect_no_match(escaped, "<img src=x", fixed = TRUE)
  expect_no_match(escaped, "<script>", fixed = TRUE)
  expect_match(escaped, "&lt;img", fixed = TRUE)
  expect_match(escaped, "&lt;script&gt;", fixed = TRUE)
})
