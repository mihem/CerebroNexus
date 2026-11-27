## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

for (file in c(
  "spatial_coordinate_contract.R",
  "spatial_coordinate_transform.R"
)) {
  sys.source(
    builder_test_inst_path("viewer", "core", file),
    envir = environment()
  )
}
for (file in c(
  "spatial.R",
  "preview.R",
  "state/core.R",
  "extras.R",
  "worker.R",
  "plan/defaults.R",
  "notify.R",
  "spatial_alignment_server.R"
)) {
  sys.source(builder_test_inst_path("builder", file), envir = environment())
}

region_server_entry <- function(region_subset = list()) {
  list(
    id = "dataset-a",
    snapshot = list(
      path = "/private/dataset-a",
      owner_token = "owner-a",
      object_md5 = strrep("A", 32L)
    ),
    profile = list(
      images = "slice1",
      spatial_scenes = list(list(id = "slice1", label = "slice1")),
      extras = list()
    ),
    levels = list(group = c("A", "B")),
    settings = list(
      name = "Dataset A",
      images = list(),
      spatial_point_appearance = list(),
      spatial_roi_settings = list(),
      spatial_region_subset = region_subset,
      default_group = "group",
      default_projection = "umap",
      palette = "cerebro"
    )
  )
}

region_server_preview <- function() {
  list(
    available = TRUE,
    bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
    section = list(id = "slice1", kind = "spatial", unit = "pixels"),
    projection_name = "umap",
    roi = list(field = NULL, values = character(), selected = ""),
    capped = FALSE,
    spatial = data.frame(
      cell_barcode = c("cell-a", "cell-b"),
      x = c(2, 8),
      y = c(3, 7),
      group = c("A", "B"),
      roi = c("", ""),
      stringsAsFactors = FALSE
    )
  )
}

region_server_event <- function(contract, action, polygons = list()) {
  list(
    action = action,
    polygons = polygons,
    viewKey = contract$viewKey,
    nonce = 1
  )
}

test_that("drawn regions go to the worker with the polygons of the section", {
  entry <- region_server_entry()
  current_entry <- shiny::reactiveVal(entry)
  alignment_preview <- shiny::reactiveVal(NULL)
  requests <- list()
  notices <- character()

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = shiny::reactiveVal(entry$id),
        entry_of = function(id) current_entry(),
        entries = shiny::reactive(list(current_entry())),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          if (identical(request$kind, "region_resolve")) {
            requests[[length(requests) + 1L]] <<- request
          }
          TRUE
        },
        commit_images = function(updated, images) invisible(updated),
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(region_server_preview())
      session$flushReact()
      contract <- alignment$canvas_contract()
      expect_identical(contract$section, "slice1")

      square <- list(list(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5)))
      ## A shape drawn on another view is not applied.
      stale <- region_server_event(contract, "apply", square)
      stale$viewKey <- "another-view"
      session$setInputs(builder_region_event = stale)
      session$flushReact()
      expect_length(requests, 0L)

      ## A shape that is not a polygon is refused.
      session$setInputs(
        builder_region_event = region_server_event(
          contract,
          "apply",
          list(list(x = c(0, 1), y = c(0, 1)))
        )
      )
      session$flushReact()
      expect_length(requests, 0L)

      session$setInputs(
        builder_region_event = region_server_event(contract, "apply", square)
      )
      session$flushReact()
      expect_length(requests, 1L)
      request <- requests[[1L]]
      expect_identical(request$kind, "region_resolve")
      expect_identical(request$id, "dataset-a")
      expect_identical(request$source_md5, strrep("a", 32L))
      expect_identical(request$levels, entry$levels)
      expect_identical(names(request$sections), "slice1")
      expect_identical(
        request$sections$slice1$polygons[[1L]],
        list(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))
      )

      ## Nothing is stored yet, so there is nothing to clear.
      session$setInputs(
        builder_region_event = region_server_event(contract, "clear")
      )
      session$flushReact()
      expect_length(requests, 1L)
    }
  )
})

test_that("clearing asks the worker to resolve the sections that remain", {
  stored <- list(
    sections = list(
      slice1 = list(polygons = list(list(x = c(0, 5, 5), y = c(0, 0, 5)))),
      slice2 = list(polygons = list(list(x = c(1, 6, 6), y = c(1, 1, 6))))
    ),
    resolved = list(
      source_md5 = strrep("a", 32L),
      cells = list(schema_version = 2L, count = 2, md5 = strrep("a", 32L)),
      kept = 2L,
      total = 4L,
      group_levels = list(),
      sections = list()
    )
  )
  entry <- region_server_entry(stored)
  alignment_preview <- shiny::reactiveVal(NULL)
  requests <- list()

  shiny::testServer(
    function(input, output, session) {
      alignment <- builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = shiny::reactiveVal(entry$id),
        entry_of = function(id) entry,
        entries = shiny::reactive(list(entry)),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) {
          if (identical(request$kind, "region_resolve")) {
            requests[[length(requests) + 1L]] <<- request
          }
          TRUE
        },
        commit_images = function(updated, images) invisible(updated),
        alignment_preview = alignment_preview,
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      alignment_preview(region_server_preview())
      session$flushReact()
      contract <- alignment$canvas_contract()

      session$setInputs(
        builder_region_event = region_server_event(contract, "clear")
      )
      session$flushReact()
      expect_length(requests, 1L)
      expect_identical(names(requests[[1L]]$sections), "slice2")

      session$setInputs(
        builder_region_event = region_server_event(contract, "clear_all")
      )
      session$flushReact()
      expect_length(requests, 2L)
      expect_length(requests[[2L]]$sections, 0L)
    }
  )
})

test_that("the canvas scene carries the stored regions and what they keep", {
  stored <- list(
    sections = list(
      slice1 = list(polygons = list(list(x = c(0, 5, 5), y = c(0, 0, 5))))
    ),
    resolved = list(
      source_md5 = strrep("a", 32L),
      cells = list(schema_version = 2L, count = 2, md5 = strrep("a", 32L)),
      kept = 2L,
      total = 4L,
      group_levels = list(),
      sections = list(slice1 = list(kept = 2L, total = 4L))
    )
  )
  scene <- builder_spatial_canvas_scene(
    preview = region_server_preview(),
    colors = c(A = "#111111", B = "#222222"),
    identity = "view",
    generation = 1L,
    section = "slice1",
    region_subset = stored
  )
  expect_identical(
    unclass(scene$regions[[1L]]$x),
    c(0, 5, 5)
  )
  expect_identical(scene$regionSummary$kept, 2L)
  expect_identical(scene$regionSummary$sectionTotal, 4L)

  other <- builder_spatial_canvas_scene(
    preview = region_server_preview(),
    colors = c(A = "#111111", B = "#222222"),
    identity = "view",
    generation = 1L,
    section = "slice2",
    region_subset = stored
  )
  expect_length(other$regions, 0L)
  expect_null(other$regionSummary$sectionKept)

  plain <- builder_spatial_canvas_scene(
    preview = region_server_preview(),
    colors = c(A = "#111111", B = "#222222"),
    identity = "view",
    generation = 1L,
    section = "slice1"
  )
  expect_length(plain$regions, 0L)
  expect_null(plain$regionSummary)
})
