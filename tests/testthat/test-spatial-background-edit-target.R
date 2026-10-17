test_that("ROI background edits remain independent, temporary and generation scoped", {
  server <- function(input, output, session) {
    context <- shiny::reactiveVal(list(epoch = "test", dataset_key = "arbitrary", generation = 1L))
    viewer_loaded_dataset_context <- context
    viewerDatasetContextEqual <- identical
    viewer_current_dataset_key <- function() context()$dataset_key
    preset <- list(opacity = .6, offsetX = 0, offsetY = 0, scaleX = 1,
                   scaleY = 1, flipX = FALSE, flipY = FALSE, rotation = 12)
    make_target <- function(roi) list(
      descriptor = list(source = "embedded", key = "same-image", label = "Image"),
      identity = list(dataset = "arbitrary", spatial_name = "section", key = "same-image", roi = roi),
      preset = preset)
    parameters <- shiny::reactiveVal(list(roi_mode = "separate",
      roi_backgrounds = list(A = make_target("A"), B = make_target("B"),
        empty = list(descriptor = NULL))))
    spatial_projection_parameters_plot <- parameters
    Cerebro.options <- list(original = TRUE)
    sent <- list()
    cerebroCellViewBackground <- function(...) sent[[length(sent) + 1L]] <<- list(...)
    sys.source(viewer_test_path("spatial", "obj_projection_background_controls.R"), environment())
  }
  shiny::testServer(server, {
    session$flushReact()
    original <- parameters()
    target_invalidations <- 0L
    tracker <- shiny::observe({
      spatial_background_edit_target()
      target_invalidations <<- target_invalidations + 1L
    })
    session$flushReact()
    before_target <- target_invalidations
    parameters(utils::modifyList(original, list(point_size = 8, color_variable = "other")))
    session$flushReact()
    expect_identical(target_invalidations, before_target)
    parameters(original)
    session$flushReact()
    tracker$destroy()
    expect_identical(spatial_background_edit_target()$roi, "A")
    send <- function(identity, opacity, ctx = context()) {
      session$setInputs(spatial_projection_background_controls = list(
        dataset_context = ctx, background_identity = identity,
        values = list(opacity = opacity, offsetX = 30, rotate = 40, flipY = TRUE)))
    }
    a <- spatial_background_edit_target()$identity
    send(a, .25)
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$opacity, .25)
    session$setInputs(spatial_projection_background_target = "B")
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$opacity, .6)
    before <- length(sent)
    send(a, .9) # a queued event from the old controls must not edit B
    expect_length(sent, before)
    send(spatial_background_edit_target()$identity, .8)
    session$setInputs(spatial_projection_background_target = "A")
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$opacity, .25)
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$rotation, 40)
    session$setInputs(spatial_projection_background_reset = 1)
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target()), preset)
    session$setInputs(spatial_projection_background_target = "B")
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$opacity, .8)
    old_context <- context()
    context(utils::modifyList(old_context, list(generation = 2L)))
    session$flushReact()
    expect_equal(spatial_background_edit_preset(spatial_background_edit_target())$opacity, .6)
    before <- length(sent)
    send(spatial_background_edit_target()$identity, .1, old_context)
    expect_length(sent, before)
    expect_identical(parameters(), original)
    expect_identical(Cerebro.options, list(original = TRUE))
    parameters(list(roi_mode = "separate", roi_backgrounds = list()))
    session$flushReact()
    expect_null(spatial_background_edit_target())
  })
})

test_that("renderer edits only the addressed ROI background", {
  skip_if(Sys.which("node") == "")
  expect_identical(system2("node", c(
    shQuote(test_path("fixtures", "spatial-background-targets.js")),
    shQuote(viewer_test_path("www", "cell_views.js"))
  )), 0L)
})
