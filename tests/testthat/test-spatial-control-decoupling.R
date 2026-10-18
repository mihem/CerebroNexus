test_that("Spatial styling and colour edits do not resample observations", {
  server <- function(input, output, session) {
    controls <- shiny::reactiveVal(list(
      projection = "fov",
      percentage_cells_to_show = 50,
      point_size = 3,
      point_opacity = .6,
      color_variable = "type"
    ))
    spatial_projection_controls <- controls
    metadata <- data.frame(cell_barcode = paste0("cell", 1:100))
    availableSpatial <- function() "fov"
    viewerSpatialCoordinates <- function(name) {
      data.frame(x = 1:100, y = 100:1, row.names = metadata$cell_barcode)
    }
    viewerPackCurrent <- function() NULL
    viewerPackSpatialIndex <- function(...) NULL
    getMetaData <- function() metadata
    viewerProjectionFirstFrameMetadata <- function() metadata
    getGroups <- function() character()
    spatial_projection_selected_sample <- function() {
      input$spatial_projection_sample %||% ""
    }
    sys.source(
      viewer_test_path("spatial", "obj_projection_cells_to_show.R"),
      environment()
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    cells <- spatial_projection_cells_to_show()
    expect_length(cells, 50)
    for (change in list(
      list(point_size = 8),
      list(point_opacity = .2),
      list(color_variable = "new_type")
    )) {
      controls(utils::modifyList(controls(), change))
      session$flushReact()
      expect_identical(spatial_projection_cells_to_show(), cells)
    }
    controls(utils::modifyList(controls(), list(percentage_cells_to_show = 25)))
    session$flushReact()
    expect_length(spatial_projection_cells_to_show(), 25)
  })
})

test_that("same-layout updates retain panels and unchanged backgrounds only", {
  skip_if(Sys.which("node") == "")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(test_path("fixtures", "single-view-update-scope.js")),
        shQuote(viewer_test_path("www", "cell_views.js"))
      )
    ),
    0L
  )
})
