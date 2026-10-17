test_that("All ROIs keeps the background selector for multiple images", {
  main_ui <- viewer_test_path(
    "spatial",
    "UI_projection_main_parameters.R"
  )
  spatial <- list(
    coordinates = data.frame(
      x = c(0, 1),
      y = c(0, 1),
      row.names = c("cell-1", "cell-2")
    )
  )
  server <- function(input, output, session) {
    data_set <- shiny::reactiveVal(TRUE)
    availableSpatial <- function() c("fov", "empty")
    getSpatialData <- function(name) spatial
    getMetaData <- function() {
      data.frame(
        cell_barcode = c("cell-1", "cell-2"),
        roi = c("A", "B")
      )
    }
    getGroups <- function() character()
    serverSideGeneSelector <- function(...) invisible(NULL)
    Cerebro.options <- list(
      spatial_images = list(
        dataset = list(
          fov = c(
            `H&E` = "spatial-assets/he.png",
            DAPI = "spatial-assets/dapi.png"
          )
        )
      )
    )
    available_crb_files <- list(
      files = c(dataset = "dataset.crb"),
      selected = "dataset.crb"
    )
    viewer_current_dataset_key <- function() input$test_dataset %||% "dataset"
    sys.source(main_ui, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(spatial_projection_roi = "__all__")
    session$flushReact()

    selector <- as.character(
      output$spatial_projection_background_selector_UI$html
    )
    expect_match(
      selector,
      'id="spatial_projection_background_image"',
      fixed = TRUE
    )
    expect_match(selector, "external::H&amp;E", fixed = TRUE)
    expect_match(selector, "external::DAPI", fixed = TRUE)
    expect_false(grepl("No Background", selector, fixed = TRUE))
    expect_match(selector, 'multiple="multiple"', fixed = TRUE)
    expect_false(grepl('option value=""', selector, fixed = TRUE))
    expect_match(selector, '"remove_button"', fixed = TRUE)
    expect_match(selector, '"onItemAdd"', fixed = TRUE)
    expect_match(selector, 'value="external::H&amp;E" selected', fixed = TRUE)

    # Same scope preserves a manual alternative and an explicit clear.
    session$setInputs(spatial_projection_background_image = "external::DAPI")
    data_set(2)
    session$flushReact()
    expect_match(output$spatial_projection_background_selector_UI$html,
                 'value="external::DAPI" selected', fixed = TRUE)
    session$setInputs(spatial_projection_background_image = character())
    data_set(3)
    session$flushReact()
    expect_no_match(output$spatial_projection_background_selector_UI$html, ' selected')

    # No image in another FOV/dataset; returning selects the first valid image.
    session$setInputs(spatial_projection_to_display = "empty")
    expect_null(output$spatial_projection_background_selector_UI)
    session$setInputs(spatial_projection_to_display = "fov")
    expect_match(output$spatial_projection_background_selector_UI$html,
                 'value="external::H&amp;E" selected', fixed = TRUE)
    session$setInputs(test_dataset = "other")
    expect_null(output$spatial_projection_background_selector_UI)
    session$setInputs(test_dataset = "dataset")
    expect_match(output$spatial_projection_background_selector_UI$html,
                 'value="external::H&amp;E" selected', fixed = TRUE)
  })
})


test_that("Separate ROIs defaults to one matching embedded image per ROI", {
  main_ui <- viewer_test_path("spatial", "UI_projection_main_parameters.R")
  images <- list()
  for (roi in c("A", "B")) {
    for (name in c("first", "second")) {
      images[[paste(roi, name, sep = "-")]] <- list(
        histology_image = paste0("data:image/png;base64,", roi, name),
        histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        roi_field = "roi", roi_value = roi
      )
    }
  }
  server <- function(input, output, session) {
    data_set <- shiny::reactiveVal(TRUE)
    availableSpatial <- function() "section"
    getSpatialData <- function(name) list(
      coordinates = data.frame(x = 1:3, y = 1:3, row.names = c("a", "b", "c")),
      histology_images = images
    )
    getMetaData <- function() data.frame(cell_barcode = c("a", "b", "c"), roi = c("A", "B", "C"))
    getGroups <- function() character()
    serverSideGeneSelector <- function(...) invisible(NULL)
    viewer_current_dataset_key <- function() "atlas"
    sys.source(main_ui, envir = environment())
  }
  shiny::testServer(server, {
    session$setInputs(spatial_projection_roi = "__separate__")
    html <- output$spatial_projection_background_selector_UI$html
    expect_match(html, 'value="roi-1-background-1" selected', fixed = TRUE)
    expect_match(html, 'value="roi-2-background-1" selected', fixed = TRUE)
    expect_no_match(html, 'value="roi-1-background-2" selected', fixed = TRUE)
    expect_no_match(html, 'roi-3-background', fixed = TRUE)

    session$setInputs(spatial_projection_roi_background_images = "roi-2-background-2")
    data_set(2)
    session$flushReact()
    html <- output$spatial_projection_background_selector_UI$html
    expect_match(html, 'value="roi-2-background-2" selected', fixed = TRUE)
    expect_no_match(html, 'value="roi-1-background-1" selected', fixed = TRUE)

    session$setInputs(spatial_projection_roi = "B")
    html <- output$spatial_projection_background_selector_UI$html
    expect_match(html, 'value="embedded::B-first" selected', fixed = TRUE)
    expect_no_match(html, 'embedded::A-first', fixed = TRUE)
    session$setInputs(spatial_projection_roi = "C")
    expect_null(output$spatial_projection_background_selector_UI)
  })
})
