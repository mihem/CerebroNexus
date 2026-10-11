##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_main_parameters_UI"]] <- renderUI({
  req(data_set())
  ## Hidden outputs are suspended below. Sidebar changes must not rebuild the
  ## mounted plot-type selector and reset Feature/RGB to ImageDimPlot.
  req(length(availableSpatial()) > 0)
  colour_groups <- viewerColourGroupChoices()

  current_spatial <- isolate(input[["spatial_projection_to_display"]])
  if (
    is.null(current_spatial) ||
      !(current_spatial %in% availableSpatial())
  ) {
    current_spatial <- availableSpatial()[1]
  }
  tagList(
    selectInput(
      "spatial_projection_to_display",
      label = "Spatial data",
      choices = availableSpatial(),
      selected = current_spatial
    ),
    selectInput(
      "spatial_projection_plot_type",
      label = "Plot type",
      choices = c("ImageDimPlot", "ImageFeaturePlot", "Co-expression (RGB)"),
      selected = "ImageDimPlot"
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageDimPlot'",
      selectInput(
        "spatial_projection_point_color",
        label = "Colour by",
        choices = colour_groups$choices,
        selected = colour_groups$selected
      )
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
      selectizeInput(
        "spatial_projection_feature_to_display",
        label = "Feature/Gene",
        choices = NULL,
        multiple = FALSE,
        options = list(
          maxOptions = 1000,
          placeholder = 'Select a gene...',
          create = FALSE,
          loadThrottle = 300
        )
      )
    ),
    ## Co-expression: one gene per RGB channel; each cell's colour blends them so
    ## spatial overlap reads as a mixed hue. Any channel may be left empty.
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'Co-expression (RGB)'",
      div(
        class = "spatial-rgb-controls",
        selectizeInput(
          "spatial_projection_coexpr_r",
          label = "Red channel gene",
          choices = NULL,
          options = list(
            maxOptions = 1000,
            placeholder = 'Gene for red...',
            create = FALSE,
            loadThrottle = 300
          )
        ),
        selectizeInput(
          "spatial_projection_coexpr_g",
          label = "Green channel gene",
          choices = NULL,
          options = list(
            maxOptions = 1000,
            placeholder = 'Gene for green...',
            create = FALSE,
            loadThrottle = 300
          )
        ),
        selectizeInput(
          "spatial_projection_coexpr_b",
          label = "Blue channel gene",
          choices = NULL,
          options = list(
            maxOptions = 1000,
            placeholder = 'Gene for blue...',
            create = FALSE,
            loadThrottle = 300
          )
        )
      )
    )
  )
})

spatial_background_selection_scope <- reactiveVal(NULL)

output[["spatial_projection_background_selector_UI"]] <- renderUI({
  req(data_set())
  req(length(availableSpatial()) > 0)

  current_spatial <- input[["spatial_projection_to_display"]]
  if (
    is.null(current_spatial) ||
      !(current_spatial %in% availableSpatial())
  ) {
    current_spatial <- availableSpatial()[1]
  }
  current_sd <- tryCatch(
    getSpatialData(current_spatial),
    error = function(e) NULL
  )
  embedded_images <- if (is.null(current_sd)) {
    list()
  } else {
    embedded_spatial_images(current_sd)
  }
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  background_choices <- spatial_background_choices(
    embedded_images,
    configured_spatial_images(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      current_spatial
    )
  )

  scope <- list(dataset = dataset, spatial = current_spatial)
  previous_scope <- isolate(spatial_background_selection_scope())
  scope_changed <- !identical(previous_scope, scope)
  if (scope_changed) {
    spatial_background_selection_scope(scope)
  }

  selectInput(
    "spatial_projection_background_image",
    label = "Background image",
    choices = background_choices,
    selected = normalize_spatial_background_choice(
      if (scope_changed) {
        NULL
      } else {
        isolate(input[["spatial_projection_background_image"]])
      },
      background_choices
    )
  )
})

serverSideGeneSelector(
  session,
  "spatial_projection_feature_to_display",
  extra_triggers = function() input[["spatial_projection_plot_type"]],
  active = function() {
    identical(input[["sidebar"]], "spatial") &&
      identical(
        input[["spatial_projection_plot_type"]],
        "ImageFeaturePlot"
      ) &&
      length(availableSpatial()) > 0
  }
)

## Co-expression channel gene pickers. Populate each hidden gene selector only
## when its plot type is active. This keeps their dataobj requests out of the
## default ImageDimPlot waterfall while preserving lazy initialization when the
## user switches modes.
##
## Use lapply, NOT a for loop: serverSideGeneSelector references the input id
## lazily, and a for loop's index variable is a single shared binding — all
## three registrations would capture its final value ("...coexpr_b"), so only
## the blue channel would get a working server-side search. lapply gives each
## iteration its own `channel_id` argument, so each selector binds correctly.
lapply(
  c(
    "spatial_projection_coexpr_r",
    "spatial_projection_coexpr_g",
    "spatial_projection_coexpr_b"
  ),
  function(channel_id) {
    serverSideGeneSelector(
      session,
      channel_id,
      extra_triggers = function() input[["spatial_projection_plot_type"]],
      active = function() {
        identical(input[["sidebar"]], "spatial") &&
          identical(
            input[["spatial_projection_plot_type"]],
            "Co-expression (RGB)"
          ) &&
          length(availableSpatial()) > 0
      }
    )
  }
)

## The browser mounts these controls when the Spatial page is shown.
outputOptions(
  output,
  "spatial_projection_main_parameters_UI",
  suspendWhenHidden = TRUE
)
outputOptions(
  output,
  "spatial_projection_background_selector_UI",
  suspendWhenHidden = TRUE
)
##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "spatial_projection_main_parameters_info",
  spatial_projection_main_parameters_info
)
##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_projection_main_parameters_info <- list(
  title = "Main parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
      <li><b>Colour by:</b> Select one of the data set's registered grouping variables.</li>
    </ul>
    "
  )
)
