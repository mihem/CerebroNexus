##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_main_parameters_UI"]] <- renderUI({
  req(data_set())
  ## This output is evaluated even while the Spatial tab is hidden
  ## (suspendWhenHidden = FALSE below). For a data set without spatial data
  ## there is nothing to configure, so bail out early instead of building the
  ## full control set (and, when spatial_images is set, the background-image
  ## picker) on every app start — that extra startup work otherwise competes
  ## with other tabs' first render.
  req(length(availableSpatial()) > 0)
  ## determine which metadata columns to include based on exclude_trivial_metadata
  exclude_trivial <- FALSE
  if (
    exists('Cerebro.options') &&
      !is.null(Cerebro.options[['exclude_trivial_metadata']])
  ) {
    exclude_trivial <- Cerebro.options[['exclude_trivial_metadata']]
  }

  ## build choices based on setting
  if (exclude_trivial == TRUE) {
    ## only include groups from getGroups()
    metadata_cols <- getGroups()
  } else {
    ## include all metadata columns except cell_barcode
    metadata_cols <- colnames(getMetaData())[
      !colnames(getMetaData()) %in% c("cell_barcode")
    ]
  }

  ## prepare background image choices
  background_choices <- c("No Background")
  if (
    exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images"]])
  ) {
    si <- Cerebro.options[["spatial_images"]]
    matched <- FALSE
    if (
      exists("available_crb_files") && !is.null(available_crb_files$selected)
    ) {
      selected <- available_crb_files$selected
      match_idx <- which(available_crb_files$files == selected)
      if (length(match_idx) > 0) {
        nm <- names(available_crb_files$files)[match_idx[1]]
        if (is.null(nm) || is.na(nm)) {
          nm <- available_crb_files$names[match_idx[1]]
        }
        if (!is.null(nm) && !is.na(nm) && nm %in% names(si)) {
          img_paths <- si[[nm]]
          background_choices <- c(
            background_choices,
            setNames(img_paths, basename(img_paths))
          )
          matched <- TRUE
        }
      }
    }
    if (!matched && length(si) > 0) {
      img_paths <- si[[1]]
      background_choices <- c(
        background_choices,
        setNames(img_paths, basename(img_paths))
      )
    }
  }

  tagList(
    selectInput(
      "spatial_projection_to_display",
      label = "Spatial data",
      choices = availableSpatial()
    ),
    selectInput(
      "spatial_projection_plot_type",
      label = "Plot type",
      choices = c("ImageDimPlot", "ImageFeaturePlot"),
      selected = "ImageDimPlot"
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageDimPlot'",
      selectInput(
        "spatial_projection_point_color",
        label = "Color cells by",
        choices = metadata_cols
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
    if (length(background_choices) > 1) {
      tagList(
        selectInput(
          "spatial_projection_background_image",
          label = "Background image",
          choices = background_choices,
          selected = "No Background"
        ),
        conditionalPanel(
          condition = "input.spatial_projection_background_image && input.spatial_projection_background_image !== 'No Background'",
          tagList(
            sliderInput(
              inputId = "spatial_projection_background_opacity",
              label = "Image opacity",
              min = 0,
              max = 1,
              value = 0.6,
              step = 0.05
            )
          )
        )
      )
    }
  )
})

serverSideGeneSelector(
  session,
  "spatial_projection_feature_to_display",
  extra_triggers = function() input[["spatial_projection_plot_type"]]
)

## Render even when tab is hidden so that input values are available for
## programmatic access (e.g. shinytest2) without waiting for tab activation.
outputOptions(
  output,
  "spatial_projection_main_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      spatial_projection_main_parameters_info[["text"]],
      title = spatial_projection_main_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})
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
      <li><b>Color cells by:</b> Select which variable, categorical or continuous, from the meta data should be used to color the cells.</li>
    </ul>
    "
  )
)
