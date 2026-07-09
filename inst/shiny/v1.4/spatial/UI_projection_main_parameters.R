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

  ## Real .crb data may carry a genuine histology image embedded in the spatial
  ## slot. Offer it ONLY when the CURRENTLY DISPLAYED spatial entry has one — not
  ## when any dataset does. Otherwise a bead-only platform (Slide-seq, no image
  ## by design) would list "__embedded__" and show a neighbouring dataset's
  ## tissue image behind its cells.
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
  has_embedded <- !is.null(current_sd) && !is.null(current_sd$histology_image)
  if (has_embedded) {
    background_choices <- c(
      background_choices,
      "Tissue background (H&E / DAPI)" = "__embedded__"
    )
  }

  if (
    exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images"]])
  ) {
    si <- Cerebro.options[["spatial_images"]]
    have_selection <-
      exists("available_crb_files") && !is.null(available_crb_files$selected)
    if (have_selection) {
      ## Multi-dataset app: only offer the external image configured for the
      ## CURRENTLY selected dataset. Do NOT fall back to another dataset's image
      ## when this one has no entry — that would show, e.g., the Visium H&E behind
      ## the Xenium cells.
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
        }
      }
    } else if (length(si) > 0) {
      ## Single-dataset app with no selection context: use the sole/first image.
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
      choices = c("ImageDimPlot", "ImageFeaturePlot", "Co-expression (RGB)"),
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
    ## Co-expression: one gene per RGB channel; each cell's colour blends them so
    ## spatial overlap reads as a mixed hue. Any channel may be left empty.
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'Co-expression (RGB)'",
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
    ),
    if (length(background_choices) > 1) {
      ## Only the image PICKER lives in Main parameters. All the appearance
      ## adjustments (opacity, move, flip, scale, rotate) live in Additional
      ## parameters and are decoupled from the scatter plot.
      selectInput(
        "spatial_projection_background_image",
        label = "Background image",
        choices = background_choices,
        selected = "No Background"
      )
    }
  )
})

serverSideGeneSelector(
  session,
  "spatial_projection_feature_to_display",
  extra_triggers = function() input[["spatial_projection_plot_type"]],
  active = function() length(availableSpatial()) > 0
)

## Co-expression channel gene pickers. Same server-side population + active gate
## as the feature selector, so their later:: callbacks don't leak into other
## tabs' tests when no spatial data is present.
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
      active = function() length(availableSpatial()) > 0
    )
  }
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
