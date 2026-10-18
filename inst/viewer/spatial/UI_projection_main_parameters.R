##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
spatial_projection_dataset <- function() {
  if (!exists("viewer_dataset_request")) {
    return("")
  }
  request <- viewer_dataset_request()
  if (!is.list(request) || !is.list(request$dataset_context)) {
    return("")
  }
  as.character(request$dataset_context$dataset_key %||% "")
}
spatial_projection_selection_dataset <- reactiveVal(
  isolate(spatial_projection_dataset())
)

## Sample, FOV and ROI choices depend on each other, so this reactive follows
## those inputs. Only the small scene output below rebuilds from it; the plot
## type and gene pickers rebuild for a new dataset, never for a scene change.
spatial_projection_scene <- reactive({
  req(data_set())
  req(length(availableSpatial()) > 0)
  dataset <- spatial_projection_dataset()
  previous_dataset <- isolate(spatial_projection_selection_dataset())
  dataset_changed <- !identical(previous_dataset, dataset)
  if (dataset_changed) {
    spatial_projection_selection_dataset(dataset)
  }
  colour_groups <- viewerColourGroupChoices()
  metadata_cols <- colour_groups$choices

  spatial_names <- availableSpatial()
  spatial_records <- stats::setNames(
    lapply(spatial_names, function(name) {
      tryCatch(getSpatialData(name), error = function(error) list())
    }),
    spatial_names
  )
  metadata <- getMetaData()
  sample_facets <- lapply(spatial_records, function(record) {
    spatial_metadata_facet(
      metadata,
      rownames(record[["coordinates"]]) %||% character(),
      c("sample", "sample_id", "orig.ident")
    )
  })
  sample_values <- unique(unlist(
    lapply(sample_facets, `[[`, "values"),
    use.names = FALSE
  ))
  sample_fields <- unique(unlist(
    lapply(sample_facets, `[[`, "field"),
    use.names = FALSE
  ))
  metadata_cols <- setdiff(
    metadata_cols,
    unique(c(sample_fields, "sample", "sample_id", "orig.ident"))
  )
  default_sample <- if (length(sample_values)) sample_values[[1L]] else ""
  requested_sample <- input[["spatial_projection_sample"]]
  selected_sample <- if (dataset_changed) {
    default_sample
  } else {
    requested_sample %||% default_sample
  }
  if (!selected_sample %in% sample_values) {
    selected_sample <- default_sample
  }
  if (nzchar(selected_sample)) {
    spatial_names <- spatial_names[vapply(
      sample_facets[spatial_names],
      function(facet) selected_sample %in% facet$values,
      logical(1)
    )]
  }
  requested_spatial <- input[["spatial_projection_to_display"]]
  current_spatial <- if (dataset_changed) {
    NULL
  } else {
    requested_spatial
  }
  if (is.null(current_spatial) || !(current_spatial %in% spatial_names)) {
    current_spatial <- spatial_names[[1L]]
  }
  current_sd <- spatial_records[[current_spatial]]
  spatial_choices <- spatial_scene_choices(
    spatial_names,
    spatial_records[spatial_names],
    metadata
  )
  current_cells <- rownames(current_sd[["coordinates"]]) %||% character()
  if (nzchar(selected_sample)) {
    current_sample <- spatial_metadata_facet(
      metadata,
      current_cells,
      c("sample", "sample_id", "orig.ident")
    )
    current_cells <- names(current_sample$by_cell)[
      !is.na(current_sample$by_cell) &
        current_sample$by_cell == selected_sample
    ]
  }
  roi_values <- spatial_metadata_facet(
    metadata,
    current_cells,
    c("sample_roi", "roi", "roi_id", "region_of_interest")
  )$values
  requested_roi <- input[["spatial_projection_roi"]]
  selected_roi <- if (dataset_changed) {
    spatial_default_roi_selection(roi_values)
  } else {
    requested_roi %||%
      spatial_default_roi_selection(roi_values)
  }
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  split_columns <- intersect(
    metadata_cols,
    spatial_split_columns(metadata, current_cells, getGroups())
  )
  spatial_split_none_value <- "__none__"
  selected_split <- if (dataset_changed) {
    spatial_split_none_value
  } else {
    isolate(input[["spatial_projection_split_by"]]) %||%
      spatial_split_none_value
  }
  if (!selected_split %in% c(spatial_split_none_value, split_columns)) {
    selected_split <- spatial_split_none_value
  }
  list(
    dataset_changed = dataset_changed,
    colour_groups = colour_groups,
    sample_values = sample_values,
    selected_sample = selected_sample,
    spatial_choices = spatial_choices,
    current_spatial = current_spatial,
    roi_values = roi_values,
    selected_roi = selected_roi,
    split_choices = c("None" = spatial_split_none_value, split_columns),
    selected_split = selected_split
  )
})

output[["spatial_projection_scene_UI"]] <- renderUI({
  scene <- spatial_projection_scene()
  tagList(
    if (length(scene$sample_values) > 1L) {
      selectInput(
        "spatial_projection_sample",
        label = "Sample",
        choices = stats::setNames(scene$sample_values, scene$sample_values),
        selected = scene$selected_sample
      )
    },
    selectInput(
      "spatial_projection_to_display",
      label = "FOV / section",
      choices = scene$spatial_choices,
      selected = scene$current_spatial
    ),
    if (length(scene$roi_values)) {
      selectizeInput(
        "spatial_projection_roi",
        label = "ROI",
        choices = c(
          "All ROIs" = "__all__",
          if (length(scene$roi_values) > 1L) {
            c("Separate ROIs" = "__separate__")
          },
          stats::setNames(scene$roi_values, scene$roi_values)
        ),
        selected = scene$selected_roi
      )
    }
  )
})

## Split-by choices follow the selected scene; update them in place so the
## surrounding controls keep their state.
observe({
  req(identical(input[["sidebar"]], "spatial"))
  ## Nothing to update until the main controls have mounted the selector.
  req(!is.null(input[["spatial_projection_split_by"]]))
  scene <- spatial_projection_scene()
  updateSelectizeInput(
    session,
    "spatial_projection_split_by",
    choices = scene$split_choices,
    selected = scene$selected_split
  )
})

output[["spatial_projection_main_parameters_UI"]] <- renderUI({
  req(data_set())
  ## Hidden outputs are suspended below. Scene changes must not rebuild the
  ## mounted plot-type selector and reset Feature/RGB to ImageDimPlot.
  req(length(availableSpatial()) > 0)
  spatial_projection_dataset()
  colour_groups <- viewerColourGroupChoices()
  scene <- isolate(spatial_projection_scene())
  plot_type <- if (scene$dataset_changed) {
    "ImageDimPlot"
  } else {
    isolate(input[["spatial_projection_plot_type"]]) %||% "ImageDimPlot"
  }
  tagList(
    uiOutput("spatial_projection_scene_UI"),
    selectInput(
      "spatial_projection_plot_type",
      label = "Plot type",
      choices = c("ImageDimPlot", "ImageFeaturePlot", "Co-expression (RGB)"),
      selected = plot_type
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageDimPlot'",
      div(
        class = "spatial-image-dim-controls",
        selectInput(
          "spatial_projection_point_color",
          label = "Colour by",
          choices = colour_groups$choices,
          selected = colour_groups$selected
        ),
        selectizeInput(
          "spatial_projection_split_by",
          label = "Split by",
          choices = scene$split_choices,
          selected = scene$selected_split
        )
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
  current_cells <- rownames(current_sd[["coordinates"]]) %||% character()
  selected_sample <- input[["spatial_projection_sample"]] %||% ""
  if (nzchar(selected_sample)) {
    current_sample <- spatial_metadata_facet(
      getMetaData(),
      current_cells,
      c("sample", "sample_id", "orig.ident")
    )
    current_cells <- names(current_sample$by_cell)[
      !is.na(current_sample$by_cell) &
        current_sample$by_cell == selected_sample
    ]
  }
  roi_values <- spatial_metadata_facet(
    getMetaData(),
    current_cells,
    c("sample_roi", "roi", "roi_id", "region_of_interest")
  )$values
  selected_roi <- input[["spatial_projection_roi"]] %||%
    spatial_default_roi_selection(roi_values)
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  embedded_images <- if (is.null(current_sd)) {
    list()
  } else {
    embedded_spatial_images(current_sd, selected_roi)
  }
  dataset <- viewer_current_dataset_key()
  background_choices <- spatial_background_choices(
    embedded_images,
    configured_spatial_images(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      current_spatial,
      selected_roi
    )
  )
  selection_scope <- paste(
    dataset %||% "",
    current_spatial,
    selected_sample,
    selected_roi,
    sep = "\r"
  )
  previous_scope <- isolate(spatial_background_selection_scope())
  scope_changed <- !identical(previous_scope, selection_scope)
  if (scope_changed) {
    spatial_background_selection_scope(selection_scope)
  }
  roi_background_groups <- if (identical(selected_roi, "__separate__")) {
    spatial_roi_background_groups(
      current_sd,
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      current_spatial,
      roi_values
    )
  } else {
    list()
  }
  background_control <- if (
    !length(roi_background_groups) && !length(background_choices)
  ) {
    NULL
  } else if (length(roi_background_groups)) {
    input_id <- "spatial_projection_roi_background_images"
    selected_input <- if (scope_changed) {
      # Each separate ROI starts with its first matching image, in source order.
      unname(vapply(
        roi_background_groups,
        function(group) {
          unname(group$tokens[[1L]])
        },
        character(1)
      ))
    } else {
      isolate(input[[input_id]]) %||% character()
    }
    selected_tokens <- intersect(
      unlist(lapply(roi_background_groups, `[[`, "tokens"), use.names = FALSE),
      selected_input %||% character()
    )
    grouped_choices <- lapply(roi_background_groups, function(group) {
      stats::setNames(unname(group$tokens), names(group$choices))
    })
    div(
      class = "spatial-roi-background-picker",
      selectizeInput(
        input_id,
        label = "Background images",
        choices = grouped_choices,
        selected = selected_tokens,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          closeAfterSelect = FALSE,
          onItemAdd = I(paste0(
            "function(value) {",
            "var group = value.replace(/-background-.*/, '');",
            "this.items.slice().forEach(function(item) {",
            "if (item !== value && item.replace(/-background-.*/, '') === group) ",
            "this.removeItem(item, true);",
            "}, this);",
            "this.refreshItems();",
            "}"
          ))
        )
      ),
      role = "group"
    )
  } else {
    selected_background <- normalize_spatial_background_choice(
      if (scope_changed) {
        # Default only on entering a new dataset/FOV/sample/ROI scope.
        # Within that scope an explicit clear remains a request to hide images.
        unname(background_choices[[1L]])
      } else {
        isolate(input[["spatial_projection_background_image"]])
      },
      background_choices
    )
    selectizeInput(
      "spatial_projection_background_image",
      label = "Background image",
      choices = background_choices,
      selected = if (identical(selected_background, "none")) {
        character()
      } else {
        selected_background
      },
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        onItemAdd = I(paste0(
          "function(value) {",
          "this.items.slice().forEach(function(item) {",
          "if (item !== value) this.removeItem(item, true);",
          "}, this);",
          "this.refreshItems();",
          "}"
        )),
        placeholder = "Select..."
      )
    )
  }
  background_control
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
  "spatial_projection_scene_UI",
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
      <li><b>Sample:</b> Limit the available spatial scenes to one biological sample.</li>
      <li><b>FOV / section:</b> Select the spatial coordinate system to display.</li>
      <li><b>ROI:</b> Limit cells or spots within the selected spatial scene.</li>
      <li><b>Colour by:</b> Select one of the data set's registered grouping variables.</li>
    </ul>
    "
  )
)
