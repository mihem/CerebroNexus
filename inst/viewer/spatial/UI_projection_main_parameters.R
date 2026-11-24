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
  metadata <- viewerProjectionFirstFrameMetadata()
  section_samples <- spatial_section_samples()
  sample_values <- unique(unlist(section_samples$values, use.names = FALSE))
  sample_fields <- section_samples$fields
  metadata_cols <- setdiff(
    metadata_cols,
    unique(c(sample_fields, spatial_sample_fields))
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
      section_samples$values[spatial_names],
      function(values) selected_sample %in% values,
      logical(1)
    )]
  }
  ## The single view's section. In the grid the scene keeps the one it had:
  ## changing among several sections only rearranges the drawn panels.
  chosen_sections <- spatial_projection_chosen_sections(
    input,
    availableSpatial()
  )
  requested_spatial <- if (length(chosen_sections) > 1L) {
    isolate(spatial_projection_scene_state())$current_spatial %||%
      chosen_sections[[1L]]
  } else if (length(chosen_sections)) {
    chosen_sections[[1L]]
  } else {
    NULL
  }
  current_spatial <- if (dataset_changed) {
    NULL
  } else {
    requested_spatial
  }
  if (is.null(current_spatial) || !(current_spatial %in% spatial_names)) {
    current_spatial <- spatial_names[[1L]]
  }
  spatial_choices <- stats::setNames(spatial_names, spatial_names)
  ## The section's cells by metadata row, kept for the data set: no cell name
  ## is matched for a sample, FOV or ROI change. Its cell names are read only
  ## without an integer index (no Viewer Pack and no stored rows).
  current_cells <- function() {
    record <- tryCatch(getSpatialData(current_spatial), error = function(e) {
      list()
    })
    rownames(record[["coordinates"]]) %||% character()
  }
  current_scope <- spatial_section_scope(
    current_spatial,
    selected_sample,
    metadata,
    current_cells
  )
  roi_values <- current_scope$roi_values
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
  current_rows <- current_scope$scoped_rows %||%
    spatial_section_metadata_rows(current_spatial, metadata, current_cells)
  split_columns <- intersect(
    metadata_cols,
    spatial_split_columns_at(metadata, current_rows, getGroups())
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
  ## The Sections field: one section is the single view, several the grid
  ## (all of the data set's sections, whatever the sample). A new data set
  ## starts as its author chose: its first section, or all of them.
  all_sections <- availableSpatial()
  sections_default <- if (
    identical(spatial_projection_default_layout(), "grid")
  ) {
    all_sections
  } else {
    current_spatial
  }
  ## A browser without a GPU shows 2 panels at a time (the browser reports the
  ## limit; read without a dependency, lifting it keeps the sections shown).
  panel_cap <- suppressWarnings(as.integer(isolate(input[[
    "cerebro_panel_cap"
  ]])))
  if (length(panel_cap) == 1L && !is.na(panel_cap) && panel_cap > 0L) {
    sections_default <- sections_default[
      seq_len(min(length(sections_default), panel_cap))
    ]
  }
  list(
    dataset_changed = dataset_changed,
    colour_groups = colour_groups,
    sample_values = sample_values,
    selected_sample = selected_sample,
    spatial_choices = spatial_choices,
    grid_choices = stats::setNames(all_sections, all_sections),
    sections_default = sections_default,
    ## Without a projection list the grid simply offers no embedding.
    grid_projections = if (length(all_sections) > 1L) {
      tryCatch(
        as.character(availableProjections() %||% character()),
        error = function(error) character()
      )
    } else {
      character()
    },
    current_spatial = current_spatial,
    roi_values = roi_values,
    selected_roi = selected_roi,
    split_choices = c("None" = spatial_split_none_value, split_columns),
    selected_split = selected_split
  )
})

## The scene the toolbar shows, published on change only. Its controls bind in
## the browser with the values the scene chose; that recomputes the scene to
## the same result, and must not render the scene and the background picker
## again (a second and third round trip on the first visit).
spatial_projection_scene_state <- reactiveVal(NULL)
observe(
  {
    ## Off the Spatial page nothing shows the scene (unset in server tests).
    sidebar <- input[["sidebar"]]
    if (!is.null(sidebar) && !identical(sidebar, "spatial")) {
      return(invisible(NULL))
    }
    scene <- tryCatch(spatial_projection_scene(), error = function(error) NULL)
    if (!is.null(scene)) {
      scene$dataset_changed <- NULL
      viewerPublishChanged(spatial_projection_scene_state, scene)
    }
  },
  priority = 1000
)
spatial_projection_scene_shown <- reactive({
  scene <- spatial_projection_scene_state()
  req(scene)
  scene
})

output[["spatial_projection_scene_UI"]] <- renderUI({
  scene <- spatial_projection_scene_shown()
  ## Several sections draw the grid; one draws the single view, with its
  ## sample and ROI controls.
  grid_js <- paste(
    "input.spatial_projection_sections &&",
    "input.spatial_projection_sections.length > 1"
  )
  ## What the field holds, in its order (a re-render must not reorder it):
  ## the chosen sections first, as selectize lists the selected options in
  ## the order of the choices.
  selected <- intersect(
    as.character(isolate(input[["spatial_projection_sections"]]) %||% ""),
    scene$grid_choices
  )
  if (!length(selected)) {
    selected <- scene$sections_default
  }
  ordered <- c(selected, setdiff(scene$grid_choices, selected))
  sections <- selectizeInput(
    "spatial_projection_sections",
    label = "Sections",
    choices = stats::setNames(ordered, ordered),
    selected = selected,
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      closeAfterSelect = FALSE,
      placeholder = "Pick one or more sections"
    )
  )
  single_controls <- tagList(
    if (length(scene$sample_values) > 1L) {
      selectInput(
        "spatial_projection_sample",
        label = "Sample",
        choices = stats::setNames(scene$sample_values, scene$sample_values),
        selected = scene$selected_sample
      )
    },
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
  if (length(scene$grid_choices) < 2L) {
    return(tagList(sections, single_controls))
  }
  tagList(
    sections,
    cerebroPanelCapNote(),
    conditionalPanel(
      condition = paste0("!(", grid_js, ")"),
      div(class = "spatial-single-controls", single_controls)
    ),
    conditionalPanel(
      condition = grid_js,
      div(
        class = "spatial-grid-controls",
        cerebroColumnsInput(
          "spatial_projection_grid_columns",
          most = length(scene$grid_choices),
          selected = isolate(input[["spatial_projection_grid_columns"]])
        ),
        selectInput(
          "spatial_projection_grid_projection",
          label = "Embedding",
          choices = c(
            "None" = "__none__",
            stats::setNames(scene$grid_projections, scene$grid_projections)
          ),
          selected = isolate(input[["spatial_projection_grid_projection"]]) %||%
            "__none__"
        ),
        ## A switch, as Linked views' Cross-view highlight: zooming or panning
        ## one section moves the others to the same part of their tissue.
        div(
          class = "cerebro-groups-switches",
          title = "Zoom and pan every section together",
          shinyWidgets::materialSwitch(
            inputId = "spatial_projection_grid_sync_zoom",
            label = "Sync zoom",
            value = isTRUE(isolate(input[[
              "spatial_projection_grid_sync_zoom"
            ]])),
            status = "primary",
            inline = TRUE
          )
        )
      )
    )
  )
})

## Split-by choices follow the selected scene; update them in place so the
## surrounding controls keep their state.
observe({
  req(identical(input[["sidebar"]], "spatial"))
  ## Nothing to update until the main controls have mounted the selector; a
  ## choice in it must not run this and reset it to the scene's old value.
  req(!is.null(isolate(input[["spatial_projection_split_by"]])))
  scene <- spatial_projection_scene_shown()
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
  ## The scene controls (spatial_projection_scene_UI) are a sibling output in
  ## the page shell (UI.R): nested here, they would wait for this output to
  ## reach the browser and bind before rendering.
  tagList(
    selectInput(
      "spatial_projection_plot_type",
      label = "Display mode",
      ## The values stay Seurat's names; the labels say what is drawn.
      choices = c(
        "Groups" = "ImageDimPlot",
        "Gene expression" = "ImageFeaturePlot",
        "RGB co-expression" = "Co-expression (RGB)"
      ),
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
        conditionalPanel(
          condition = "!(input.spatial_projection_sections && input.spatial_projection_sections.length > 1)",
          selectizeInput(
            "spatial_projection_split_by",
            label = "Split by",
            choices = scene$split_choices,
            selected = scene$selected_split
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
      selectizeInput(
        "spatial_projection_feature_to_display",
        label = "Gene",
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
        cerebroRgbGeneFields(
          paste0("spatial_projection_coexpr_", c("r", "g", "b"))
        ),
        ## As on Gene expression; in the grid, every section splits.
        selectInput(
          "spatial_projection_rgb_layout",
          "RGB layout",
          choices = c(
            "Composite RGB" = "composite",
            "Separate channels" = "separate"
          ),
          selected = isolate(input[["spatial_projection_rgb_layout"]]) %||%
            "composite"
        )
      )
    )
  )
})

spatial_background_selection_scope <- reactiveVal(NULL)

## The FOV, sample and ROI the background picker is for: the bound scene
## inputs or, until they bind, the values they will bind with (the first
## sample when there are several, the first section showing it), as the frame
## takes them (obj_projection_parameters_plot.R). Published on change only, so
## the scene controls binding in the browser do not render the picker again.
spatial_background_scope <- reactive({
  req(data_set())
  spatial_names <- availableSpatial()
  req(length(spatial_names) > 0)
  selected_sample <- input[["spatial_projection_sample"]]
  if (
    is.null(selected_sample) &&
      !"spatial_projection_sample" %in% isolate(names(input))
  ) {
    samples <- tryCatch(
      unique(unlist(spatial_section_samples()$values, use.names = FALSE)),
      error = function(error) character()
    )
    selected_sample <- if (length(samples) > 1L) samples[[1L]] else ""
  }
  selected_sample <- selected_sample %||% ""
  current_spatial <- spatial_projection_single_section(
    input,
    availableSpatial()
  )
  if (is.null(current_spatial) || !(current_spatial %in% spatial_names)) {
    shown <- tryCatch(
      spatial_sample_sections(selected_sample)$sections,
      error = function(error) character()
    )
    current_spatial <- if (length(shown) && shown[[1L]] %in% spatial_names) {
      shown[[1L]]
    } else {
      spatial_names[[1L]]
    }
  }
  roi_values <- spatial_section_scope(
    current_spatial,
    selected_sample,
    viewerProjectionFirstFrameMetadata(),
    function() {
      record <- tryCatch(getSpatialData(current_spatial), error = function(e) {
        list()
      })
      rownames(record[["coordinates"]]) %||% character()
    }
  )$roi_values
  selected_roi <- input[["spatial_projection_roi"]] %||%
    spatial_default_roi_selection(roi_values)
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  list(
    data = data_set(),
    spatial = current_spatial,
    sample = selected_sample,
    roi_values = roi_values,
    roi = selected_roi
  )
})
spatial_background_scope_state <- reactiveVal(NULL)
observe(
  {
    ## Off the Spatial page nothing shows the picker (unset in server tests).
    sidebar <- input[["sidebar"]]
    if (!is.null(sidebar) && !identical(sidebar, "spatial")) {
      return(invisible(NULL))
    }
    scope <- tryCatch(spatial_background_scope(), error = function(error) NULL)
    if (!is.null(scope)) {
      viewerPublishChanged(spatial_background_scope_state, scope)
    }
  },
  priority = 1000
)

output[["spatial_projection_background_selector_UI"]] <- renderUI({
  scope <- spatial_background_scope_state()
  req(scope)
  current_spatial <- scope$spatial
  selected_sample <- scope$sample
  roi_values <- scope$roi_values
  selected_roi <- scope$roi
  current_sd <- tryCatch(
    getSpatialData(current_spatial),
    error = function(e) NULL
  )
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

## The grid's picture picker: the pictures grouped by section, as the Sections
## field and Linked views' Colour by look. A section starts with its first
## picture, as the single view does, and keeps what was chosen for it (or
## removed) while other sections come and go; a section shows at most one
## (choosing another in its group replaces it); another data set starts over.
## Each chosen picture is a removable chip named "section: picture".
spatial_grid_background_dataset <- reactiveVal(NULL)
spatial_grid_background_seen <- reactiveVal(character())
spatial_grid_background_js <- list(
  on_item_add = paste0(
    "function(value) {",
    "var group = value.replace(/-background-.*/, '');",
    "this.items.slice().forEach(function(item) {",
    "if (item !== value && item.replace(/-background-.*/, '') === group) ",
    "this.removeItem(item, true);",
    "}, this);",
    "this.refreshItems();",
    "}"
  ),
  render = paste0(
    "{",
    "item: function(item, escape) {",
    "return '<div class=\"item\">' + escape(item.optgroup || '') + ': ' +",
    "escape(item.label) + '</div>';",
    "}",
    "}"
  )
)

output[["spatial_projection_grid_background_UI"]] <- renderUI({
  sections <- availableSpatial()
  chosen <- spatial_projection_chosen_sections(input, sections)
  req(length(chosen) > 1L)
  dataset <- viewer_current_dataset_key()
  groups <- spatial_grid_background_groups(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    chosen,
    sections,
    spatial_grid_section_record
  )
  if (!length(groups)) {
    return(NULL)
  }
  previous <- isolate(input[["spatial_projection_grid_background_images"]])
  if (!identical(isolate(spatial_grid_background_dataset()), dataset)) {
    previous <- NULL
    spatial_grid_background_dataset(dataset)
    spatial_grid_background_seen(character())
  }
  tokens <- unlist(lapply(groups, function(group) unname(group$tokens)))
  ## The sections new to the picker start with their first picture.
  seen <- isolate(spatial_grid_background_seen())
  fresh <- setdiff(names(groups), seen)
  spatial_grid_background_seen(union(seen, names(groups)))
  starts <- unlist(lapply(groups[fresh], function(group) {
    unname(group$tokens)[[1L]]
  }))
  div(
    class = "spatial-grid-background-picker",
    selectizeInput(
      "spatial_projection_grid_background_images",
      label = "Background images",
      choices = lapply(groups, function(group) {
        stats::setNames(unname(group$tokens), names(group$tokens))
      }),
      selected = union(intersect(previous %||% character(), tokens), starts),
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        onItemAdd = I(spatial_grid_background_js$on_item_add),
        render = I(spatial_grid_background_js$render)
      )
    ),
    role = "group"
  )
})
outputOptions(
  output,
  "spatial_projection_grid_background_UI",
  suspendWhenHidden = TRUE
)
