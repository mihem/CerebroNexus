##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
## Hidden Settings inputs bind after the first frame. Keep their defaults in
## server state so that mounting the drawer cannot invalidate the million-cell
## data reactive. Labels, borders and square layout are browser-only appearance
## patches; region outlines remain reactive because they require hull geometry.
spatial_projection_appearance <- reactiveValues(
  group_labels = FALSE,
  draw_border = FALSE,
  keep_square = FALSE
)
spatial_projection_region_outlines <- reactiveVal(FALSE)
spatial_author_overridden <- new.env(parent = emptyenv())
observeEvent(
  viewer_requested_dataset_context()$dataset_key,
  {
    display <- current_page_display_defaults("spatial")
    controls <- c(
      group_labels = "spatial_projection_group_labels",
      draw_border = "spatial_projection_point_border",
      keep_square = "spatial_projection_keep_square",
      region_outlines = "spatial_projection_show_region_outlines"
    )
    for (key in names(controls)) {
      if (isTRUE(spatial_author_overridden[[key]])) {
        next
      }
      if (identical(key, "region_outlines")) {
        spatial_projection_region_outlines(display[[key]])
      } else {
        spatial_projection_appearance[[key]] <- display[[key]]
      }
      updateCheckboxInput(session, controls[[key]], value = display[[key]])
    }
  },
  ignoreInit = FALSE,
  priority = 1100
)

## The Spatial shell can request its first frame before its renderUI controls
## have made a browser round trip. Resolve the exact same defaults on the
## server and publish them through a deduplicating reactive value: later input
## bindings with those values are a no-op instead of causing a second 1M frame.
spatial_projection_controls <- reactiveVal(NULL)

## Until a renderUI control has bound, frames must use the value that control
## will bind with; otherwise its binding invalidates the first million-cell
## frame. Bound inputs keep their value even when it is NULL (a cleared
## multi-select), so presence in `input` separates "not yet bound" from "none".
spatial_input_bound <- function(id) id %in% isolate(names(input))

## The sample and FOV the main controls select by default (see
## UI_projection_main_parameters.R): the first sample when there are several,
## and the first section showing it.
spatial_projection_scene_memo <- viewerMemoOne()
spatial_projection_scene_defaults <- function() {
  spatial_projection_scene_memo(list(data_set()), function() {
    spatial_names <- availableSpatial()
    metadata <- getMetaData()
    facets <- lapply(spatial_names, function(name) {
      record <- tryCatch(getSpatialData(name), error = function(error) list())
      spatial_metadata_facet(
        metadata,
        rownames(record[["coordinates"]]) %||% character(),
        c("sample", "sample_id", "orig.ident")
      )$values
    })
    samples <- unique(unlist(facets, use.names = FALSE))
    sample <- if (length(samples) > 1L) samples[[1L]] else ""
    shown <- if (length(samples)) {
      spatial_names[vapply(facets, function(values) samples[[1L]] %in% values, logical(1))]
    } else {
      spatial_names
    }
    list(
      sample = sample,
      projection = if (length(shown)) shown[[1L]] else spatial_names[1L]
    )
  })
}

spatial_projection_selected_sample <- function() {
  value <- input[["spatial_projection_sample"]]
  if (is.null(value) && !spatial_input_bound("spatial_projection_sample")) {
    return(spatial_projection_scene_defaults()$sample)
  }
  value %||% ""
}

observe(
  {
    req(input[["spatial_projection_render_request"]])
    spatial_names <- availableSpatial()
    req(length(spatial_names) > 0L)

    defaults <- current_spatial_scatter_defaults()
    projection <- input[["spatial_projection_to_display"]]
    if (is.null(projection) || !(projection %in% spatial_names)) {
      projection <- spatial_projection_scene_defaults()$projection
    }
    if (is.null(projection) || !(projection %in% spatial_names)) {
      projection <- spatial_names[[1L]]
    }
    plot_type <- input[["spatial_projection_plot_type"]]
    if (is.null(plot_type) || !nzchar(plot_type)) {
      plot_type <- "ImageDimPlot"
    }

    exclude_trivial <- isTRUE(as.logical(
      Cerebro.options[["exclude_trivial_metadata"]]
    ))
    color_choices <- if (exclude_trivial) {
      getGroups()
    } else {
      setdiff(colnames(viewerProjectionFirstFrameMetadata()), "cell_barcode")
    }
    color_variable <- input[["spatial_projection_point_color"]]
    if (is.null(color_variable) || !(color_variable %in% color_choices)) {
      req(length(color_choices) > 0L)
      ## The Colour by control selects the main group by default.
      default_color <- viewerColourGroupChoices()$selected
      color_variable <- if (isTRUE(default_color %in% color_choices)) {
        default_color
      } else {
        color_choices[[1L]]
      }
    }

    value_or_default <- function(value, default) {
      if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        default
      } else {
        value[[1L]]
      }
    }
    controls <- list(
      projection = as.character(projection[[1L]]),
      plot_type = as.character(plot_type[[1L]]),
      color_variable = as.character(color_variable[[1L]]),
      ## Gene selectors bind as "" when empty; treat not-yet-bound the same.
      feature_to_display = input[["spatial_projection_feature_to_display"]] %||% "",
      coexpr_r = input[["spatial_projection_coexpr_r"]] %||% "",
      coexpr_g = input[["spatial_projection_coexpr_g"]] %||% "",
      coexpr_b = input[["spatial_projection_coexpr_b"]] %||% "",
      point_size = as.numeric(value_or_default(
        isolate(input[["spatial_projection_point_size"]]),
        defaults$point_size
      )),
      point_opacity = as.numeric(value_or_default(
        isolate(input[["spatial_projection_point_opacity"]]),
        defaults$point_opacity
      )),
      percentage_cells_to_show = as.numeric(value_or_default(
        input[["spatial_projection_percentage_cells_to_show"]],
        defaults$percentage_cells_to_show
      ))
    )
    if (!identical(isolate(spatial_projection_controls()), controls)) {
      spatial_projection_controls(controls)
    }
  },
  priority = 1000
)

spatial_update_appearance <- function(name, value) {
  if (identical(spatial_projection_appearance[[name]], value)) {
    return(FALSE)
  }
  spatial_projection_appearance[[name]] <- value
  TRUE
}

spatial_send_appearance <- function(values) {
  identity <- viewerDatasetIdentity()
  req(identity$fingerprint)
  session$sendCustomMessage(
    "cell_view_appearance",
    list(
      id = "spatial_projection",
      dataset_context = viewer_loaded_dataset_context(),
      dataset_fingerprint = identity$fingerprint,
      values = values
    )
  )
}

## Point styling is a renderer patch, not a new coordinate/sample request.
spatial_point_patch_values <- new.env(parent = emptyenv())
observeEvent(list(input[["spatial_projection_point_size"]],
                  input[["spatial_projection_point_opacity"]]), {
  values <- list(point_size = input[["spatial_projection_point_size"]],
                 point_opacity = input[["spatial_projection_point_opacity"]])
  values <- Filter(function(x) is.numeric(x) && length(x) == 1L && is.finite(x), values)
  key <- as.character(jsonlite::toJSON(list(viewer_loaded_dataset_context(),
    input[["spatial_projection_to_display"]], input[["spatial_projection_roi"]]), auto_unbox = TRUE))
  previous <- spatial_point_patch_values[[key]] %||%
    spatial_projection_controls()[c("point_size", "point_opacity")]
  spatial_point_patch_values[[key]] <- utils::modifyList(previous, values)
  changed <- values[!vapply(names(values), function(name) {
    isTRUE(all.equal(values[[name]], previous[[name]]))
  }, logical(1))]
  if (length(changed)) spatial_send_appearance(changed)
}, ignoreInit = TRUE)

observeEvent(
  input[["spatial_projection_group_labels"]],
  {
    value <- isTRUE(input[["spatial_projection_group_labels"]])
    if (spatial_update_appearance("group_labels", value)) {
      spatial_author_overridden$group_labels <- TRUE
      spatial_send_appearance(list(group_labels = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_point_border"]],
  {
    value <- isTRUE(input[["spatial_projection_point_border"]])
    if (spatial_update_appearance("draw_border", value)) {
      spatial_author_overridden$draw_border <- TRUE
      spatial_send_appearance(list(draw_border = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_keep_square"]],
  {
    value <- isTRUE(input[["spatial_projection_keep_square"]])
    if (spatial_update_appearance("keep_square", value)) {
      spatial_author_overridden$keep_square <- TRUE
      spatial_send_appearance(list(keep_square = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_show_region_outlines"]],
  {
    value <- isTRUE(input[["spatial_projection_show_region_outlines"]])
    if (!identical(spatial_projection_region_outlines(), value)) {
      spatial_author_overridden$region_outlines <- TRUE
      spatial_projection_region_outlines(value)
    }
  },
  ignoreNULL = TRUE
)

spatial_projection_facet_memo <- viewerMemoOne()
spatial_projection_parameters_plot_current <- reactive({
  controls <- spatial_projection_controls()
  req(
    controls,
    controls$projection %in% availableSpatial(),
    controls$plot_type,
    controls$point_size,
    controls$point_opacity,
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  plot_type <- controls$plot_type
  color_variable <- NULL
  feature_to_display <- NULL
  split_by <- ""

  if (plot_type == "ImageDimPlot") {
    color_variable <- controls$color_variable
    req(
      color_variable,
      color_variable %in% colnames(viewerProjectionFirstFrameMetadata())
    )
  } else if (plot_type == "ImageFeaturePlot") {
    feature_to_display <- controls$feature_to_display
    req(feature_to_display)
    color_variable <- feature_to_display
  } else if (plot_type == "Co-expression (RGB)") {
    ## One gene per channel; any channel may be empty. Require at least one so
    ## the render has something to colour by.
    coexpr_genes <- list(
      r = controls$coexpr_r,
      g = controls$coexpr_g,
      b = controls$coexpr_b
    )
    blank <- function(x) is.null(x) || !nzchar(x)
    req(
      !(blank(coexpr_genes$r) && blank(coexpr_genes$g) && blank(coexpr_genes$b))
    )
    ## The cell colour comes from the RGB blend, not a metadata column, but the
    ## downstream code still indexes metadata[[color_variable]] — give it a valid
    ## placeholder column so that lookup can't fail on a NULL name.
    color_variable <- "cell_index"
  }

  spatial_data <- getSpatialData(controls$projection)
  req(spatial_data, spatial_data$coordinates)
  n_dimensions <- ncol(spatial_data$coordinates)
  spatial_name <- controls$projection
  dataset <- viewer_current_dataset_key()
  selected_sample <- spatial_projection_selected_sample()
  ## Facets match every barcode against the metadata. They depend only on the
  ## section and sample, so colour or feature changes reuse them.
  roi_facet <- spatial_projection_facet_memo(
    list(data_set(), spatial_name, selected_sample),
    function() {
      spatial_cells <- rownames(spatial_data$coordinates) %||% character()
      if (nzchar(selected_sample)) {
        sample_facet <- spatial_metadata_facet(
          getMetaData(),
          spatial_cells,
          c("sample", "sample_id", "orig.ident")
        )
        spatial_cells <- names(sample_facet$by_cell)[
          !is.na(sample_facet$by_cell) &
            sample_facet$by_cell == selected_sample
        ]
      }
      spatial_metadata_facet(
        getMetaData(),
        spatial_cells,
        c("sample_roi", "roi", "roi_id", "region_of_interest")
      )
    }
  )
  roi_values <- roi_facet$values
  selected_roi <- input[["spatial_projection_roi"]] %||%
    spatial_default_roi_selection(roi_values)
  roi_modes <- c("__all__", if (length(roi_values) > 1L) "__separate__")
  if (!selected_roi %in% c(roi_modes, roi_values)) {
    selected_roi <- spatial_default_roi_selection(roi_values)
  }
  roi_value <- spatial_roi_value(selected_roi)
  separate_roi_values <- character()
  if (identical(selected_roi, "__separate__")) {
    split_by <- roi_facet$field %||% ""
    separate_roi_values <- roi_facet$values
  }
  roi_point_appearance <- list()
  if (identical(selected_roi, "__separate__")) {
    default_appearance <- current_spatial_scatter_defaults()
    section_appearance <- spatialPointAppearance(
      Cerebro.options,
      dataset,
      spatial_name
    )
    if (!is.null(section_appearance)) {
      default_appearance[names(section_appearance)] <- section_appearance
    }
    keep_size <- isTRUE(all.equal(
      as.numeric(isolate(input[["spatial_projection_point_size"]])),
      as.numeric(default_appearance$point_size)
    ))
    keep_opacity <- isTRUE(all.equal(
      as.numeric(isolate(input[["spatial_projection_point_opacity"]])),
      as.numeric(default_appearance$point_opacity)
    ))
    roi_point_appearance <- stats::setNames(
      lapply(separate_roi_values, function(roi) {
        setting <- spatialRoiSetting(
          Cerebro.options,
          dataset,
          spatial_name,
          roi
        )
        if (is.null(setting)) {
          return(list())
        }
        setting[c(
          if (keep_opacity) "point_opacity",
          if (keep_size) "point_size"
        )]
      }),
      separate_roi_values
    )
  }
  roi_backgrounds <- list()
  if (identical(selected_roi, "__separate__")) {
    groups <- spatial_roi_background_groups(
      spatial_data,
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      separate_roi_values
    )
    selected_tokens <- input[["spatial_projection_roi_background_images"]]
    if (
      is.null(selected_tokens) &&
        !spatial_input_bound("spatial_projection_roi_background_images")
    ) {
      ## The picker starts with each ROI's first image.
      selected_tokens <- unname(vapply(groups, function(group) {
        unname(group$tokens[[1L]])
      }, character(1)))
    }
    selections <- spatial_roi_background_selections(groups, selected_tokens)
    roi_backgrounds <- Map(
      function(group, selected) {
        descriptor <- resolve_spatial_background(
          selected,
          group$embedded,
          group$external
        )
        identity <- spatial_background_identity(
          dataset,
          spatial_name,
          descriptor
        )
        if (!is.null(identity)) {
          identity$roi <- group$roi
        }
        list(
          descriptor = descriptor,
          identity = identity,
          preset = spatial_background_preset(
            if (exists("Cerebro.options")) Cerebro.options else NULL,
            dataset,
            spatial_name,
            descriptor
          ),
          image_allowlist = vapply(
            group$external,
            `[[`,
            character(1),
            "path"
          )
        )
      },
      groups,
      selections
    )
    names(roi_backgrounds) <- names(groups)
    embedded_images <- list()
    configured_background_images <- list()
    background_image <- "none"
    background_descriptor <- NULL
  } else {
    embedded_images <- embedded_spatial_images(spatial_data, roi_value)
    configured_background_images <- configured_spatial_images(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      roi_value
    )
    background_choices <- spatial_background_choices(
      embedded_images,
      configured_background_images
    )
    background_input <- input[["spatial_projection_background_image"]]
    if (
      is.null(background_input) &&
        !spatial_input_bound("spatial_projection_background_image") &&
        length(background_choices)
    ) {
      ## The selector starts with the first image of the scope.
      background_input <- unname(background_choices[[1L]])
    }
    background_image <- normalize_spatial_background_choice(
      background_input,
      background_choices
    )
    background_descriptor <- resolve_spatial_background(
      background_image,
      embedded_images,
      configured_background_images
    )
  }
  background_identity <- spatial_background_identity(
    dataset,
    spatial_name,
    background_descriptor
  )
  background_preset <- spatial_background_preset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    background_descriptor
  )
  ## Every new image starts from its own preset. Interactive appearance changes
  ## arrive later as one context-and-image-scoped atomic payload, so stale
  ## dynamic input bindings can never seed a new render.
  background_opacity <- background_preset$opacity

  parameters <- list(
    projection = controls$projection,
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    split_by = split_by,
    roi_selection = selected_roi,
    roi_mode = if (identical(selected_roi, "__separate__")) {
      "separate"
    } else if (spatial_roi_is_specific(selected_roi)) {
      "single"
    } else {
      "all"
    },
    roi_order = separate_roi_values,
    roi_point_appearance = roi_point_appearance,
    feature_to_display = feature_to_display,
    coexpr_r = controls$coexpr_r,
    coexpr_g = controls$coexpr_g,
    coexpr_b = controls$coexpr_b,
    point_size = controls$point_size,
    point_opacity = controls$point_opacity,
    draw_border = isolate(spatial_projection_appearance$draw_border),
    group_labels = isolate(spatial_projection_appearance$group_labels),
    keep_square = isolate(spatial_projection_appearance$keep_square),
    show_region_outlines = spatial_projection_region_outlines(),
    x_range = NULL,
    y_range = NULL,
    background_image = background_image,
    background_descriptor = background_descriptor,
    roi_backgrounds = roi_backgrounds,
    background_identity = background_identity,
    background_image_allowlist = vapply(
      configured_background_images,
      `[[`,
      character(1),
      "path"
    ),
    background_flip_x = background_preset$flipX,
    background_flip_y = background_preset$flipY,
    background_scale_x = background_preset$scaleX,
    background_scale_y = background_preset$scaleY,
    background_offset_x = background_preset$offsetX,
    background_offset_y = background_preset$offsetY,
    background_rotation = background_preset$rotation,
    background_opacity = background_opacity,
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

## Controls re-bind (first visit, re-rendered selectors) with the values the
## parameters above already used. Recomputing would still rebuild the
## million-cell frame downstream, so publish the parameters on change only.
spatial_projection_parameters_state <- reactiveVal(NULL)
observe({
  value <- tryCatch(
    spatial_projection_parameters_plot_current(),
    error = function(error) NULL
  )
  if (!identical(isolate(spatial_projection_parameters_state()), value)) {
    spatial_projection_parameters_state(value)
  }
}, priority = 900)
spatial_projection_parameters_plot <- reactive({
  value <- spatial_projection_parameters_state()
  req(value)
  value
})

##
spatial_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['spatial_projection_to_display']], {
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
