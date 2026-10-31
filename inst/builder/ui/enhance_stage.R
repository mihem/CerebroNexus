## Guided Enhance stage.

builder_enhance_model <- function(
  id,
  profile,
  state,
  settings,
  modules,
  active_section = NULL,
  active_image = NULL,
  active_roi = NULL
) {
  manifest <- state$manifest %||% list()
  retained <- Filter(
    function(entry) {
      identical(entry$status, "valid") &&
        !identical(entry$disposition, "rejected")
    },
    unname(manifest)
  )
  auto_retained <- lapply(retained, function(entry) {
    list(
      id = entry$id,
      label = entry$summary %||% entry$id,
      enabled_pages = entry$pages %||% character(),
      cost = "No additional build cost.",
      network = "No network access.",
      prerequisite = "Already validated by the frozen content manifest.",
      replacement_policy = paste0(
        "Existing validated content is ",
        entry$disposition %||% "retained",
        "."
      ),
      skip_consequence = "Not optional: frozen valid content stays in the CRB."
    )
  })
  extras <- profile$extras %||% list()
  has_trekker <- any(vapply(
    extras,
    function(entry) {
      identical(entry$key %||% "", "trekker") && isTRUE(entry$found)
    },
    logical(1)
  ))
  spatial_scenes <- profile$spatial_scenes %||% list()
  spatial_sections <- if (length(spatial_scenes)) {
    vapply(spatial_scenes, `[[`, character(1), "id")
  } else {
    unique(c(
      builder_profile_spatial_reductions(profile),
      profile$images %||% character(),
      if (has_trekker) "trekker" else character()
    ))
  }
  spatial_section_labels <- if (length(spatial_scenes)) {
    stats::setNames(
      vapply(
        spatial_scenes,
        function(scene) scene$label %||% scene$id,
        character(1)
      ),
      spatial_sections
    )
  } else {
    stats::setNames(spatial_sections, spatial_sections)
  }
  spatial_samples <- unique(unlist(
    lapply(spatial_scenes, function(scene) {
      scene$annotations$sample$values %||% character()
    }),
    use.names = FALSE
  ))
  if (length(spatial_sections) && !length(spatial_samples)) {
    spatial_samples <- settings$name %||% id
  }
  spatial_sample_sections <- stats::setNames(
    lapply(spatial_samples, function(sample) {
      matched <- vapply(
        spatial_scenes,
        function(scene) {
          sample %in% (scene$annotations$sample$values %||% character())
        },
        logical(1)
      )
      sections <- unique(c(
        spatial_sections[matched],
        setdiff(
          spatial_sections,
          vapply(spatial_scenes, `[[`, character(1), "id")
        )
      ))
      if (length(sections)) sections else spatial_sections
    }),
    spatial_samples
  )
  spatial_images <- builder_image_collection_normalize(
    settings$images %||% list()
  )
  selected_section <- active_section
  if (is.null(selected_section) || !selected_section %in% spatial_sections) {
    selected_section <- if (length(spatial_sections)) {
      spatial_sections[[1L]]
    } else {
      NULL
    }
  }
  active_roi <- as.character(active_roi %||% "")[[1L]]
  active_roi_settings <- if (
    is.null(selected_section) ||
      !nzchar(active_roi) ||
      identical(active_roi, "__separate__")
  ) {
    NULL
  } else {
    (settings$spatial_roi_settings %||% list())[[selected_section]][[
      active_roi
    ]]
  }
  active_coordinate_transform <- if (is.null(selected_section)) {
    NULL
  } else if (!is.null(active_roi_settings)) {
    active_roi_settings
  } else {
    (settings$spatial_coordinate_transforms %||% list())[[selected_section]]
  }
  image_choices <- if (is.null(selected_section)) {
    stats::setNames(character(), character())
  } else {
    choices <- builder_image_collection_choices(
      spatial_images,
      selected_section,
      active_roi
    )
    if (!length(choices) && nzchar(active_roi)) {
      choices <- builder_image_collection_choices(
        spatial_images,
        selected_section,
        ""
      )
    }
    choices
  }
  image_labels <- unname(image_choices)
  selected_image <- if (
    !is.null(active_image) && active_image %in% image_labels
  ) {
    active_image
  } else if (length(image_labels)) {
    image_labels[[1L]]
  } else {
    NULL
  }
  active_image_record <- if (is.null(selected_image)) {
    NULL
  } else {
    spatial_images[[selected_section]][[selected_image]]
  }
  control_defaults <- builder_alignment_defaults()
  controls <- .builder_alignment_parameters(
    active_image_record %||% control_defaults
  )
  if (!is.null(active_roi_settings)) {
    controls[c("point_opacity", "point_size")] <-
      active_roi_settings[c("point_opacity", "point_size")]
  }
  controls$cell_percentage <- settings$spatial_percentage_cells_to_show %||% 100
  ranges <- builder_alignment_control_ranges(active_image_record)
  controls$dx_min <- ranges$dx$min
  controls$dx_max <- ranges$dx$max
  controls$dx_step <- ranges$dx$step
  controls$dy_min <- ranges$dy$min
  controls$dy_max <- ranges$dy$max
  controls$dy_step <- ranges$dy$step
  list(
    id = id,
    modules = modules,
    attachments = list(
      tables = list(
        label = "Extra material",
        enabled_pages = "extra material",
        relevant = TRUE,
        cost = "Lists sheets now and reads selected tables when building.",
        network = "No network access.",
        prerequisite = "Requires a readable CSV, TSV, XLS, XLSX, or XLSM file.",
        selected = names(settings$tables %||% list()) %||% character(),
        replacement_policy = "Each selected sheet is kept separately.",
        skip_consequence = paste(
          "Skipped tables will not appear in Extra material."
        )
      ),
      histology = list(
        label = "Spatial alignment",
        enabled_pages = "spatial",
        relevant = length(spatial_sections) > 0L ||
          any(vapply(
            manifest,
            function(entry) {
              (identical(entry$id, "spatial") ||
                "spatial" %in% (entry$pages %||% character())) &&
                !identical(entry$status, "not_applicable")
            },
            logical(1)
          )),
        cost = "Image validation and alignment.",
        network = "No network access.",
        prerequisite = "Requires spatial FOVs and coordinates.",
        sections = spatial_sections,
        section_labels = spatial_section_labels,
        samples = spatial_samples,
        sample_sections = spatial_sample_sections,
        scenes = spatial_scenes,
        active_section = selected_section,
        active_image = selected_image,
        image_choices = image_choices,
        coordinate_rotation = active_coordinate_transform$rotation_degrees %||%
          0,
        controls = controls,
        images = spatial_images,
        spatial_image_storage = "external",
        selected = names(settings$images %||% list()) %||% character(),
        replacement_policy = "Named images remain separate within each FOV.",
        skip_consequence = paste(
          "Sections without an image keep points-only spatial views."
        )
      )
    ),
    auto_retained = auto_retained
  )
}

builder_enhance_analysis_profile <- function(profile, organism, assay = NULL) {
  profile$organism_guess <- organism
  assay <- assay %||% profile$default_assay %||% NULL
  assay_profile <- if (is.character(assay) && length(assay) == 1L) {
    profile$assay_profiles[[assay]] %||% NULL
  }
  if (is.list(assay_profile)) {
    profile$analysis_assay <- assay
    profile$analysis_layers <- as.character(
      assay_profile$layers %||% character()
    )
  }
  profile
}

builder_enhance_analysis_applicability <- function(
  step,
  organism,
  blocked_reason = NULL
) {
  organism <- organism %||% ""
  intrinsic_not_applicable <- identical(step$id, "percent_mt_ribo") &&
    !organism %in% c("hg", "mm")
  list(
    relevant = !intrinsic_not_applicable,
    blocked = !intrinsic_not_applicable &&
      builder_stage_has_text(blocked_reason %||% ""),
    blocked_reason = if (intrinsic_not_applicable) NULL else blocked_reason
  )
}

builder_enhance_modules <- function(profile, settings) {
  steps <- builder_analysis_steps()
  marker_imports <- settings$marker_imports %||% list()
  analysis_profile <- builder_enhance_analysis_profile(
    profile,
    settings$organism,
    settings$assay
  )
  lapply(steps, function(step) {
    imported_marker_summary <- if (
      identical(step$id, "marker_genes") && length(marker_imports)
    ) {
      methods <- unique(vapply(
        marker_imports,
        function(record) as.character(record$method %||% ""),
        character(1)
      ))
      methods <- methods[nzchar(methods)]
      source_count <- sum(lengths(lapply(marker_imports, `[[`, "sources")))
      paste0(
        "Imported: ",
        if (length(methods)) {
          paste(methods, collapse = ", ")
        } else {
          "Marker genes"
        },
        " · ",
        source_count,
        if (source_count == 1L) " source." else " sources."
      )
    } else {
      NULL
    }
    blocked <- try(
      builder_step_blocked(step, analysis_profile, settings$analyses),
      silent = TRUE
    )
    blocked_reason <- if (inherits(blocked, "try-error")) {
      "Prerequisites could not be evaluated."
    } else {
      blocked
    }
    applicability <- builder_enhance_analysis_applicability(
      step,
      settings$organism,
      blocked_reason
    )
    enabled_pages <- switch(
      step$id,
      percent_mt_ribo = "overview",
      most_expressed = "most expressed genes",
      marker_genes = "marker genes",
      enriched_pathways = "enriched pathways",
      character()
    )
    list(
      id = step$id,
      label = step$label,
      relevant = applicability$relevant,
      blocked = applicability$blocked,
      blocked_reason = applicability$blocked_reason,
      selected = step$id %in%
        settings$analyses ||
        (identical(step$id, "marker_genes") &&
          length(settings$marker_imports %||% list()) > 0L),
      enabled_pages = enabled_pages,
      replacement_policy = paste0(
        "A newly computed ",
        step$label,
        " result replaces the same existing result."
      ),
      skip_consequence = paste0(
        "No new ",
        step$label,
        " result or matching page is added."
      ),
      consequence = imported_marker_summary %||% step$note,
      cost = step$cost,
      network = if (isTRUE(step$network)) {
        "Network access is required."
      } else {
        "No network access."
      },
      prerequisite = if (is.null(step$needs)) {
        "No analysis dependency."
      } else {
        paste("Requires", step$needs, "first.")
      }
    )
  })
}

builder_enhance_modules_ui <- function(id, modules) {
  ns <- NS(id)
  modules <- Filter(function(module) isTRUE(module$relevant), modules)
  if (!length(modules)) {
    return(p(class = "hint", "No optional modules apply to this dataset."))
  }
  tagList(lapply(modules, function(module) {
    marker_action <- identical(module$id, "marker_genes")
    div(
      class = paste(
        c(
          "enhance-module",
          if (isTRUE(module$selected)) "is-selected" else NULL,
          if (isTRUE(module$blocked)) "is-blocked" else NULL
        ),
        collapse = " "
      ),
      if (marker_action) {
        tags$button(
          id = ns("analysis_marker_genes_action"),
          type = "button",
          class = paste(
            "enhance-module-select marker-genes-action action-button",
            if (isTRUE(module$selected)) "is-selected" else ""
          ),
          `data-val` = "0",
          `aria-pressed` = if (isTRUE(module$selected)) "true" else "false",
          disabled = if (isTRUE(module$blocked)) "disabled",
          tags$span(class = "enhance-module-title", module$label),
          p(class = "consequence", module$consequence %||% ""),
          if (isTRUE(module$blocked)) {
            p(class = "blocked", module$blocked_reason %||% "Unavailable")
          }
        )
      } else {
        tags$label(
          class = "enhance-module-select",
          tags$input(
            id = ns(paste0("analysis_", module$id)),
            type = "checkbox",
            class = paste(
              "enhance-module-checkbox visually-hidden shiny-input-checkbox"
            ),
            checked = if (isTRUE(module$selected)) "checked",
            disabled = if (isTRUE(module$blocked)) "disabled"
          ),
          tags$span(class = "enhance-module-title", module$label),
          p(class = "consequence", module$consequence %||% ""),
          if (isTRUE(module$blocked)) {
            p(class = "blocked", module$blocked_reason %||% "Unavailable")
          }
        )
      },
      tags$button(
        type = "button",
        class = "enhance-info-button",
        `aria-label` = paste("More information about", module$label),
        `data-title` = module$label %||% "",
        `data-description` = module$consequence %||% "",
        `data-pages` = module$enabled_pages %||% "",
        `data-cost` = module$cost %||% "",
        `data-network` = module$network %||% "",
        `data-prerequisite` = module$prerequisite %||% "",
        `data-replacement` = module$replacement_policy %||% "",
        `data-skip` = module$skip_consequence %||% "",
        "i"
      )
    )
  }))
}

builder_tissue_image_file_ui <- function(id, record) {
  ns <- NS(id)
  source <- record$source %||% list()
  filename <- builder_safe_file_name(source$name, "Tissue image")
  extension <- tools::file_ext(filename)
  stem <- if (nzchar(extension)) {
    substr(filename, 1L, nchar(filename) - nchar(extension) - 1L)
  } else {
    filename
  }
  div(
    class = "builder-file-list builder-file-list--single",
    div(
      class = "builder-file-item enhance-tissue-file-item",
      div(
        class = "enhance-tissue-file-meta",
        strong(
          class = "enhance-tissue-file-name",
          title = filename,
          span(class = "enhance-tissue-file-stem", stem),
          if (nzchar(extension)) {
            span(
              class = "enhance-tissue-file-extension",
              paste0(".", extension)
            )
          }
        ),
        span(
          class = "hint enhance-tissue-file-size",
          builder_file_human_size(source$size %||% NA_real_)
        )
      ),
      div(
        class = "builder-action-row enhance-tissue-file-action-row",
        div(
          class = "enhance-tissue-file-actions",
          actionButton(
            ns("rename_image"),
            "Rename image",
            class = "btn"
          ),
          actionButton(
            ns("drop_image"),
            "Remove image",
            class = "btn btn-remove-soft"
          )
        )
      )
    )
  )
}

builder_alignment_plot_output <- function(id, label) {
  div(
    class = "spatial-alignment-plot-frame",
    tags$canvas(
      id = id,
      class = "builder-spatial-canvas",
      role = "img",
      `aria-label` = label,
      `aria-describedby` = paste0(id, "-summary")
    ),
    tags$div(
      id = paste0(id, "-tooltip"),
      class = "builder-spatial-canvas-tooltip",
      role = "tooltip",
      hidden = "hidden"
    ),
    tags$p(
      id = paste0(id, "-summary"),
      class = "visually-hidden builder-spatial-canvas-summary",
      "Spatial alignment preview is loading."
    )
  )
}

builder_coordinate_control_ui <- function(
  ns,
  id,
  label,
  min,
  max,
  value,
  step
) {
  div(
    class = "spatial-coordinate-control",
    tags$label(
      class = "spatial-coordinate-control-label",
      `for` = ns(id),
      label
    ),
    div(
      class = "spatial-coordinate-control-slider",
      sliderInput(
        ns(id),
        NULL,
        min,
        max,
        value,
        step = step,
        ticks = FALSE
      )
    ),
    div(
      class = "spatial-coordinate-control-number",
      numericInput(
        ns(paste0(id, "_number")),
        tags$span(class = "visually-hidden", paste(label, "value")),
        value,
        min = min,
        max = max,
        step = step %||% NA_real_
      )
    )
  )
}

builder_spatial_scene_inventory_ui <- function(scenes) {
  scenes <- Filter(is.list, scenes %||% list())
  if (!length(scenes)) {
    return(NULL)
  }
  annotation <- function(value, label) {
    if (!is.list(value)) {
      return(NULL)
    }
    values <- value$values %||% character()
    if (!is.atomic(values)) {
      return(NULL)
    }
    values <- as.character(values)
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) {
      return(NULL)
    }
    field <- value$field %||% "metadata"
    suffix <- if (isTRUE(value$truncated)) ", …" else ""
    p(
      class = "builder-spatial-scene-annotation",
      paste0(
        label,
        " (",
        field,
        "): ",
        paste(values, collapse = ", "),
        suffix
      )
    )
  }
  tags$details(
    class = "builder-spatial-scene-inventory",
    tags$summary("Scene inventory"),
    lapply(scenes, function(scene) {
      count <- scene$observations$count %||% 0L
      if (
        !is.numeric(count) ||
          length(count) != 1L ||
          !is.finite(count) ||
          count < 0
      ) {
        count <- 0L
      }
      layers <- unique(as.character(scene$layers %||% character()))
      layers <- layers[!is.na(layers) & nzchar(layers)]
      div(
        class = "builder-spatial-scene",
        tags$strong(scene$label %||% scene$id %||% "Spatial scene"),
        p(
          class = "hint",
          paste0(
            formatC(count, format = "f", big.mark = ",", digits = 0),
            " observations · ",
            scene$unit %||% "Spatial coordinate units"
          )
        ),
        annotation(scene$annotations$sample, "Samples"),
        annotation(scene$annotations$roi, "ROIs"),
        div(
          class = "builder-spatial-scene-layers",
          lapply(layers, function(layer) {
            span(class = "label label-default", layer)
          })
        ),
        if ("molecules" %in% layers) {
          p(
            class = "hint",
            "Molecule export is capped at 200,000 records; the CRB records ",
            "whether truncation occurred."
          )
        }
      )
    })
  )
}

builder_spatial_alignment_ui <- function(id, model) {
  ns <- NS(id)
  sections <- as.character(model$sections %||% character())
  section_labels <- model$section_labels %||% sections
  if (!is.null(names(section_labels))) {
    section_labels <- unname(section_labels[sections])
  }
  section_labels <- as.character(section_labels)
  if (length(section_labels) != length(sections) || anyNA(section_labels)) {
    section_labels <- sections
  }
  section_labels[
    sections == "trekker" & section_labels == "trekker"
  ] <- "Trekker physical space"
  choices <- stats::setNames(sections, section_labels)
  selected_section <- model$active_section
  if (
    length(sections) &&
      (is.null(selected_section) || !selected_section %in% sections)
  ) {
    selected_section <- sections[[1L]]
  }
  initial_image_choices <- if (length(sections)) {
    model$image_choices %||% stats::setNames(character(), character())
  } else {
    stats::setNames(character(), character())
  }
  selected_scene <- Filter(
    function(scene) identical(scene$id, selected_section),
    model$scenes %||% list()
  )
  initial_rois <- if (length(selected_scene)) {
    selected_scene[[1L]]$annotations$roi$values %||% character()
  } else {
    character()
  }
  selected_image <- model$active_image
  if (
    is.null(selected_image) ||
      !selected_image %in% unname(initial_image_choices)
  ) {
    selected_image <- if (length(initial_image_choices)) {
      unname(initial_image_choices[[1L]])
    } else {
      character()
    }
  }
  controls <- model$controls %||% builder_alignment_defaults()
  scale_value <- .builder_alignment_parameters(controls)$scale
  scale_max <- max(10, scale_value)
  scale_step <- builder_alignment_scale_step(scale_value)
  samples <- model$samples %||% "Sample"
  tagList(
    if (length(sections)) {
      div(
        class = "spatial-alignment-layout",
        div(
          class = "spatial-alignment-sidebar builder-controls-grid",
          div(
            class = "spatial-alignment-sidebar-fixed",
            selectInput(
              ns("active_sample"),
              "Sample",
              choices = stats::setNames(samples, samples),
              selected = samples[[1L]],
              selectize = FALSE
            ),
            selectInput(
              ns("active_section"),
              "FOV / section",
              choices = choices,
              selected = selected_section,
              selectize = FALSE
            ),
            conditionalPanel(
              condition = "output['has_rois']",
              selectInput(
                ns("active_roi"),
                "ROI view",
                choices = c(
                  "All ROIs" = "",
                  "Separate ROIs" = "__separate__",
                  stats::setNames(initial_rois, initial_rois)
                ),
                selected = if (length(initial_rois) > 1L) {
                  "__separate__"
                } else {
                  ""
                },
                selectize = FALSE
              ),
              ns = ns
            ),
            builder_spatial_scene_inventory_ui(model$scenes)
          ),
          div(
            class = "spatial-alignment-sidebar-body",
            div(
              class = "spatial-alignment-sidebar-primary",
              conditionalPanel(
                condition = "output['has_coordinate_frame']",
                tags$details(
                  class = "spatial-coordinate-settings",
                  open = "open",
                  tags$summary("Coordinate settings"),
                  div(
                    class = "spatial-coordinate-settings-body",
                    builder_coordinate_control_ui(
                      ns,
                      "coordinate_rotation",
                      "Rotation",
                      -180,
                      180,
                      model$coordinate_rotation %||% 0,
                      0.1
                    ),
                    builder_coordinate_control_ui(
                      ns,
                      "point_opacity",
                      "Opacity",
                      0,
                      100,
                      (controls$point_opacity %||% 0.85) * 100,
                      5
                    ),
                    builder_coordinate_control_ui(
                      ns,
                      "point_size",
                      "Size",
                      1,
                      20,
                      controls$point_size %||% 5,
                      1
                    ),
                    builder_coordinate_control_ui(
                      ns,
                      "cell_percentage",
                      "Cells shown (%)",
                      10,
                      100,
                      controls$cell_percentage %||% 100,
                      1
                    ),
                    div(
                      class = "builder-action-row",
                      actionButton(
                        ns("reset_coordinate_transform"),
                        "Reset coordinates",
                        class = "btn"
                      )
                    )
                  )
                ),
                ns = ns
              ),
              div(
                class = "enhance-tissue-file-control builder-file-picker builder-file-picker--compact",
                tags$input(
                  id = ns("tissue_image_file"),
                  name = ns("tissue_image_file"),
                  class = "shiny-input-file enhance-tissue-file-input builder-file-input",
                  type = "file",
                  accept = builder_file_accept(builder_image_extensions()),
                  `tabindex` = "-1"
                ),
                tags$label(
                  `for` = ns("tissue_image_file"),
                  class = "btn btn-action enhance-tissue-file-button",
                  `tabindex` = "0",
                  role = "button",
                  tags$span(
                    `aria-hidden` = "true",
                    shiny::icon("image")
                  ),
                  uiOutput(ns("add_image_label"), inline = TRUE)
                )
              ),
              conditionalPanel(
                condition = "output['has_multiple_images']",
                selectInput(
                  ns("active_image"),
                  "Image",
                  choices = initial_image_choices,
                  selected = selected_image
                ),
                ns = ns
              ),
              div(
                class = "spatial-alignment-status",
                `aria-live` = "polite",
                uiOutput(ns("alignment_status"))
              ),
            ),
            div(
              class = "spatial-alignment-sidebar-scroll",
              conditionalPanel(
                condition = "output['has_image']",
                div(
                  class = "spatial-alignment-controls builder-controls-grid builder-controls-grid--sliders",
                  tags$details(
                    class = "spatial-coordinate-settings",
                    open = "open",
                    tags$summary("Image settings"),
                    div(
                      class = "spatial-coordinate-settings-body",
                      div(
                        class = "spatial-image-file-summary",
                        uiOutput(ns("image_file"))
                      ),
                      builder_coordinate_control_ui(
                        ns,
                        "image_opacity",
                        "Opacity",
                        0,
                        100,
                        (controls$image_opacity %||% 0.8) * 100,
                        5
                      ),
                      builder_coordinate_control_ui(
                        ns,
                        "img_rotate",
                        "Rotation",
                        -180,
                        180,
                        controls$rotation %||% 0,
                        NULL
                      ),
                      builder_coordinate_control_ui(
                        ns,
                        "img_scale",
                        "Scale",
                        0,
                        scale_max,
                        scale_value,
                        scale_step
                      ),
                      div(
                        class = "spatial-image-position",
                        numericInput(
                          ns("img_dx"),
                          "X pos.",
                          controls$dx %||% 0,
                          step = controls$dx_step %||% NA_real_
                        ),
                        numericInput(
                          ns("img_dy"),
                          "Y pos.",
                          controls$dy %||% 0,
                          step = controls$dy_step %||% NA_real_
                        )
                      ),
                      div(
                        class = "spatial-image-nudge",
                        tags$button(
                          type = "button",
                          class = "btn",
                          `data-target` = ns("img_dy"),
                          `data-delta` = 1,
                          `aria-label` = "Move image up",
                          "↑"
                        ),
                        tags$button(
                          type = "button",
                          class = "btn",
                          `data-target` = ns("img_dx"),
                          `data-delta` = -1,
                          `aria-label` = "Move image left",
                          "←"
                        ),
                        actionButton(
                          ns("center_image"),
                          "Center image",
                          class = "btn spatial-image-center",
                          `aria-label` = "Center image"
                        ),
                        tags$button(
                          type = "button",
                          class = "btn",
                          `data-target` = ns("img_dx"),
                          `data-delta` = 1,
                          `aria-label` = "Move image right",
                          "→"
                        ),
                        tags$button(
                          type = "button",
                          class = "btn",
                          `data-target` = ns("img_dy"),
                          `data-delta` = -1,
                          `aria-label` = "Move image down",
                          "↓"
                        )
                      ),
                      div(
                        class = "spatial-image-flips",
                        checkboxInput(
                          ns("image_flip_x"),
                          "Flip X",
                          isTRUE(controls$flip_x)
                        ),
                        checkboxInput(
                          ns("image_flip_y"),
                          "Flip Y",
                          isTRUE(controls$flip_y)
                        )
                      ),
                      div(
                        class = "builder-action-row",
                        actionButton(
                          ns("reset_align"),
                          "Reset image",
                          class = "btn"
                        )
                      )
                    )
                  )
                ),
                ns = ns
              )
            )
          )
        ),
        div(
          class = "spatial-alignment-main",
          div(
            class = "spatial-alignment-plots builder-preview-grid",
            tags$figure(
              class = "spatial-alignment-figure",
              builder_alignment_plot_output(
                ns("alignment_spatial_plot"),
                "Spatial-space cell plot"
              ),
              div(
                class = "spatial-alignment-legend-wrap",
                h5("Groups"),
                uiOutput(ns("alignment_legend"))
              )
            )
          )
        )
      )
    }
  )
}

builder_enhance_stage_ui <- function(
  id,
  model,
  dynamic_modules = FALSE,
  include_spatial = TRUE
) {
  ns <- NS(id)
  analysis_modules <- Filter(
    function(module) isTRUE(module$relevant),
    model$modules %||% list()
  )
  selected_analysis_count <- sum(vapply(
    analysis_modules,
    function(module) isTRUE(module$selected),
    logical(1)
  ))
  div(
    id = ns("stage"),
    class = "builder-enhancement-stack",
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    tags$section(
      class = "builder-stage-section builder-stage-enhance",
      tags$details(
        class = paste(
          "enhance-group enhance-group--analyses",
          "builder-viewer-card builder-viewer-optional-analyses"
        ),
        `data-disclosure-key` = "optional-analyses",
        tags$summary(
          span(class = "builder-viewer-card-title", "Optional analyses"),
          span(
            class = "builder-viewer-card-count",
            `data-analysis-count` = "true",
            paste(selected_analysis_count, "included")
          )
        ),
        div(
          class = "builder-viewer-card-body",
          div(
            class = "enhance-module-grid",
            if (isTRUE(dynamic_modules)) {
              uiOutput(ns("analysis_modules"))
            } else {
              builder_enhance_modules_ui(id, model$modules %||% list())
            }
          )
        )
      ),
      div(
        class = "enhance-group enhance-group--attachments",
        tags$details(
          class = paste(
            "enhance-attachment-block enhance-attachment-block--tables",
            "builder-viewer-card builder-viewer-extra-material"
          ),
          `data-disclosure-key` = "extra-material",
          tags$summary(
            span(class = "builder-viewer-card-title", "Extra material"),
            span(
              class = "builder-viewer-card-count",
              `data-extra-material-count` = "true",
              paste(
                length(model$attachments$tables$selected %||% character()),
                "included"
              )
            )
          ),
          div(
            class = "builder-viewer-card-body",
            p(
              class = "enhance-attachment-description",
              "Add CSV, TSV or Excel tables. Selected sheets are included at build time."
            ),
            div(
              class = "enhance-table-file-actions builder-action-row",
              tags$button(
                class = "btn enhance-table-add-button",
                type = "button",
                span(
                  `aria-hidden` = "true",
                  shiny::icon("folder-open")
                ),
                span(class = "builder-add-label", "Add tables")
              ),
              tags$input(
                id = ns("table_files"),
                name = ns("table_files"),
                class = "shiny-input-file enhance-table-file-input builder-file-input",
                type = "file",
                multiple = "multiple",
                accept = builder_file_accept(builder_table_extensions()),
                `tabindex` = "-1"
              )
            ),
            uiOutput(ns("table_list"))
          )
        )
      )
    ),
    if (isTRUE(include_spatial)) builder_spatial_stage_ui(id, model)
  )
}

builder_spatial_stage_ui <- function(id, model) {
  histology <- model$attachments$histology %||% list()
  if (!isTRUE(histology$relevant)) {
    return(NULL)
  }
  tags$div(
    class = "builder-stage-spatial spatial-alignment-workbench",
    tags$details(
      class = "builder-viewer-card builder-viewer-spatial-alignment",
      `data-disclosure-key` = "spatial-alignment",
      tags$summary(
        span(class = "builder-viewer-card-title", "Spatial alignment"),
        span(
          class = "builder-viewer-card-count",
          paste(
            length(histology$sections %||% character()),
            if (length(histology$sections %||% character()) == 1L) {
              "section"
            } else {
              "sections"
            }
          )
        )
      ),
      div(
        class = "builder-viewer-card-body builder-spatial-alignment-body",
        builder_spatial_alignment_ui(id, histology)
      )
    )
  )
}
