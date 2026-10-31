#' Entries "Apply to all unchecked datasets" changes, with the organism set.
#'
#' Only loaded datasets that are not checked yet are touched; checked ones and
#' saved CRB artifacts keep their organism. Like an edit in Essentials, an
#' organism without built-in gene lists drops the mito/ribo analysis.
builder_core_organism_targets <- function(entries, organism, checked) {
  updated <- lapply(entries, function(entry) {
    if (
      !is.list(entry) ||
        entry$id %in% checked ||
        !identical(entry$load_state %||% "loaded", "loaded")
    ) {
      return(NULL)
    }
    settings <- entry$settings %||% list()
    settings$organism <- organism
    if (
      !organism %in% c("hg", "mm") &&
        "percent_mt_ribo" %in% settings$analyses
    ) {
      settings$analyses <- setdiff(
        settings$analyses %||% character(),
        "percent_mt_ribo"
      )
    }
    if (identical(settings, entry$settings)) {
      return(NULL)
    }
    entry$settings <- settings
    entry
  })
  Filter(Negate(is.null), updated)
}

builder_core_stage_ui <- function(id, model) {
  ns <- NS(id)
  organism_choices <- model$organism_choices %||% character()
  organism <- model$organism %||% ""
  organism_choices <- c("Select organism" = "", organism_choices)
  if (
    builder_stage_has_text(organism) &&
      !organism %in% unname(organism_choices)
  ) {
    organism_choices <- c(organism_choices, organism)
  }
  group_catalog <- builder_group_catalog_model(model)
  cell_cycle_catalog <- builder_cell_cycle_catalog_model(model)
  analysis_results <- builder_analysis_results_model(model)
  specialized_content <- builder_specialized_content_model(model)
  div(
    id = ns("stage"),
    class = "builder-stage-section builder-stage-core",
    ## Sits directly under the dataset summary it describes.
    tags$details(
      class = "builder-configure-metadata",
      `data-disclosure-key` = "available-metadata",
      tags$summary(
        "Available metadata",
        span(
          class = "builder-detected-content-count",
          paste(length(group_catalog$items), "columns")
        )
      ),
      div(
        class = "builder-configure-metadata-chips",
        lapply(group_catalog$items, function(item) {
          span(item$label)
        })
      )
    ),
    h3("Essentials"),
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    div(
      class = "builder-form-grid",
      div(
        class = "builder-field builder-field--name",
        textInput(ns("name"), "Dataset name", value = model$name %||% "")
      ),
      div(
        class = "builder-field builder-field--organism",
        ## CSS shows the note only while the selection still equals the guess.
        `data-builder-detected-organism` = if (
          builder_stage_has_text(model$organism_detected)
        ) {
          model$organism_detected
        },
        `data-builder-creatable-select` = "true",
        `data-builder-create-input-label` = "Custom organism",
        `data-builder-create-placeholder` = "Type another organism",
        `data-builder-create-action-label` = "Add custom organism",
        `data-builder-create-maxlength` = "80",
        selectizeInput(
          ns("organism"),
          "Organism (required)",
          choices = organism_choices,
          selected = organism,
          options = list(
            create = FALSE,
            persist = TRUE,
            maxItems = 1L,
            placeholder = "Select organism"
          )
        ),
        ## One caption line in the space every field reserves, so the
        ## Assay/Layer row is not pushed down under this field alone.
        div(
          class = "builder-field-help",
          if (builder_stage_has_text(model$organism_detected)) {
            span(
              class = "builder-organism-detected",
              "Detected from gene names"
            )
          },
          tags$button(
            id = ns("organism_apply_all"),
            type = "button",
            class = "btn btn-link action-button builder-organism-apply-all",
            title = "Use this organism for every dataset not checked yet",
            "Apply to all unchecked"
          )
        )
      ),
      div(
        class = "builder-field builder-field--assay",
        selectInput(
          ns("assay"),
          "Assay",
          choices = model$assay_choices,
          selected = model$assay
        )
      ),
      div(
        class = "builder-field builder-field--layer",
        selectInput(
          ns("layer"),
          "Layer",
          choices = model$layer_choices,
          selected = model$layer
        )
      )
    ),
    tags$section(
      id = ns("content"),
      class = "builder-viewer-content",
      div(
        class = "builder-viewer-content-head",
        h4("Viewer content"),
        p(
          "Choose what appears in the Viewer. Metadata is retained automatically."
        )
      ),
      if (specialized_content$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-specialized-content",
          `data-disclosure-key` = "viewer-specialized-content",
          tags$summary(
            span(class = "builder-viewer-card-title", "Specialized content"),
            span(
              class = "builder-viewer-card-count",
              specialized_content$summary
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_specialized_content_ui(specialized_content, id)
          )
        )
      },
      tags$details(
        id = ns("groups"),
        class = "builder-viewer-card builder-viewer-groups",
        `data-disclosure-key` = "viewer-groups",
        tags$summary(
          span(class = "builder-viewer-card-title", "Groups"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-group-count` = "true",
            paste0(
              group_catalog$included_count,
              " included · ",
              "Default: ",
              group_catalog$default %||% "None"
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          builder_group_catalog_ui(id, group_catalog)
        )
      ),
      if (length(cell_cycle_catalog$items)) {
        tags$details(
          class = "builder-viewer-card builder-viewer-cell-cycle",
          `data-disclosure-key` = "viewer-cell-cycle",
          tags$summary(
            span(class = "builder-viewer-card-title", "Cell cycle"),
            span(
              class = "builder-viewer-card-count",
              paste(cell_cycle_catalog$included_count, "included")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_cell_cycle_catalog_ui(id, cell_cycle_catalog)
          )
        )
      },
      if (analysis_results$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-analysis-results",
          `data-disclosure-key` = "viewer-analysis-results",
          tags$summary(
            span(class = "builder-viewer-card-title", "Analysis results"),
            span(
              class = "builder-viewer-card-count",
              paste(analysis_results$total_count, "detected or planned")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_analysis_results_ui(analysis_results, id)
          )
        )
      }
    ),
    if (builder_stage_has_text(model$metadata_attention %||% "")) {
      div(
        class = "notice warn",
        p(model$metadata_attention),
        builder_acknowledgement_button(id, model$metadata_acknowledgement)
      )
    },
    if (builder_stage_has_text(model$layer_attention %||% "")) {
      div(class = "notice bad", model$layer_attention)
    }
  )
}

## Settings come after every content group (Optional analyses, Extra
## material), so Configure renders this below the enhancement stack.
builder_core_advanced_ui <- function(id, model) {
  ns <- NS(id)
  tags$section(
    class = paste(
      "builder-stage-section builder-stage-advanced",
      "builder-viewer-content"
    ),
    tags$details(
      class = "builder-viewer-card builder-viewer-advanced-settings",
      `data-disclosure-key` = "viewer-advanced-settings",
      tags$summary(
        span(class = "builder-viewer-card-title", "Advanced settings"),
        span(class = "builder-viewer-card-count", "3 settings")
      ),
      div(
        class = "builder-viewer-card-body",
        div(
          class = "builder-advanced-grid",
          selectInput(
            ns("nUMI"),
            "UMI QC column",
            choices = model$nUMI_choices,
            selected = model$nUMI
          ),
          selectInput(
            ns("nGene"),
            "Gene QC column",
            choices = model$nGene_choices,
            selected = model$nGene
          ),
          selectInput(
            ns("backend"),
            "Expression backend",
            choices = model$backend_choices,
            selected = model$backend
          )
        )
      )
    )
  )
}

builder_views_stage_ui <- function(id, model, spatial = NULL) {
  ns <- NS(id)
  projection_catalog <- builder_projection_catalog_model(model)
  trajectory_catalog <- builder_trajectory_catalog_model(model)
  projection_default <- Filter(
    function(item) identical(item$id, projection_catalog$default),
    projection_catalog$items
  )
  projection_default_label <- if (length(projection_default)) {
    projection_default[[1L]]$label
  } else {
    "None"
  }
  if (
    !length(trajectory_catalog$items) &&
      !length(projection_catalog$items) &&
      is.null(spatial)
  ) {
    return(NULL)
  }

  tags$section(
    class = "builder-stage-section builder-stage-views builder-viewer-content",
    div(
      class = "builder-viewer-content-head",
      h4("Views"),
      p("Choose the trajectories, projections, and spatial views to show.")
    ),
    if (length(trajectory_catalog$items)) {
      tags$details(
        class = "builder-viewer-card builder-viewer-trajectories",
        `data-disclosure-key` = "viewer-trajectories",
        tags$summary(
          span(class = "builder-viewer-card-title", "Trajectories"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-trajectory-count` = "true",
            paste0(
              trajectory_catalog$included_count,
              " included",
              if (is.list(trajectory_catalog$default)) {
                paste0(" · Default: ", trajectory_catalog$default$name)
              } else {
                ""
              }
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          uiOutput(ns("trajectory_gallery"))
        )
      )
    },
    if (length(projection_catalog$items)) {
      tags$details(
        class = "builder-viewer-card builder-viewer-projections",
        `data-disclosure-key` = "viewer-projections",
        tags$summary(
          span(class = "builder-viewer-card-title", "Projections"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-projection-count` = "true",
            paste0(
              projection_catalog$included_count,
              " included · Default: ",
              projection_default_label
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          uiOutput(ns("projection_gallery"))
        )
      )
    },
    spatial,
    tags$details(
      class = "builder-viewer-card",
      `data-disclosure-key` = "viewer-display",
      tags$summary(span(
        class = "builder-viewer-card-title",
        "Viewer appearance"
      )),
      div(class = "builder-viewer-card-body", uiOutput(ns("viewer_display")))
    )
  )
}

builder_viewer_display_ui <- function(id, entry, epoch) {
  ns <- NS(id)
  value <- builderViewerDisplayDefaults(entry$settings$viewer_display)
  select <- function(key, label, choices) {
    tags$div(
      class = "form-group",
      tags$label(`for` = ns(key), label),
      tags$select(
        id = ns(key),
        class = "form-control",
        `data-display-field` = key,
        lapply(names(choices), function(label) {
          tags$option(
            value = choices[[label]],
            selected = if (identical(value[[key]], choices[[label]])) {
              "selected"
            } else {
              NULL
            },
            label
          )
        })
      )
    )
  }
  flag <- function(key, label) {
    tags$label(
      class = "checkbox-inline",
      tags$input(
        type = "checkbox",
        `data-display-field` = key,
        checked = if (isTRUE(value[[key]])) "checked" else NULL
      ),
      label
    )
  }
  number <- function(key, label, min, max, step) {
    div(
      class = "form-group",
      tags$label(`for` = ns(key), label),
      tags$input(
        id = ns(key),
        type = "number",
        class = "form-control",
        min = min,
        max = max,
        step = step,
        value = value[[key]],
        `data-display-field` = key
      )
    )
  }
  ## Checkboxes sit together on their own row; inside the two-column grid
  ## they would each take a cell next to the number fields.
  flags <- function(...) {
    div(class = "builder-display-flags", ...)
  }
  page <- function(prefix, label, labels = TRUE, extras = NULL) {
    tags$details(
      class = "builder-viewer-card",
      tags$summary(span(class = "builder-viewer-card-title", label)),
      div(
        class = "builder-viewer-card-body builder-advanced-grid",
        flags(
          if (labels) {
            flag(paste0(prefix, "_group_labels"), "Show group labels")
          },
          flag(paste0(prefix, "_draw_border"), "Draw cell borders"),
          flag(paste0(prefix, "_keep_square"), "Keep plots square")
        ),
        extras
      )
    )
  }
  div(
    class = "builder-viewer-display",
    `data-dataset-id` = entry$id,
    `data-project-epoch` = epoch,
    `data-input-id` = ns("viewer_display_action"),
    `data-display-values` = as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      digits = NA
    )),
    p(
      "Author defaults for Linked views. Readers can change these while browsing."
    ),
    div(
      class = "builder-advanced-grid",
      flags(
        flag("group_labels", "Show group labels"),
        flag("draw_border", "Draw cell borders"),
        flag("keep_square", "Keep plots square")
      ),
      div(
        class = "form-group",
        tags$label(`for` = ns("label_size"), "Group label size (CSS pixels)"),
        tags$input(
          id = ns("label_size"),
          type = "number",
          class = "form-control",
          min = 8,
          max = 24,
          step = 1,
          value = value$label_size,
          `data-display-field` = "label_size"
        )
      ),
      select(
        "font_family",
        "Group label font",
        c("System" = "system-ui", "Arial" = "Arial", "Georgia" = "Georgia")
      ),
      select(
        "color_clip",
        "Continuous colour range",
        c("Full range" = 0, "1–99%" = .01, "2–98%" = .02, "5–95%" = .05)
      )
    ),
    h5("Gene expression and Spatial feature plots"),
    div(
      class = "builder-advanced-grid",
      select(
        "gene_palette",
        "Expression colour scale",
        c(
          "Cerebro orange" = "cerebro_orange",
          "Blues" = "blues",
          "Greens" = "greens",
          "Viridis" = "viridis"
        )
      ),
      select(
        "gene_panel_mode",
        "Gene panel colours",
        c("Shared scale" = "shared", "Distinct colours" = "distinct")
      ),
      select(
        "gene_panel_palette",
        "Distinct panel palette",
        c("Default" = "default", "Soft" = "soft")
      )
    ),
    h5("Page-specific appearance"),
    p(
      class = "help-block",
      "These settings preserve each page's defaults. Reader selections and analysis filters remain interactive."
    ),
    page("projection", "Projection"),
    page(
      "gene",
      "Gene expression",
      labels = FALSE,
      extras = tagList(
        select(
          "gene_plotting_order",
          "Plotting order",
          c(
            "Highest expression on top" = "Highest expression on top",
            "Input order" = "Input order"
          )
        ),
        flag("gene_show_trend_line", "Show pseudotime trend line"),
        number(
          "gene_trend_line_width",
          "Pseudotime line width (plot units)",
          1,
          10,
          1
        )
      )
    ),
    page(
      "spatial",
      "Spatial",
      extras = flag(
        "spatial_region_outlines",
        "Outline group regions (convex hull)"
      )
    ),
    page(
      "trajectory",
      "Trajectory",
      extras = tagList(
        number(
          "trajectory_density_opacity",
          "Pseudotime density opacity",
          0,
          1,
          .1
        ),
        flag("trajectory_show_trend_line", "Show pseudotime trend line"),
        number(
          "trajectory_trend_line_width",
          "Pseudotime line width (plot units)",
          1,
          10,
          1
        )
      )
    ),
    page(
      "ir",
      "Immune repertoire",
      extras = tagList(
        p(
          class = "help-block",
          "Clonal UMAP uses projection point defaults. Scatter and Paired Scatter use the controls below (plot units, not CSS pixels)."
        ),
        number("ir_scatter_point_size", "Scatter point size", .1, 6, .1),
        number("ir_scatter_opacity", "Scatter point opacity", .1, 1, .05)
      )
    ),
    tags$details(
      class = "builder-viewer-card",
      tags$summary(span(
        class = "builder-viewer-card-title",
        "HLA & TCR Motifs"
      )),
      div(
        class = "builder-viewer-card-body builder-advanced-grid",
        select(
          "hla_legend_mode",
          "Legend",
          c("Auto" = "auto", "Always show" = "always", "Hide" = "never")
        ),
        number("hla_node_scale", "Node size multiplier", .3, 2.5, .1)
      )
    ),
    p(
      class = "help-block",
      "Projection thumbnails show sampled geometry and point appearance. Label typography is shown in the generated Viewer."
    ),
    div(
      class = "visually-hidden builder-display-status",
      role = "status",
      `aria-live` = "polite"
    )
  )
}
