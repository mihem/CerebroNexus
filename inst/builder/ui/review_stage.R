## Frozen-plan Review stage. This file never reads mutable draft inputs.

builder_review_options <- function(
  welcome_message = "Welcome to CerebroNexus!",
  initial_page = "data_info",
  point_size = 5,
  variable_to_compare = FALSE,
  host = "127.0.0.1",
  port = 8080L,
  max_request_size = 8000,
  display_mode = "normal",
  launch_browser = FALSE,
  show_upload_ui = FALSE
) {
  valid_number <- function(value, lower, upper = Inf, whole = FALSE) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= lower &&
      value <= upper &&
      (!whole || value == floor(value))
  }
  valid_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (
    !builder_stage_has_text(welcome_message) ||
      !builder_stage_has_text(initial_page) ||
      !initial_page %in% builder_viewer_known_page_ids() ||
      !valid_number(point_size, 1, 20) ||
      !valid_flag(variable_to_compare) ||
      !builder_stage_has_text(host) ||
      !valid_number(port, 1, 65535, whole = TRUE) ||
      !valid_number(max_request_size, .Machine$double.eps) ||
      !is.character(display_mode) ||
      length(display_mode) != 1L ||
      is.na(display_mode) ||
      !display_mode %in% c("auto", "normal", "showcase") ||
      !valid_flag(launch_browser) ||
      !valid_flag(show_upload_ui)
  ) {
    stop("Review options are invalid.", call. = FALSE)
  }
  structure(
    list(
      welcome_message = welcome_message,
      initial_page = initial_page,
      point_size = as.double(point_size),
      variable_to_compare = variable_to_compare,
      host = host,
      port = as.integer(port),
      max_request_size = as.double(max_request_size),
      display_mode = display_mode,
      launch_browser = launch_browser,
      show_upload_ui = show_upload_ui
    ),
    class = c("builder_review_options", "list")
  )
}

builder_review_options_for_plan <- function(options, initial_dataset = NULL) {
  if (!inherits(options, "builder_review_options")) {
    stop("Typed Review options are required.", call. = FALSE)
  }
  initial <- if (is.null(initial_dataset)) {
    list()
  } else {
    list(initial_dataset = initial_dataset)
  }
  c(
    list(show_upload_ui = options$show_upload_ui),
    initial,
    list(
      initial_page = options$initial_page,
      welcome_message = options$welcome_message,
      variable_to_compare = options$variable_to_compare,
      host = options$host,
      port = options$port,
      max_request_size = options$max_request_size,
      display_mode = options$display_mode,
      launch_browser = options$launch_browser
    )
  )
}

builder_review_initial_page_choices <- function(page_expectations = list()) {
  catalog <- builder_viewer_page_catalog()
  pages <- rbind(catalog$always, catalog$conditional)
  # Saved checkpoints describe data-dependent pages, not the current app's
  # unconditional routes. Older projects may predate a newly exposed page.
  always_ids <- catalog$always$id
  allowed <- unique(c(
    always_ids,
    page_expectations$visible_conditional %||% character()
  ))
  pages <- pages[pages$id %in% allowed, , drop = FALSE]
  stats::setNames(pages$id, pages$label)
}

builder_auth_dialog_ui <- function() {
  div(
    id = "builder-auth-backdrop",
    class = "builder-auth-backdrop",
    hidden = "hidden",
    div(
      id = "builder-auth-dialog",
      class = "builder-auth-dialog",
      role = "dialog",
      `aria-modal` = "true",
      `aria-labelledby` = "builder-auth-title",
      `aria-describedby` = paste(
        "builder-auth-description",
        "builder-auth-password-note"
      ),
      tabindex = "-1",
      div(
        class = "builder-auth-header",
        div(
          tags$span(class = "builder-project-modal-kicker", "Build"),
          h2(id = "builder-auth-title", "Login accounts"),
          p(
            id = "builder-auth-description",
            "Only these accounts can open the Viewer App."
          )
        ),
        tags$button(
          type = "button",
          class = "builder-auth-close builder-auth-cancel",
          `aria-label` = "Close login accounts",
          "×"
        )
      ),
      div(
        class = "builder-auth-body",
        div(
          id = "builder-auth-error",
          class = "builder-auth-error",
          role = "alert",
          tabindex = "-1",
          hidden = "hidden"
        ),
        div(class = "builder-auth-rows", `data-auth-rows` = "true"),
        p(
          id = "builder-auth-password-note",
          class = "builder-auth-password-note",
          "Passwords must contain at least 8 characters."
        )
      ),
      div(
        class = "builder-auth-footer",
        tags$button(
          type = "button",
          class = "btn builder-auth-add",
          "+ Add account"
        ),
        tags$button(
          type = "button",
          class = "btn builder-auth-cancel",
          "Cancel"
        ),
        tags$button(
          type = "button",
          class = "btn btn-primary builder-auth-save",
          "Save accounts"
        )
      )
    )
  )
}

builder_review_can_build <- function(plan) {
  builder_review_snapshot_valid(plan)
}

builder_review_confirmation_ui <- function() {
  builder_stage_footer_ui(
    "Configuration checked",
    actionButton(
      "back_to_settings",
      "Back to Configure",
      class = "btn"
    ),
    actionButton(
      "confirm_review",
      "Continue to Build",
      class = "btn btn-action"
    )
  )
}

builder_review_human_size <- function(bytes) {
  builder_file_human_size(bytes)
}

builder_review_page_tone <- function(id) {
  switch(
    as.character(id %||% ""),
    marker_genes = ,
    most_expressed_genes = ,
    enriched_pathways = "is-analysis",
    spatial = ,
    trekker = "is-spatial",
    trajectory = "is-trajectory",
    immune_repertoire = ,
    hla_tcr_motifs = "is-immune",
    extra_material = "is-extra",
    "is-core"
  )
}

builder_review_pages <- function(items, plan_contract = list()) {
  catalog <- builder_viewer_page_catalog()
  visible_ids <- unique(unlist(
    lapply(items, function(item) {
      item$viewer_page_expectations$visible_conditional %||% character()
    }),
    use.names = FALSE
  ))
  if (!length(visible_ids)) {
    visible_ids <- unique(unlist(
      lapply(plan_contract, function(contract) {
        contract$visible_conditional %||% character()
      }),
      use.names = FALSE
    ))
  }
  pages <- rbind(catalog$always, catalog$conditional)
  page_ids <- unique(c(
    catalog$always$id,
    intersect(catalog$conditional$id, visible_ids)
  ))
  pages <- pages[match(page_ids, pages$id), , drop = FALSE]
  unname(lapply(seq_len(nrow(pages)), function(index) {
    list(
      id = pages$id[[index]],
      label = pages$label[[index]],
      tone = builder_review_page_tone(pages$id[[index]])
    )
  }))
}

## The column name as Configure and the generated Viewer show it; a prettified
## "Neuron cluster ID" for `Neuron_cluster_ID` reads like a different column.
builder_review_group_label <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!length(value) || !nzchar(value[[1L]])) {
    return("Not selected")
  }
  value[[1L]]
}

builder_review_projection_label <- function(value) {
  value <- as.character(value %||% "")
  known <- c(
    umap = "UMAP",
    tsne = "t-SNE",
    `t-sne` = "t-SNE",
    pca = "PCA"
  )
  key <- tolower(value)
  labels <- unname(known[key])
  labels[is.na(labels)] <- value[is.na(labels)]
  labels
}

builder_review_trajectory_model <- function(included, default = NULL) {
  if (!is.list(included) || !length(included)) {
    return(NULL)
  }
  trajectory_names <- unname(unlist(included, use.names = FALSE))
  trajectory_names <- trajectory_names[
    !is.na(trajectory_names) &
      nzchar(trajectory_names)
  ]
  if (!length(trajectory_names)) {
    return(NULL)
  }
  default_name <- if (
    is.list(default) && builder_stage_has_text(default$name %||% "")
  ) {
    default$name
  } else if (builder_stage_has_text(default %||% "")) {
    default
  } else {
    trajectory_names[[1L]]
  }
  list(
    included_count = as.integer(length(trajectory_names)),
    included = trajectory_names,
    default = as.character(default_name)
  )
}

builder_review_metadata_model <- function(
  policy,
  manifest = list(),
  acknowledgements = character()
) {
  if (!is.list(policy)) {
    return(list(
      total_count = 0L,
      kept_count = 0L,
      excluded_count = 0L,
      attention_count = 0L
    ))
  }
  columns <- policy$columns
  if (is.list(columns) && length(columns)) {
    ids <- setdiff(names(columns) %||% character(), "cell_barcode")
    records <- columns[ids]
    effective_included <- vapply(
      records,
      function(record) {
        value <- if (is.list(record)) {
          record$retain_in_crb %||% record$effective_included
        } else {
          NULL
        }
        if (is.logical(value) && length(value) == 1L && !is.na(value)) {
          value
        } else {
          NA
        }
      },
      logical(1),
      USE.NAMES = FALSE
    )
    retained <- !is.na(effective_included) & effective_included
    dispositions <- vapply(
      records,
      function(record) {
        if (is.list(record)) record$disposition %||% "unknown" else "unknown"
      },
      character(1)
    )
    attention_count <- as.integer(sum(
      dispositions %in% c("attention", "blocking")
    ))
    metadata_entry <- manifest$metadata_policy %||% list()
    action <- metadata_entry$required_action %||% list()
    acknowledged <- identical(metadata_entry$status %||% "", "attention") &&
      identical(action$type %||% "", "acknowledge") &&
      builder_stage_has_text(action$token %||% "") &&
      (action$token %||% "") %in% acknowledgements
    if (identical(metadata_entry$status %||% "", "valid") || acknowledged) {
      attention_count <- 0L
    }
    return(list(
      total_count = as.integer(length(records)),
      kept_count = as.integer(sum(retained)),
      excluded_count = as.integer(sum(
        dispositions == "excluded" |
          (!is.na(effective_included) & !effective_included)
      )),
      attention_count = attention_count
    ))
  }
  included <- setdiff(
    policy$retained %||% policy$included %||% character(),
    "cell_barcode"
  )
  excluded <- setdiff(policy$excluded %||% character(), "cell_barcode")
  attention <- setdiff(policy$attention %||% character(), "cell_barcode")
  list(
    total_count = as.integer(length(unique(c(included, excluded, attention)))),
    kept_count = as.integer(length(unique(included))),
    excluded_count = as.integer(length(unique(excluded))),
    attention_count = as.integer(length(unique(attention)))
  )
}

builder_review_model <- function(plan, verification = NULL) {
  if (!builder_review_can_build(plan)) {
    stop("Review requires a checked configuration snapshot.", call. = FALSE)
  }
  revision <- plan$revision
  valid_revision <- (is.integer(revision) &&
    length(revision) == 1L &&
    !is.na(revision)) ||
    (is.character(revision) &&
      length(revision) == 1L &&
      !is.na(revision) &&
      nzchar(revision))
  if (!valid_revision) {
    stop("Review requires a checked configuration revision.", call. = FALSE)
  }
  items <- plan$items %||% list()
  names_by_id <- stats::setNames(
    vapply(items, function(item) item$name %||% "Dataset", character(1)),
    vapply(items, function(item) item$id %||% "", character(1))
  )
  review_dataset <- function(item) {
    group_values <- item$included_groups %||% item$groups %||% character()
    group_levels <- item$artifact_identity$group_levels %||% list()
    default_group <- item$default_group %||% ""
    selected_group_levels <- if (
      is.list(group_levels) &&
        nzchar(default_group) &&
        default_group %in% names(group_levels)
    ) {
      group_levels[[default_group]] %||% character()
    } else {
      character()
    }
    group_colors <- item$colors[[default_group]] %||% character()
    color_preview_limit <- 5L
    color_preview <- head(unname(group_colors), color_preview_limit)
    color_custom_count <- as.integer(item$color_custom_count %||% 0L)
    projection_values <- item$included_projections %||%
      item$reductions %||%
      character()
    initial_projection_values <- item$initial_projections %||%
      item$default_projection %||%
      character()
    color_overrides <- item$group_color_overrides %||% list()
    custom_color_count <- if (
      is.list(color_overrides) && length(color_overrides)
    ) {
      as.integer(sum(vapply(color_overrides, length, integer(1))))
    } else {
      color_custom_count
    }
    point_size <- item$overview_point_size %||% 5
    point_opacity <- item$overview_point_opacity %||% 1
    trajectory_model <- builder_review_trajectory_model(
      item$included_trajectories %||% list(),
      item$default_trajectory %||% NULL
    )
    analysis_results <- builder_analysis_results_model(list(
      analysis_manifest = item$manifest %||% list(),
      analysis_acknowledgements = item$acknowledgements %||% character()
    ))
    specialized_content <- builder_specialized_content_model(list(
      content_manifest = item$manifest %||% list(),
      content_acknowledgements = item$acknowledgements %||% character()
    ))
    spatial_alignment <- item$spatial_alignment %||% NULL
    if (!is.null(spatial_alignment)) {
      image_count <- as.integer(spatial_alignment$image_count %||% 0L)
      spatial_alignment$storage <- if (image_count > 0L) {
        "External spatial-assets"
      } else {
        NULL
      }
    }
    list(
      name = item$name %||% "Dataset",
      cells = as.integer(item$cell_count %||% 0L),
      genes = as.integer(item$gene_count %||% 0L),
      group_count = as.integer(
        if (length(selected_group_levels)) {
          length(selected_group_levels)
        } else {
          length(group_values)
        }
      ),
      projection_count = as.integer(length(projection_values)),
      default_group = item$default_group %||% "Not selected",
      viewer_content = list(
        metadata = builder_review_metadata_model(
          item$metadata_policy,
          item$manifest %||% list(),
          item$acknowledgements %||% character()
        ),
        groups = list(
          included_count = as.integer(length(group_values)),
          included = unname(group_values),
          default = builder_review_group_label(item$default_group %||% ""),
          custom_color_count = custom_color_count
        ),
        cell_cycle = if (length(item$cell_cycle %||% character())) {
          list(included = unname(item$cell_cycle))
        } else {
          NULL
        },
        projections = list(
          included_count = as.integer(length(projection_values)),
          included = builder_review_projection_label(projection_values),
          initial = builder_review_projection_label(
            initial_projection_values
          ),
          default = if (
            builder_stage_has_text(item$default_projection %||% "")
          ) {
            builder_review_projection_label(item$default_projection)
          } else {
            "Not selected"
          },
          point_size = as.numeric(point_size),
          point_opacity = as.numeric(point_opacity)
        ),
        trajectories = trajectory_model,
        analysis_results = analysis_results,
        specialized = specialized_content
      ),
      group_colors = list(
        group = default_group,
        count = as.integer(length(group_colors)),
        custom_count = custom_color_count,
        preview = color_preview,
        remaining = as.integer(max(
          0L,
          length(group_colors) - color_preview_limit
        ))
      ),
      default_projection = if (
        builder_stage_has_text(item$default_projection %||% "")
      ) {
        toupper(item$default_projection)
      } else {
        "Not selected"
      },
      organism = switch(
        item$organism %||% "",
        hg = "Human",
        mm = "Mouse",
        item$organism %||% "Not specified"
      ),
      expression_storage = switch(
        item$expression_backend %||% "embedded",
        embedded = "Embedded",
        h5 = "HDF5",
        item$expression_backend %||% "Embedded"
      ),
      spatial_alignment = spatial_alignment,
      output_file = basename(item$filename %||% "dataset.crb"),
      pages = builder_review_pages(list(item))
    )
  }
  runtime_values <- tolower(as.character(c(
    plan$output_release$estimated_runtime %||% character(),
    unlist(
      lapply(items, function(item) {
        item$estimated_runtime %||% character()
      }),
      use.names = FALSE
    )
  )))
  runtime <- if (any(grepl("network", runtime_values, fixed = TRUE))) {
    "Depends on network response"
  } else if (any(grepl("minute", runtime_values, fixed = TRUE))) {
    "A few minutes"
  } else {
    "Less than a minute"
  }
  warning_values <- plan$warnings
  warning_values <- as.character(unlist(warning_values, use.names = FALSE))
  warning_values <- warning_values[nzchar(trimws(warning_values))]
  if (length(warning_values)) {
    for (dataset_id in names(names_by_id)) {
      warning_values <- gsub(
        dataset_id,
        names_by_id[[dataset_id]],
        warning_values,
        fixed = TRUE
      )
    }
  }
  model <- list(
    revision = revision,
    dataset_count = as.integer(length(items)),
    output_label = if (isTRUE(plan$make_app)) {
      "CRB files + Viewer app"
    } else {
      "CRB files"
    },
    datasets = lapply(items, review_dataset),
    pages = builder_review_pages(items, plan$viewer_page_expectations),
    output = list(
      crb_count = as.integer(length(items)),
      make_app = isTRUE(plan$make_app),
      estimated_size = builder_review_human_size(
        plan$output_release$estimated_disk_bytes %||% 0
      ),
      estimated_time = runtime
    ),
    warnings = warning_values,
    can_build = builder_review_can_build(plan)
  )
  model
}

## The Review surface consumes only the user-facing configuration snapshot.
builder_review_stage_ui <- function(id, model, footer = NULL) {
  ns <- NS(id)
  plural <- function(value, singular, plural = paste0(singular, "s")) {
    paste(value, if (identical(as.integer(value), 1L)) singular else plural)
  }
  field <- function(label, value, class = NULL) {
    div(
      class = paste("review-field", class),
      tags$dt(label),
      tags$dd(value)
    )
  }
  page_tags <- function(pages) {
    div(
      class = "review-page-tags",
      lapply(seq_along(pages), function(index) {
        page <- pages[[index]]
        span(
          class = paste("review-page-tag", page$tone),
          page$label
        )
      })
    )
  }

  div(
    id = ns("stage"),
    class = "builder-stage builder-stage-shell builder-stage-review",
    builder_stage_header_ui(
      "Review",
      "Review before building",
      "Resolve blocking issues, then confirm the output summary."
    ),
    builder_stage_summary_ui(
      class = "review-summary-strip",
      span(plural(model$dataset_count, "dataset")),
      span(paste("Creates", model$output_label))
    ),
    if (!length(model$warnings %||% character())) {
      div(
        class = "review-ready-banner",
        span(class = "review-ready-banner-icon", "✓"),
        strong("Everything is ready to build")
      )
    },
    if (length(model$warnings %||% character())) {
      tags$section(
        class = paste(
          "builder-stage-section review-section",
          "review-needs-attention"
        ),
        h3("Needs attention"),
        tags$ul(lapply(model$warnings, tags$li))
      )
    },
    tags$section(
      class = "builder-stage-section review-section review-datasets",
      h3("Datasets"),
      div(
        class = paste(
          "review-dataset-grid",
          if (identical(length(model$datasets), 1L)) "is-single-dataset"
        ),
        lapply(model$datasets, function(dataset) {
          viewer_content <- dataset$viewer_content
          pages <- dataset$pages %||% list()
          shown_pages <- utils::head(pages, 8L)
          more_pages <- utils::tail(pages, max(0L, length(pages) - 8L))
          tags$details(
            class = "builder-object review-dataset-card",
            tags$summary(
              tags$strong(dataset$name),
              span(
                class = "review-dataset-counts",
                paste0(
                  format(dataset$cells, big.mark = ","),
                  " cells · ",
                  format(dataset$genes, big.mark = ","),
                  " genes"
                )
              )
            ),
            div(
              class = "review-dataset-body",
              div(
                class = "review-viewer-content",
                if (viewer_content$metadata$total_count > 0L) {
                  div(
                    class = "review-viewer-content-item review-viewer-metadata",
                    h4("Metadata"),
                    p(paste0(
                      viewer_content$metadata$kept_count,
                      " retained · ",
                      viewer_content$metadata$excluded_count,
                      " excluded"
                    )),
                    if (viewer_content$metadata$attention_count > 0L) {
                      p(
                        class = "hint",
                        paste(
                          viewer_content$metadata$attention_count,
                          "needs attention"
                        )
                      )
                    }
                  )
                },
                div(
                  class = "review-viewer-content-item review-expression-storage",
                  h4("Expression storage"),
                  p(dataset$expression_storage)
                ),
                div(
                  class = "review-viewer-content-item review-viewer-groups",
                  h4("Groups"),
                  p(paste0(
                    viewer_content$groups$included_count,
                    " included · Default: ",
                    viewer_content$groups$default
                  )),
                  p(
                    class = "hint",
                    paste(
                      viewer_content$groups$custom_color_count,
                      "colors customized"
                    )
                  )
                ),
                if (!is.null(viewer_content$cell_cycle)) {
                  div(
                    class = "review-viewer-content-item review-viewer-cell-cycle",
                    h4("Cell cycle"),
                    p(paste(
                      viewer_content$cell_cycle$included,
                      collapse = ", "
                    ))
                  )
                },
                div(
                  class = "review-viewer-content-item review-viewer-projections",
                  h4("Projections"),
                  p(paste(
                    viewer_content$projections$included,
                    collapse = ", "
                  )),
                  p(paste0(
                    "Shown initially: ",
                    paste(
                      viewer_content$projections$initial,
                      collapse = ", "
                    )
                  )),
                  p(paste0(
                    "Default: ",
                    viewer_content$projections$default
                  )),
                  p(
                    class = "hint",
                    paste0(
                      "Point size ",
                      viewer_content$projections$point_size,
                      " · Opacity ",
                      viewer_content$projections$point_opacity
                    )
                  )
                ),
                if (!is.null(viewer_content$trajectories)) {
                  div(
                    class = "review-viewer-content-item review-viewer-trajectories",
                    h4("Trajectories"),
                    p(paste0(
                      viewer_content$trajectories$included_count,
                      " included · Default: ",
                      viewer_content$trajectories$default
                    ))
                  )
                },
                if (viewer_content$analysis_results$total_count > 0L) {
                  div(
                    class = paste(
                      "review-viewer-content-item",
                      "review-viewer-analysis-results"
                    ),
                    h4("Analysis results"),
                    p(paste(
                      c(
                        if (
                          viewer_content$analysis_results$existing_count > 0L
                        ) {
                          paste(
                            viewer_content$analysis_results$existing_count,
                            "existing"
                          )
                        },
                        if (
                          viewer_content$analysis_results$generated_count > 0L
                        ) {
                          paste(
                            viewer_content$analysis_results$generated_count,
                            "will be generated"
                          )
                        },
                        if (
                          viewer_content$analysis_results$attention_count > 0L
                        ) {
                          paste(
                            viewer_content$analysis_results$attention_count,
                            "needs attention"
                          )
                        },
                        if (
                          viewer_content$analysis_results$excluded_count > 0L
                        ) {
                          paste(
                            viewer_content$analysis_results$excluded_count,
                            "not included"
                          )
                        }
                      ),
                      collapse = " · "
                    )),
                    p(
                      class = "hint",
                      paste(
                        vapply(
                          viewer_content$analysis_results$items,
                          `[[`,
                          character(1),
                          "label"
                        ),
                        collapse = ", "
                      )
                    )
                  )
                },
                if (viewer_content$specialized$total_count > 0L) {
                  div(
                    class = paste(
                      "review-viewer-content-item",
                      "review-viewer-specialized-content"
                    ),
                    h4("Specialized content"),
                    p(viewer_content$specialized$summary),
                    p(
                      class = "hint",
                      paste(
                        vapply(
                          viewer_content$specialized$items,
                          `[[`,
                          character(1),
                          "label"
                        ),
                        collapse = ", "
                      )
                    )
                  )
                }
              ),
              if (
                !is.null(dataset$spatial_alignment) &&
                  dataset$spatial_alignment$section_count > 0L
              ) {
                div(
                  class = "review-spatial-alignment",
                  tags$b("Spatial"),
                  p(paste0(
                    dataset$spatial_alignment$section_count,
                    " sections · ",
                    dataset$spatial_alignment$image_count %||% 0L,
                    " images",
                    if (!is.null(dataset$spatial_alignment$storage)) {
                      paste0(" · ", dataset$spatial_alignment$storage)
                    } else {
                      ""
                    }
                  )),
                  if (length(dataset$spatial_alignment$points_only)) {
                    p(
                      class = "hint",
                      paste0(
                        length(dataset$spatial_alignment$points_only),
                        if (
                          length(dataset$spatial_alignment$points_only) == 1L
                        ) {
                          " section remains points-only."
                        } else {
                          " sections remain points-only."
                        }
                      )
                    )
                  }
                )
              },
              div(
                class = "review-dataset-pages",
                h4("Viewer pages"),
                page_tags(shown_pages),
                if (length(more_pages)) {
                  tags$details(
                    class = "review-pages-more",
                    tags$summary(paste("Show", length(more_pages), "more")),
                    page_tags(more_pages)
                  )
                }
              ),
              p(
                class = "review-dataset-file",
                span("Output file: "),
                tags$code(dataset$output_file)
              )
            )
          )
        })
      )
    ),
    tags$section(
      class = "builder-stage-section review-section review-output",
      h3("Output"),
      p(
        class = "review-output-download-note",
        paste(
          "Creates a new release folder. Existing releases are kept."
        )
      ),
      tags$dl(
        class = "review-fields review-output-fields",
        field(
          "Creates",
          paste0(
            plural(model$output$crb_count, "CRB file"),
            if (isTRUE(model$output$make_app)) " + Viewer app" else ""
          )
        )
      )
    ),
    footer
  )
}
