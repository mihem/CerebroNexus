## Marker gene source dialogs.

builder_marker_source_choice_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "marker-source-choice",
    p(
      class = "marker-source-choice-intro",
      "Choose how this dataset should get Marker genes."
    ),
    div(
      class = "marker-source-choice-grid",
      actionButton(
        ns("marker_genes_calculate"),
        tagList(
          span(
            class = "marker-source-choice-title",
            "Calculate for all Groups"
          ),
          span(
            class = "marker-source-choice-description",
            "Run the built-in differential-expression analysis for every included grouping variable."
          )
        ),
        class = "marker-source-choice-card"
      ),
      actionButton(
        ns("marker_genes_upload"),
        tagList(
          span(
            class = "marker-source-choice-title",
            "Upload precomputed results"
          ),
          span(
            class = "marker-source-choice-description",
            "Import CSV, TSV, or XLSX tables and confirm their cluster mapping."
          )
        ),
        class = "marker-source-choice-card"
      )
    )
  )
}

builder_marker_dialog_ui <- function() {
  div(
    id = "builder-marker-dialog-backdrop",
    class = "builder-confirm-backdrop builder-marker-dialog-backdrop",
    hidden = "hidden",
    div(
      id = "builder-marker-dialog",
      class = "builder-dialog builder-marker-dialog",
      role = "dialog",
      `aria-modal` = "true",
      `aria-labelledby` = "builder-marker-dialog-title",
      `aria-describedby` = "enhance-marker_dialog_body",
      tabindex = "-1",
      h2(id = "builder-marker-dialog-title", "Add Marker genes"),
      uiOutput("enhance-marker_dialog_body"),
      div(
        class = "builder-dialog-actions marker-dialog-actions",
        tags$button(
          id = "builder-marker-dialog-close",
          type = "button",
          class = "btn btn-quiet",
          "Cancel"
        )
      )
    )
  )
}

builder_marker_import_status_label <- function(source) {
  if (builder_marker_import_source_ready(source)) {
    return("Ready")
  }
  switch(
    source$error %||% source$status %||% "mapping_required",
    confirmation_required = "Confirm or change the inferred cluster.",
    mapping_required = "Choose how clusters are represented, then confirm.",
    unknown_cluster = "The table contains a cluster that is not in this Group.",
    missing_cluster = "Cluster labels cannot be empty.",
    missing_cluster_column = "Choose a column that contains cluster labels.",
    empty_table = "This source has no usable rows.",
    file_too_large = "This file exceeds the Marker gene import size limit.",
    too_many_rows = "This source has too many rows.",
    unreadable_table = "This table could not be read.",
    unreadable_sheet = "This worksheet could not be read.",
    unsupported_format = "Use CSV, TSV, or XLSX files.",
    "Resolve this source before saving."
  )
}

builder_marker_import_source_ui <- function(source, id, known_levels) {
  ns <- NS(id)
  source_id <- source$id
  mode <- source$mapping %||% "single"
  selected_cluster <- source$cluster %||%
    if (length(known_levels)) known_levels[[1L]] else ""
  selected_column <- source$cluster_column %||%
    if (length(source$columns)) source$columns[[1L]] else ""
  div(
    class = paste(
      "marker-import-source",
      if (builder_marker_import_source_ready(source)) {
        "is-ready"
      } else {
        "is-unresolved"
      }
    ),
    div(
      class = "marker-import-source-heading",
      div(
        strong(class = "marker-import-source-name", source$source_name),
        span(
          class = "marker-import-source-file",
          paste(
            source$file_name,
            if (!is.null(source$sheet)) paste0("· ", source$sheet) else ""
          )
        )
      ),
      span(
        class = "marker-import-source-size",
        paste(source$rows, "rows ·", length(source$columns), "columns")
      )
    ),
    if (is.null(source$raw_table)) {
      NULL
    } else {
      div(
        class = "marker-import-source-controls",
        selectInput(
          ns(paste0("marker_source_mode_", source_id)),
          "Cluster layout",
          choices = c(
            "One cluster in this source" = "single",
            "A column contains multiple clusters" = "multiple"
          ),
          selected = mode
        ),
        if (identical(mode, "multiple")) {
          selectInput(
            ns(paste0("marker_source_column_", source_id)),
            "Cluster column",
            choices = source$columns,
            selected = selected_column
          )
        } else {
          selectInput(
            ns(paste0("marker_source_cluster_", source_id)),
            "Cluster",
            choices = known_levels,
            selected = selected_cluster
          )
        },
        tags$button(
          type = "button",
          class = "btn btn-action marker-source-confirm",
          `data-source-id` = source_id,
          "Confirm mapping"
        )
      )
    },
    p(
      class = "marker-import-source-status",
      builder_marker_import_status_label(source)
    )
  )
}

builder_marker_import_ui <- function(id, groups, draft = NULL) {
  ns <- NS(id)
  sources <- draft$sources %||% list()
  known_levels <- draft$known_levels %||% character()
  validation <- draft$validation %||%
    list(
      ready = FALSE,
      errors = "missing_sources",
      warnings = character()
    )
  div(
    class = "marker-import-workbench",
    if (!length(sources)) {
      tagList(
        p(
          class = "marker-import-intro",
          "Add one or more result files. Every workbook worksheet becomes a source to map."
        ),
        textInput(
          ns("marker_import_method"),
          "Method name",
          value = draft$method %||% ""
        ),
        selectInput(
          ns("marker_import_group"),
          "Groups",
          choices = groups,
          selected = draft$group %||% if (length(groups)) groups[[1L]] else ""
        ),
        fileInput(
          ns("marker_import_files"),
          "Marker gene tables",
          multiple = TRUE,
          accept = builder_file_accept(builder_table_extensions())
        )
      )
    } else {
      tagList(
        tags$dl(
          class = "marker-import-summary",
          div(tags$dt("Method"), tags$dd(draft$method)),
          div(tags$dt("Groups"), tags$dd(draft$group))
        ),
        div(
          class = "marker-import-source-list",
          lapply(
            sources,
            builder_marker_import_source_ui,
            id = id,
            known_levels = known_levels
          )
        )
      )
    },
    div(
      class = "marker-import-validation",
      `aria-live` = "polite",
      if (length(validation$warnings %||% character())) {
        p(class = "marker-import-warning", validation$warnings)
      },
      if (length(validation$errors %||% character())) {
        p(
          class = "marker-import-error",
          paste(
            sum(
              !vapply(sources, builder_marker_import_source_ready, logical(1))
            ),
            "source(s) still need confirmation."
          )
        )
      }
    ),
    actionButton(
      ns("marker_import_save"),
      "Save imported method",
      class = "btn-action marker-import-save",
      disabled = !isTRUE(validation$ready)
    )
  )
}
