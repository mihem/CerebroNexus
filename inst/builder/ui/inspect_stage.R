## Guided Import / Inspect stage.

builder_stage_has_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

builder_stage_text_items <- function(values, class = NULL) {
  values <- as.character(values %||% character())
  if (!length(values)) {
    return(NULL)
  }
  tags$ul(
    class = class,
    lapply(values, function(value) tags$li(value))
  )
}

builder_inspect_issue_label <- function(value) {
  value <- as.character(value %||% "")
  switch(
    value,
    settings_organism = "Choose an organism in Essentials.",
    dataset = "Review this dataset before continuing.",
    tools::toTitleCase(gsub("[_:]", " ", value))
  )
}

builder_inspect_content_tag <- function(entry) {
  id <- entry$id %||% ""
  if (
    !identical(entry$status %||% "", "valid") ||
      id %in% c("dataset_identity", "projection", "metadata_policy")
  ) {
    return(NULL)
  }
  label <- switch(
    id,
    expression = "Expression",
    metadata = "Metadata",
    groups = "Groups",
    spatial = "Spatial",
    trekker = "Trekker",
    marker_genes = "Marker genes",
    most_expressed_genes = "Most expressed genes",
    mean_expression = "Mean expression",
    enriched_pathways = "Enriched pathways",
    trajectory = "Trajectory",
    gene_lists = "Gene lists",
    extra_material = "Extra material",
    immune_repertoire = "Immune repertoire",
    hla_tcr_motifs = "HLA & TCR motifs",
    hla = "HLA",
    if (startsWith(id, "reduction:")) {
      toupper(sub("^reduction:", "", id))
    } else {
      tools::toTitleCase(gsub("_", " ", id))
    }
  )
  tone <- if (startsWith(id, "reduction:")) {
    "projection"
  } else if (id %in% c("metadata", "groups")) {
    "metadata"
  } else if (id %in% c("spatial", "trekker")) {
    "spatial"
  } else if (
    id %in%
      c(
        "marker_genes",
        "most_expressed_genes",
        "mean_expression",
        "enriched_pathways",
        "gene_lists"
      )
  ) {
    "analysis"
  } else if (identical(id, "trajectory")) {
    "trajectory"
  } else if (id %in% c("immune_repertoire", "hla_tcr_motifs", "hla")) {
    "immune"
  } else if (identical(id, "extra_material")) {
    "extra"
  } else {
    "core"
  }
  list(label = label, tone = tone)
}

builder_inspect_model <- function(
  profile,
  state,
  format,
  dataset_id,
  settings = list()
) {
  manifest <- state$manifest %||% list()
  content_tags <- Filter(
    Negate(is.null),
    lapply(manifest, builder_inspect_content_tag)
  )
  if (length(content_tags)) {
    labels <- tolower(vapply(content_tags, `[[`, character(1), "label"))
    content_tags <- content_tags[!duplicated(labels)]
  }
  list(
    summary = c(
      paste(format(profile$n_cells, big.mark = ","), "cells"),
      paste(format(profile$n_genes, big.mark = ","), "genes")
    ),
    attention = vapply(
      state$attention_ids %||% character(),
      builder_inspect_issue_label,
      character(1)
    ),
    blockers = vapply(
      state$blocking_ids %||% character(),
      builder_inspect_issue_label,
      character(1)
    ),
    content_tags = content_tags,
    diagnostics = c(paste("Format", format), paste("Dataset id", dataset_id))
  )
}

builder_inspect_stage_ui <- function(id, model) {
  ns <- NS(id)
  attention <- as.character(model$attention %||% character())
  blockers <- as.character(model$blockers %||% character())
  div(
    id = ns("stage"),
    class = "builder-stage-section builder-stage-inspect",
    h3("Dataset summary"),
    if (length(c(attention, blockers))) {
      div(
        class = "notice warn",
        h4("Needs attention"),
        builder_stage_text_items(c(attention, blockers))
      )
    },
    div(
      class = "builder-dataset-summary",
      div(
        class = "facts builder-dataset-summary-facts",
        lapply(as.character(model$summary %||% character()), function(value) {
          # "5,020 cells" reads as a stat figure: the number over its unit.
          parts <- regmatches(
            value,
            regexec("^([0-9][0-9,.]*) (.+)$", value)
          )[[1L]]
          if (length(parts) == 3L) {
            div(
              class = "fact builder-stat",
              span(class = "builder-stat-value", parts[[2L]]),
              span(class = "builder-stat-label", parts[[3L]])
            )
          } else {
            div(class = "fact", value)
          }
        })
      ),
      if (length(model$content_tags %||% list())) {
        div(
          class = "builder-detected-content",
          div(
            class = "builder-detected-content-head",
            h4("Detected content"),
            span(
              class = "builder-detected-content-count",
              paste(
                length(model$content_tags),
                if (length(model$content_tags) == 1L) {
                  "content type"
                } else {
                  "content types"
                }
              )
            )
          ),
          div(
            class = "builder-content-tags",
            lapply(model$content_tags, function(tag) {
              span(
                class = paste(
                  "builder-content-tag",
                  paste0("is-", tag$tone)
                ),
                tag$label
              )
            })
          )
        )
      }
    )
  )
}
