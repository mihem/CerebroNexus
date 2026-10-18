viewerOverviewNumber <- function(value) {
  formatC(value, format = "f", digits = 0, big.mark = ",")
}

viewerOverviewBars <- function(summary) {
  if (is.null(summary) || !length(summary$counts)) {
    return(NULL)
  }
  counts <- summary$counts
  shiny::tags$div(
    class = "dataset-bars",
    lapply(seq_along(counts), function(i) {
      percent <- 100 * counts[[i]] / summary$total
      shiny::tags$div(
        class = "dataset-bar-row",
        shiny::tags$span(
          class = "dataset-bar-label",
          title = names(counts)[i],
          names(counts)[i]
        ),
        shiny::tags$span(
          class = "dataset-bar-track",
          `aria-hidden` = "true",
          shiny::tags$span(
            style = paste0(
              "width:",
              format(percent, scientific = FALSE, trim = TRUE),
              "%"
            )
          )
        ),
        shiny::tags$span(
          class = "dataset-bar-value",
          viewerOverviewNumber(counts[[i]]),
          shiny::tags$small(sprintf("%.1f%%", percent))
        )
      )
    }),
    if (summary$missing > 0) {
      shiny::tags$p(
        class = "dataset-note",
        paste(
          viewerOverviewNumber(summary$missing),
          "cells without an annotation are excluded."
        )
      )
    }
  )
}

viewerOverviewUI <- function(info) {
  tags <- shiny::tags
  overview <- info$overview
  number <- function(value, label) {
    if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
      return(NULL)
    }
    tags$div(
      class = "dataset-stat",
      id = if (label == "Cells") "load_data_number_of_cells" else NULL,
      tags$strong(viewerOverviewNumber(value)),
      tags$span(label)
    )
  }
  organism <- viewerOverviewText(info$organism)
  if (!is.null(organism)) {
    organism <- switch(organism, hg = "Human", mm = "Mouse", organism)
  }
  date <- viewerOverviewText(info$date)
  annotations <- overview$annotations
  links <- list()
  add_link <- function(target, title, detail) {
    links[[length(links) + 1L]] <<- shiny::actionLink(
      paste0("dataset_open_", target),
      label = tags$span(
        tags$strong(title),
        tags$small(detail),
        tags$span(class = "dataset-link-arrow", `aria-hidden` = "true", "→")
      ),
      class = "dataset-explore-link"
    )
  }
  projections <- if (length(overview$projections)) {
    overview$projections
  } else {
    info$projection_names
  }
  if (length(projections)) {
    add_link("overview", "Projection", paste(projections, collapse = " · "))
  }
  if (!is.null(overview$genes)) {
    add_link(
      "geneExpression",
      "Gene expression",
      "Explore expression across cells"
    )
  }
  if (length(annotations) || !is.null(overview$sample)) {
    add_link("groups", "Groups", "Compare cell populations")
  }
  if (length(overview$spatial)) {
    add_link("spatial", "Spatial", "Explore tissue and spatial coordinates")
  }
  capabilities <- info$capabilities
  if (isTRUE(capabilities$trajectory)) {
    add_link("trajectory", "Trajectory", "Explore cell trajectories")
  }
  if (isTRUE(info$immune_repertoire)) {
    add_link(
      "immune_repertoire",
      "Immune repertoire",
      "Explore receptor repertoires"
    )
  }
  if (isTRUE(capabilities$marker_genes)) {
    add_link("markerGenes", "Marker genes", "Inspect exported marker results")
  }
  if (isTRUE(capabilities$enriched_pathways)) {
    add_link(
      "enrichedPathways",
      "Enriched pathways",
      "Explore exported enrichment results"
    )
  }
  if (isTRUE(capabilities$extra_material)) {
    add_link("extra_material", "Extra material", "Browse supporting material")
  }
  shiny::tagList(
    tags$section(
      class = "dataset-overview",
      `aria-label` = "Dataset overview",
      tags$header(
        class = "dataset-summary-heading",
        tags$h2(info$label),
        if (!is.null(overview$description)) {
          tags$p(class = "dataset-description", overview$description)
        },
        if (!is.null(organism) || !is.null(date)) {
          tags$div(
            class = "dataset-meta",
            if (!is.null(organism)) {
              tags$span(id = "load_data_organism", organism)
            },
            if (!is.null(date)) {
              tags$span(
                id = "load_data_date_of_export",
                paste("Exported", date)
              )
            }
          )
        }
      ),
      tags$div(
        class = "cerebro-dataset-stats dataset-metrics",
        number(info$cells, "Cells"),
        number(overview$genes, "Genes"),
        number(overview$samples, "Samples"),
        if (length(overview$spatial)) {
          number(length(overview$spatial), "Spatial views")
        }
      ),
      if (!is.null(overview$sample) || length(annotations)) {
        tags$div(
          class = "dataset-compositions",
          if (!is.null(overview$sample)) {
            tags$section(
              class = "dataset-panel",
              tags$h3("Sample composition"),
              tags$p(class = "dataset-note", "Cells per sample"),
              viewerOverviewBars(overview$sample)
            )
          },
          if (length(annotations)) {
            tags$section(
              class = "dataset-panel",
              tags$h3("Cell annotations"),
              shiny::selectInput(
                "load_data_annotation",
                "Group by",
                choices = names(annotations),
                width = "100%"
              ),
              htmltools::tagAppendChild(
                shiny::uiOutput("load_data_annotation_plot"),
                viewerOverviewBars(annotations[[1L]])
              )
            )
          }
        )
      },
      if (length(links)) {
        tags$section(
          class = "dataset-explore",
          tags$h3("Explore this dataset"),
          tags$div(class = "dataset-explore-grid", links)
        )
      },
      if (!is.null(overview$source)) {
        tags$section(
          class = "dataset-source",
          tags$h3("Data source"),
          tags$p(overview$source)
        )
      }
    )
  )
}
