## Every projection tab with box/lasso selection exposes the "Zoom to selection"
## button alongside "Clear selection": a hidden actionButton keyed
## <plot_id>_zoom_to_selection, a shinyjs bridge to the shared zoomToSelection(),
## that bridge registered in extendShinyjs(functions=), and an observer that
## shows/hides it with the selection. Spatial is included here (it already had
## Clear), so all four projection tabs stay in lock-step.

repo_file <- function(...) {
  parts <- c(...)
  stripped <- if (length(parts) && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }
  # 1) installed package (R CMD check) or load_all-shimmed source location
  if (length(stripped)) {
    p <- system.file(
      do.call(file.path, as.list(stripped)),
      package = "CerebroNexus"
    )
    if (nzchar(p)) {
      return(p)
    }
  }
  # 2) fall back to the source tree (devtools::test_dir run from the repo)
  testthat::test_path("..", "..", ...)
}

read_all <- function(dir) {
  files <- list.files(
    repo_file("inst", "shiny", "v1.4", dir),
    full.names = TRUE,
    recursive = TRUE
  )
  paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
}

tabs <- list(
  overview = list(
    plot_id = "overview_projection",
    bridge = "overviewZoomToSelection"
  ),
  gene_expression = list(
    plot_id = "expression_projection",
    bridge = "expressionZoomToSelection"
  ),
  trajectory = list(
    plot_id = "trajectory_projection",
    bridge = "trajectoryZoomToSelection"
  ),
  spatial = list(
    plot_id = "spatial_projection",
    bridge = "spatialZoomToSelection"
  )
)

for (tab in names(tabs)) {
  local({
    dir <- tab
    plot_id <- tabs[[tab]][["plot_id"]]
    bridge <- tabs[[tab]][["bridge"]]
    src <- read_all(dir)

    test_that(paste0(dir, ": has a hidden zoom-to-selection actionButton"), {
      expect_match(
        src,
        paste0('inputId = "', plot_id, '_zoom_to_selection"'),
        fixed = TRUE
      )
      expect_match(src, '"Zoom to selection"', fixed = TRUE)
      expect_match(
        src,
        paste0(
          "shinyjs::hidden\\([\\s\\S]{0,120}?",
          plot_id,
          "_zoom_to_selection"
        ),
        perl = TRUE
      )
    })

    test_that(paste0(dir, ": bridges to the shared zoomToSelection()"), {
      expect_match(
        src,
        paste0("shinyjs.", bridge, " = function"),
        fixed = TRUE
      )
      expect_match(src, "cerebroProjection.zoomToSelection(", fixed = TRUE)
      expect_match(src, paste0('"', bridge, '"'), fixed = TRUE)
    })

    test_that(paste0(dir, ": event zooms and observer toggles the button"), {
      expect_match(
        src,
        paste0('input\\[\\["', plot_id, '_zoom_to_selection"\\]\\]'),
        perl = TRUE
      )
      expect_match(src, paste0("js\\$", bridge, "\\(\\)"), perl = TRUE)
      expect_match(
        src,
        paste0('shinyjs::show\\("', plot_id, '_zoom_to_selection"\\)'),
        perl = TRUE
      )
      expect_match(
        src,
        paste0('shinyjs::hide\\("', plot_id, '_zoom_to_selection"\\)'),
        perl = TRUE
      )
    })

    test_that(paste0(dir, ": both buttons share the no-wrap flex row"), {
      expect_match(src, "cerebro-selection-actions", fixed = TRUE)
    })

    test_that(paste0(dir, ": zoom-state observer swaps style and label"), {
      # driven by <plot_id>_zoom_state reported from the JS toggle
      expect_match(
        src,
        paste0('input\\[\\["', plot_id, '_zoom_state"\\]\\]'),
        perl = TRUE
      )
      expect_match(src, '"Reset zoom"', fixed = TRUE)
      expect_match(src, "is-zoomed", fixed = TRUE)
    })
  })
}
