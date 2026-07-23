## Every projection tab with box/lasso selection should expose the same "Clear
## selection" affordance spatial has: a hidden actionButton keyed
## <plot_id>_clear_selection (so the shared Delete/Esc key handler in
## projection_scatter.js can click it), a shinyjs bridge to the shared
## clearSelection(), that bridge registered in extendShinyjs(functions=), and an
## observer that shows/hides the button with the selection. This test locks the
## wiring across overview / gene_expression / trajectory so it can't drift back
## to spatial-only again.

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

# tab dir -> (plot_id, shinyjs bridge name)
tabs <- list(
  overview = list(
    plot_id = "overview_projection",
    bridge = "overviewClearSelection"
  ),
  gene_expression = list(
    plot_id = "expression_projection",
    bridge = "expressionClearSelection"
  ),
  trajectory = list(
    plot_id = "trajectory_projection",
    bridge = "trajectoryClearSelection"
  )
)

for (tab in names(tabs)) {
  local({
    dir <- tab
    plot_id <- tabs[[tab]][["plot_id"]]
    bridge <- tabs[[tab]][["bridge"]]
    src <- read_all(dir)

    test_that(paste0(dir, ": has a hidden clear-selection actionButton"), {
      # keyed <plot_id>_clear_selection so the shared key handler finds it
      expect_match(
        src,
        paste0('inputId = "', plot_id, '_clear_selection"'),
        fixed = TRUE
      )
      expect_match(src, '"Clear selection"', fixed = TRUE)
      # ships hidden; the observer reveals it once cells are selected
      expect_match(
        src,
        paste0(
          "shinyjs::hidden\\([\\s\\S]{0,120}?",
          plot_id,
          "_clear_selection"
        ),
        perl = TRUE
      )
    })

    test_that(paste0(dir, ": bridges to the shared clearSelection()"), {
      expect_match(
        src,
        paste0("shinyjs.", bridge, " = function"),
        fixed = TRUE
      )
      expect_match(
        src,
        paste0("cerebroProjection.clearSelection("),
        fixed = TRUE
      )
      # bridge is registered so js$<bridge>() resolves
      expect_match(src, paste0('"', bridge, '"'), fixed = TRUE)
    })

    test_that(paste0(dir, ": event clears and observer toggles the button"), {
      expect_match(
        src,
        paste0('input\\[\\["', plot_id, '_clear_selection"\\]\\]'),
        perl = TRUE
      )
      expect_match(src, paste0("js\\$", bridge, "\\(\\)"), perl = TRUE)
      # show/hide driven off the selection reactive
      expect_match(
        src,
        paste0('shinyjs::show\\("', plot_id, '_clear_selection"\\)'),
        perl = TRUE
      )
      expect_match(
        src,
        paste0('shinyjs::hide\\("', plot_id, '_clear_selection"\\)'),
        perl = TRUE
      )
    })
  })
}
