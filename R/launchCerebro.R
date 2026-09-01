#' @title
#' Launch CerebroNexus
#'
#' @description
#' Launch the CerebroNexus Shiny application.
#'
#' @param mode Cerebro can be ran in \code{open} or \code{closed} mode, allowing
#' the user to load their own data set (\code{open}) or only show a pre-loaded
#' data set (\code{closed}, removes the "Load data" element); defaults to
#' \code{open}.
#' @param maxFileSize Maximum size of input file; defaults to \code{800}
#' (800 MB).
#' @param crb_file_to_load Path to \code{.crb} file to load on launch of
#' Cerebro. Useful when using/hosting Cerebro in \code{closed} mode. Defaults to
#' \code{NULL}.
#' @param expression_matrix_mode  Mode of expression matrix. Can be either
#' crb, h5, or BPCells. Default is crb.
#' @param expression_matrix_h5 Optional: Path to \code{.h5} file containing an expression
#' matrix created with \code{HDF5Array::writeTENxMatrix()}, with genes as
#' columns and cells as rows, contrary to the conventional format of genes as
#' rows and cells as columns. This format greatly favors performance for
#' extracting expression values for a gene (column), rather than a cell (row),
#' which is the primary action in Cerebro. Importantly, the matrix should be
#' stored with "expression" as group name (see parameters of the
#' \code{HDF5Array::writeTENxMatrix()} function). Saving the expression matrix
#' in \code{TENxMatrix} format has the benefit of a low memory footprint since
#' the expression values are directly read from disk. This is particularly
#' useful when working with very large data sets and/or when startup of the
#' CerebroNexus app is a priority (which is shorter because only the rest of the data
#' that needs to be loaded tends to be very small). By default, this value is
#' set to \code{NULL}, meaning that the expression matrix is expected to be part
#' of the \code{.crb} file.
#' @param expression_matrix_BPCells Optional: Path to BPCells directory created with
#' \code{BPCells::write_matrix_dir()}. This is a hopefully faster alternative to h5
#' with a similar approach.
#' @param welcome_message \code{string} with custom welcome message to display
#' in the "Load data" tab. Can contain HTML formatting, e.g.
#' \code{'<h3>Hi!</h3>'}. Defaults to \code{NULL}.
#' @param point_size Default point size for cell scatter views. This value can
#'   be changed in the UI; defaults to 5.
#' @param point_opacity Default point opacity for cell scatter views. This value
#'   can be changed in the UI; defaults to 1.
#' @param percentage_cells_to_show Default percentage of cells shown in every
#'   cell scatter view. This value can be changed independently in each page;
#'   defaults to 100.
#' @param projections_show_hover_info Show hover infos in projections. This
#' setting can be changed in the UI; defaults to TRUE.
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#'
#' @return
#' Shiny application.
#'
#' @examples
#' if ( interactive() ) {
#'   launchCerebro(
#'     mode = "open",
#'     maxFileSize = 800
#'   )
#' }
#'
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @importFrom DT datatable formatPercentage formatRound formatSignif formatStyle styleColorBar styleEqual styleInterval
#' @import ggplot2
#' @importFrom grDevices col2rgb rgb
#' @importFrom msigdbr msigdbr
#' @importFrom plotly add_lines add_trace event_data layout plot_ly plotlyOutput renderPlotly toWebGL
#' @importFrom stringr str_length
#' @importFrom tidyr pivot_longer pivot_wider
#' @import scales
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyFiles getVolumes parseSavePath shinyFileSave shinySaveButton
#' @importFrom shinyjs inlineCSS
#' @importFrom shinyWidgets awesomeCheckbox dropdownButton materialSwitch radioGroupButtons sendSweetAlert
#'
#' @export
#'
launchCerebro <- function(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  expression_matrix_mode = "crb",
  expression_matrix_h5 = NULL,
  expression_matrix_BPCells = NULL,
  welcome_message = NULL,
  point_size = 5,
  point_opacity = 1,
  percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  ...
) {
  ##--------------------------------------------------------------------------##
  ## Check validity of input parameters.
  ##--------------------------------------------------------------------------##
  if (mode %in% c('open', 'closed') == FALSE) {
    stop(
      "'mode' parameter must be set to either 'open' or 'closed'.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size < 1 ||
      point_size > 20
  ) {
    stop(
      "'point_size' parameter must be between 1 and 20",
      call. = FALSE
    )
  }
  if (
    !is.numeric(point_opacity) ||
      length(point_opacity) != 1L ||
      is.na(point_opacity) ||
      !is.finite(point_opacity) ||
      point_opacity < 0.1 ||
      point_opacity > 1
  ) {
    stop(
      "'point_opacity' parameter must be between 0.1 and 1",
      call. = FALSE
    )
  }
  if (
    !is.numeric(percentage_cells_to_show) ||
      length(percentage_cells_to_show) != 1L ||
      is.na(percentage_cells_to_show) ||
      !is.finite(percentage_cells_to_show) ||
      percentage_cells_to_show < 10 ||
      percentage_cells_to_show > 100
  ) {
    stop(
      "'percentage_cells_to_show' parameter must be between 10 and 100",
      call. = FALSE
    )
  }
  if (projections_show_hover_info %in% c(TRUE, FALSE) == FALSE) {
    stop(
      "'projections_show_hover_info' parameter must be set to either TRUE or FALSE.",
      call. = FALSE
    )
  }

  ## --------------------------------------------------------------------------##
  ## Create global variable with options that need to be available inside the
  ## Shiny app.
  ## --------------------------------------------------------------------------##
  cerebro_options <- list(
    "mode" = mode,
    "cerebro_version" = as.character(
      utils::packageVersion("CerebroNexus")
    ),
    "expression_matrix_mode" = expression_matrix_mode,
    "crb_file_to_load" = crb_file_to_load,
    "expression_matrix_h5" = expression_matrix_h5,
    "expression_matrix_BPCells" = expression_matrix_BPCells,
    "welcome_message" = welcome_message,
    "cerebro_root" = system.file(package = "CerebroNexus"),
    "point_size" = point_size,
    "point_opacity" = point_opacity,
    "percentage_cells_to_show" = percentage_cells_to_show,
    "projections_show_hover_info" = projections_show_hover_info
  )
  assign("Cerebro.options", cerebro_options, envir = .GlobalEnv)

  ##--------------------------------------------------------------------------##
  ## Allow upload of files up to 800 MB.
  ##--------------------------------------------------------------------------##
  options(shiny.maxRequestSize = maxFileSize * 1024^2)

  ##--------------------------------------------------------------------------##
  ## Load server and UI functions.
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("viewer/shiny_UI.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )
  source(
    system.file(
      paste0("viewer/shiny_server.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Launch Cerebro.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching CerebroNexus\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  shiny::shinyApp(
    ui = ui,
    server = server,
    ...
  )
}
