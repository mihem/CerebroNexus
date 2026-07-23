#' Remove Common Leading Whitespace from a String
#'
#' Eliminates the minimal common indentation shared by all non-empty lines of
#' the input, preserving relative indentation within blocks.
#'
#' @param string A character string containing text with indentation.
#' @return A dedented character string.
#' @keywords internal
#' @noRd
dedent <- function(string) {
  if (!is.character(string) || length(string) != 1) {
    stop("Input must be a single character string")
  }
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]
  while (length(lines) > 0 && grepl("^\\s*$", lines[1])) {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && grepl("^\\s*$", lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  if (length(lines) == 0) {
    return("")
  }
  non_empty_lines <- lines[!grepl("^\\s*$", lines)]
  if (length(non_empty_lines) == 0) {
    return("")
  }
  lead_spaces <- vapply(
    non_empty_lines,
    function(line) {
      m <- regmatches(line, regexpr("^\\s*", line))
      nchar(m)
    },
    integer(1)
  )
  min_indent <- min(lead_spaces)
  if (min_indent > 0) {
    pat <- paste0("^\\s{", min_indent, "}")
    lines <- vapply(
      lines,
      function(line) {
        if (grepl("^\\s*$", line)) line else sub(pat, "", line)
      },
      character(1)
    )
  }
  paste(lines, collapse = "\n")
}

#' Create a self-contained Shiny app folder for Cerebro v1.4
#'
#' Bundles a Cerebro v1.4 Shiny app into \code{result_dir}, copying the
#' \code{inst/shiny/v1.4/} sources, the requested \code{.crb} data file(s),
#' and \code{extdata/}, and writes an \code{app.R} that sources the bundled
#' UI/server. The output directory can be served directly by shiny-server or
#' run with \code{shiny::runApp(result_dir)}.
#'
#' Supports external expression backends (\code{bpcells}, \code{h5}) in
#' addition to the embedded mode. When \code{cerebro_data} points to a
#' \code{.crb} with an external backend, the sibling \code{.bpcells/}
#' directory or \code{.h5} file is detected and copied into the bundle
#' alongside the \code{.crb}.
#'
#' @param cerebro_data Named character vector or list of \code{.crb} (or
#'   \code{.rds}) file paths. Names are used as dataset labels.
#' @param result_dir Output directory.
#' @param max_request_size Max upload size in MB; defaults to 8000.
#' @param port Port the generated app listens on; defaults to 1337.
#' @param host Host the generated app binds to; defaults to "127.0.0.1".
#' @param launch_browser Whether to launch a browser; defaults to TRUE.
#' @param quiet Passed to \code{shiny::runApp}; defaults to FALSE.
#' @param display_mode \code{shiny::runApp} display mode; defaults to "normal".
#' @param colors Optional named list of colour palettes per dataset.
#' @param cerebro_options Extra entries merged into \code{Cerebro.options} in
#'   the generated app.
#' @param overwrite If \code{TRUE} (default), wipe \code{result_dir} first.
#' @param verbose Print progress messages; defaults to TRUE.
#' @param crb_pick_smallest_file Forwarded to \code{Cerebro.options}.
#' @param show_upload_ui Forwarded to \code{Cerebro.options}.
#' @param welcome_message Welcome message shown in the Load Data tab.
#' @param point_size Named list with \code{overview_projection_point_size}
#'   (and optionally other keys) forwarded to \code{Cerebro.options}.
#' @param variable_to_compare Forwarded to \code{Cerebro.options}.
#' @param spatial_images Named list/vector of paths to spatial background images
#'   (e.g. tissue histology) shown behind the Spatial tab projection. Names must
#'   match \code{cerebro_data}. Images are copied into the app bundle.
#' @param spatial_images_flip_x Named list/vector; whether to flip the spatial
#'   background image horizontally. Names must match \code{cerebro_data}.
#' @param spatial_images_flip_y Named list/vector; whether to flip the spatial
#'   background image vertically. Names must match \code{cerebro_data}.
#' @param spatial_images_scale_x Named list/vector; scaling factor for the X
#'   axis of the spatial background image. Names must match \code{cerebro_data}.
#' @param spatial_images_scale_y Named list/vector; scaling factor for the Y
#'   axis of the spatial background image. Names must match \code{cerebro_data}.
#' @param spatial_images_offset_x Named list/vector; horizontal offset (in data
#'   units) applied to move the spatial background image. Names must match
#'   \code{cerebro_data}.
#' @param spatial_images_offset_y Named list/vector; vertical offset (in data
#'   units) applied to move the spatial background image. Names must match
#'   \code{cerebro_data}.
#' @param spatial_plot_rotation Named list/vector; initial rotation (degrees)
#'   applied to spatial cell coordinates. Names must match \code{cerebro_data}.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return Invisibly returns \code{result_dir}.
#' @importFrom later later
#' @importFrom stats setNames
#' @export
createShinyApp <- function(
  cerebro_data,
  result_dir = NULL,
  max_request_size = 8000,
  port = 8080,
  host = "127.0.0.1",
  launch_browser = TRUE,
  quiet = FALSE,
  display_mode = "normal",
  colors = NULL,
  cerebro_options = list(exclude_trivial_metadata = TRUE),
  overwrite = TRUE,
  verbose = TRUE,
  crb_pick_smallest_file = TRUE,
  show_upload_ui = TRUE,
  welcome_message = "Welcome to CerebroNexus!",
  point_size = list(
    overview_projection_point_size = NULL
  ),
  variable_to_compare = NULL,
  spatial_images = NULL,
  spatial_images_flip_x = NULL,
  spatial_images_flip_y = NULL,
  spatial_images_scale_x = NULL,
  spatial_images_scale_y = NULL,
  spatial_images_offset_x = NULL,
  spatial_images_offset_y = NULL,
  spatial_plot_rotation = NULL,
  ...
) {
  # Validate inputs ----------------------------------------------------------##
  if (!all(file.exists(cerebro_data))) {
    missing <- cerebro_data[!file.exists(cerebro_data)]
    stop(
      "Cerebro data file(s) not found: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(grepl("\\.(crb|rds)$", cerebro_data, ignore.case = TRUE))) {
    warning(
      "Some input files do not have .crb or .rds extension. Make sure they are valid Cerebro files."
    )
  }

  if (is.null(names(cerebro_data)) || any(names(cerebro_data) == "")) {
    stop(
      "cerebro_data must be a named list or vector, and every element must have a name.",
      call. = FALSE
    )
  }

  if (!is.null(colors)) {
    if (is.null(names(colors)) || any(names(colors) == "")) {
      stop("colors must be a named list or vector.", call. = FALSE)
    }
    if (length(intersect(names(colors), names(cerebro_data))) == 0) {
      warning(
        "Colors and cerebro_data do not match, random colors will be used.",
        call. = FALSE
      )
      colors <- NULL
    }
  }

  if (!is.null(variable_to_compare) && !is.logical(variable_to_compare)) {
    if (
      (is.list(variable_to_compare) || is.vector(variable_to_compare)) &&
        !is.null(names(variable_to_compare))
    ) {
      if (
        length(intersect(names(variable_to_compare), names(cerebro_data))) == 0
      ) {
        warning(
          "No matching names found between variable_to_compare and cerebro_data. Ignoring.",
          call. = FALSE
        )
        variable_to_compare <- NULL
      }
    } else {
      warning(
        "variable_to_compare must be NULL, a single boolean, or a named list/vector. Ignoring.",
        call. = FALSE
      )
      variable_to_compare <- NULL
    }
  }

  ## Spatial background images (and their per-dataset transforms) must be named
  ## to match cerebro_data; drop with a warning if malformed rather than error,
  ## so a bad image spec never blocks app generation.
  validate_named_against_data <- function(x, arg_name) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.null(names(x)) || any(names(x) == "")) {
      warning(
        arg_name,
        " must be a named list or vector. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    if (length(intersect(names(x), names(cerebro_data))) == 0) {
      warning(
        "No matching names found between ",
        arg_name,
        " and cerebro_data. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    x
  }
  spatial_images <- validate_named_against_data(
    spatial_images,
    "spatial_images"
  )
  spatial_images_flip_x <- validate_named_against_data(
    spatial_images_flip_x,
    "spatial_images_flip_x"
  )
  spatial_images_flip_y <- validate_named_against_data(
    spatial_images_flip_y,
    "spatial_images_flip_y"
  )
  spatial_images_scale_x <- validate_named_against_data(
    spatial_images_scale_x,
    "spatial_images_scale_x"
  )
  spatial_images_scale_y <- validate_named_against_data(
    spatial_images_scale_y,
    "spatial_images_scale_y"
  )
  spatial_images_offset_x <- validate_named_against_data(
    spatial_images_offset_x,
    "spatial_images_offset_x"
  )
  spatial_images_offset_y <- validate_named_against_data(
    spatial_images_offset_y,
    "spatial_images_offset_y"
  )
  spatial_plot_rotation <- validate_named_against_data(
    spatial_plot_rotation,
    "spatial_plot_rotation"
  )

  if (!requireNamespace("CerebroNexus", quietly = TRUE)) {
    stop(
      "Package 'CerebroNexus' is required but not installed.",
      call. = FALSE
    )
  }

  if (is.null(result_dir)) {
    stop("'result_dir' must be provided.", call. = FALSE)
  }

  # Setup directories --------------------------------------------------------##
  data_dir <- file.path(result_dir, "data")
  app_file <- file.path(result_dir, "app.R")

  if (overwrite && dir.exists(result_dir)) {
    if (verbose) {
      cat("Removing existing directory:", result_dir, "\n")
    }
    unlink(result_dir, recursive = TRUE, force = TRUE)
  }

  if (verbose) {
    cat("Creating directory structure...\n")
  }
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy Shiny source --------------------------------------------------------##
  shiny_source <- system.file("shiny", package = "CerebroNexus")
  if (!dir.exists(shiny_source)) {
    stop(
      "Shiny source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }

  if (verbose) {
    cat("Copying Shiny source files...\n")
  }
  if (!file.copy(shiny_source, result_dir, recursive = TRUE)) {
    stop("Failed to copy Shiny source files.", call. = FALSE)
  }

  # Copy Cerebro data file(s) -----------------------------------------------##
  if (verbose) {
    cat("Copying Cerebro data file(s)...\n")
  }
  for (file in cerebro_data) {
    if (verbose) {
      cat("  -", basename(file), "\n")
    }
    if (!file.copy(file, data_dir, recursive = TRUE)) {
      stop("Failed to copy Cerebro data file: ", basename(file), call. = FALSE)
    }
    ## External-backend crbs store only metadata; the expression matrix
    ## lives in a sibling file/dir resolved relative to the crb at runtime.
    ## Copy the sibling alongside so the bundle stays portable.
    crb_stem <- tools::file_path_sans_ext(basename(file))
    bpc_src <- file.path(dirname(file), paste0(crb_stem, ".bpcells"))
    if (dir.exists(bpc_src)) {
      if (verbose) {
        cat("  -", basename(bpc_src), "(bpcells sibling)\n")
      }
      if (!file.copy(bpc_src, data_dir, recursive = TRUE)) {
        stop(
          "Failed to copy bpcells sibling directory: ",
          basename(bpc_src),
          call. = FALSE
        )
      }
    }
    h5_src <- file.path(dirname(file), paste0(crb_stem, ".h5"))
    if (file.exists(h5_src)) {
      if (verbose) {
        cat("  -", basename(h5_src), "(h5 sibling)\n")
      }
      if (!file.copy(h5_src, data_dir, overwrite = TRUE)) {
        stop(
          "Failed to copy h5 sibling file: ",
          basename(h5_src),
          call. = FALSE
        )
      }
    }
  }

  # Copy spatial images ------------------------------------------------------##
  ## Side-copy each background image into data_dir and rewrite the stored path
  ## to the bundle-relative "data/<file>" so the generated app is portable.
  if (!is.null(spatial_images) && length(spatial_images) > 0) {
    if (verbose) {
      cat("Copying spatial images...\n")
    }
    for (nm in names(spatial_images)) {
      img_paths <- spatial_images[[nm]]
      copied_paths <- character(0)
      for (img in img_paths) {
        if (file.exists(img)) {
          dest <- file.path(data_dir, basename(img))
          if (!file.copy(img, dest, overwrite = TRUE)) {
            warning("Failed to copy spatial image: ", img, call. = FALSE)
            copied_paths <- c(copied_paths, img)
          } else {
            if (verbose) {
              cat("  -", basename(img), "\n")
            }
            copied_paths <- c(copied_paths, file.path("data", basename(img)))
          }
        } else {
          warning("Spatial image not found: ", img, call. = FALSE)
          copied_paths <- c(copied_paths, img)
        }
      }
      spatial_images[[nm]] <- copied_paths
    }
  }

  # Copy extdata -------------------------------------------------------------##
  if (verbose) {
    cat("Copying extdata files...\n")
  }
  extdata_source <- system.file("extdata", package = "CerebroNexus")
  if (!dir.exists(extdata_source)) {
    stop(
      "extdata source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  if (!file.copy(extdata_source, result_dir, recursive = TRUE)) {
    stop("Failed to copy extdata files.", call. = FALSE)
  }

  # Build Cerebro.options ----------------------------------------------------##
  if (verbose) {
    cat("Generating app.R file...\n")
  }

  crb_files <- setNames(
    paste0("data/", basename(cerebro_data)),
    names(cerebro_data)
  )

  cerebro_options[["mode"]] <- "open"
  ## Resolve the version while the package is present, then serialize it into
  ## the generated app. The standalone bundle never needs CerebroNexus at
  ## runtime merely to render its About page.
  cerebro_options[["cerebro_version"]] <- as.character(
    utils::packageVersion("CerebroNexus")
  )
  cerebro_options[["crb_file_to_load"]] <- crb_files
  cerebro_options[["cerebro_root"]] <- "."
  if (!is.null(crb_pick_smallest_file)) {
    cerebro_options[["crb_pick_smallest_file"]] <- crb_pick_smallest_file
  }
  if (!is.null(show_upload_ui)) {
    cerebro_options[["show_upload_ui"]] <- show_upload_ui
  }
  if (!is.null(point_size)) {
    cerebro_options[["point_size"]] <- point_size
  }
  if (!is.null(colors)) {
    cerebro_options[["colors"]] <- colors
  }
  if (!is.null(welcome_message)) {
    cerebro_options[["welcome_message"]] <- welcome_message
  }
  if (!is.null(variable_to_compare)) {
    cerebro_options[["variable_to_compare"]] <- variable_to_compare
  }
  if (!is.null(spatial_images)) {
    cerebro_options[["spatial_images"]] <- spatial_images
  }
  if (!is.null(spatial_images_flip_x)) {
    cerebro_options[["spatial_images_flip_x"]] <- spatial_images_flip_x
  }
  if (!is.null(spatial_images_flip_y)) {
    cerebro_options[["spatial_images_flip_y"]] <- spatial_images_flip_y
  }
  if (!is.null(spatial_images_scale_x)) {
    cerebro_options[["spatial_images_scale_x"]] <- spatial_images_scale_x
  }
  if (!is.null(spatial_images_scale_y)) {
    cerebro_options[["spatial_images_scale_y"]] <- spatial_images_scale_y
  }
  if (!is.null(spatial_images_offset_x)) {
    cerebro_options[["spatial_images_offset_x"]] <- spatial_images_offset_x
  }
  if (!is.null(spatial_images_offset_y)) {
    cerebro_options[["spatial_images_offset_y"]] <- spatial_images_offset_y
  }
  if (!is.null(spatial_plot_rotation)) {
    cerebro_options[["spatial_plot_rotation"]] <- spatial_plot_rotation
  }

  saveRDS(cerebro_options, file.path(result_dir, "cerebro_config.rds"))

  # Generate app.R -----------------------------------------------------------##
  app_content <- glue::glue(
    '
    library(dplyr)
    library(DT)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)

    cerebro_root <- "."

    if (file.exists("cerebro_config.rds")) {{
      Cerebro.options <<- readRDS("cerebro_config.rds")
    }} else {{
      stop("cerebro_config.rds not found!")
    }}

    if (!is.null(Cerebro.options$colors)) {{
      colors <- Cerebro.options$colors
    }}

    shiny_options <- list(
      maxRequestSize = {max_request_size} * 1024^2,
      port = {port},
      host = "{host}",
      launch.browser = {toupper(as.character(launch_browser))},
      quiet = {toupper(as.character(quiet))},
      display.mode = "{display_mode}"
    )

    shiny::addResourcePath("data", file.path(cerebro_root, "data"))

    source(file.path(cerebro_root, "shiny/v1.4/shiny_UI.R"))
    source(file.path(cerebro_root, "shiny/v1.4/shiny_server.R"))

    shiny::shinyApp(
      ui = ui,
      server = server,
      options = shiny_options
    )
    ',
    .trim = FALSE
  )

  writeLines(dedent(app_content), app_file)

  # Summary ------------------------------------------------------------------##
  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("Shiny app successfully created!\n")
    cat("========================================\n")
    cat("App directory:", result_dir, "\n")
    cat("Data file(s):\n")
    for (i in seq_along(cerebro_data)) {
      label <- names(cerebro_data)[i]
      if (!is.null(label) && nzchar(label)) {
        cat("  -", label, ":", basename(cerebro_data[i]), "\n")
      } else {
        cat("  -", basename(cerebro_data[i]), "\n")
      }
    }
    cat("Port:", port, "\n")
    cat("Host:", host, "\n")
    cat("Launch browser:", launch_browser, "\n")
    cat("\nTo launch the app, run:\n")
    cat("  setwd('", result_dir, "')\n", sep = "")
    cat("  shiny::runApp('app.R')\n")
    cat("========================================\n")
  }

  invisible(result_dir)
}
