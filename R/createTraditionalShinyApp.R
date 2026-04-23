

# 添加智能缩进处理函数 Add intelligent indentation processing function
#' Remove Common Leading Whitespace from a String
#'
#' This utility function eliminates the minimal common indentation shared by all
#' non-empty lines of the input string, preserving relative indentation within blocks.
#'
#' @param string A character string containing text with indentation
#'
#' @return A character string with shared indentation removed. Output retains:
#'   - Relative indentation between lines
#'   - Trailing whitespace (after content)
#'   - Purely empty lines
#'
#' @examples
#' code <- "
#'     first line
#'         indented line
#'     last line
#' "
#' dedent(code)
#'
#' @keywords internal
#' @noRd
dedent <- function(string) {
  # Input validation
  if (!is.character(string) || length(string) != 1) {
    stop("Input must be a single character string")
  }

  # Split string into lines
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]

  # Remove leading and trailing empty lines
  while (length(lines) > 0 && grepl("^\\s*$", lines[1])) {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && grepl("^\\s*$", lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }

  if (length(lines) == 0) return("")

  # Identify minimum indentation from non-empty lines
  non_empty_lines <- lines[!grepl("^\\s*$", lines)]
  if (length(non_empty_lines) == 0) return("")

  # Calculate minimal indentation
  lead_spaces <- vapply(non_empty_lines, function(line) {
    nchar(sub("^(\\s*).*", "\\1", line))
  }, integer(1), USE.NAMES = FALSE)
  min_indent <- min(lead_spaces)

  # Remove common indentation from all lines
  dedented <- vapply(lines, function(line) {
    if (grepl("^\\s*$", line)) {
      ""  # Preserve blank lines as empty strings
    } else if (nchar(line) > min_indent) {
      substring(line, min_indent + 1)
    } else {
      line
    }
  }, character(1), USE.NAMES = FALSE)

  # Reassemble and return
  paste(dedented, collapse = "\n")
}



#' Create Traditional Shiny Application
#'
#' This function generates a complete Shiny application structure for visualizing
#' Cerebro data. It copies the necessary Shiny source files, data files, and
#' creates an app.R file configured to launch the Cerebro visualization interface.
#'
#' @param cerebro_data Character vector. Path(s) to the Cerebro data file(s) to be visualized.
#'   Supported formats are .crb, .rds, and .qs. Can be a single path or a named vector for multiple datasets.
#' @param result_dir Character. Directory where the Shiny app structure will be
#'   created. Default is "result/20_cerebro_shinyapp".
#' @param max_request_size Numeric. Maximum file upload size in MB. Default is 8000.
#' @param port Numeric. Port number for the Shiny server. Default is 1337.
#' @param host Character. Host address for the Shiny server. Use "0.0.0.0" to
#'   allow external access, or "127.0.0.1" for localhost only. Default is "127.0.0.1".
#' @param launch_browser Logical. If TRUE, automatically open the app in the
#'   default web browser when launched. Default is TRUE.
#' @param quiet Logical. If TRUE, suppress Shiny server startup messages.
#'   Default is FALSE.
#' @param display_mode Character. Display mode for the app: "normal" or "showcase".
#'   Default is "normal".
#' @param colors List. Optional nested list of color schemes for different datasets.
#'   Default is NULL.
#' @param cerebro_options List. Additional options to pass to Cerebro.options.
#'   Default is list(exclude_trivial_metadata = TRUE).
#' @param overwrite Logical. If TRUE, existing result_dir will be deleted before
#'   creating new files. Default is TRUE.
#' @param verbose Logical. If TRUE, prints detailed progress messages. Default is TRUE.
#' @param enable_auth Logical. If TRUE, enables authentication using shinymanager.
#'   Default is FALSE.
#' @param admin_user Character. Admin username. Default is "admin".
#' @param admin_pass Character. Admin password. Default is "CHANGE_ME_ON_DEPLOYMENT".
#' @param users Character vector. Additional usernames. Default is NULL.
#' @param users_pass Character vector. Passwords for additional users. Default is NULL.
#' @param auth_passphrase Character. Passphrase for encrypting credentials database.
#'   Default is "123123".
#' @param crb_pick_smallest_file Logical. If TRUE, the smallest file is selected by default.
#'   Default is TRUE.
#' @param show_upload_ui Logical. If TRUE, shows the file upload UI. Default is TRUE.
#' @param point_size Named list. Default point sizes for various plots.
#'   The list can contain the following keys, with either numeric values or NULL (to use defaults):
#'   \itemize{
#'     \item \code{"overview_projection_point_size"}: Point size for the overview projection.
#'     \item \code{"trajectory_point_size"}: Point size for the trajectory projection.
#'     \item \code{"expression_projection_point_size"}: Point size for the gene expression projection.
#'     \item \code{"spatial_projection_point_size"}: Point size for the spatial projection. Can be a numeric value or a named list matching \code{cerebro_data} names for dataset-specific defaults.
#'   }
#'   Default is a list with all keys set to NULL.
#' @param spatial_images Named list/vector. Paths to spatial images (e.g. tissue histology), names must match cerebro_data.
#' @param spatial_plot_rotation Named list/vector. Initial rotation for spatial plots, names must match cerebro_data.
#' @param spatial_images_flip_x Named list/vector. Whether to flip spatial images horizontally, names must match cerebro_data.
#' @param spatial_images_flip_y Named list/vector. Whether to flip spatial images vertically, names must match cerebro_data.
#' @param spatial_images_scale_x Named list/vector. Scaling factor for X axis of spatial images, names must match cerebro_data.
#' @param spatial_images_scale_y Named list/vector. Scaling factor for Y axis of spatial images, names must match cerebro_data.
#'
#' @return Invisibly returns the path to the result directory.
#'
#' @details
#' The function creates the following directory structure:
#' \itemize{
#'   \item result_dir/shiny/ - Contains Shiny UI and server source files
#'   \item result_dir/data/ - Contains the Cerebro .crb data file(s)
#'   \item result_dir/app.R - Main application file to launch the Shiny app
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with single dataset
#' createTraditionalShinyApp(
#'   cerebro_data = "result/cerebro_all.crb"
#' )
#'
#' # Multiple datasets with custom colors
#' createTraditionalShinyApp(
#'   cerebro_data = c(
#'     `All cells` = "result/cerebro_all.crb",
#'     `B cells` = "result/cerebro_Bc.crb"
#'   ),
#'   colors = list(
#'     `All cells` = list(condition = c(Ctrl = "black", MS = "blue")),
#'     `B cells` = list(cluster = c(B1 = "red", B2 = "green"))
#'   ),
#'   port = 8080,
#'   host = "0.0.0.0",  # Allow external access
#'   launch_browser = FALSE
#' )
#' }
#'
#' @export
createTraditionalShinyApp <- function(cerebro_data,
                                      result_dir = NULL,
                                      spatial_images = NULL,
                                      spatial_plot_rotation = NULL,
                                      spatial_images_flip_x = NULL,
                                      spatial_images_flip_y = NULL,
                                      spatial_images_scale_x = NULL,
                                      spatial_images_scale_y = NULL,
                                      max_request_size = 8000,
                                      port = 1337,
                                      host = "127.0.0.1",
                                      launch_browser = TRUE,
                                      quiet = FALSE,
                                      display_mode = "normal",
                                      colors = NULL,
                                      cerebro_options = list(exclude_trivial_metadata = TRUE),
                                      overwrite = TRUE,
                                      verbose = TRUE,
                                      enable_auth = FALSE,
                                      admin_user = "admin",
                                      admin_pass = "CHANGE_ME_ON_DEPLOYMENT",
                                      users = NULL,
                                      users_pass = NULL,
                                      auth_passphrase = "123123",
                                      crb_pick_smallest_file = TRUE,
                                      show_upload_ui = TRUE,
                                      point_size = list(
                                        overview_projection_point_size = NULL,
                                        trajectory_point_size = NULL,
                                        expression_projection_point_size = NULL,
                                        spatial_projection_point_size = NULL
                                      )) {

  # Validate input parameters ------------------------------------------------##
  if (!all(file.exists(cerebro_data))) {
    missing <- cerebro_data[!file.exists(cerebro_data)]
    stop("Cerebro data file(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!all(grepl("\\.(crb|rds|qs)$", cerebro_data, ignore.case = TRUE))) {
    warning("Some input files do not have .crb, .rds or .qs extension. Make sure they are valid Cerebro files.")
  }

  # Enforce named list/vector for cerebro_data
  if (is.null(names(cerebro_data)) || any(names(cerebro_data) == "")) {
    stop("cerebro_data must be a named list or vector, and every element must have a name.", call. = FALSE)
  }

  # Validate colors if provided
  if (!is.null(colors)) {
    if (is.null(names(colors)) || any(names(colors) == "")) {
      stop("colors must be a named list or vector.", call. = FALSE)
    }

    if (length(intersect(names(colors), names(cerebro_data))) == 0) {
      warning("Colors and cerebro_data do not match, random colors will be used.", call. = FALSE)
      colors <- NULL
    }
  }

  # Validate spatial_images if provided
  if (!is.null(spatial_images)) {
    if (is.null(names(spatial_images)) || any(names(spatial_images) == "")) {
      warning("spatial_images must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_images <- NULL
    } else if (length(intersect(names(spatial_images), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_images and cerebro_data. Ignoring.", call. = FALSE)
      spatial_images <- NULL
    }
  }

  # Validate spatial_plot_rotation if provided
  if (!is.null(spatial_plot_rotation)) {
    if (is.null(names(spatial_plot_rotation)) || any(names(spatial_plot_rotation) == "")) {
      warning("spatial_plot_rotation must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_plot_rotation <- NULL
    } else if (length(intersect(names(spatial_plot_rotation), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_plot_rotation and cerebro_data. Ignoring.", call. = FALSE)
      spatial_plot_rotation <- NULL
    }
  }

  # Validate spatial_images_flip_x if provided
  if (!is.null(spatial_images_flip_x)) {
    if (is.null(names(spatial_images_flip_x)) || any(names(spatial_images_flip_x) == "")) {
      warning("spatial_images_flip_x must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_images_flip_x <- NULL
    } else if (length(intersect(names(spatial_images_flip_x), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_images_flip_x and cerebro_data. Ignoring.", call. = FALSE)
      spatial_images_flip_x <- NULL
    }
  }

  # Validate spatial_images_flip_y if provided
  if (!is.null(spatial_images_flip_y)) {
    if (is.null(names(spatial_images_flip_y)) || any(names(spatial_images_flip_y) == "")) {
      warning("spatial_images_flip_y must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_images_flip_y <- NULL
    } else if (length(intersect(names(spatial_images_flip_y), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_images_flip_y and cerebro_data. Ignoring.", call. = FALSE)
      spatial_images_flip_y <- NULL
    }
  }

  # Validate spatial_images_scale_x if provided
  if (!is.null(spatial_images_scale_x)) {
    if (is.null(names(spatial_images_scale_x)) || any(names(spatial_images_scale_x) == "")) {
      warning("spatial_images_scale_x must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_images_scale_x <- NULL
    } else if (length(intersect(names(spatial_images_scale_x), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_images_scale_x and cerebro_data. Ignoring.", call. = FALSE)
      spatial_images_scale_x <- NULL
    }
  }

  # Validate spatial_images_scale_y if provided
  if (!is.null(spatial_images_scale_y)) {
    if (is.null(names(spatial_images_scale_y)) || any(names(spatial_images_scale_y) == "")) {
      warning("spatial_images_scale_y must be a named list or vector. Ignoring.", call. = FALSE)
      spatial_images_scale_y <- NULL
    } else if (length(intersect(names(spatial_images_scale_y), names(cerebro_data))) == 0) {
      warning("No matching names found between spatial_images_scale_y and cerebro_data. Ignoring.", call. = FALSE)
      spatial_images_scale_y <- NULL
    }
  }

  # Check if cerebroAppLite package is available
  if (!requireNamespace("cerebroAppLite", quietly = TRUE)) {
    stop("Package 'cerebroAppLite' is required but not installed.", call. = FALSE)
  }

  # Setup directories ---------------------------------------------------------##
  shiny_dir   <- file.path(result_dir, '')
  data_dir    <- file.path(result_dir, 'data')
  app_file    <- file.path(result_dir, 'app.R')

  # Prepare Shiny files -------------------------------------------------------##
  if (overwrite && dir.exists(result_dir)) {
    if (verbose) cat("Removing existing directory:", result_dir, "\n")
    unlink(result_dir, recursive = TRUE, force = TRUE)
  }

  if (verbose) cat("Creating directory structure...\n")
  dir.create(shiny_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy Shiny source files ---------------------------------------------------##
  shiny_source <- system.file('shiny', package = 'cerebroAppLite')
  if (!dir.exists(shiny_source)) {
    stop("Shiny source files not found in cerebroAppLite package.",
         call. = FALSE)
  }

  if (verbose) cat("Copying Shiny source files...\n")
  copy_result <- file.copy(shiny_source, shiny_dir, recursive = TRUE)
  if (!copy_result) {
    stop("Failed to copy Shiny source files.", call. = FALSE)
  }

  # Copy Cerebro data file(s) -------------------------------------------------##
  if (verbose) cat("Copying Cerebro data file(s)...\n")
  for (file in cerebro_data) {
    if (verbose) cat("  -", basename(file), "\n")
    copy_result <- file.copy(file, data_dir, recursive = TRUE)
    if (!copy_result) {
      stop("Failed to copy Cerebro data file: ", basename(file), call. = FALSE)
    }
  }

  # Copy spatial images (if any) ----------------------------------------------##
  if (!is.null(spatial_images)) {
    if (verbose) cat("Copying spatial images...\n")

    # Helper to process and copy images recursively
    process_spatial_images <- function(item) {
      if (is.list(item)) {
        return(lapply(item, process_spatial_images))
      } else if (is.character(item)) {
        new_paths <- character(length(item))
        for (i in seq_along(item)) {
          src_path <- item[i]
          if (file.exists(src_path)) {
            dest_name <- basename(src_path)
            dest_path <- file.path(data_dir, dest_name)
            file.copy(src_path, dest_path, overwrite = TRUE)
            new_paths[i] <- file.path("data", dest_name)
            if (verbose) cat("  -", dest_name, "\n")
          } else {
            warning("Spatial image not found: ", src_path, call. = FALSE)
            new_paths[i] <- src_path
          }
        }
        if (!is.null(names(item))) names(new_paths) <- names(item)
        return(new_paths)
      } else {
        return(item)
      }
    }

    spatial_images <- lapply(spatial_images, process_spatial_images)
  }

  # Copy extdata files (if any) -----------------------------------------------##
  if (verbose) cat("Copying extdata files...\n")
  extdata_source <- system.file('extdata', package = 'cerebroAppLite')
  if (!dir.exists(extdata_source)) {
    stop("extdata source files not found in cerebroAppLite package.", call. = FALSE)
  }
  copy_extdata <- file.copy(extdata_source, result_dir, recursive = TRUE)
  if (!copy_extdata) {
    stop("Failed to copy extdata files.", call. = FALSE)
  }

  # Setup authentication (if enabled) ----------------------------------------##
  auth_enabled <- FALSE
  if (enable_auth) {
    if (verbose) cat("Setting up authentication system...\n")

    # Check if shinymanager is available
    if (!requireNamespace("shinymanager", quietly = TRUE)) {
      stop("Package 'shinymanager' is required for authentication but not installed.",
           "Please install it with: install.packages('shinymanager')", call. = FALSE)
    }

    # Validate users and passwords
    if (!is.null(users) && is.null(users_pass)) {
      stop("'users_pass' must be provided when 'users' is specified.", call. = FALSE)
    }
    if (!is.null(users_pass) && is.null(users)) {
      stop("'users' must be provided when 'users_pass' is specified.", call. = FALSE)
    }
    if (!is.null(users) && !is.null(users_pass) && length(users) != length(users_pass)) {
      stop("'users' and 'users_pass' must have the same length.", call. = FALSE)
    }

    # Build users data frame
    users_list <- list(
      user = c(admin_user, users),
      password = c(admin_pass, users_pass),
      admin = c(TRUE, rep(FALSE, length(users)))
    )
    users_df <- do.call(data.frame, c(users_list, stringsAsFactors = FALSE))

    # Set up credentials database path
    sqlite_path <- file.path(result_dir, "credentials.sqlite")

    # Remove existing database if it exists
    if (file.exists(sqlite_path)) {
      if (verbose) cat("  Removing existing credentials database...\n")
      file.remove(sqlite_path)
    }

    # Create credentials database
    tryCatch({
      shinymanager::create_db(
        credentials_data = users_df,
        sqlite_path = sqlite_path,
        passphrase = auth_passphrase
      )
      auth_enabled <- TRUE
      if (verbose) {
        info <- file.info(sqlite_path)
        cat("  Created credentials database:", normalizePath(sqlite_path), "\n")
        cat("  Database size:", info$size, "bytes\n")
        cat("  Admin user:", admin_user, "\n")
        if (!is.null(users)) {
          cat("  Additional users:", paste(users, collapse = ", "), "\n")
        }
      }
    }, error = function(e) {
      stop("Failed to create credentials database: ", conditionMessage(e), call. = FALSE)
    })
  }

  # Create app.R file ---------------------------------------------------------##
  if (verbose) cat("Generating app.R file...\n")

  # 1. Build configuration list and save to RDS
  # Generate crb_file_to_load configuration (named vector)
  crb_files <- setNames(
    paste0("data/", basename(cerebro_data)),
    names(cerebro_data)
  )

  # Populate cerebro_options
  cerebro_options[["mode"]] <- "open"
  cerebro_options[["crb_file_to_load"]] <- crb_files
  cerebro_options[["cerebro_root"]] <- "."

  if (!is.null(crb_pick_smallest_file)) cerebro_options[["crb_pick_smallest_file"]] <- crb_pick_smallest_file
  if (!is.null(show_upload_ui)) cerebro_options[["show_upload_ui"]] <- show_upload_ui
  if (!is.null(point_size)) cerebro_options[["point_size"]] <- point_size

  # Add complex objects directly to the list
  if (!is.null(colors)) cerebro_options[["colors"]] <- colors
  if (!is.null(spatial_images)) cerebro_options[["spatial_images"]] <- spatial_images
  if (!is.null(spatial_plot_rotation)) cerebro_options[["spatial_plot_rotation"]] <- spatial_plot_rotation
  if (!is.null(spatial_images_flip_x)) cerebro_options[["spatial_images_flip_x"]] <- spatial_images_flip_x
  if (!is.null(spatial_images_flip_y)) cerebro_options[["spatial_images_flip_y"]] <- spatial_images_flip_y
  if (!is.null(spatial_images_scale_x)) cerebro_options[["spatial_images_scale_x"]] <- spatial_images_scale_x
  if (!is.null(spatial_images_scale_y)) cerebro_options[["spatial_images_scale_y"]] <- spatial_images_scale_y

  # Save configuration to RDS
  saveRDS(cerebro_options, file.path(result_dir, "cerebro_config.rds"))

  # Generate authentication code if enabled
  auth_code <- ""
  auth_wrapper_code <- ""
  app_ui_code <- "ui"
  app_server_code <- "server"

  if (auth_enabled) {
    auth_code <- glue::glue('
# Authentication setup
library(shinymanager)

credentials_path <- file.path(cerebro_root, "credentials.sqlite")
auth_passphrase <- "{auth_passphrase}"

# Check if credentials database exists
if (!file.exists(credentials_path)) {{
  stop("Credentials database not found: ", credentials_path)
}}

# Initialize credentials check
check_credentials <- shinymanager::check_credentials(
  credentials_path,
  passphrase = auth_passphrase
)
')
    auth_wrapper_code <- glue::glue('
# Wrap UI with secure_app
secure_ui <- shinymanager::secure_app(ui)
')
    app_ui_code <- "secure_ui"
    app_server_code <- 'function(input, output, session) {
  res_auth <- shinymanager::secure_server(check_credentials = check_credentials)
  server(input, output, session)
}'
  }

  # Generate app.R content
  app_content <- glue::glue('
#==============================================================================
# Cerebro Shiny App Builder - RStudio 自动生成的应用程序
#==============================================================================

library(dplyr)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# 定义结果保存目录
cerebro_root <- "."

## 加载配置
if (file.exists("cerebro_config.rds")) {{
  Cerebro.options <<- readRDS("cerebro_config.rds")
}} else {{
  stop("cerebro_config.rds not found!")
}}

# 兼容旧代码：如果有 colors 选项，设置为全局变量
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

{auth_code}
## 加载服务器和界面函数
source(file.path(cerebro_root, "shiny/shiny_UI.R"))
source(file.path(cerebro_root, "shiny/shiny_server.R"))

## Expose data directory for spatial images
shiny::addResourcePath("data", file.path(cerebro_root, "data"))

## Start Shiny App
{auth_wrapper_code}
shiny::shinyApp(
  ui = {app_ui_code},
  server = {app_server_code},
  options = shiny_options
)
',
    .trim = FALSE
  )

  # 应用智能缩进处理
  processed_content <- dedent(app_content)
  writeLines(processed_content, app_file)

  # Summary -------------------------------------------------------------------##
  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("Shiny app successfully created!\n")
    cat("========================================\n")
    cat("App directory:", result_dir, "\n")
    cat("Data file(s):\n")
    for (i in seq_along(cerebro_data)) {
      if (!is.null(names(cerebro_data)[i]) && names(cerebro_data)[i] != "") {
        cat("  -", names(cerebro_data)[i], ":", basename(cerebro_data[i]), "\n")
      } else {
        cat("  -", basename(cerebro_data[i]), "\n")
      }
    }

    cat("Port:", port, "\n")
    cat("Host:", host, "\n")
    cat("Launch browser:", launch_browser, "\n")
    if (auth_enabled) {
      cat("Authentication: ENABLED\n")
      cat("  Admin user:", admin_user, "\n")
      if (!is.null(users)) {
        cat("  Additional users:", paste(users, collapse = ", "), "\n")
      }
    } else {
      cat("Authentication: DISABLED\n")
    }
    cat("\nTo launch the app, run:\n")
    cat("  setwd('", result_dir, "')\n", sep = "")
    cat("  shiny::runApp('app.R')\n")
    cat("========================================\n")
  }

  invisible(result_dir)
}
