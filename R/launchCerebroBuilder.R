#' @title
#' Build a Cerebro data set from a Seurat object, by pointing and clicking.
#'
#' @description
#' Opens a local page that reads a trusted Seurat `.rds`, `.qs2` or `.qs` file,
#' works out what it contains,
#' and lets you choose what the resulting data set should show: which metadata
#' columns are the grouping variables, which dimensional reductions to carry,
#' which assay and layer the expression comes from. It then writes the `.crb`
#' and, if asked, packages it into a standalone Shiny app.
#'
#' It does the same work as calling \code{\link{exportFromSeurat}} yourself. The
#' difference is that it reads the object first and only offers what is actually
#' there -- the layers that exist, the columns that can serve as groups, the
#' reductions that will survive the export -- so the choices that quietly go
#' wrong in a hand-written call are visible before anything is written.
#'
#' @section What this is not:
#' The object is read by a local background R worker, so run it on your own machine or in
#' your own RStudio Server session. It is not meant to be deployed for other
#' people to upload data to: it reads whatever path it is given, and reading an
#' `.rds` runs whatever code is inside it.
#'
#' @param host Address to bind; defaults to \code{127.0.0.1}, i.e. this machine
#'   only. Any other address (for example \code{0.0.0.0}) gives a warning,
#'   because other machines could then reach the Builder.
#' @param port Port to bind; defaults to a free one chosen by Shiny.
#' @param launch_browser Open a browser; defaults to \code{TRUE}.
#' @param max_file_size Optional maximum size in MB for each local upload;
#'   defaults to \code{Inf}, so the local Builder does not impose an upload-size
#'   limit. The corresponding Shiny process option is restored when the Builder
#'   stops.
#'
#' @return Does not return; runs until the app is stopped.
#'
#' @examples
#' if (interactive()) {
#'   launchCerebroBuilder()
#' }
#'
#' @export
#'
launchCerebroBuilder <- function(
  host = "127.0.0.1",
  port = NULL,
  launch_browser = TRUE,
  max_file_size = Inf
) {
  if (
    length(max_file_size) != 1L ||
      !is.numeric(max_file_size) ||
      is.na(max_file_size) ||
      max_file_size <= 0
  ) {
    stop("'max_file_size' must be one positive number or Inf.", call. = FALSE)
  }
  if (!nzchar(system.file(package = "Seurat"))) {
    stop(
      "Building a data set from a Seurat object needs the Seurat package. ",
      "Install it and try again.",
      call. = FALSE
    )
  }

  # These dependencies are used by the isolated inst/builder runtime. Load
  # them only when Builder launches, without adding to Viewer startup work.
  # callr, processx and ps are Suggests: only the Builder needs them.
  available <- c(
    callr = requireNamespace("callr", quietly = TRUE),
    digest = requireNamespace("digest", quietly = TRUE),
    htmltools = requireNamespace("htmltools", quietly = TRUE),
    processx = requireNamespace("processx", quietly = TRUE),
    ps = requireNamespace("ps", quietly = TRUE)
  )
  if (!all(available)) {
    missing <- names(available)[!available]
    stop(
      "The Builder needs these packages: ",
      paste(missing, collapse = ", "),
      ". Install them with install.packages(",
      paste(deparse(missing), collapse = ""),
      ") and try again.",
      call. = FALSE
    )
  }

  app_dir <- system.file("builder", package = "CerebroNexus")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop(
      "Could not find the builder app inside the installed package.",
      call. = FALSE
    )
  }

  old_request_size <- getOption("shiny.maxRequestSize")
  on.exit(options(shiny.maxRequestSize = old_request_size), add = TRUE)
  options(shiny.maxRequestSize = max_file_size * 1024^2)
  shiny_was_attached <- "package:shiny" %in% search()
  on.exit(
    {
      if (!shiny_was_attached && "package:shiny" %in% search()) {
        try(
          detach("package:shiny", unload = FALSE, character.only = TRUE),
          silent = TRUE
        )
      }
    },
    add = TRUE
  )

  restore_locale <- .builderUseUtf8Locale()
  on.exit(restore_locale(), add = TRUE)

  if (!.builderHostIsLoopback(host)) {
    warning(
      "The Builder is listening on '",
      host,
      "', so other machines may reach it. It reads any path it is given ",
      "and runs the code inside `.rds` files; use host = \"127.0.0.1\" ",
      "unless this network is trusted.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = launch_browser
  )
}

## TRUE for addresses that only this machine can reach.
.builderHostIsLoopback <- function(host) {
  is.character(host) &&
    length(host) == 1L &&
    !is.na(host) &&
    grepl("^(localhost|127([.][0-9]{1,3}){3}|::1|[[]::1[]])$", tolower(host))
}

## In a C/POSIX (ASCII) locale R cannot hold "ü" in its native encoding: a
## picked or uploaded file "Zürich.rds" is not found, basename() fails and
## names reach the page as "Z<U+00FC>rich". The Builder, and the workers it
## starts, then run with a UTF-8 character type; the caller's settings come
## back when it stops. Returns the function that restores them.
.builderUseUtf8Locale <- function(
  candidates = c("C.UTF-8", "en_US.UTF-8", "UTF-8")
) {
  info <- l10n_info()
  codeset <- toupper(as.character(info$codeset %||% NA_character_))
  if (
    isTRUE(info[["UTF-8"]]) ||
      !codeset %in% c("ANSI_X3.4-1968", "ASCII", "US-ASCII", "646")
  ) {
    return(function() invisible(FALSE))
  }
  old_ctype <- Sys.getlocale("LC_CTYPE")
  old_env <- Sys.getenv(c("LC_ALL", "LC_CTYPE"), unset = NA)
  for (candidate in candidates) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", candidate)))) {
      # Workers read the locale from the environment; LC_ALL wins there.
      if (!is.na(old_env[["LC_ALL"]]) && nzchar(old_env[["LC_ALL"]])) {
        Sys.setenv(LC_ALL = candidate)
      } else {
        Sys.setenv(LC_CTYPE = candidate)
      }
      return(function() {
        suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype))
        for (name in names(old_env)) {
          if (is.na(old_env[[name]])) {
            Sys.unsetenv(name)
          } else {
            do.call(Sys.setenv, stats::setNames(list(old_env[[name]]), name))
          }
        }
        invisible(TRUE)
      })
    }
  }
  warning(
    "R runs in a C locale and no UTF-8 locale is available, so file and ",
    "dataset names with non-ASCII characters may not work in the Builder.",
    call. = FALSE,
    immediate. = TRUE
  )
  function() invisible(FALSE)
}
