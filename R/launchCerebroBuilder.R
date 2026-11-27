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
#' Each run gets a random access token. The page opens with it in its address
#' (\code{?token=...}), which is also printed in the R console; without it the
#' page is refused and no session starts, so neither a web page you visit nor
#' another user of a shared machine can drive the Builder.
#'
#' @section Examples-only mode:
#' With \code{examples_only = TRUE} the Builder is a demonstration: visitors
#' load the bundled examples, configure and build them, and download the result
#' as a \code{.zip} file. It offers no upload, no server folder and no Project,
#' starts no App on the server, and refuses such requests on the server as
#' well; Shiny accepts no file larger than one byte. Each session builds into a
#' private folder below \code{tempdir()} that is removed when the session ends.
#'
#' A server that runs the Builder's \code{app.R} directly (for example Shiny
#' Server) sets the option before the app starts:
#' \preformatted{
#' options(CerebroNexus.builder.examples_only = TRUE)
#' shiny::shinyAppDir(system.file("builder", package = "CerebroNexus"))
#' }
#' Run this way the Builder has no access token, so anyone who can reach it can
#' use it. Each session starts background R processes; limit request sizes in
#' the server or proxy in front of a public demonstration (Shiny checks the
#' size a page declares, not the bytes it sends).
#'
#' Two options, read when the app starts, keep a demonstration server from
#' running out of memory: \code{CerebroNexus.builder.examples_only_max_sessions}
#' (default 4) is the number of sessions one R process serves at a time, and a
#' visitor beyond it is told the demonstration is busy;
#' \code{CerebroNexus.builder.examples_only_idle_minutes} (default 15, at least
#' 1) closes a session that has seen no click, key press or scroll for that
#' long while no build or import runs. \code{Inf} turns either limit off. A
#' server that runs several R processes for the app allows that many sessions
#' in each.
#'
#' @param host Address to bind; defaults to \code{127.0.0.1}, i.e. this machine
#'   only. Any other address (for example \code{0.0.0.0}) gives a warning,
#'   because other machines could then reach the Builder.
#' @param port Port to bind; defaults to a free one chosen by Shiny.
#' @param launch_browser Open a browser at the address with the access
#'   token; defaults to \code{TRUE}. The address is printed either way.
#' @param max_file_size Optional maximum size in MB for each local upload;
#'   defaults to \code{Inf}, so the local Builder does not impose an upload-size
#'   limit. The corresponding Shiny process option is restored when the Builder
#'   stops. Ignored when \code{examples_only = TRUE}.
#' @param examples_only Run the Builder as a demonstration that works with the
#'   bundled examples only; defaults to \code{FALSE}. See the section
#'   "Examples-only mode".
#'
#' @return Does not return; runs until the app is stopped.
#'
#' @examples
#' if (interactive()) {
#'   launchCerebroBuilder()
#'   ## A demonstration with the bundled examples only:
#'   launchCerebroBuilder(examples_only = TRUE)
#' }
#'
#' @export
#'
launchCerebroBuilder <- function(
  host = "127.0.0.1",
  port = NULL,
  launch_browser = TRUE,
  max_file_size = Inf,
  examples_only = FALSE
) {
  if (
    !is.logical(examples_only) ||
      length(examples_only) != 1L ||
      is.na(examples_only)
  ) {
    stop("'examples_only' must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    length(max_file_size) != 1L ||
      !is.numeric(max_file_size) ||
      is.na(max_file_size) ||
      max_file_size <= 0
  ) {
    stop("'max_file_size' must be one positive number or Inf.", call. = FALSE)
  }
  # These dependencies are used by the isolated inst/builder runtime. Load
  # them only when Builder launches, without adding to Viewer startup work.
  # Seurat, callr, digest, processx and ps are Suggests: only the Builder needs
  # them. Seurat is checked without loading it; one message names them all.
  available <- c(
    Seurat = nzchar(system.file(package = "Seurat")),
    callr = requireNamespace("callr", quietly = TRUE),
    digest = requireNamespace("digest", quietly = TRUE),
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

  if (!examples_only && !.builderHostIsLoopback(host)) {
    warning(
      "The Builder is listening on '",
      host,
      "', so other machines may reach it. Its access token travels ",
      "unencrypted, and whoever has it can read files and run the code ",
      "inside `.rds` files; use host = \"127.0.0.1\" unless this network ",
      "is trusted.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # The page and every session need this token (inst/builder/access.R): Shiny
  # checks neither Origin nor Host, so without it a web page the user visits,
  # or another user of a shared machine, could drive the Builder.
  token <- .builderAccessToken()
  old_token <- getOption("CerebroNexus.builder.access_token")
  on.exit(
    options(CerebroNexus.builder.access_token = old_token),
    add = TRUE
  )
  options(CerebroNexus.builder.access_token = token)
  old_examples_only <- getOption("CerebroNexus.builder.examples_only")
  on.exit(
    options(CerebroNexus.builder.examples_only = old_examples_only),
    add = TRUE
  )
  options(CerebroNexus.builder.examples_only = examples_only)

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = .builderOpenWithToken(token, launch_browser)
  )
}

## A fresh random access token for one Builder run.
.builderAccessToken <- function(.random_bytes = .builderRandomBytes) {
  paste(sprintf("%02x", as.integer(.random_bytes(32L))), collapse = "")
}

## Shiny calls this with the app address once it listens: print the address
## with the token, and open it if asked.
.builderOpenWithToken <- function(
  token,
  launch_browser,
  .browse = utils::browseURL
) {
  force(token)
  force(launch_browser)
  function(url) {
    url <- paste0(sub("/*$", "/", url), "?token=", token)
    message("Open the Builder at ", url)
    if (isTRUE(launch_browser)) {
      .browse(url)
    }
    invisible(url)
  }
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
