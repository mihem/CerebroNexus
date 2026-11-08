builder_browser_diagnostic_root <- function() {
  configured <- trimws(Sys.getenv("CEREBRO_TEST_ARTIFACT_DIR", unset = ""))
  package_root <- tryCatch(
    normalizePath(
      testthat::test_path("..", ".."),
      winslash = "/",
      mustWork = TRUE
    ),
    error = function(...) getwd()
  )
  candidates <- if (nzchar(configured)) {
    configured <- path.expand(configured)
    if (fs::is_absolute_path(configured)) {
      configured
    } else {
      file.path(package_root, configured)
    }
  } else {
    c(file.path(package_root, "tests", "testthat", "_artifacts"), tempdir())
  }

  for (candidate in candidates) {
    created <- tryCatch(
      {
        dir.create(candidate, recursive = TRUE, showWarnings = FALSE)
        dir.exists(candidate)
      },
      error = function(...) FALSE
    )
    if (isTRUE(created)) {
      return(candidate)
    }
  }
  tempdir()
}

builder_capture_browser_diagnostics <- function(app, label) {
  tryCatch(
    {
      safe_label <- as.character(label)[1L]
      if (is.na(safe_label)) {
        safe_label <- "browser-error"
      }
      safe_label <- gsub("[^A-Za-z0-9._-]+", "-", safe_label)
      safe_label <- gsub("(^-+|-+$)", "", safe_label)
      if (!nzchar(safe_label)) {
        safe_label <- "browser-error"
      }
      artifact_dir <- tempfile(
        pattern = paste0(safe_label, "-"),
        tmpdir = builder_browser_diagnostic_root()
      )
      dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(artifact_dir)) {
        return(invisible(NULL))
      }

      capture_item <- function(name, code) {
        tryCatch(
          {
            value <- force(code)
            output <- capture.output(dput(value))
            writeLines(output, file.path(artifact_dir, paste0(name, ".txt")))
          },
          error = function(error) {
            try(
              writeLines(
                conditionMessage(error),
                file.path(artifact_dir, paste0(name, ".error.txt"))
              ),
              silent = TRUE
            )
          }
        )
        invisible(NULL)
      }

      truncate_text <- function(value, max_characters) {
        value <- as.character(value)
        lengths <- nchar(value, type = "chars", allowNA = TRUE, keepNA = TRUE)
        truncated <- !is.na(lengths) & lengths > max_characters
        value[truncated] <- paste0(
          substr(value[truncated], 1L, max_characters),
          sprintf("\n...[truncated after %d characters]", max_characters)
        )
        value
      }

      capture_item("current-url", app$get_js("window.location.href"))
      capture_item(
        "document-html",
        app$get_js(paste0(
          "(() => {",
          "const maximumCharacters = 2 * 1024 * 1024;",
          "const html = document.documentElement.outerHTML;",
          "return {",
          "originalCharacters: html.length,",
          "truncated: html.length > maximumCharacters,",
          "maximumCharacters: maximumCharacters,",
          "html: html.slice(0, maximumCharacters)",
          "};",
          "})()"
        ))
      )
      capture_item("appdriver-logs", {
        logs <- app$get_logs()
        original_rows <- if (is.data.frame(logs)) nrow(logs) else length(logs)
        max_rows <- 2000L
        if (is.data.frame(logs) && nrow(logs) > max_rows) {
          logs <- utils::tail(logs, max_rows)
        } else if (!is.data.frame(logs) && length(logs) > max_rows) {
          logs <- utils::tail(logs, max_rows)
        }
        if (is.data.frame(logs)) {
          character_columns <- vapply(logs, is.character, logical(1))
          logs[character_columns] <- lapply(
            logs[character_columns],
            truncate_text,
            max_characters = 4096L
          )
        } else if (is.character(logs)) {
          logs <- truncate_text(logs, 4096L)
        }
        list(
          original_rows = original_rows,
          retained_rows = if (is.data.frame(logs)) nrow(logs) else length(logs),
          retained = "last 2000 rows/items; character values capped at 4096 characters",
          logs = logs
        )
      })
      capture_item(
        "browser-state",
        app$get_js(paste0(
          "(() => {",
          "const shiny = window.Shiny;",
          "const shinyapp = shiny && shiny.shinyapp;",
          "const socket = shinyapp && shinyapp.$socket;",
          "const active = Array.from(document.querySelectorAll('[aria-current=true]'));",
          "const errors = Array.from(document.querySelectorAll(",
          "'.shiny-output-error, .shiny-notification-error, .alert-danger, ",
          ".validation-error, [role=alert]')).map(node => ",
          "(node.innerText || node.textContent || '').trim()).filter(Boolean);",
          "return {",
          "readyState: document.readyState,",
          "shinyExists: Boolean(shiny),",
          "shinyappExists: Boolean(shinyapp),",
          "shinyConnected: Boolean(socket && socket.readyState === 1),",
          "shinySocketReadyState: socket ? socket.readyState : null,",
          "shinyPublicState: shinyapp ? {",
          "inputNames: Object.keys(shinyapp.$inputValues || {}).sort(),",
          "outputNames: Object.keys(shinyapp.$values || {}).sort(),",
          "conditionalsPending: Boolean(shinyapp.$updateConditionalsPending)",
          "} : null,",
          "allContentButtonExists: Boolean(document.querySelector(",
          "'.example-btn[data-ex=complete_viewer_data]')),",
          "datasetPickerCount: document.querySelectorAll('.ds-pick').length,",
          "ariaCurrentTrueCount: active.length,",
          "ariaCurrentTrueHtml: active.map(node => node.outerHTML.slice(0, 4000)),",
          "errorText: errors.join('\\n').slice(0, 12000),",
          "pageText: (document.body ? document.body.innerText : '').slice(0, 12000)",
          "};",
          "})()"
        ))
      )

      log_inventory <- new.env(parent = emptyenv())
      log_inventory$roots <- character()
      log_inventory$paths <- character()
      log_inventory$roots_truncated <- FALSE
      capture_item("shinytest2-paths", {
        temporary_root <- normalizePath(
          dirname(tempdir()),
          winslash = "/",
          mustWork = FALSE
        )
        roots <- unique(Sys.glob(file.path(temporary_root, "shinytest2*")))
        log_inventory$roots <- utils::head(roots, 20L)
        log_inventory$roots_truncated <-
          length(roots) > length(log_inventory$roots)
        list(
          roots = log_inventory$roots,
          truncated = log_inventory$roots_truncated,
          maximum_roots = 20L
        )
      })
      capture_item("server-worker-log-paths", {
        maximum_entries <- 2000L
        maximum_depth <- 4L
        maximum_candidates <- 50L
        queue <- lapply(log_inventory$roots, function(root) {
          list(path = root, depth = 0L)
        })
        queue_index <- 1L
        visited_entries <- 0L
        paths <- character()
        seen_directories <- character()
        scan_errors <- character()
        entry_limit_reached <- FALSE
        depth_limit_reached <- FALSE
        candidate_limit_reached <- FALSE

        while (
          queue_index <= length(queue) &&
            visited_entries < maximum_entries &&
            length(paths) < maximum_candidates
        ) {
          current <- queue[[queue_index]]
          queue_index <- queue_index + 1L
          current_path <- current$path
          current_depth <- current$depth

          if (!dir.exists(current_path)) {
            if (
              file.exists(current_path) &&
                grepl(
                  "(server|worker).*\\.(log|out|err|txt)$",
                  basename(current_path),
                  ignore.case = TRUE
                )
            ) {
              paths <- c(paths, current_path)
            }
            next
          }

          directory_key <- normalizePath(
            current_path,
            winslash = "/",
            mustWork = FALSE
          )
          if (directory_key %in% seen_directories) {
            next
          }
          seen_directories <- c(seen_directories, directory_key)
          if (current_depth >= maximum_depth) {
            depth_limit_reached <- TRUE
            next
          }

          entry_result <- tryCatch(
            list(
              entries = list.files(
                current_path,
                all.files = TRUE,
                full.names = TRUE,
                no.. = TRUE,
                recursive = FALSE
              ),
              error = NULL
            ),
            error = function(error) {
              list(entries = character(), error = conditionMessage(error))
            }
          )
          if (!is.null(entry_result$error)) {
            scan_errors[[current_path]] <- entry_result$error
          }
          entries <- entry_result$entries
          remaining_entries <- maximum_entries - visited_entries
          if (length(entries) > remaining_entries) {
            entries <- utils::head(entries, remaining_entries)
            entry_limit_reached <- TRUE
          }

          for (entry in entries) {
            visited_entries <- visited_entries + 1L
            if (dir.exists(entry)) {
              queue[[length(queue) + 1L]] <- list(
                path = entry,
                depth = current_depth + 1L
              )
            } else if (
              file.exists(entry) &&
                grepl(
                  "(server|worker).*\\.(log|out|err|txt)$",
                  basename(entry),
                  ignore.case = TRUE
                )
            ) {
              paths <- c(paths, entry)
              if (length(paths) >= maximum_candidates) {
                candidate_limit_reached <- TRUE
                break
              }
            }
          }
        }
        if (
          visited_entries >= maximum_entries && queue_index <= length(queue)
        ) {
          entry_limit_reached <- TRUE
        }
        log_inventory$paths <- unique(paths)
        list(
          paths = log_inventory$paths,
          roots_considered = length(log_inventory$roots),
          visited_entries = visited_entries,
          maximum_entries = maximum_entries,
          maximum_depth = maximum_depth,
          maximum_candidates = maximum_candidates,
          roots_truncated = log_inventory$roots_truncated,
          entry_limit_reached = entry_limit_reached,
          depth_limit_reached = depth_limit_reached,
          candidate_limit_reached = candidate_limit_reached,
          truncated = log_inventory$roots_truncated ||
            entry_limit_reached ||
            depth_limit_reached ||
            candidate_limit_reached,
          scan_errors = scan_errors
        )
      })
      capture_item("server-worker-log-excerpts", {
        paths <- log_inventory$paths
        stats <- file.info(paths)
        per_file_limit <- 1L * 1024L * 1024L
        total_limit <- 10L * 1024L * 1024L
        bytes_to_read <- pmin(stats$size, per_file_limit)
        eligible <- !is.na(bytes_to_read) &
          bytes_to_read >= 0 &
          cumsum(replace(bytes_to_read, is.na(bytes_to_read), 0)) <= total_limit
        paths <- paths[eligible]
        stats <- stats[eligible, , drop = FALSE]
        bytes_to_read <- bytes_to_read[eligible]
        excerpts <- setNames(
          lapply(seq_along(paths), function(index) {
            tryCatch(
              {
                path <- paths[[index]]
                file_size <- stats$size[[index]]
                read_size <- as.integer(bytes_to_read[[index]])
                connection <- file(path, open = "rb")
                on.exit(close(connection), add = TRUE)
                seek(
                  connection,
                  where = max(0, file_size - read_size),
                  origin = "start"
                )
                contents <- rawToChar(readBin(connection, "raw", n = read_size))
                lines <- strsplit(contents, "\n", fixed = TRUE)[[1L]]
                list(
                  original_bytes = file_size,
                  bytes_read = read_size,
                  truncated = file_size > read_size,
                  tail = utils::tail(lines, 500L)
                )
              },
              error = function(error) {
                list(error = conditionMessage(error))
              }
            )
          }),
          paths
        )
        list(
          maximum_candidates = 50L,
          per_file_byte_limit = per_file_limit,
          total_byte_limit = total_limit,
          total_bytes_read = sum(bytes_to_read),
          excerpts = excerpts
        )
      })

      capture_item("r-version", R.version.string)
      capture_item("seurat-version", {
        if (!requireNamespace("Seurat", quietly = TRUE)) {
          "not installed"
        } else {
          as.character(utils::packageVersion("Seurat"))
        }
      })
      capture_item("seuratobject-version", {
        if (!requireNamespace("SeuratObject", quietly = TRUE)) {
          "not installed"
        } else {
          as.character(utils::packageVersion("SeuratObject"))
        }
      })
      capture_item("all-content-fixture", {
        candidates <- c(
          tryCatch(
            testthat::test_path(
              "..",
              "..",
              "inst",
              "builder",
              "fixtures",
              "complete_viewer",
              "complete_viewer_data.rds"
            ),
            error = function(...) NA_character_
          ),
          file.path(
            getwd(),
            "inst",
            "builder",
            "fixtures",
            "complete_viewer",
            "complete_viewer_data.rds"
          ),
          file.path(
            "inst",
            "builder",
            "fixtures",
            "complete_viewer",
            "complete_viewer_data.rds"
          )
        )
        fixture <- candidates[file.exists(candidates)][1L]
        if (is.na(fixture)) {
          stop(
            paste0(
              "Committed inst/builder/fixtures/",
              "complete_viewer/",
              "complete_viewer_data.rds was not found."
            )
          )
        }
        fixture <- normalizePath(fixture, winslash = "/", mustWork = TRUE)
        warnings <- character()
        read_error <- NULL
        object <- tryCatch(
          withCallingHandlers(
            readRDS(fixture),
            warning = function(warning) {
              warnings <<- c(warnings, conditionMessage(warning))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(error) {
            read_error <<- conditionMessage(error)
            NULL
          }
        )
        safe_value <- function(code) {
          tryCatch(force(code), error = function(error) {
            paste("ERROR:", conditionMessage(error))
          })
        }
        if (is.null(object)) {
          list(path = fixture, warnings = warnings, error = read_error)
        } else {
          image_names <- safe_value(names(object@images))
          image_info <- if (
            is.character(image_names) &&
              length(image_names) == 1L &&
              startsWith(image_names, "ERROR:")
          ) {
            image_names
          } else {
            setNames(
              lapply(image_names, function(name) {
                image <- object@images[[name]]
                slots <- safe_value(methods::slotNames(image))
                list(
                  class = class(image),
                  dim = safe_value(dim(image)),
                  slots = slots,
                  is_fov = inherits(image, "FOV"),
                  boundaries = safe_value(
                    if ("boundaries" %in% slots) {
                      names(image@boundaries)
                    } else {
                      character()
                    }
                  ),
                  molecules = safe_value(
                    if ("molecules" %in% slots) {
                      names(image@molecules)
                    } else {
                      character()
                    }
                  )
                )
              }),
              image_names
            )
          }
          list(
            path = fixture,
            warnings = warnings,
            error = read_error,
            class = class(object),
            dim = safe_value(dim(object)),
            assays = safe_value(names(object@assays)),
            reductions = safe_value(names(object@reductions)),
            images = image_info,
            fov_names = if (is.list(image_info)) {
              names(image_info)[vapply(
                image_info,
                function(info) {
                  isTRUE(info$is_fov)
                },
                logical(1)
              )]
            } else {
              image_info
            }
          )
        }
      })

      invisible(artifact_dir)
    },
    error = function(...) invisible(NULL)
  )
}

builder_with_browser_diagnostics <- function(app, label, code) {
  tryCatch(
    force(code),
    error = function(error) {
      builder_capture_browser_diagnostics(app, label)
      stop(error)
    }
  )
}
