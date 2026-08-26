.findSerializedPathLeaks <- function(
  object,
  patterns = c(
    "/Users/",
    "/private/tmp/",
    "/tmp/",
    "cerebroAppLite",
    "_wt_colleague_spatial_builder",
    "_wt_builder_auth",
    "_wt_coord_views",
    "cerebro-builder-lib"
  )
) {
  findings <- character()
  seen_environments <- new.env(parent = emptyenv())
  visited <- 0L
  max_visited <- 100000L

  has_prohibited_path <- function(value) {
    vapply(
      value,
      function(item) {
        any(vapply(patterns, grepl, logical(1), x = item, fixed = TRUE))
      },
      logical(1)
    )
  }

  add_characters <- function(value, path) {
    hits <- has_prohibited_path(value)
    if (any(hits)) {
      selected <- value[hits]
      names(selected) <- paste0(path, "[", which(hits), "]")
      findings <<- c(findings, selected)
    }
  }

  walk_attributes <- function(
    value,
    path,
    walk,
    inspect_function_environment = TRUE
  ) {
    attrs <- attributes(value)
    if (is.null(attrs)) {
      return(invisible(NULL))
    }
    for (name in names(attrs)) {
      walk(
        attrs[[name]],
        paste0(path, "$attributes$", name),
        inspect_function_environment
      )
    }
    invisible(NULL)
  }

  walk <- function(
    value,
    path = "root",
    inspect_function_environment = TRUE,
    inspect_environment_contents = TRUE
  ) {
    visited <<- visited + 1L
    if (visited > max_visited) {
      stop("Serialized path audit exceeded its traversal limit.", call. = FALSE)
    }

    if (is.character(value)) {
      add_characters(value, path)
      walk_attributes(value, path, walk, inspect_function_environment)
      return(invisible(NULL))
    }
    if (is.null(value)) {
      return(invisible(NULL))
    }
    if (is.function(value)) {
      walk_attributes(value, path, walk, inspect_function_environment)
      if (isTRUE(inspect_function_environment)) {
        walk(environment(value), paste0(path, "$environment"))
      }
      return(invisible(NULL))
    }
    if (is.environment(value)) {
      if (identical(value, emptyenv())) {
        return(invisible(NULL))
      }
      key <- format(value)
      if (exists(key, envir = seen_environments, inherits = FALSE)) {
        return(invisible(NULL))
      }
      assign(key, TRUE, envir = seen_environments)

      environment_name <- environmentName(value)
      inspect_bindings <- !identical(value, globalenv()) &&
        !identical(value, baseenv()) &&
        !startsWith(environment_name, "package:") &&
        !startsWith(environment_name, "namespace:") &&
        !startsWith(environment_name, "imports:")
      if (inspect_bindings) {
        for (name in ls(value, all.names = TRUE)) {
          if (bindingIsActive(name, value)) {
            next
          }
          binding <- tryCatch(
            get(name, envir = value, inherits = FALSE),
            error = function(error) NULL
          )
          if (is.null(binding)) {
            next
          }
          binding_path <- paste0(path, "$", name)
          if (isTRUE(inspect_environment_contents)) {
            walk(
              binding,
              binding_path,
              inspect_function_environment = FALSE
            )
          } else if (is.character(binding)) {
            add_characters(binding, binding_path)
          }
        }
      }
      parent <- parent.env(value)
      parent_name <- environmentName(parent)
      inspect_parent <- inspect_bindings &&
        !identical(parent, globalenv()) &&
        !identical(parent, baseenv()) &&
        !identical(parent, emptyenv()) &&
        !startsWith(parent_name, "package:") &&
        !startsWith(parent_name, "namespace:") &&
        !startsWith(parent_name, "imports:")
      if (inspect_parent) {
        walk(
          parent,
          paste0(path, "$parent"),
          inspect_environment_contents = FALSE
        )
      }
      return(invisible(NULL))
    }
    if (isS4(value)) {
      for (name in methods::slotNames(value)) {
        slot_value <- tryCatch(
          methods::slot(value, name),
          error = function(error) NULL
        )
        walk(
          slot_value,
          paste0(path, "@", name),
          inspect_function_environment
        )
      }
      walk_attributes(value, path, walk, inspect_function_environment)
      return(invisible(NULL))
    }
    if (is.list(value) || is.pairlist(value)) {
      value_names <- names(value)
      for (index in seq_along(value)) {
        label <- if (
          length(value_names) >= index &&
            !is.na(value_names[[index]]) &&
            nzchar(value_names[[index]])
        ) {
          value_names[[index]]
        } else {
          as.character(index)
        }
        walk(
          .subset2(value, index),
          paste0(path, "$", label),
          inspect_function_environment
        )
      }
      walk_attributes(value, path, walk, inspect_function_environment)
      return(invisible(NULL))
    }
    walk_attributes(value, path, walk, inspect_function_environment)
    invisible(NULL)
  }

  walk(object)
  findings
}

.stripCerebroSourceReferences <- function(object) {
  enclos <- tryCatch(object$.__enclos_env__, error = function(error) NULL)
  environments <- list(object)
  if (is.environment(enclos)) {
    private <- tryCatch(enclos$private, error = function(error) NULL)
    if (is.environment(private)) {
      environments <- c(environments, list(private))
    }
  }

  for (environment in environments) {
    if (!is.environment(environment)) {
      next
    }
    for (name in ls(environment, all.names = TRUE)) {
      if (bindingIsActive(name, environment)) {
        next
      }
      value <- tryCatch(
        get(name, envir = environment, inherits = FALSE),
        error = function(error) NULL
      )
      if (!is.function(value)) {
        next
      }
      source_attributes <- intersect(
        c("srcref", "srcfile", "wholeSrcref"),
        names(attributes(value))
      )
      if (!length(source_attributes)) {
        next
      }
      for (attribute in source_attributes) {
        attr(value, attribute) <- NULL
      }
      locked <- bindingIsLocked(name, environment)
      if (locked) {
        unlockBinding(name, environment)
      }
      assign(name, value, envir = environment)
      if (locked) {
        lockBinding(name, environment)
      }
    }
  }
  object
}
