# Load real server entry points into a small Shiny session. Treat $ and [[,
# assignment spelling, whitespace and observer-local names as implementation
# details. Browser tests cover the assembled application as well.
viewer_server_key <- function(node) {
  if (is.symbol(node)) {
    return(as.character(node))
  }
  if (
    is.call(node) &&
      as.character(node[[1]])[1] %in% c("$", "[[") &&
      length(node) == 3L &&
      (is.symbol(node[[3]]) ||
        (is.character(node[[3]]) && length(node[[3]]) == 1L))
  ) {
    return(paste(
      viewer_server_key(node[[2]]),
      as.character(node[[3]]),
      sep = "."
    ))
  }
  ""
}

viewer_r_assignment <- function(path, name) {
  statements <- Filter(
    function(x) {
      is.call(x) &&
        as.character(x[[1]])[1] %in% c("<-", "=", "<<-") &&
        identical(viewer_server_key(x[[2]]), name)
    },
    as.list(parse(path))
  )
  ## A file may re-assign the name after defining it (shiny_server.R byte
  ## compiles its server at the end), so prefer the single definition.
  definitions <- Filter(
    function(x) {
      is.call(x[[3]]) && identical(as.character(x[[3]][[1]])[1], "function")
    },
    statements
  )
  if (length(definitions) == 1L) {
    return(definitions[[1]])
  }
  if (length(statements) != 1L) {
    stop(
      "Expected one R declaration for ",
      name,
      "; found ",
      length(statements)
    )
  }
  statements[[1]]
}

viewer_demo_options <- function() {
  scope <- new.env(parent = globalenv())
  path <- file.path(viewer_app_test_path(), "app.R")
  withr::local_envvar(c(CEREBRO_1M_DEMO_CRB = NA, CEREBRO_REN_DEMO_CRB = NA))
  sys.source(viewer_test_path("million_cell_demo.R"), scope)
  eval(viewer_r_assignment(path, "custom_welcome_message"), scope)
  for (statement in parse(path)) {
    if (
      is.call(statement) &&
        as.character(statement[[1]])[1] %in% c("<-", "=", "<<-") &&
        identical(viewer_server_key(statement[[2]]), "Cerebro.options")
    ) {
      scope$Cerebro.options <- eval(statement[[3]], scope)
    }
  }
  stopifnot(is.list(scope$Cerebro.options))
  scope$Cerebro.options
}

viewer_server_statements <- function(..., server = FALSE) {
  expressions <- as.list(parse(viewer_test_path(...)))
  if (server) {
    definition <- Filter(
      function(x) {
        is.call(x) &&
          as.character(x[[1]])[1] %in% c("<-", "=") &&
          identical(viewer_server_key(x[[2]]), "server")
      },
      expressions
    )[[1]]
    expressions <- as.list(definition[[3]][[3]])[-1]
  }
  expressions
}

viewer_server_load <- function(
  expressions,
  scope,
  names = character(),
  events = character()
) {
  keys <- vapply(
    names,
    function(x) viewer_server_key(parse(text = x)[[1]]),
    character(1)
  )
  found_names <- found_events <- character()
  for (statement in expressions) {
    if (!is.call(statement)) {
      next
    }
    operator <- as.character(statement[[1]])[1]
    key <- if (operator %in% c("<-", "=")) {
      viewer_server_key(statement[[2]])
    } else {
      ""
    }
    assignment <- key %in% keys
    matches <- if (operator %in% c("observe", "observeEvent")) {
      calls <- r_test_calls(statement[[2]])
      input_keys <- vapply(calls, viewer_server_key, character(1))
      events[vapply(
        events,
        function(event) {
          event %in%
            all.names(statement[[2]]) ||
            paste0("input.", event) %in% input_keys
        },
        logical(1)
      )]
    } else {
      character()
    }
    if (assignment || length(matches)) {
      eval(statement, scope)
    }
    if (assignment) {
      found_names <- c(found_names, key)
    }
    found_events <- c(found_events, matches)
  }
  missing <- c(names[!keys %in% found_names], setdiff(events, found_events))
  if (length(missing)) {
    stop(
      "Server test entry points not found: ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(NULL)
}

## Load a stored-result page (marker_genes, enriched_pathways) into `scope`.
## The shared cerebroResultPageServer() is evaluated in `scope` too, so stubs
## defined there (getters, cerebroResultTableControls, ...) are the ones used.
viewer_result_page_load <- function(page, scope) {
  eval(
    viewer_r_assignment(
      viewer_test_path("utility_functions.R"),
      "cerebroResultPageServer"
    ),
    scope
  )
  if (!exists("cerebroRegisterInfo", scope, inherits = FALSE)) {
    scope$cerebroRegisterInfo <- function(...) invisible(NULL)
  }
  sys.source(viewer_test_path(page, "server.R"), scope)
}
