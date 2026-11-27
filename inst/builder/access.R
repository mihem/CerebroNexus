## Who may use this Builder.
##
## launchCerebroBuilder() starts the Builder with a random access token and
## opens the page with it in the address (?token=...). Shiny checks neither the
## Origin nor the Host of a connection, so without the token any web page the
## user happens to visit (through a cross-site WebSocket or DNS rebinding), or
## another user of a shared machine, could open a session on the port and drive
## the Builder: browse the home folder, open files, run the code inside an
## `.rds`. The page and every session therefore need the token. Running app.R
## directly, as the tests do, sets no token and is not checked.

builder_access_token <- function(
  token = getOption("CerebroNexus.builder.access_token")
) {
  if (
    is.character(token) && length(token) == 1L && !is.na(token) && nzchar(token)
  ) {
    token
  } else {
    NULL
  }
}

## `query` is a URL query string ("?token=..."); it is only read when a token
## is set.
builder_access_allowed <- function(query, token) {
  if (is.null(token)) {
    return(TRUE)
  }
  query <- as.character(query %||% "")
  if (length(query) != 1L || is.na(query)) {
    return(FALSE)
  }
  supplied <- shiny::parseQueryString(query)$token
  if (!is.character(supplied) || length(supplied) != 1L || is.na(supplied)) {
    return(FALSE)
  }
  supplied <- charToRaw(enc2utf8(supplied))
  expected <- charToRaw(enc2utf8(token))
  ## Compare every byte, so the time taken does not tell how much matched.
  length(supplied) == length(expected) &&
    sum(as.integer(xor(supplied, expected))) == 0L
}

builder_access_denied_response <- function() {
  shiny::httpResponse(
    status = 403L,
    content_type = "text/html; charset=UTF-8",
    content = paste0(
      "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
      "<title>Cerebro Builder</title></head><body>",
      "<h1>Access token required</h1>",
      "<p>Open the Builder with the address that launchCerebroBuilder() ",
      "printed in the R console; it ends in <code>?token=...</code>.</p>",
      "</body></html>"
    ),
    headers = list(`Cache-Control` = "no-store")
  )
}

## Examples-only mode, for a public demonstration (issue #272).
##
## Visitors load the bundled examples, configure them, build, and download the
## result. Nothing of theirs reaches the server: the page offers no upload,
## no server folder and no Project, and the server refuses each of them as
## well (builder_examples_only_refused() in server/foundation.R), so a page
## that sends the inputs anyway changes nothing. Builds go to a folder of
## their own below tempdir(), removed when the session ends.
##
## launchCerebroBuilder(examples_only = TRUE) sets the option; a server that
## runs app.R directly sets it before the app starts. Read once, like the
## access token.
builder_examples_only_mode <- function(
  value = getOption("CerebroNexus.builder.examples_only", FALSE)
) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(
      "The option CerebroNexus.builder.examples_only must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  value
}

## TRUE in an examples-only Builder. Code sourced without app.R (as many tests
## do) sees no setting and runs the full Builder.
builder_examples_only <- function() {
  isTRUE(get0(".builder_examples_only", inherits = TRUE, ifnotfound = FALSE))
}

builder_examples_only_message <- paste(
  "This demonstration Builder works with the bundled examples only:",
  "uploads, server folders and Projects are turned off."
)

## Every visitor of a public demonstration holds a session with its own
## background R process and its own copy of the example data. Two limits keep
## a demonstration server from running out of memory:
##
## - CerebroNexus.builder.examples_only_max_sessions (default 4): sessions
##   one R process serves at a time. A visitor beyond it is told the
##   demonstration is busy and gets no session; nothing is loaded for them.
## - CerebroNexus.builder.examples_only_idle_minutes (default 15): a session
##   with no click, key press or scroll for this long, and no build or import
##   running, is closed.
##
## Inf turns a limit off. The page reports activity at most every 30 seconds
## and the server checks every 30 seconds, so the idle limit is at least one
## minute. Read once, when app.R starts.
builder_examples_only_limits <- function(
  max_sessions = getOption(
    "CerebroNexus.builder.examples_only_max_sessions",
    4L
  ),
  idle_minutes = getOption(
    "CerebroNexus.builder.examples_only_idle_minutes",
    15
  )
) {
  if (
    !is.numeric(max_sessions) ||
      length(max_sessions) != 1L ||
      is.na(max_sessions) ||
      max_sessions < 1 ||
      (is.finite(max_sessions) && max_sessions != round(max_sessions))
  ) {
    stop(
      "The option CerebroNexus.builder.examples_only_max_sessions must be ",
      "a whole number of at least 1, or Inf.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(idle_minutes) ||
      length(idle_minutes) != 1L ||
      is.na(idle_minutes) ||
      idle_minutes < 1
  ) {
    stop(
      "The option CerebroNexus.builder.examples_only_idle_minutes must be ",
      "a number of minutes of at least 1, or Inf.",
      call. = FALSE
    )
  }
  list(
    max_sessions = as.numeric(max_sessions),
    idle_seconds = as.numeric(idle_minutes) * 60
  )
}

## Sessions are counted in an environment shared by all sessions of this R
## process. builder_examples_only_enter() returns NULL when every seat is
## taken, and otherwise a function that gives the seat back; calling it again
## does nothing, so a session that ends twice frees one seat only.
builder_examples_only_seats <- function() {
  seats <- new.env(parent = emptyenv())
  seats$count <- 0L
  seats
}

builder_examples_only_enter <- function(seats, max_sessions) {
  if (seats$count >= max_sessions) {
    return(NULL)
  }
  seats$count <- seats$count + 1L
  left <- FALSE
  function() {
    if (!left) {
      left <<- TRUE
      seats$count <- seats$count - 1L
    }
    invisible(seats$count)
  }
}

builder_examples_only_busy_message <- paste(
  "This demonstration Builder is busy: all of its sessions are in use.",
  "Please try again in a few minutes."
)

builder_examples_only_idle_message <- function(idle_seconds) {
  paste0(
    "This demonstration session was closed after ",
    format(idle_seconds / 60),
    " minutes without activity. Reload the page to start again."
  )
}

## Tell the page why its session ends, then end it once the message is out.
builder_examples_only_close <- function(session, text) {
  session$sendCustomMessage(
    "builder_examples_only_closed",
    list(text = text)
  )
  session$onFlushed(function() session$close(), once = TRUE)
  invisible(TRUE)
}

## Close the session after `idle_seconds` without activity. The page reports
## clicks, key presses and scrolls as input$builder_examples_only_activity
## (builder.js); while busy() is TRUE (a build or an import runs) the session
## counts as active.
builder_examples_only_watch_idle <- function(
  session,
  input,
  idle_seconds,
  busy = function() FALSE,
  now = Sys.time
) {
  if (!is.finite(idle_seconds)) {
    return(invisible(NULL))
  }
  last <- now()
  closed <- FALSE
  shiny::observeEvent(
    input$builder_examples_only_activity,
    last <<- now(),
    ignoreInit = TRUE
  )
  shiny::observe({
    shiny::invalidateLater(30000)
    if (closed) {
      return()
    }
    if (isTRUE(shiny::isolate(busy()))) {
      last <<- now()
      return()
    }
    if (as.numeric(difftime(now(), last, units = "secs")) >= idle_seconds) {
      closed <<- TRUE
      builder_examples_only_close(
        session,
        builder_examples_only_idle_message(idle_seconds)
      )
    }
  })
  invisible(NULL)
}
