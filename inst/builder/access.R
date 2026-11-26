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
