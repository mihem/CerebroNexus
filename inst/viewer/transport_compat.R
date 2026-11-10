## Shiny Server runs an App through SockJS, and SockJS cannot carry the raw
## binary WebSocket frames the Viewer sends its cell-view data in: the page
## opens, but plots never arrive and datasets load forever. Where Shiny Server
## runs the App, each binary frame goes out as a base64 text message instead,
## and transport_compat.js turns it back into the original frame before the
## Viewer sees it. Anywhere else the frames go as they are, without the third
## more bytes and the encoding time.

## TRUE when the wrapper has to be applied. `sockjs_compat` in `options`
## (Cerebro.options) or, when that is unset, the `cerebro.sockjs_compat` R
## option forces it: TRUE on, FALSE off. "auto", the default, turns it on when
## Shiny Server started this R process, which sets SHINY_SERVER_VERSION.
## The test runs where the App runs, so an App built on one machine and copied
## to a Shiny Server decides there.
viewerSockjsCompatEnabled <- function(options = Cerebro.options) {
  setting <- options[["sockjs_compat"]]
  if (is.null(setting)) {
    setting <- getOption("cerebro.sockjs_compat", "auto")
  }
  if (identical(setting, "auto")) {
    return(nzchar(Sys.getenv("SHINY_SERVER_VERSION")))
  }
  if (is.logical(setting) && length(setting) == 1L && !is.na(setting)) {
    return(setting)
  }
  stop("`sockjs_compat` must be TRUE, FALSE or \"auto\".", call. = FALSE)
}

## Returns list(ui, server). `ui` is the page or a function of the request;
## `root` holds viewer/transport_compat.js. Apply it before any wrapper that
## needs the finished ui and server, such as the login page.
viewerApplyTransportCompat <- function(
  ui,
  server,
  root,
  enabled = viewerSockjsCompatEnabled()
) {
  if (!isTRUE(enabled)) {
    return(list(ui = ui, server = server))
  }
  script <- shiny::includeScript(
    file.path(root, "viewer", "transport_compat.js")
  )
  with_script <- function(page) {
    shiny::tagList(shiny::tags$head(script), page)
  }
  wrapped_ui <- if (is.function(ui)) {
    function(request) with_script(ui(request))
  } else {
    with_script(ui)
  }
  wrapped_server <- function(input, output, session) {
    send_binary <- function(type, message) {
      stopifnot(is.character(type), length(type) == 1L, is.raw(message))
      session$sendCustomMessage(
        "cerebro_sockjs_binary",
        list(
          type = type,
          data = gsub("\n", "", jsonlite::base64_enc(message), fixed = TRUE)
        )
      )
    }
    locked <- bindingIsLocked("sendBinaryMessage", session)
    if (locked) {
      unlockBinding("sendBinaryMessage", session)
    }
    session$sendBinaryMessage <- send_binary
    if (locked) {
      lockBinding("sendBinaryMessage", session)
    }
    server(input, output, session)
  }
  list(ui = wrapped_ui, server = wrapped_server)
}
