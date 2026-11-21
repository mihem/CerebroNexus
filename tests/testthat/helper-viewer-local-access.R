## Exercise the real launcher and its access boundary without preparing the
## analysis modules or loading a dataset.
viewer_local_access_fixture <- function(...) {
  app <- launchCerebro(...)
  env <- environment(app$serverFuncSource())
  env$prepared_count <- 0L
  env$viewer_prepare_ui <- function() {
    env$ui <- shiny::fluidPage(
      shiny::textInput("ping", "Ping", ""),
      shiny::textOutput("pong"),
      shiny::fileInput("upload", "Upload"),
      shiny::textOutput("uploaded"),
      shiny::downloadButton("download", "Download")
    )
    env$viewer_ui_prepared <- TRUE
    invisible(TRUE)
  }
  env$viewer_prepare <- function() {
    env$prepared_count <- env$prepared_count + 1L
    env$viewer_prepare_ui()
    env$viewer_prepared <- TRUE
    invisible(TRUE)
  }
  env$.viewerSessionActivity <- function() invisible(NULL)
  env$.viewerWhenQuiet <- function(...) invisible(NULL)
  env$server <- function(input, output, session) {
    output$pong <- shiny::renderText(paste0("pong:", input$ping))
    output$uploaded <- shiny::renderText(input$upload$name)
    output$download <- shiny::downloadHandler(
      filename = function() "viewer-access.txt",
      content = function(file) writeLines("authorized download", file)
    )
  }
  app
}

viewer_local_access_process <- function(
  mode = "open",
  require_token = NULL,
  host = "127.0.0.1",
  launch_browser = FALSE,
  start = FALSE,
  cached = FALSE
) {
  skip_if_not_installed("callr")
  skip_if_not_installed("curl")
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  if (
    !file.exists(file.path(root, "DESCRIPTION")) ||
      !dir.exists(file.path(root, "R"))
  ) {
    root <- NULL
  }
  if (!is.null(root)) {
    skip_if_not_installed("pkgload")
  }
  fixture <- viewer_local_access_fixture
  environment(fixture) <- baseenv()
  port <- httpuv::randomPort()
  process <- callr::r_bg(
    function(
      root,
      fixture,
      port,
      mode,
      require_token,
      host,
      launch_browser,
      start,
      cached
    ) {
      cache <- tempfile("viewer-access-cache-")
      dir.create(cache)
      Sys.setenv(R_USER_CACHE_DIR = cache)
      if (is.null(root)) {
        library(CerebroNexus)
      } else {
        pkgload::load_all(root, quiet = TRUE)
      }
      environment(fixture) <- globalenv()
      options(cerebro.sockjs_compat = FALSE)
      if (start) {
        starter <- new.env(parent = globalenv())
        sys.source(system.file("start.R", package = "CerebroNexus"), starter)
        start_root <- system.file(package = "CerebroNexus")
        if (cached) {
          ## A tiny installed-layout fixture exercises the real early server
          ## under load_all() too (inst/ itself has no DESCRIPTION).
          page_app <- launchCerebro(mode = mode, require_token = require_token)
          page_env <- environment(page_app$serverFuncSource())
          response <- page_app$httpHandler(list(
            PATH_INFO = "/",
            REQUEST_METHOD = "GET",
            SCRIPT_NAME = "",
            QUERY_STRING = ""
          ))
          start_root <- tempfile("viewer-access-install-")
          dir.create(start_root)
          file.copy(
            system.file("viewer", package = "CerebroNexus"),
            start_root,
            recursive = TRUE
          )
          writeLines(
            c(
              "Package: CerebroNexus",
              paste("Version:", utils::packageVersion("CerebroNexus"))
            ),
            file.path(start_root, "DESCRIPTION")
          )
          key <- page_env$viewerUiCacheKey(
            file.path(start_root, "viewer"),
            c(
              version = as.character(utils::packageVersion("CerebroNexus")),
              www_prefix = "cerebro_www"
            )
          )
          content <- response$content
          if (!is.raw(content)) {
            content <- charToRaw(as.character(content))
          }
          page_env$viewerDocumentKeep(key, content)
          starter$loadNamespace <- function(...) {
            list(
              launchCerebro = function(...) {
                message(
                  "EARLY_CACHE_RUNNING:",
                  !is.null(getOption("CerebroNexus.early_server"))
                )
                CerebroNexus::launchCerebro(...)
              }
            )
          }
        }
        starter$cerebroStart(
          port = port,
          host = host,
          mode = mode,
          require_token = require_token,
          launch.browser = launch_browser,
          root = start_root
        )
      } else {
        app <- fixture(
          mode = mode,
          require_token = require_token,
          options = list(launch.browser = launch_browser)
        )
        if (cached) {
          env <- environment(app$serverFuncSource())
          response <- app$httpHandler(list(
            PATH_INFO = "/",
            REQUEST_METHOD = "GET",
            SCRIPT_NAME = "",
            QUERY_STRING = paste0("?token=", env$viewer_access_token())
          ))
          directory <- tempfile("viewer-access-document-")
          dir.create(directory)
          content <- response$content
          if (!is.raw(content)) {
            content <- charToRaw(as.character(content))
          }
          writeBin(content, file.path(directory, "index.html"))
          env$viewer_document <- directory
          env$viewer_document_capture <- FALSE
        }
        shiny::runApp(app, host = host, port = port, quiet = TRUE)
      }
    },
    args = list(
      root,
      fixture,
      port,
      mode,
      require_token,
      host,
      launch_browser,
      start,
      cached
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  withr::defer(process$kill(), envir = parent.frame())
  address <- sprintf("http://127.0.0.1:%d/", port)
  deadline <- Sys.time() + 45
  response <- NULL
  while (is.null(response) && process$is_alive() && Sys.time() < deadline) {
    response <- tryCatch(
      curl::curl_fetch_memory(address),
      error = function(error) NULL
    )
    if (is.null(response)) Sys.sleep(0.05)
  }
  if (is.null(response)) {
    stop(paste(
      "Viewer did not start:",
      paste(process$read_error_lines(), collapse = "\n")
    ))
  }
  log <- paste(process$read_error_lines(), collapse = "\n")
  # An early cached shell may arrive before package loading and the browser
  # callback. Wait for the credential, not just for a successful page response.
  if (.viewerAccessRequired(mode, require_token, host)) {
    while (
      !grepl("token=[0-9a-f]{64}", log) &&
        process$is_alive() &&
        Sys.time() < deadline
    ) {
      Sys.sleep(0.05)
      log <- paste(
        log,
        paste(process$read_error_lines(), collapse = "\n"),
        sep = "\n"
      )
    }
    if (!grepl("token=[0-9a-f]{64}", log)) {
      stop(paste("No Viewer credential:", log))
    }
  }
  token <- regmatches(log, regexpr("(?<=token=)[0-9a-f]{64}", log, perl = TRUE))
  authorized_url <- regmatches(
    log,
    regexpr("(?<=Open the Viewer at )[^\\r\\n]+", log, perl = TRUE)
  )
  list(
    process = process,
    port = port,
    address = address,
    token = if (length(token)) token else NULL,
    authorized_url = if (length(authorized_url)) authorized_url else address,
    initial = response,
    log = log
  )
}

viewer_local_access_socket <- function(
  app,
  query,
  origin = "http://evil.example"
) {
  skip_if_not_installed("websocket")
  state <- new.env(parent = emptyenv())
  state$opened <- FALSE
  state$closed <- FALSE
  state$values <- NULL
  state$errors <- character()
  ws <- websocket::WebSocket$new(
    sprintf("ws://127.0.0.1:%d/websocket", app$port),
    headers = c(Origin = origin),
    autoConnect = FALSE
  )
  on.exit(ws$close(), add = TRUE)
  ws$onOpen(function(event) {
    state$opened <- TRUE
    ws$send(jsonlite::toJSON(
      list(
        method = "init",
        data = list(
          .clientdata_url_protocol = "http:",
          .clientdata_url_hostname = "127.0.0.1",
          .clientdata_url_port = as.character(app$port),
          .clientdata_url_pathname = "/",
          .clientdata_url_search = query,
          .clientdata_url_hash_initial = "",
          .clientdata_singletons = "",
          .clientdata_output_pong_hidden = FALSE,
          ping = "socket"
        )
      ),
      auto_unbox = TRUE
    ))
  })
  ws$onMessage(function(event) {
    message <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
    if (length(message$values)) state$values <- message$values
  })
  ws$onClose(function(event) state$closed <- TRUE)
  ws$onError(function(event) state$errors <- c(state$errors, event$message))
  ws$connect()
  deadline <- Sys.time() + 10
  while (!state$closed && is.null(state$values) && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  as.list(state)
}
