.viewer_auth_error <- function(message) {
  stop(message, call. = FALSE)
}

.viewer_auth_validate_config <- function(config) {
  expected <- c(
    "credentials_path",
    "passphrase_env",
    "timeout_minutes"
  )
  valid <- is.list(config) &&
    identical(names(config), expected) &&
    identical(
      config$credentials_path,
      "private-data/auth/credentials.sqlite"
    ) &&
    is.character(config$passphrase_env) &&
    length(config$passphrase_env) == 1L &&
    !is.na(config$passphrase_env) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$passphrase_env) &&
    is.integer(config$timeout_minutes) &&
    length(config$timeout_minutes) == 1L &&
    !is.na(config$timeout_minutes) &&
    config$timeout_minutes >= 1L &&
    config$timeout_minutes <= 1440L
  if (!valid) {
    .viewer_auth_error("Invalid Viewer authentication configuration.")
  }
  config
}

.viewer_auth_require_provider <- function() {
  available <- requireNamespace("shinymanager", quietly = TRUE)
  version <- if (available) {
    tryCatch(
      utils::packageVersion("shinymanager"),
      error = function(condition) NULL
    )
  } else {
    NULL
  }
  if (is.null(version) || version < "1.1.0") {
    .viewer_auth_error(
      "Authentication requires shinymanager (>= 1.1.0)."
    )
  }
}

.viewer_auth_load_local_passphrase <- function(root, env_name) {
  current <- Sys.getenv(env_name, unset = NA_character_)
  if (
    is.character(current) &&
      length(current) == 1L &&
      !is.na(current) &&
      nzchar(current)
  ) {
    return(invisible(FALSE))
  }
  path <- file.path(root, "viewer-auth.env")
  info <- suppressWarnings(tryCatch(
    fs::file_info(path, follow = FALSE, fail = FALSE),
    error = function(condition) NULL
  ))
  if (is.null(info) || nrow(info) != 1L || is.na(info$type[[1L]])) {
    if (
      .Platform$OS.type != "windows" &&
        !isTRUE(file.access(root, mode = 1L) == 0L)
    ) {
      .viewer_auth_error(paste0(
        "viewer-auth.env could not be inspected because the App directory is not accessible. ",
        "Check parent directory access for the actual Shiny/R runtime UID/GID."
      ))
    }
    .viewer_auth_error(paste0(
      "viewer-auth.env was not found in the App directory. ",
      "Restore the file matching credentials.sqlite ",
      "or provide ",
      env_name,
      " to the application process."
    ))
  }
  type <- as.character(info$type[[1L]])
  if (identical(type, "symlink")) {
    .viewer_auth_error(
      "viewer-auth.env is a symbolic link; authentication requires a regular file, not a symlink."
    )
  }
  if (!identical(type, "file")) {
    .viewer_auth_error(
      "viewer-auth.env is not a regular file; authentication rejected this file type."
    )
  }
  if (.Platform$OS.type != "windows") {
    # fs includes the file type; retain permission and special bits (07777).
    mode <- bitwAnd(as.integer(info$permissions[[1L]]), 4095L)
    if (!identical(mode, 384L)) {
      .viewer_auth_error(paste0(
        "viewer-auth.env has insecure permissions: ",
        sprintf("%04o", mode),
        ". ",
        "Required permissions: 0600. Set ownership for the actual Shiny/R runtime ",
        "UID/GID and run chmod 600 on this file. For a read-only filesystem, ",
        "correct the source file's permissions before deployment."
      ))
    }
  }
  unreadable <- function() {
    .viewer_auth_error(paste0(
      "viewer-auth.env ",
      if (.Platform$OS.type != "windows") "has secure permissions but " else "",
      "is not readable by the current application user. ",
      "Check file ownership, parent directory access and the actual Shiny/R runtime UID/GID. ",
      "For a read-only filesystem, correct source file ownership before deployment."
    ))
  }
  if (!isTRUE(file.access(path, mode = 4L) == 0L)) {
    unreadable()
  }
  lines <- suppressWarnings(tryCatch(
    readLines(path, n = 2L, warn = FALSE, encoding = "UTF-8"),
    error = function(condition) NULL
  ))
  if (is.null(lines)) {
    unreadable()
  }
  pattern <- paste0("^", env_name, "=([0-9a-f]{64})$")
  if (length(lines) != 1L || !grepl(pattern, lines, perl = TRUE)) {
    .viewer_auth_error(paste0(
      "viewer-auth.env has invalid content. Expected exactly one line: ",
      env_name,
      "=<64 lowercase hexadecimal characters>, without quotes or export. ",
      "Restore the original file matching credentials.sqlite; do not use a login password."
    ))
  }
  value <- sub(paste0("^", env_name, "="), "", lines)
  on.exit(value <- NULL, add = TRUE)
  do.call(Sys.setenv, stats::setNames(list(value), env_name))
  invisible(TRUE)
}

.viewer_auth_brand <- function(cerebro_root) {
  www <- file.path(cerebro_root, "viewer", "www")
  css <- file.path(www, "auth.css")
  js <- file.path(www, "auth.js")
  logo <- file.path(www, "cerebronexus.svg")
  available <- isTRUE(utils::file_test("-f", css)) &&
    isTRUE(utils::file_test("-f", js)) &&
    isTRUE(utils::file_test("-f", logo))
  if (!available) {
    .viewer_auth_error("Authentication branding assets are unavailable.")
  }
  svg <- paste(readLines(logo, warn = FALSE), collapse = "\n")
  list(
    head = shiny::tagList(
      shiny::tags$title("Sign in | CerebroNexus"),
      shiny::includeCSS(css),
      shiny::includeScript(js)
    ),
    top = shiny::tags$div(
      class = "cerebro-auth-brand",
      role = "img",
      `aria-label` = "CerebroNexus",
      shiny::tags$div(`aria-hidden` = "true", shiny::HTML(svg))
    ),
    bottom = shiny::tags$div(
      class = "cerebro-auth-footer",
      "Need access? Contact the person who shared this app."
    )
  )
}

# Only customize the provider's login form; retain its authentication lifecycle.
.viewer_auth_present <- function(ui) {
  query <- htmltools::tagQuery(shiny::tags$div(ui))
  if (!length(query$find("#auth-auth-mod")$selectedTags())) {
    return(ui)
  }
  query$find("#auth-auth-mod")$addAttrs(role = "main")
  query$find("#auth-shinymanager-auth-head")$replaceWith(shiny::tagList(
    shiny::tags$h1(id = "auth-shinymanager-auth-head", "Sign in"),
    shiny::tags$p(
      class = "cerebro-auth-intro",
      "Use the account provided for this app."
    )
  ))
  query$find("#auth-user_id")$addAttrs(
    name = "username",
    autocomplete = "username",
    autocapitalize = "none",
    spellcheck = "false"
  )$removeAttrs("autofocus")
  query$find("#auth-user_pwd")$addAttrs(
    name = "password",
    autocomplete = "current-password"
  )
  query$find("#auth-go_auth")$append(shiny::tags$span(
    class = "cerebro-auth-busy-label",
    `aria-hidden` = "true",
    "Signing in\u2026"
  ))
  query$find("#auth-container-btn-ok")$append(shiny::tags$span(
    id = "cerebro-auth-status",
    class = "sr-only",
    role = "status",
    `aria-live` = "polite"
  ))
  query$find("#auth-result_auth")$addAttrs(
    `aria-live` = "polite",
    `aria-atomic` = "true"
  )
  shiny::tagList(query$allTags()$children)
}

## Failed sign-ins, counted per account and per client address. From the
## fifth consecutive failure of a key, that key waits 30 s, doubling with each
## further failure up to 15 min; while it waits the password is not checked.
## A success clears the account and the client.
.viewer_auth_limit_free <- 4L
.viewer_auth_limit_base_seconds <- 30
.viewer_auth_limit_max_seconds <- 900

.viewer_auth_limiter <- function(clock = function() as.numeric(Sys.time())) {
  state <- new.env(parent = emptyenv())
  entry <- function(key) {
    value <- get0(key, envir = state, inherits = FALSE)
    if (
      is.null(value) || clock() - value$last > .viewer_auth_limit_max_seconds
    ) {
      list(failures = 0L, until = -Inf, last = -Inf)
    } else {
      value
    }
  }
  list(
    waiting = function(keys) {
      any(vapply(keys, function(key) clock() < entry(key)$until, logical(1)))
    },
    failure = function(keys) {
      for (key in keys) {
        value <- entry(key)
        value$failures <- value$failures + 1L
        value$last <- clock()
        excess <- value$failures - .viewer_auth_limit_free
        if (excess > 0L) {
          value$until <- clock() +
            min(
              .viewer_auth_limit_max_seconds,
              .viewer_auth_limit_base_seconds * 2^(excess - 1L)
            )
        }
        assign(key, value, envir = state)
      }
      invisible(NULL)
    },
    success = function(keys) {
      present <- keys[vapply(
        keys,
        exists,
        logical(1),
        envir = state,
        inherits = FALSE
      )]
      if (length(present)) {
        rm(list = present, envir = state)
      }
      invisible(NULL)
    }
  )
}

.viewer_auth_limited_checker <- function(checker, limiter, client) {
  function(user, password) {
    account <- tolower(trimws(paste(as.character(user), collapse = "")))
    keys <- c(paste0("account:", account), paste0("client:", client))
    if (limiter$waiting(keys)) {
      return(list(
        result = FALSE,
        expired = FALSE,
        authorized = FALSE,
        user_info = NULL
      ))
    }
    result <- checker(user, password)
    if (isTRUE(result$result)) {
      limiter$success(keys)
    } else {
      limiter$failure(keys)
    }
    result
  }
}

.viewer_auth_client <- function(session) {
  address <- tryCatch(
    suppressWarnings(as.character(session$request$REMOTE_ADDR)),
    error = function(condition) character()
  )
  if (length(address) == 1L && !is.na(address) && nzchar(address)) {
    address
  } else {
    "unknown"
  }
}

## After sign-in the provider puts its login token in the page URL, and its
## server accepts that token from any session that presents it. The Viewer
## therefore starts only in the session that also returns the one-time grant
## written into the one page the provider rendered for the token's first use.
.viewer_auth_grant_seconds <- 120

.viewer_auth_query_token <- function(query) {
  token <- shiny::parseQueryString(query %||% "")$token
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    return("")
  }
  gsub("\"", "", token, fixed = TRUE)
}

.viewer_auth_grants <- function(clock = function() as.numeric(Sys.time())) {
  state <- new.env(parent = emptyenv())
  list(
    issue = function(token) {
      for (old in ls(state)) {
        if (get(old, envir = state)$expires < clock()) {
          rm(list = old, envir = state)
        }
      }
      key <- paste(as.character(openssl::rand_bytes(32L)), collapse = "")
      assign(
        key,
        list(token = token, expires = clock() + .viewer_auth_grant_seconds),
        envir = state
      )
      key
    },
    redeem = function(key, token) {
      if (
        !is.character(key) ||
          length(key) != 1L ||
          is.na(key) ||
          !grepl("^[0-9a-f]{64}$", key) ||
          !exists(key, envir = state, inherits = FALSE)
      ) {
        return(FALSE)
      }
      grant <- get(key, envir = state)
      rm(list = key, envir = state)
      grant$expires >= clock() && identical(grant$token, token)
    }
  )
}

.viewer_auth_grant_tag <- function(key) {
  shiny::tags$script(shiny::HTML(paste0(
    "(function(){var k='",
    key,
    "';function send(){",
    "Shiny.setInputValue('.cerebro_auth_grant',k,{priority:'event'});}",
    "if(window.Shiny&&Shiny.shinyapp&&Shiny.shinyapp.isConnected()){send();}",
    "else{$(document).one('shiny:connected',send);}})();"
  )))
}

viewer_auth_apply <- function(ui, server, config, cerebro_root = ".") {
  if (is.null(config)) {
    return(list(ui = ui, server = server))
  }

  config <- .viewer_auth_validate_config(config)
  root <- tryCatch(
    normalizePath(cerebro_root, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  database <- if (is.null(root)) {
    NULL
  } else {
    tryCatch(
      normalizePath(
        file.path(root, config$credentials_path),
        winslash = "/",
        mustWork = TRUE
      ),
      error = function(condition) NULL
    )
  }
  accessible <- !is.null(database) &&
    isTRUE(utils::file_test("-f", database)) &&
    isTRUE(file.access(database, mode = 4L) == 0L) &&
    isTRUE(file.access(dirname(database), mode = 1L) == 0L)
  if (!accessible) {
    .viewer_auth_error(
      paste0(
        "Authentication credentials database is not accessible. Check ",
        "private-data/auth/credentials.sqlite and parent directory access for ",
        "the actual Shiny/R runtime UID/GID. Recommended permissions: database ",
        "0600, auth directory 0700. For a read-only filesystem, correct ",
        "source ownership and permissions before deployment."
      )
    )
  }

  .viewer_auth_load_local_passphrase(root, config$passphrase_env)
  passphrase <- Sys.getenv(config$passphrase_env, unset = NA_character_)
  on.exit(passphrase <- NULL, add = TRUE)
  if (
    length(passphrase) != 1L ||
      is.na(passphrase) ||
      !nzchar(passphrase)
  ) {
    .viewer_auth_error(paste0(config$passphrase_env, " is not set."))
  }

  .viewer_auth_require_provider()
  checker <- suppressWarnings(suppressMessages(tryCatch(
    {
      credentials <- shinymanager::read_db_decrypt(
        conn = database,
        name = "credentials",
        passphrase = passphrase
      )
      credentials_checker <- shinymanager::check_credentials(credentials)
      credentials <- NULL
      credentials_checker
    },
    error = function(condition) NULL
  )))
  passphrase <- NULL
  if (!is.function(checker)) {
    .viewer_auth_error(
      "Authentication database or passphrase is invalid."
    )
  }
  brand <- .viewer_auth_brand(root)
  # Use the provider's labels so its initial language update preserves our copy.
  shinymanager::set_labels(
    "en",
    list(
      "Please authenticate" = "Sign in",
      "Login" = "Sign in",
      "Username:" = "Username",
      "Password:" = "Password",
      "Username or password are incorrect" = "Username or password is incorrect. Check your details and try again."
    )
  )

  limiter <- .viewer_auth_limiter()
  grants <- .viewer_auth_grants()

  secured_server <- function(input, output, session) {
    ## The token this session was opened with, read before the provider
    ## clears it from the address bar.
    session_token <- .viewer_auth_query_token(
      shiny::isolate(session$clientData$url_search)
    )
    auth <- shinymanager::secure_server(
      check_credentials = .viewer_auth_limited_checker(
        checker,
        limiter,
        .viewer_auth_client(session)
      ),
      timeout = config$timeout_minutes,
      keep_token = FALSE,
      session = session
    )
    granted <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(
      input$.cerebro_auth_grant,
      {
        if (!granted()) {
          granted(grants$redeem(input$.cerebro_auth_grant, session_token))
        }
      },
      once = TRUE
    )
    started <- shiny::reactiveVal(FALSE)
    revoked <- shiny::reactiveVal(FALSE)
    subject <- shiny::reactiveVal(NULL)
    last_activity <- shiny::reactiveVal(NULL)
    timeout_ms <- config$timeout_minutes * 60 * 1000
    now_ms <- function() {
      if (is.function(session$.now)) {
        return(as.numeric(session$.now()))
      }
      as.numeric(Sys.time()) * 1000
    }
    revoke <- function() {
      if (started() && !revoked()) {
        revoked(TRUE)
        session$reload()
        session$onFlushed(function() session$close(), once = TRUE)
      }
      invisible(NULL)
    }
    shiny::observe({
      user <- auth$user
      authorized <- is.character(user) &&
        length(user) == 1L &&
        !is.na(user) &&
        nzchar(user)
      if (authorized && granted() && !started()) {
        started(TRUE)
        subject(user)
        last_activity(now_ms())
        server(input, output, session)
      } else if (
        started() &&
          (!authorized || !identical(user, subject()))
      ) {
        revoke()
      }
    })
    shiny::observeEvent(
      input$.shinymanager_logout,
      {
        revoke()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$.shinymanager_timeout,
      {
        if (started() && !revoked()) {
          current <- now_ms()
          if (current - last_activity() > timeout_ms) {
            revoke()
          } else {
            last_activity(current)
          }
        }
      },
      ignoreInit = TRUE
    )
    shiny::observe({
      last <- last_activity()
      shiny::req(!is.null(last), started(), !revoked())
      remaining <- timeout_ms - (now_ms() - last)
      if (remaining < 0) {
        revoke()
      } else {
        shiny::invalidateLater(as.integer(remaining) + 1L, session)
      }
    })
    invisible(auth)
  }

  secured_ui <- shinymanager::secure_app(
    ui,
    enable_admin = FALSE,
    head_auth = brand$head,
    tags_top = brand$top,
    tags_bottom = brand$bottom
  )
  ## The provider renders the application only for a token's first use; that
  ## page alone carries the grant.
  grant_ui <- function(request, rendered) {
    token <- .viewer_auth_query_token(request$QUERY_STRING)
    query <- htmltools::tagQuery(shiny::tags$div(rendered))
    if (!nzchar(token) || length(query$find("#auth-auth-mod")$selectedTags())) {
      return(rendered)
    }
    shiny::tagList(rendered, .viewer_auth_grant_tag(grants$issue(token)))
  }
  list(
    ui = if (is.function(secured_ui)) {
      function(request) {
        .viewer_auth_present(grant_ui(request, secured_ui(request)))
      }
    } else {
      secured_ui
    },
    server = compiler::cmpfun(secured_server),
    grants = grants
  )
}
