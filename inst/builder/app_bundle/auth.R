if (!exists(".builderRandomBytes", mode = "function", inherits = TRUE)) {
  .builderRandomBytes <- function(n) {
    getFromNamespace(".builderRandomBytes", "CerebroNexus")(n)
  }
}

.builder_auth_env_name <- "CEREBRO_AUTH_PASSPHRASE"
.builder_auth_timeout_minutes <- 15L
.builder_auth_max_accounts <- 50L

builder_auth_empty_accounts <- function() {
  structure(list(), class = c("builder_auth_accounts", "list"))
}

builder_auth_validate_payload <- function(enabled, accounts) {
  invalid <- function(message) {
    list(ok = FALSE, error = message, accounts = NULL)
  }
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    return(invalid("The login setting is invalid."))
  }
  if (!isTRUE(enabled)) {
    return(list(
      ok = TRUE,
      error = NULL,
      accounts = builder_auth_empty_accounts()
    ))
  }
  if (!is.list(accounts) || !length(accounts)) {
    return(invalid("Add at least one login account."))
  }
  if (is.object(accounts)) {
    if (
      !identical(
        attr(accounts, "class", exact = TRUE),
        c("builder_auth_accounts", "list")
      )
    ) {
      return(invalid("The login accounts are invalid."))
    }
    accounts <- unclass(accounts)
  }
  if (length(accounts) > .builder_auth_max_accounts) {
    return(invalid("Login supports at most 50 accounts."))
  }
  normalized <- vector("list", length(accounts))
  for (index in seq_along(accounts)) {
    account <- accounts[[index]]
    expected <- c("id", "username", "password")
    if (
      !is.list(account) ||
        is.object(account) ||
        !identical(sort(names(account)), sort(expected))
    ) {
      return(invalid(paste0("Account ", index, " is incomplete.")))
    }
    scalar_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value)
    }
    if (
      !scalar_text(account$id) ||
        !grepl("^auth-account-[1-9][0-9]*$", account$id)
    ) {
      return(invalid(paste0("Account ", index, " has an invalid row.")))
    }
    username <- if (scalar_text(account$username)) {
      trimws(account$username)
    } else {
      ""
    }
    if (!nzchar(username)) {
      return(invalid(paste0("Account ", index, " needs a username.")))
    }
    password <- account$password
    if (!scalar_text(password) || !nzchar(password) || nchar(password) < 8L) {
      return(invalid(paste0(
        "Account ",
        index,
        " needs a password of at least 8 characters."
      )))
    }
    normalized[[index]] <- list(
      id = account$id,
      username = username,
      password = password
    )
  }
  ids <- vapply(normalized, `[[`, character(1), "id")
  users <- vapply(normalized, `[[`, character(1), "username")
  if (anyDuplicated(ids)) {
    return(invalid("Each login row must be unique."))
  }
  if (anyDuplicated(users)) {
    return(invalid("Usernames must be unique."))
  }
  list(
    ok = TRUE,
    error = NULL,
    accounts = structure(
      normalized,
      class = c("builder_auth_accounts", "list")
    )
  )
}

builder_auth_summary <- function(enabled, accounts) {
  parsed <- builder_auth_validate_payload(enabled, accounts)
  if (!isTRUE(parsed$ok)) {
    stop(parsed$error, call. = FALSE)
  }
  list(
    enabled = isTRUE(enabled),
    account_count = as.integer(length(parsed$accounts)),
    timeout_minutes = .builder_auth_timeout_minutes
  )
}

.builder_auth_random_passphrase <- function(
  .random_bytes = .builderRandomBytes
) {
  bytes <- tryCatch(.random_bytes(32L), error = function(error) NULL)
  if (!is.raw(bytes) || length(bytes) != 32L) {
    stop("A secure authentication key could not be generated.", call. = FALSE)
  }
  paste(sprintf("%02x", as.integer(bytes)), collapse = "")
}

.builder_auth_path_is_link <- function(path) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    return(FALSE)
  }
  link <- tryCatch(Sys.readlink(path), error = function(error) NA_character_)
  is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
}

.builder_auth_stage <- function(stage) {
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !nzchar(stage) ||
      !dir.exists(stage) ||
      .builder_auth_path_is_link(stage)
  ) {
    stop("The authentication workspace is invalid.", call. = FALSE)
  }
  normalizePath(stage, winslash = "/", mustWork = TRUE)
}

.builder_auth_remove_path <- function(path, recursive, .unlink = unlink) {
  if (
    !file.exists(path) && !dir.exists(path) && !.builder_auth_path_is_link(path)
  ) {
    return(invisible(TRUE))
  }
  status <- tryCatch(
    .unlink(path, recursive = recursive, force = TRUE),
    error = function(error) NA_integer_
  )
  if (
    length(status) != 1L ||
      is.na(status) ||
      status != 0L ||
      file.exists(path) ||
      dir.exists(path) ||
      .builder_auth_path_is_link(path)
  ) {
    stop("The authentication files could not be cleaned up.", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_auth_remove_partial_material <- function(stage, .unlink = unlink) {
  stage <- .builder_auth_stage(stage)
  .builder_auth_remove_path(
    file.path(stage, ".builder-auth-source"),
    recursive = TRUE,
    .unlink = .unlink
  )
  .builder_auth_remove_path(
    file.path(stage, "viewer-auth.env"),
    recursive = FALSE,
    .unlink = .unlink
  )
  invisible(TRUE)
}

.builder_auth_write_env <- function(
  path,
  passphrase,
  .write_lines = writeLines,
  .chmod = Sys.chmod,
  .rename = file.rename,
  .file_info = file.info,
  .unlink = unlink
) {
  if (
    !is.character(passphrase) ||
      length(passphrase) != 1L ||
      is.na(passphrase) ||
      !grepl("^[0-9a-f]{64}$", passphrase)
  ) {
    stop("The generated authentication key is invalid.", call. = FALSE)
  }
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      file.exists(path) ||
      dir.exists(path) ||
      .builder_auth_path_is_link(path)
  ) {
    stop("The authentication secret target is invalid.", call. = FALSE)
  }
  temporary <- tempfile(".viewer-auth-", tmpdir = dirname(path))
  on.exit(
    {
      if (
        file.exists(temporary) ||
          dir.exists(temporary) ||
          .builder_auth_path_is_link(temporary)
      ) {
        removed <- tryCatch(
          .unlink(temporary, recursive = FALSE, force = TRUE),
          error = function(error) NA_integer_
        )
        if (
          length(removed) != 1L ||
            is.na(removed) ||
            removed != 0L ||
            file.exists(temporary) ||
            dir.exists(temporary) ||
            .builder_auth_path_is_link(temporary)
        ) {
          stop(
            "The authentication temporary file could not be cleaned up.",
            call. = FALSE
          )
        }
      }
      passphrase <- NULL
    },
    add = TRUE
  )
  written <- try(
    .write_lines(
      paste0(.builder_auth_env_name, "=", passphrase),
      temporary,
      useBytes = TRUE
    ),
    silent = TRUE
  )
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    stop("The authentication secret file could not be written.", call. = FALSE)
  }
  protected <- try(
    .chmod(temporary, mode = "0600", use_umask = FALSE),
    silent = TRUE
  )
  if (inherits(protected, "try-error") || !isTRUE(protected)) {
    stop(
      "The authentication secret file could not be protected.",
      call. = FALSE
    )
  }
  if (.Platform$OS.type != "windows") {
    mode <- tryCatch(
      as.integer(.file_info(temporary)$mode),
      error = function(error) NA_integer_
    )
    if (!identical(mode, 384L)) {
      stop("The authentication secret file is not private.", call. = FALSE)
    }
  }
  finalized <- try(.rename(temporary, path), silent = TRUE)
  if (inherits(finalized, "try-error") || !isTRUE(finalized)) {
    stop(
      "The authentication secret file could not be finalized.",
      call. = FALSE
    )
  }
  if (
    !file.exists(path) ||
      dir.exists(path) ||
      .builder_auth_path_is_link(path) ||
      (.Platform$OS.type != "windows" &&
        !identical(as.integer(.file_info(path)$mode), 384L))
  ) {
    stop("The authentication secret file could not be verified.", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

builder_auth_read_env_file <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      .builder_auth_path_is_link(path)
  ) {
    stop("The authentication secret file is missing or unsafe.", call. = FALSE)
  }
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(error) NULL,
    warning = function(warning) NULL
  )
  pattern <- paste0("^", .builder_auth_env_name, "=([0-9a-f]{64})$")
  if (length(lines) != 1L || !grepl(pattern, lines, perl = TRUE)) {
    stop("The authentication secret file is invalid.", call. = FALSE)
  }
  if (
    .Platform$OS.type != "windows" &&
      !identical(as.integer(file.info(path)$mode), 384L)
  ) {
    stop("The authentication secret file is not private.", call. = FALSE)
  }
  sub(paste0("^", .builder_auth_env_name, "="), "", lines)
}

builder_auth_validate_material <- function(material, stage) {
  invalid <- function() {
    stop("The authentication files could not be verified.", call. = FALSE)
  }
  if (
    !is.list(material) ||
      !identical(
        names(material),
        c(
          "source_dir",
          "credentials",
          "env_file",
          "descriptor"
        )
      )
  ) {
    invalid()
  }
  stage <- tryCatch(
    .builder_auth_stage(stage),
    error = function(error) NULL
  )
  if (is.null(stage)) {
    invalid()
  }
  source_dir <- file.path(stage, ".builder-auth-source")
  credentials <- file.path(source_dir, "credentials.sqlite")
  env_file <- file.path(stage, "viewer-auth.env")
  source_mode <- tryCatch(
    as.integer(file.info(source_dir)$mode),
    error = function(error) NA_integer_
  )
  credentials_mode <- tryCatch(
    as.integer(file.info(credentials)$mode),
    error = function(error) NA_integer_
  )
  if (
    !identical(material$source_dir, source_dir) ||
      !identical(material$credentials, credentials) ||
      !identical(material$env_file, env_file) ||
      !dir.exists(source_dir) ||
      .builder_auth_path_is_link(source_dir) ||
      !file.exists(credentials) ||
      dir.exists(credentials) ||
      .builder_auth_path_is_link(credentials) ||
      (.Platform$OS.type != "windows" &&
        (!identical(source_mode, 448L) ||
          !identical(credentials_mode, 384L))) ||
      !identical(
        material$descriptor,
        list(
          credentials = credentials,
          passphrase_env = .builder_auth_env_name,
          timeout_minutes = .builder_auth_timeout_minutes
        )
      )
  ) {
    invalid()
  }
  passphrase <- builder_auth_read_env_file(env_file)
  passphrase <- NULL
  material
}

builder_auth_create_material <- function(
  accounts,
  stage,
  .random_bytes = .builderRandomBytes,
  .create_db = shinymanager::create_db,
  .capability = builder_auth_capability,
  .write_env = .builder_auth_write_env,
  .db_chmod = Sys.chmod,
  .validate_material = builder_auth_validate_material,
  .rollback = .builder_auth_remove_partial_material,
  .file_info = file.info
) {
  parsed <- builder_auth_validate_payload(TRUE, accounts)
  if (!isTRUE(parsed$ok)) {
    stop(parsed$error, call. = FALSE)
  }
  capability <- .capability()
  if (!is.list(capability) || !isTRUE(capability$available)) {
    reason <- if (is.list(capability)) capability$reason else NULL
    if (!is.character(reason) || length(reason) != 1L || is.na(reason)) {
      reason <- "Login App creation is unavailable."
    }
    stop(reason, call. = FALSE)
  }
  stage <- .builder_auth_stage(stage)
  source_dir <- file.path(stage, ".builder-auth-source")
  env_file <- file.path(stage, "viewer-auth.env")
  credentials <- file.path(source_dir, "credentials.sqlite")
  completed <- FALSE
  passphrase <- NULL
  credentials_data <- NULL
  on.exit(
    {
      if (is.data.frame(credentials_data)) {
        credentials_data[] <- ""
      }
      accounts <- NULL
      parsed$accounts <- NULL
      credentials_data <- NULL
      passphrase <- NULL
      if (!completed) {
        rolled_back <- try(.rollback(stage), silent = TRUE)
        if (inherits(rolled_back, "try-error") || !isTRUE(rolled_back)) {
          stop(
            "The authentication files could not be cleaned up.",
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )
  if (!dir.create(source_dir, mode = "0700", showWarnings = FALSE)) {
    stop(
      "The private authentication workspace could not be created.",
      call. = FALSE
    )
  }
  if (
    .Platform$OS.type != "windows" &&
      !identical(as.integer(.file_info(source_dir)$mode), 448L)
  ) {
    stop("The private authentication workspace is not private.", call. = FALSE)
  }
  passphrase <- .builder_auth_random_passphrase(.random_bytes)
  credentials_data <- data.frame(
    user = vapply(parsed$accounts, `[[`, character(1), "username"),
    password = vapply(parsed$accounts, `[[`, character(1), "password"),
    stringsAsFactors = FALSE
  )
  captured <- tryCatch(
    {
      invisible(capture.output(
        invisible(capture.output(
          suppressWarnings(suppressMessages(.create_db(
            credentials_data = credentials_data,
            sqlite_path = credentials,
            passphrase = passphrase
          ))),
          type = "message"
        )),
        type = "output"
      ))
      list(ok = TRUE)
    },
    error = function(error) list(ok = FALSE)
  )
  protected <- tryCatch(
    .db_chmod(credentials, mode = "0600", use_umask = FALSE),
    error = function(error) FALSE,
    warning = function(warning) FALSE
  )
  if (
    !isTRUE(captured$ok) ||
      !file.exists(credentials) ||
      dir.exists(credentials) ||
      .builder_auth_path_is_link(credentials) ||
      !isTRUE(protected)
  ) {
    stop(
      "The encrypted authentication database could not be created.",
      call. = FALSE
    )
  }
  if (
    .Platform$OS.type != "windows" &&
      !identical(as.integer(.file_info(credentials)$mode), 384L)
  ) {
    stop("The encrypted authentication database is not private.", call. = FALSE)
  }
  env_file <- tryCatch(
    .write_env(env_file, passphrase),
    error = function(error) NULL,
    warning = function(warning) NULL
  )
  if (!is.character(env_file) || length(env_file) != 1L || is.na(env_file)) {
    stop("The authentication secret file could not be created.", call. = FALSE)
  }
  material <- list(
    source_dir = source_dir,
    credentials = credentials,
    env_file = env_file,
    descriptor = list(
      credentials = credentials,
      passphrase_env = .builder_auth_env_name,
      timeout_minutes = .builder_auth_timeout_minutes
    )
  )
  material <- tryCatch(
    .validate_material(material, stage),
    error = function(error) NULL,
    warning = function(warning) NULL
  )
  if (is.null(material)) {
    stop("The authentication files could not be verified.", call. = FALSE)
  }
  passphrase <- NULL
  completed <- TRUE
  material
}

.builder_auth_with_passphrase <- function(passphrase, action) {
  if (
    !is.character(passphrase) ||
      length(passphrase) != 1L ||
      is.na(passphrase) ||
      !grepl("^[0-9a-f]{64}$", passphrase) ||
      !is.function(action)
  ) {
    stop("The authentication build context is invalid.", call. = FALSE)
  }
  previous <- Sys.getenv(.builder_auth_env_name, unset = NA_character_)
  on.exit(
    {
      if (is.na(previous)) {
        Sys.unsetenv(.builder_auth_env_name)
      } else {
        do.call(
          Sys.setenv,
          stats::setNames(list(previous), .builder_auth_env_name)
        )
      }
      passphrase <- NULL
    },
    add = TRUE
  )
  do.call(Sys.setenv, stats::setNames(list(passphrase), .builder_auth_env_name))
  action()
}

builder_auth_cleanup_material <- function(
  material,
  stage,
  keep_env = FALSE,
  .unlink = unlink
) {
  if (!is.logical(keep_env) || length(keep_env) != 1L || is.na(keep_env)) {
    stop("The authentication cleanup request is invalid.", call. = FALSE)
  }
  stage <- .builder_auth_stage(stage)
  expected_source <- file.path(stage, ".builder-auth-source")
  expected_credentials <- file.path(expected_source, "credentials.sqlite")
  expected_env <- file.path(stage, "viewer-auth.env")
  if (
    !is.list(material) ||
      !identical(
        names(material),
        c("source_dir", "credentials", "env_file", "descriptor")
      ) ||
      !identical(material$source_dir, expected_source) ||
      !identical(material$credentials, expected_credentials) ||
      !identical(material$env_file, expected_env) ||
      !identical(
        material$descriptor,
        list(
          credentials = expected_credentials,
          passphrase_env = .builder_auth_env_name,
          timeout_minutes = .builder_auth_timeout_minutes
        )
      ) ||
      .builder_auth_path_is_link(expected_source) ||
      .builder_auth_path_is_link(expected_credentials) ||
      .builder_auth_path_is_link(expected_env)
  ) {
    stop("The authentication cleanup request is unsafe.", call. = FALSE)
  }
  .builder_auth_remove_path(expected_source, TRUE, .unlink)
  if (!isTRUE(keep_env)) {
    .builder_auth_remove_path(expected_env, FALSE, .unlink)
  }
  invisible(TRUE)
}

builder_auth_verify_database_pair <- function(
  database,
  env_file,
  .validate = NULL
) {
  invalid <- function() {
    stop("The authentication database could not be verified.", call. = FALSE)
  }
  database_is_regular <- function(path) {
    if (
      !is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !nzchar(path) ||
        !file.exists(path) ||
        dir.exists(path) ||
        .builder_auth_path_is_link(path)
    ) {
      return(FALSE)
    }
    info <- tryCatch(
      fs::file_info(path, follow = FALSE),
      error = function(error) NULL
    )
    !is.null(info) &&
      "type" %in% names(info) &&
      identical(as.character(info$type), "file")
  }
  if (is.null(.validate)) {
    .validate <- get0(
      ".viewerAuthValidateDatabase",
      envir = environment(builder_auth_verify_database_pair),
      mode = "function",
      inherits = TRUE
    )
    if (is.null(.validate)) {
      .validate <- tryCatch(
        utils::getFromNamespace(".viewerAuthValidateDatabase", "CerebroNexus"),
        error = function(error) NULL
      )
    }
  }
  if (!is.function(.validate) || !database_is_regular(database)) {
    invalid()
  }
  passphrase <- tryCatch(
    builder_auth_read_env_file(env_file),
    error = function(error) NULL
  )
  on.exit(passphrase <- NULL, add = TRUE)
  valid <- tryCatch(
    !is.null(passphrase) && identical(.validate(database, passphrase), TRUE),
    error = function(error) FALSE,
    warning = function(warning) FALSE
  )
  if (!isTRUE(valid)) {
    invalid()
  }
  TRUE
}
