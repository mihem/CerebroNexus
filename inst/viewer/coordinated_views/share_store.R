##----------------------------------------------------------------------------##
## Persistent storage for expiring linked-view share records.
##
## This file is sourced by the Viewer, not the package namespace. Keep its
## surface small: it receives canonical JSON and returns only opaque tokens.
##----------------------------------------------------------------------------##

CV_SHARE_TOKEN_BYTES <- 32L
CV_SHARE_TOKEN_CHARS <- 43L
CV_SHARE_DEFAULT_TTL_SECONDS <- 7L * 24L * 60L * 60L
CV_SHARE_HISTORY_SECONDS <- 30L * 24L * 60L * 60L
CV_SHARE_MAX_BYTES <- 5L * 1024L * 1024L
CV_SHARE_RATE_WINDOW_SECONDS <- 60L
CV_SHARE_RATE_LIMIT <- 20L
CV_SHARE_MAX_ACTIVE_PER_CREATOR <- 50L
CV_SHARE_MAX_RECORDS <- 1000L
CV_SHARE_MAX_STORE_BYTES <- 256L * 1024L * 1024L

cv_share_abort <- function(code, message) {
  stop(structure(
    list(message = message, call = NULL, code = code),
    class = c("cv_share_error", "error", "condition")
  ))
}

cv_share_time <- function(value = Sys.time()) {
  if (!inherits(value, "POSIXt") || length(value) != 1L || is.na(value)) {
    cv_share_abort("internal", "The share timestamp is invalid.")
  }
  format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

cv_share_token <- function() {
  token <- openssl::base64_encode(openssl::rand_bytes(CV_SHARE_TOKEN_BYTES))
  token <- chartr("+/", "-_", sub("=+$", "", token))
  if (!identical(nchar(token, type = "bytes"), CV_SHARE_TOKEN_CHARS)) {
    cv_share_abort("internal", "The share token could not be created.")
  }
  token
}

cv_share_token_input <- function(value, field) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !grepl("^[A-Za-z0-9_-]{43}$", value)
  ) {
    cv_share_abort("invalid_token", paste0("The share ", field, " is invalid."))
  }
  value
}

cv_share_namespace_input <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value) ||
      nchar(enc2utf8(value), type = "bytes") > 200L
  ) {
    cv_share_abort("share_unavailable", "The Share namespace is invalid.")
  }
  value
}

cv_share_namespace <- function(root, configured = "") {
  if (
    is.character(configured) && length(configured) == 1L && nzchar(configured)
  ) {
    return(cv_share_namespace_input(configured))
  }
  if (
    !is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)
  ) {
    cv_share_abort("share_unavailable", "The Share namespace is unavailable.")
  }
  path <- normalizePath(root, winslash = "/", mustWork = FALSE)
  paste0(
    "path-",
    as.character(openssl::sha256(charToRaw(enc2utf8(path))))
  )
}

cv_share_creator_id <- function(
  user = NULL,
  remote_addr = NULL,
  session_token = NULL
) {
  scalar_text <- function(value) {
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    ) {
      enc2utf8(value)
    } else {
      ""
    }
  }
  user <- scalar_text(user)
  if (nzchar(user)) {
    return(paste0("user:", user))
  }
  identity <- scalar_text(remote_addr)
  prefix <- "client:"
  if (!nzchar(identity)) {
    identity <- scalar_text(session_token)
    prefix <- "session:"
  }
  if (!nzchar(identity)) {
    return("anonymous")
  }
  paste0(prefix, as.character(openssl::sha256(charToRaw(identity))))
}

cv_share_store_open <- function(path, namespace) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    cv_share_abort("share_unavailable", "Share links are not configured here.")
  }
  namespace <- cv_share_namespace_input(namespace)
  parent <- dirname(path)
  private_umask <- !identical(.Platform$OS.type, "windows")
  old_umask <- if (private_umask) Sys.umask("0077") else NULL
  on.exit(
    {
      if (private_umask) Sys.umask(old_umask)
    },
    add = TRUE
  )
  dir.create(
    parent,
    recursive = TRUE,
    showWarnings = FALSE,
    mode = "0700"
  )
  if (!dir.exists(parent)) {
    cv_share_abort("share_unavailable", "Share links are unavailable here.")
  }
  con <- NULL
  opened <- FALSE
  on.exit(
    {
      if (!opened && !is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path),
    error = function(error) {
      cv_share_abort("share_unavailable", "Share links are unavailable here.")
    }
  )
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  try(DBI::dbGetQuery(con, "PRAGMA journal_mode = TRUNCATE"), silent = TRUE)
  create_table <- paste(
    "CREATE TABLE IF NOT EXISTS linked_view_shares (",
    "app_namespace TEXT NOT NULL, token TEXT PRIMARY KEY,",
    "json TEXT NOT NULL, fingerprint TEXT NOT NULL,",
    "created_at TEXT NOT NULL, expires_at TEXT NOT NULL, revoked_at TEXT,",
    "creator TEXT NOT NULL, dataset_label TEXT NOT NULL)"
  )
  DBI::dbExecute(con, create_table)
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS linked_view_shares_expiry_v2",
      "ON linked_view_shares (app_namespace, expires_at)"
    )
  )
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS linked_view_shares_creator_v2",
      "ON linked_view_shares (app_namespace, creator, created_at)"
    )
  )
  if (private_umask) {
    private_files <- c(path, paste0(path, c("-journal", "-wal", "-shm")))
    private_files <- private_files[file.exists(private_files)]
    if (
      length(private_files) &&
        !all(vapply(private_files, Sys.chmod, logical(1), mode = "0600"))
    ) {
      cv_share_abort("share_unavailable", "Share storage is not private.")
    }
  }
  opened <- TRUE
  structure(
    list(
      con = con,
      namespace = namespace,
      path = normalizePath(path, winslash = "/", mustWork = FALSE)
    ),
    class = "cv_share_store"
  )
}

cv_share_store_cleanup <- function(store, now = Sys.time()) {
  cutoff <- cv_share_time(
    as.POSIXct(now, tz = "UTC") - CV_SHARE_HISTORY_SECONDS
  )
  DBI::dbExecute(
    store$con,
    paste(
      "DELETE FROM linked_view_shares WHERE app_namespace = ? AND (",
      "(revoked_at IS NOT NULL AND revoked_at <= ?) OR",
      "(revoked_at IS NULL AND expires_at <= ?))"
    ),
    params = list(store$namespace, cutoff, cutoff)
  )
  invisible(NULL)
}

cv_share_store_transaction <- function(store, code) {
  DBI::dbExecute(store$con, "BEGIN IMMEDIATE")
  committed <- FALSE
  on.exit(
    {
      if (!committed && DBI::dbIsValid(store$con)) {
        try(DBI::dbExecute(store$con, "ROLLBACK"), silent = TRUE)
      }
    },
    add = TRUE
  )
  value <- force(code)
  DBI::dbExecute(store$con, "COMMIT")
  committed <- TRUE
  value
}

cv_share_store_check_limits <- function(store, creator, now, incoming_bytes) {
  now_text <- cv_share_time(now)
  cutoff <- cv_share_time(
    as.POSIXct(now, tz = "UTC") - CV_SHARE_RATE_WINDOW_SECONDS
  )
  recent <- DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT COUNT(*) AS n FROM linked_view_shares",
      "WHERE app_namespace = ? AND creator = ? AND created_at >= ?"
    ),
    params = list(store$namespace, creator, cutoff)
  )$n[[1L]]
  creator_records <- DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT COUNT(*) AS n FROM linked_view_shares",
      "WHERE app_namespace = ? AND creator = ?",
      "AND revoked_at IS NULL AND expires_at > ?"
    ),
    params = list(store$namespace, creator, now_text)
  )$n[[1L]]
  records <- DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT COUNT(*) AS n FROM linked_view_shares",
      "WHERE app_namespace = ? AND revoked_at IS NULL AND expires_at > ?"
    ),
    params = list(store$namespace, now_text)
  )$n[[1L]]
  payload_bytes <- DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT COALESCE(SUM(length(CAST(json AS BLOB))), 0) AS n",
      "FROM linked_view_shares WHERE app_namespace = ?",
      "AND revoked_at IS NULL AND expires_at > ?"
    ),
    params = list(store$namespace, now_text)
  )$n[[1L]]
  if (
    recent >= CV_SHARE_RATE_LIMIT ||
      creator_records >= CV_SHARE_MAX_ACTIVE_PER_CREATOR ||
      records >= CV_SHARE_MAX_RECORDS ||
      as.double(payload_bytes) + as.double(incoming_bytes) >
        CV_SHARE_MAX_STORE_BYTES
  ) {
    cv_share_abort(
      "share_limit",
      "Share creation is temporarily limited. Try again later."
    )
  }
  invisible(NULL)
}

cv_share_store_create <- function(
  store,
  json,
  fingerprint,
  now = Sys.time(),
  creator = "",
  dataset_label = "",
  ttl_seconds = CV_SHARE_DEFAULT_TTL_SECONDS
) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  if (
    !is.character(json) ||
      length(json) != 1L ||
      is.na(json) ||
      nchar(enc2utf8(json), type = "bytes") > CV_SHARE_MAX_BYTES
  ) {
    cv_share_abort("invalid_config", "The shared configuration is invalid.")
  }
  if (
    !is.character(fingerprint) ||
      length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !nzchar(fingerprint)
  ) {
    cv_share_abort("invalid_dataset", "The shared cell population is invalid.")
  }
  if (
    !is.numeric(ttl_seconds) ||
      length(ttl_seconds) != 1L ||
      is.na(ttl_seconds) ||
      !is.finite(ttl_seconds) ||
      ttl_seconds <= 0 ||
      ttl_seconds != floor(ttl_seconds)
  ) {
    cv_share_abort("internal", "The share retention period is invalid.")
  }
  audit_text <- function(value, field, maximum) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(enc2utf8(value), type = "bytes") > maximum
    ) {
      cv_share_abort(
        "invalid_audit",
        paste0("The share ", field, " is invalid.")
      )
    }
    value
  }
  creator <- audit_text(creator, "creator", 200L)
  dataset_label <- audit_text(dataset_label, "dataset label", 500L)
  json_bytes <- nchar(enc2utf8(json), type = "bytes")
  created_at <- cv_share_time(now)
  expires_at <- cv_share_time(
    as.POSIXct(now, tz = "UTC") + ttl_seconds
  )
  for (attempt in seq_len(3L)) {
    token <- cv_share_token()
    written <- tryCatch(
      {
        cv_share_store_transaction(store, {
          cv_share_store_cleanup(store, now)
          cv_share_store_check_limits(store, creator, now, json_bytes)
          DBI::dbExecute(
            store$con,
            paste(
              "INSERT INTO linked_view_shares",
              "(app_namespace, token, json, fingerprint, created_at,",
              "expires_at, revoked_at, creator, dataset_label)",
              "VALUES (?, ?, ?, ?, ?, ?, NULL, ?, ?)"
            ),
            params = list(
              store$namespace,
              token,
              json,
              fingerprint,
              created_at,
              expires_at,
              creator,
              dataset_label
            )
          )
        })
        TRUE
      },
      error = function(error) {
        if (inherits(error, "cv_share_error")) {
          stop(error)
        }
        FALSE
      }
    )
    if (isTRUE(written)) {
      return(list(token = token, expires_at = expires_at))
    }
  }
  cv_share_abort(
    "internal",
    "The share link could not be created. Try again."
  )
}

cv_share_store_list <- function(store, now = Sys.time()) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  cv_share_store_cleanup(store, now)
  DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT token, fingerprint, dataset_label, creator, created_at,",
      "expires_at, revoked_at FROM linked_view_shares",
      "WHERE app_namespace = ?",
      "ORDER BY created_at DESC"
    ),
    params = list(store$namespace)
  )
}

cv_share_store_fetch <- function(store, token, fingerprint, now = Sys.time()) {
  token <- cv_share_token_input(token, "link")
  row <- DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT json, fingerprint, revoked_at FROM linked_view_shares",
      "WHERE app_namespace = ? AND token = ? AND expires_at > ?"
    ),
    params = list(store$namespace, token, cv_share_time(now))
  )
  if (!nrow(row)) {
    cv_share_abort(
      "share_unavailable",
      "This share link is unavailable or has expired."
    )
  }
  if (!is.na(row$revoked_at[[1L]])) {
    cv_share_abort("share_revoked", "This share link has been revoked.")
  }
  if (!identical(row$fingerprint[[1L]], fingerprint)) {
    cv_share_abort(
      "dataset_mismatch",
      "This configuration belongs to a different cell population."
    )
  }
  list(json = row$json[[1L]])
}

cv_share_store_revoke_admin <- function(store, token, now = Sys.time()) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  token <- cv_share_token_input(token, "link")
  cv_share_store_cleanup(store, now)
  revoked_at <- cv_share_time(now)
  changed <- DBI::dbExecute(
    store$con,
    paste(
      "UPDATE linked_view_shares SET revoked_at = ?",
      "WHERE app_namespace = ? AND token = ? AND revoked_at IS NULL"
    ),
    params = list(revoked_at, store$namespace, token)
  )
  if (!identical(as.integer(changed), 1L)) {
    cv_share_abort(
      "share_revoke_denied",
      "This share link is unavailable or already revoked."
    )
  }
  revoked_at
}
