#!/usr/bin/env Rscript

# One deterministic test plan shared by local precheck and CI.

ci_browser_test_files <- function() {
  c(
    "test-app-immune_repertoire.R",
    "test-app-inst.R",
    "test-app-new-modules.R",
    "test-app-trajectory.R",
    "test-app-viewport-layout.R",
    "test-smoke-production.R"
  )
}

ci_known_process_sensitive_test_files <- function() {
  c("test-builder-worker.R")
}

ci_test_plan <- function(test_dir = file.path("tests", "testthat")) {
  if (!dir.exists(test_dir)) {
    stop("Test directory does not exist: ", test_dir, call. = FALSE)
  }
  all <- sort(list.files(
    test_dir,
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))
  browser <- ci_browser_test_files()
  missing_browser <- setdiff(browser, all)
  if (length(missing_browser)) {
    stop(
      "Classified browser test file(s) do not exist: ",
      paste(missing_browser, collapse = ", "),
      call. = FALSE
    )
  }
  process_sensitive <- intersect(
    ci_known_process_sensitive_test_files(),
    all
  )
  explicit <- c(browser, process_sensitive)
  if (anyDuplicated(explicit)) {
    stop("A test file belongs to more than one explicit group.", call. = FALSE)
  }
  list(
    all = all,
    logic = setdiff(all, explicit),
    process_sensitive = process_sensitive,
    browser = browser
  )
}

ci_stable_hash <- function(value) {
  codes <- utf8ToInt(enc2utf8(value))
  hash <- 0
  for (code in codes) {
    hash <- (hash * 131 + code) %% 2147483647
  }
  hash
}

ci_test_shards <- function(files, shards) {
  if (
    length(shards) != 1L || is.na(shards) || shards < 1L || shards %% 1L != 0L
  ) {
    stop("shards must be one positive integer", call. = FALSE)
  }
  assignments <- lapply(
    seq_len(as.integer(shards)),
    function(index) character()
  )
  for (file in sort(as.character(files))) {
    index <- (ci_stable_hash(file) %% shards) + 1L
    assignments[[index]] <- c(assignments[[index]], file)
  }
  assignments
}

ci_group_key <- function(group) {
  if (identical(group, "process-sensitive")) {
    return("process_sensitive")
  }
  group
}

ci_test_shard_files <- function(plan, group, shard = 1L, shards = 1L) {
  groups <- c("logic", "process-sensitive", "browser")
  if (length(group) != 1L || !group %in% groups) {
    stop(
      "group must be logic, process-sensitive, or browser",
      call. = FALSE
    )
  }
  assignments <- ci_test_shards(plan[[ci_group_key(group)]], shards)
  if (
    length(shard) != 1L ||
      is.na(shard) ||
      shard < 1L ||
      shard %% 1L != 0L ||
      shard > length(assignments)
  ) {
    stop("shard must select one configured shard", call. = FALSE)
  }
  assignments[[as.integer(shard)]]
}

ci_regex_escape <- function(value) {
  special <- c(
    "\\",
    ".",
    "|",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "^",
    "$",
    "*",
    "+",
    "?"
  )
  vapply(
    strsplit(value, "", fixed = TRUE),
    function(characters) {
      paste0(
        ifelse(
          characters %in% special,
          paste0("\\", characters),
          characters
        ),
        collapse = ""
      )
    },
    character(1)
  )
}

ci_run_test_files <- function(files, repo_root = ".") {
  if (!length(files)) {
    message("No test files assigned to this shard")
    return(invisible(files))
  }
  old_options <- options(cerebro.quiet_runtime = TRUE)
  on.exit(options(old_options), add = TRUE)
  devtools::load_all(repo_root, quiet = TRUE)
  contexts <- sub("[.]R$", "", sub("^test-", "", files))
  testthat::test_dir(
    file.path(repo_root, "tests", "testthat"),
    filter = paste0(
      "^(?:",
      paste(ci_regex_escape(contexts), collapse = "|"),
      ")$"
    ),
    reporter = testthat::default_reporter(),
    stop_on_failure = TRUE
  )
  invisible(files)
}

ci_parse_args <- function(args) {
  options <- list(
    group = NULL,
    shard = 1L,
    shards = 1L,
    list = FALSE,
    validate = FALSE
  )
  index <- 1L
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument %in% c("--group", "--shard", "--shards")) {
      if (index == length(args)) {
        stop("Missing value after ", argument, call. = FALSE)
      }
      key <- sub("^--", "", argument)
      value <- args[[index + 1L]]
      options[[key]] <- if (key %in% c("shard", "shards")) {
        suppressWarnings(as.integer(value))
      } else {
        value
      }
      index <- index + 2L
    } else if (identical(argument, "--list")) {
      options$list <- TRUE
      index <- index + 1L
    } else if (identical(argument, "--validate")) {
      options$validate <- TRUE
      index <- index + 1L
    } else {
      stop("Unknown argument: ", argument, call. = FALSE)
    }
  }
  options
}

ci_script_repo_root <- function() {
  file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(file_argument)) {
    return(normalizePath(".", mustWork = TRUE))
  }
  dirname(dirname(normalizePath(
    sub("^--file=", "", file_argument[[1L]]),
    mustWork = TRUE
  )))
}

ci_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- ci_parse_args(args)
  repo_root <- ci_script_repo_root()
  plan <- ci_test_plan(file.path(repo_root, "tests", "testthat"))
  if (isTRUE(options$validate)) {
    message(
      "Validated ",
      length(plan$all),
      " tests: ",
      length(plan$logic),
      " logic, ",
      length(plan$process_sensitive),
      " process-sensitive, ",
      length(plan$browser),
      " browser"
    )
    if (is.null(options$group)) return(invisible(plan))
  }
  if (is.null(options$group)) {
    stop("--group is required unless --validate is used", call. = FALSE)
  }
  files <- ci_test_shard_files(
    plan,
    options$group,
    options$shard,
    options$shards
  )
  if (isTRUE(options$list)) {
    writeLines(files)
    return(invisible(files))
  }
  message(
    "Running ",
    options$group,
    " shard ",
    options$shard,
    "/",
    options$shards,
    " (",
    length(files),
    " files)"
  )
  ci_run_test_files(files, repo_root)
}

if (sys.nframe() == 0L) {
  ci_main()
}
