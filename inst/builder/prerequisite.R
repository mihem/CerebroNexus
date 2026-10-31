##----------------------------------------------------------------------------##
## Gate app publication on the installed viewer's private-data contract.
##----------------------------------------------------------------------------##

builder_source_utf8 <- function(file, envir = parent.frame(), chdir = FALSE) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    stop("A readable UTF-8 runtime source file is required.", call. = FALSE)
  }
  if (!is.environment(envir)) {
    stop("A runtime source environment is required.", call. = FALSE)
  }
  ofile <- normalizePath(file, winslash = "/", mustWork = TRUE)
  lines <- readLines(ofile, encoding = "UTF-8", warn = FALSE)
  if (length(lines)) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }
  expressions <- parse(
    text = lines,
    srcfile = srcfilecopy(ofile, lines, isFile = TRUE),
    encoding = "UTF-8",
    keep.source = FALSE
  )
  previous <- NULL
  if (isTRUE(chdir)) {
    previous <- getwd()
    setwd(dirname(ofile))
    on.exit(setwd(previous), add = TRUE)
  }
  value <- NULL
  for (index in seq_along(expressions)) {
    value <- eval(expressions[[index]], envir = envir)
  }
  invisible(value)
}
builder_source_utf8 <- utils::removeSource(builder_source_utf8)
environment(builder_source_utf8) <- baseenv()

builder_installed_app_contract_version <- function(namespace = NULL) {
  if (is.null(namespace)) {
    namespace <- tryCatch(
      asNamespace("CerebroNexus"),
      error = function(e) NULL
    )
  }
  if (!is.environment(namespace)) {
    return(0L)
  }

  marker <- ".cerebro_bundle_privacy_contract_version"
  if (!exists(marker, envir = namespace, inherits = FALSE)) {
    return(0L)
  }
  if (bindingIsActive(marker, namespace)) {
    return(0L)
  }
  is_lazy <- tryCatch(
    isTRUE(unname(rlang::env_binding_are_lazy(namespace, marker))),
    error = function(e) TRUE
  )
  if (is_lazy) {
    return(0L)
  }
  if (!bindingIsLocked(marker, namespace)) {
    return(0L)
  }
  version <- tryCatch(
    get(marker, envir = namespace, inherits = FALSE),
    error = function(e) NULL
  )
  if (identical(version, 1L)) 1L else 0L
}

builder_source_package_root <- function(
  candidates = c(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = ""),
    file.path("..", ".."),
    file.path("..")
  )
) {
  candidates <- unique(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "DESCRIPTION")) &&
        dir.exists(file.path(candidate, "R")) &&
        length(list.files(
          file.path(candidate, "R"),
          pattern = "[.][Rr]$"
        )) >
          0L
    ) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  NULL
}

builder_runtime_package_version <- function(
  source_root = builder_source_package_root(),
  .installed_version = function() {
    as.character(utils::packageVersion("CerebroNexus"))
  }
) {
  version <- NULL
  if (!is.null(source_root)) {
    version <- tryCatch(
      read.dcf(
        file.path(source_root, "DESCRIPTION"),
        fields = "Version"
      )[[1L]],
      error = function(error) NULL
    )
  }
  valid <- is.character(version) &&
    length(version) == 1L &&
    !is.na(version) &&
    grepl("^[0-9]+([.][0-9]+)+$", version)
  if (isTRUE(valid)) {
    return(version)
  }
  version <- tryCatch(.installed_version(), error = function(error) NULL)
  valid <- is.character(version) &&
    length(version) == 1L &&
    !is.na(version) &&
    grepl("^[0-9]+([.][0-9]+)+$", version)
  if (isTRUE(valid)) version else NULL
}

builder_source_app_contract_version <- function(
  source_root = builder_source_package_root()
) {
  if (is.null(source_root)) {
    return(NULL)
  }
  marker <- file.path(
    source_root,
    "inst",
    "builder",
    "app_bundle",
    "privacy-contract-version"
  )
  value <- tryCatch(
    readLines(marker, warn = FALSE, n = 2L),
    error = function(e) character()
  )
  if (identical(value, "1")) 1L else 0L
}

builder_activate_source_package <- function(source_root = NULL) {
  source_root <- if (is.null(source_root)) {
    builder_source_package_root()
  } else {
    builder_source_package_root(source_root)
  }
  if (is.null(source_root)) {
    return(invisible(NULL))
  }
  Sys.setenv(CEREBRO_PACKAGE_SOURCE = source_root)
  invisible(source_root)
}

builder_app_capability <- function(
  installed_contract_version = builder_installed_app_contract_version(),
  source_contract_version = builder_source_app_contract_version()
) {
  contract_version <- if (is.null(source_contract_version)) {
    installed_contract_version
  } else {
    source_contract_version
  }
  version <- if (identical(contract_version, 1L)) 1L else 0L
  available <- identical(version, 1L)
  list(
    available = available,
    version = version,
    reason = if (available) {
      NULL
    } else {
      "This Builder cannot find a secure Viewer App export runtime."
    }
  )
}

.builder_prerequisite_install_command <- function(requirements) {
  packages <- unique(sub("[[:space:]]*\\(.*$", "", requirements))
  quoted <- paste0('"', packages, '"')
  argument <- if (length(quoted) == 1L) {
    quoted
  } else {
    paste0("c(", paste(quoted, collapse = ", "), ")")
  }
  paste0("install.packages(", argument, ")")
}

.builder_prerequisite_requirements <- function(value) {
  value <- trimws(as.character(value))
  value <- value[!is.na(value) & nzchar(value)]
  value <- value[!grepl("^R([[:space:]]*\\(|$)", value)]
  if (!length(value)) {
    return(character())
  }
  packages <- sub("[[:space:]]*\\(.*$", "", value)
  value[!duplicated(packages)]
}

## Only packages needed to paint the Builder and operate its background worker
## belong in the synchronous startup gate. Analysis, import, and publication
## dependencies are checked in the worker when the corresponding operation is
## requested; loading every DESCRIPTION import here makes optional Bioconductor
## features delay the first page even when they are never used.
builder_startup_package_requirements <- function() {
  c(
    "callr",
    "jsonlite",
    "later",
    "processx",
    "ps",
    "shiny"
  )
}

## Build preflight is intentionally a capability inventory, not a mirror of
## DESCRIPTION.  Generated Viewers keep feature packages lazy (for example,
## scRepertoire is loaded only when an immune-repertoire view is rendered), so
## requiring every package import here would block unrelated builds.  Keep the
## unconditional list to packages used by orchestration and publication; add
## data- and feature-specific packages from the frozen plan below.
builder_build_base_package_requirements <- function() {
  c(
    builder_startup_package_requirements(),
    "fs",
    "digest"
  )
}

.builder_prerequisite_requirement <- function(requirement) {
  match <- regexec(
    paste0(
      "^([[:alnum:].]+)[[:space:]]*",
      "(?:\\((>=|<=|==|>|<)[[:space:]]*([^)]+)\\))?$"
    ),
    requirement,
    perl = TRUE
  )
  fields <- regmatches(requirement, match)[[1L]]
  if (!length(fields)) {
    stop("Invalid Builder package requirement: ", requirement, call. = FALSE)
  }
  list(
    label = requirement,
    package = fields[[2L]],
    operator = if (length(fields) >= 3L) fields[[3L]] else "",
    version = if (length(fields) >= 4L) trimws(fields[[4L]]) else ""
  )
}

.builder_prerequisite_problem <- function(error) {
  message <- tryCatch(conditionMessage(error), error = function(e) "")
  message <- trimws(gsub("[[:space:]]+", " ", message))
  if (nzchar(message)) message else "could not be loaded"
}

.builder_prerequisite_check <- function(
  requirement,
  .load,
  .version
) {
  parsed <- .builder_prerequisite_requirement(requirement)
  loaded <- tryCatch(.load(parsed$package), error = identity)
  if (inherits(loaded, "condition")) {
    return(c(
      parsed,
      list(
        ok = FALSE,
        problem = .builder_prerequisite_problem(loaded)
      )
    ))
  }
  if (identical(loaded, FALSE)) {
    return(c(
      parsed,
      list(
        ok = FALSE,
        problem = "package is not installed or cannot be loaded"
      )
    ))
  }
  if (!nzchar(parsed$operator)) {
    return(c(parsed, list(ok = TRUE, problem = NULL)))
  }
  installed <- tryCatch(.version(parsed$package), error = identity)
  if (inherits(installed, "condition")) {
    return(c(
      parsed,
      list(
        ok = FALSE,
        problem = .builder_prerequisite_problem(installed)
      )
    ))
  }
  required <- tryCatch(
    base::package_version(parsed$version),
    error = identity
  )
  installed <- tryCatch(base::package_version(installed), error = identity)
  if (inherits(required, "condition") || inherits(installed, "condition")) {
    return(c(
      parsed,
      list(
        ok = FALSE,
        problem = "package version could not be verified"
      )
    ))
  }
  supported <- switch(
    parsed$operator,
    `>=` = installed >= required,
    `<=` = installed <= required,
    `==` = installed == required,
    `>` = installed > required,
    `<` = installed < required,
    FALSE
  )
  c(
    parsed,
    list(
      ok = isTRUE(supported),
      problem = if (isTRUE(supported)) {
        NULL
      } else {
        paste0(
          "installed version ",
          as.character(installed),
          " does not satisfy ",
          parsed$operator,
          " ",
          parsed$version
        )
      }
    )
  )
}

builder_dependency_capability <- function(
  requirements,
  context = "Builder cannot start",
  .load = function(package) loadNamespace(package),
  .version = function(package) utils::packageVersion(package)
) {
  requirements <- .builder_prerequisite_requirements(requirements)
  checks <- lapply(
    requirements,
    .builder_prerequisite_check,
    .load = .load,
    .version = .version
  )
  failed <- checks[!vapply(checks, `[[`, logical(1), "ok")]
  missing <- vapply(failed, `[[`, character(1), "label")
  details <- if (length(failed)) {
    vapply(
      failed,
      function(check) paste0(check$label, ": ", check$problem),
      character(1)
    )
  } else {
    character()
  }
  list(
    available = !length(failed),
    missing = missing,
    failures = details,
    reason = if (length(failed)) {
      paste0(
        context,
        " because required R dependencies failed to load:\n- ",
        paste(details, collapse = "\n- "),
        "\nRepair or install the listed packages in this R library, then retry."
      )
    } else {
      NULL
    }
  )
}

builder_runtime_capability <- function(
  .available = function(package) loadNamespace(package),
  .version = function(package) utils::packageVersion(package),
  requirements = builder_startup_package_requirements()
) {
  builder_dependency_capability(
    requirements,
    context = "Builder cannot start",
    .load = .available,
    .version = .version
  )
}

builder_build_package_requirements <- function(plan) {
  required <- builder_build_base_package_requirements()
  items <- if (is.list(plan) && is.list(plan$items)) plan$items else list()
  materialized <- Filter(
    function(item) is.list(item) && !is.list(item$reused_artifact),
    items
  )
  if (length(materialized)) {
    required <- c(required, "Seurat (>= 3.0.0)", "SeuratObject")
  }

  backends <- unlist(
    lapply(materialized, function(item) {
      as.character(item$expression_backend)
    }),
    use.names = FALSE
  )
  if ("h5" %in% backends) {
    required <- c(required, "HDF5Array (>= 1.18.1)", "DelayedArray")
  }

  serializations <- unlist(
    lapply(materialized, function(item) {
      identity <- item$source_snapshot_identity
      snapshot <- if (is.list(identity)) identity$snapshot else NULL
      if (is.list(snapshot)) snapshot$serialization else NULL
    }),
    use.names = FALSE
  )
  if ("qs" %in% serializations) {
    required <- c(required, "qs")
  }
  if ("qs2" %in% serializations) {
    required <- c(required, "qs2")
  }

  if (
    is.list(plan) && is.list(plan$app_auth) && isTRUE(plan$app_auth$enabled)
  ) {
    required <- c(required, "shinymanager (>= 1.1.0)")
  }
  .builder_prerequisite_requirements(required)
}

builder_build_dependency_capability <- function(
  plan,
  .load = function(package) loadNamespace(package),
  .version = function(package) utils::packageVersion(package)
) {
  builder_dependency_capability(
    builder_build_package_requirements(plan),
    context = "Build cannot start",
    .load = .load,
    .version = .version
  )
}

builder_auth_capability <- function(
  .available = function(package) loadNamespace(package),
  .version = function(package) utils::packageVersion(package)
) {
  available <- function(package) {
    isTRUE(tryCatch(
      {
        loaded <- .available(package)
        !identical(loaded, FALSE)
      },
      error = function(error) FALSE
    ))
  }
  manager_available <- available("shinymanager")
  manager_version <- if (manager_available) {
    try(.version("shinymanager"), silent = TRUE)
  } else {
    NULL
  }
  manager_supported <- manager_available &&
    !inherits(manager_version, "try-error") &&
    isTRUE(manager_version >= base::package_version("1.1.0"))
  missing <- c(
    character(),
    if (!manager_supported) "shinymanager (>= 1.1.0)"
  )
  list(
    available = !length(missing),
    missing = missing,
    reason = if (length(missing)) {
      paste(
        paste0(
          "Login is unavailable because ",
          if (length(missing) == 1L) {
            "this required R package is missing or too old: "
          } else {
            "these required R packages are missing or too old: "
          },
          paste(missing, collapse = ", "),
          "."
        ),
        paste0(
          "Run ",
          .builder_prerequisite_install_command(missing),
          ", then restart Builder."
        )
      )
    } else {
      NULL
    }
  )
}
