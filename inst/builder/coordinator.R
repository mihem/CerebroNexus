##----------------------------------------------------------------------------##
## Parent-side ownership of Builder stages and release publication.

builder_release_runtime_files <- function() {
  roots <- unique(c(
    getwd(),
    file.path(getwd(), "inst", "builder"),
    system.file("builder", package = "CerebroNexus")
  ))
  roots <- roots[nzchar(roots)]
  required <- c(
    contract = file.path("core", "bundle_path_contract.R"),
    viewer_contract = file.path(
      "..",
      "viewer",
      "core",
      "viewer_content_contract.R"
    ),
    publish = "publish.R",
    app_bundle = "app_bundle.R",
    report = "report.R",
    coordinator = "coordinator.R"
  )
  usable <- vapply(
    roots,
    function(root) all(file.exists(file.path(root, required))),
    logical(1)
  )
  if (!any(usable)) {
    stop("The release runtime is unavailable.", call. = FALSE)
  }
  root <- normalizePath(roots[usable][[1L]], winslash = "/", mustWork = TRUE)
  c(
    list(root = root),
    as.list(stats::setNames(file.path(root, required), names(required)))
  )
}

## `.previous` is an earlier list(identity, stats) of the same runtime: files
## whose size and modification time are unchanged keep their MD5, so a Build
## hashes the ~250 runtime files once per session instead of once per
## process. `.stats` (an environment) receives the stats for the next call.
builder_runtime_file_identity <- function(
  paths,
  .md5 = tools::md5sum,
  .previous = NULL,
  .stats = NULL
) {
  paths <- if (is.null(paths)) character() else paths
  paths <- as.character(paths)
  if (!length(paths) || anyNA(paths) || any(!nzchar(paths))) {
    stop("The Builder runtime file set is empty or invalid.", call. = FALSE)
  }
  # Resolve aliases before ordering/deduplicating. Parent and worker may reach
  # the same installed runtime through different symlink roots.
  paths <- sort(
    unique(normalizePath(paths, winslash = "/", mustWork = FALSE)),
    method = "radix"
  )
  if (
    any(!file.exists(paths)) ||
      any(dir.exists(paths)) ||
      any(!utils::file_test("-f", paths))
  ) {
    stop("The Builder runtime file set is incomplete.", call. = FALSE)
  }
  before <- file.info(paths)[, c("size", "mtime"), drop = FALSE]
  digests <- rep(NA_character_, length(paths))
  previous_files <- if (is.list(.previous)) .previous$identity$files else NULL
  previous_stats <- if (is.list(.previous)) .previous$stats else NULL
  if (
    is.list(previous_files) &&
      is.list(previous_stats) &&
      is.character(previous_stats$paths)
  ) {
    previous_paths <- vapply(
      previous_files,
      function(file) {
        if (is.character(file$path) && length(file$path) == 1L) {
          file$path
        } else {
          NA_character_
        }
      },
      character(1)
    )
    file_index <- match(paths, previous_paths)
    stat_index <- match(paths, previous_stats$paths)
    unchanged <- !is.na(file_index) &
      !is.na(stat_index) &
      as.double(before$size) == as.double(previous_stats$size)[stat_index] &
      as.double(before$mtime) == as.double(previous_stats$mtime)[stat_index]
    unchanged[is.na(unchanged)] <- FALSE
    for (index in which(unchanged)) {
      previous <- previous_files[[file_index[[index]]]]
      if (
        is.character(previous$md5) &&
          length(previous$md5) == 1L &&
          grepl("^[[:xdigit:]]{32}$", previous$md5) &&
          identical(as.double(previous$bytes), as.double(before$size[[index]]))
      ) {
        digests[[index]] <- previous$md5
      }
    }
  }
  missing <- is.na(digests)
  if (any(missing)) {
    digests[missing] <- unname(as.character(.md5(paths[missing])))
  }
  after <- file.info(paths)[, c("size", "mtime"), drop = FALSE]
  if (is.environment(.stats)) {
    assign(
      "stats",
      list(
        paths = paths,
        size = as.double(after$size),
        mtime = as.double(after$mtime)
      ),
      envir = .stats
    )
  }
  stable <- identical(
    suppressWarnings(as.double(before$size)),
    suppressWarnings(as.double(after$size))
  ) &&
    identical(
      suppressWarnings(as.double(before$mtime)),
      suppressWarnings(as.double(after$mtime))
    )
  if (
    !isTRUE(stable) ||
      length(digests) != length(paths) ||
      anyNA(digests) ||
      any(!grepl("^[[:xdigit:]]{32}$", digests))
  ) {
    stop(
      "The Builder runtime changed while its identity was captured.",
      call. = FALSE
    )
  }
  list(
    schema_version = 1L,
    files = Map(
      function(path, bytes, md5) {
        list(path = path, bytes = as.double(bytes), md5 = tolower(md5))
      },
      paths,
      after$size,
      digests
    )
  )
}
environment(builder_runtime_file_identity) <- baseenv()

builder_runtime_assert_identity <- function(
  expected,
  current,
  context = "parent verification"
) {
  valid <- function(identity) {
    is.list(identity) &&
      identical(identity$schema_version, 1L) &&
      is.list(identity$files) &&
      length(identity$files) > 0L &&
      all(vapply(
        identity$files,
        function(file) {
          is.list(file) &&
            is.character(file$path) &&
            length(file$path) == 1L &&
            !is.na(file$path) &&
            nzchar(file$path) &&
            is.numeric(file$bytes) &&
            length(file$bytes) == 1L &&
            is.finite(file$bytes) &&
            is.character(file$md5) &&
            length(file$md5) == 1L &&
            grepl("^[[:xdigit:]]{32}$", file$md5)
        },
        logical(1)
      ))
  }
  if (!valid(expected) || !valid(current)) {
    stop("The Builder runtime identity is invalid.", call. = FALSE)
  }
  if (identical(expected, current)) {
    return(invisible(TRUE))
  }
  keyed <- function(identity) {
    paths <- vapply(identity$files, `[[`, character(1), "path")
    stats::setNames(identity$files, paths)
  }
  expected_files <- keyed(expected)
  current_files <- keyed(current)
  paths <- union(names(expected_files), names(current_files))
  changed <- paths[vapply(
    paths,
    function(path) {
      !identical(expected_files[[path]], current_files[[path]])
    },
    logical(1)
  )]
  labels <- if (length(changed)) {
    paste(utils::head(changed, 5L), collapse = ", ")
  } else {
    "unknown runtime files"
  }
  stop(
    "The Builder runtime changed before ",
    context,
    ". Nothing was published. Changed files: ",
    labels,
    call. = FALSE
  )
}
environment(builder_runtime_assert_identity) <- baseenv()
##----------------------------------------------------------------------------##

.builder_coordinator_freeze <- function(value) {
  if (.builder_app_has_reference(value)) {
    stop("The App publication expectation is not inert.", call. = FALSE)
  }
  tryCatch(
    unserialize(serialize(value, NULL, version = 3L)),
    error = function(error) {
      stop(
        "The App publication expectation could not be frozen.",
        call. = FALSE
      )
    }
  )
}

.builder_coordinator_report_plan <- function(plan) {
  app_auth <- .subset2(plan, "app_auth")
  items <- lapply(.subset2(plan, "items"), function(item) {
    list(
      id = .subset2(item, "id"),
      name = .subset2(item, "name"),
      filename = .subset2(item, "filename"),
      organism = .subset2(item, "organism") %||% NULL,
      analyses = .subset2(item, "analyses") %||% character(),
      included_groups = .subset2(item, "included_groups") %||% character(),
      included_projections = .subset2(item, "included_projections") %||%
        character(),
      metadata_policy = {
        policy <- .subset2(item, "metadata_policy") %||% list()
        retained <- .subset2(policy, "retained") %||%
          .subset2(policy, "included") %||%
          character()
        list(
          retained = retained,
          included = retained,
          excluded = .subset2(policy, "excluded") %||% character(),
          forced = .subset2(policy, "forced") %||% character()
        )
      },
      expression_backend = .subset2(item, "expression_backend"),
      spatial_image_storage = "external",
      spatial_alignment = {
        alignment <- .subset2(item, "spatial_alignment") %||% list()
        list(
          section_count = as.integer(
            .subset2(alignment, "section_count") %||% 0L
          ),
          image_count = as.integer(
            .subset2(alignment, "image_count") %||% 0L
          )
        )
      },
      sidecars = .subset2(item, "sidecars") %||% character(),
      viewer_page_expectations = list(
        visible_conditional = .subset2(
          .subset2(item, "viewer_page_expectations") %||% list(),
          "visible_conditional"
        ) %||%
          character()
      )
    )
  })
  manifest <- lapply(.subset2(plan, "manifest") %||% list(), function(entry) {
    list(
      status = .subset2(entry, "status") %||% NULL,
      disposition = .subset2(entry, "disposition") %||% NULL,
      pages = .subset2(entry, "pages") %||% character()
    )
  })
  projected <- structure(
    list(
      revision = .subset2(plan, "revision"),
      readiness = .subset2(plan, "readiness"),
      out_dir = .subset2(plan, "out_dir"),
      make_app = .subset2(plan, "make_app"),
      dataset_order = .subset2(plan, "dataset_order"),
      app_auth = list(
        enabled = isTRUE(.subset2(app_auth, "enabled")),
        account_count = as.integer(.subset2(app_auth, "account_count")),
        timeout_minutes = 15L
      ),
      items = items,
      manifest = manifest,
      acknowledgements = as.character(unique(.builder_report_strings(
        .subset2(plan, "acknowledgements") %||% list()
      ))),
      viewer_bundle_assets = .subset2(plan, "viewer_bundle_assets") %||%
        character(),
      private_assets = .subset2(plan, "private_assets") %||% character(),
      output_release = list(
        targets = .subset2(
          .subset2(plan, "output_release") %||% list(),
          "targets"
        ) %||%
          .subset2(plan, "targets") %||%
          character()
      )
    ),
    class = c("builder_build_plan", "list")
  )
  .builder_coordinator_freeze(projected)
}

.builder_coordinator_app_contract <- function(plan) {
  plan_class <- attr(plan, "class", exact = TRUE)
  if (
    !identical(typeof(plan), "list") ||
      (!is.null(plan_class) &&
        !identical(plan_class, c("builder_build_plan", "list")))
  ) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  app_auth <- .subset2(plan, "app_auth")
  if (
    !.builder_app_auth_summary_valid(app_auth) ||
      (!isTRUE(.subset2(plan, "make_app")) &&
        isTRUE(.subset2(app_auth, "enabled")))
  ) {
    stop("The App publication expectation is invalid.", call. = FALSE)
  }
  if (!isTRUE(.subset2(plan, "make_app"))) {
    return(list(
      plan = NULL,
      expectation = list(expected = FALSE)
    ))
  }
  if (!identical(plan_class, c("builder_build_plan", "list"))) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  plan <- tryCatch(
    .builder_app_plan_contract(plan, context = "App publication"),
    error = function(error) error
  )
  if (
    inherits(plan, "condition") ||
      !identical(plan$app_contract_version, 1L)
  ) {
    stop(
      "App publication requires an inert contract-v1 BuildPlan.",
      call. = FALSE
    )
  }
  dataset_ids <- plan$dataset_order
  items <- plan$items
  if (
    !is.character(dataset_ids) ||
      !length(dataset_ids) ||
      anyNA(dataset_ids) ||
      any(!nzchar(dataset_ids)) ||
      anyDuplicated(dataset_ids) ||
      !is.list(items) ||
      length(items) != length(dataset_ids)
  ) {
    stop("The App publication dataset order is invalid.", call. = FALSE)
  }
  item_ids <- vapply(items, `[[`, character(1), "id")
  labels <- vapply(items, `[[`, character(1), "name")
  filenames <- vapply(items, `[[`, character(1), "filename")
  colors <- lapply(items, `[[`, "colors")
  names(colors) <- dataset_ids
  options <- plan$app_options
  if (
    !identical(item_ids, dataset_ids) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyNA(filenames) ||
      any(!nzchar(filenames)) ||
      anyDuplicated(tolower(filenames)) ||
      !.builder_app_colors_valid(colors, dataset_ids) ||
      .builder_app_has_reference(options) ||
      !.builder_app_options_valid(options, dataset_ids) ||
      !.builder_app_auth_summary_valid(plan$app_auth)
  ) {
    stop("The App publication expectation is invalid.", call. = FALSE)
  }
  backend_entries <- lapply(items, .builder_app_backend_entry)
  names(backend_entries) <- file.path("private-data", filenames)
  initial_index <- match(options$initial_dataset, dataset_ids)
  request_items <- lapply(items, function(item) {
    item[c(
      "id",
      "name",
      "filename",
      "colors",
      "default_projection",
      "initial_projections",
      "default_trajectory",
      "viewer_display",
      "extra_material_table_index",
      "overview_point_size",
      "overview_point_opacity",
      "overview_percentage_cells_to_show",
      "spatial_percentage_cells_to_show",
      "spatial_point_appearance",
      "spatial_roi_settings",
      "expression_backend",
      "sidecars",
      "spatial_image_storage"
    )]
  })
  request_plan <- structure(
    list(
      app_contract_version = 1L,
      dataset_order = dataset_ids,
      make_app = TRUE,
      items = request_items,
      app_options = options,
      app_auth = plan$app_auth
    ),
    class = c("builder_build_plan", "list")
  )
  frozen_plan <- .builder_coordinator_freeze(request_plan)
  expectation <- .builder_coordinator_freeze(list(
    expected = TRUE,
    contract_version = 1L,
    dataset_ids = dataset_ids,
    labels = labels,
    dataset_labels = stats::setNames(labels, dataset_ids),
    filenames = filenames,
    initial_dataset = dataset_ids[[initial_index]],
    initial_dataset_mode = options$initial_dataset_mode,
    initial_page = options$initial_page,
    show_upload_ui = options$show_upload_ui,
    welcome_message = options$welcome_message,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = as.integer(options$port),
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = options$launch_browser,
    auth = .builder_app_auth_request(plan$app_auth),
    colors = colors,
    backend_plan = list(schema_version = 1L, entries = backend_entries),
    app_dir = NULL
  ))
  list(plan = frozen_plan, expectation = expectation)
}

.builder_coordinator_app_verification <- function(value, expectation) {
  if (
    !identical(typeof(value), "list") ||
      !identical(
        attr(value, "class", exact = TRUE),
        c("builder_app_verification", "list")
      ) ||
      .builder_app_has_reference(value)
  ) {
    stop("App verification evidence is missing or forged.", call. = FALSE)
  }
  value <- .builder_app_plain_value(value)
  required <- c(
    "valid",
    "contract_version",
    "app_dir",
    "selector_order",
    "dataset_labels",
    "initial_dataset",
    "show_upload_ui",
    "colors",
    "backend_plan",
    "private_files",
    "legacy_data_absent",
    "auth_enabled",
    "auth_database",
    "diagnostic_tree_identity"
  )
  diagnostic <- value$diagnostic_tree_identity
  diagnostic_valid <- is.list(diagnostic) &&
    !is.object(diagnostic) &&
    identical(
      names(diagnostic),
      c(
        "schema_version",
        "entry_count",
        "file_count",
        "directory_count",
        "aggregate_md5"
      )
    ) &&
    identical(diagnostic$schema_version, 1L) &&
    all(vapply(
      diagnostic[c("entry_count", "file_count", "directory_count")],
      function(count) {
        is.numeric(count) &&
          length(count) == 1L &&
          is.finite(count) &&
          count >= 0 &&
          count == floor(count)
      },
      logical(1)
    )) &&
    is.character(diagnostic$aggregate_md5) &&
    length(diagnostic$aggregate_md5) == 1L &&
    !is.na(diagnostic$aggregate_md5) &&
    grepl("^[0-9a-f]{32}$", diagnostic$aggregate_md5)
  private_root <- file.path(expectation$app_dir, "private-data")
  private_files_valid <- is.character(value$private_files) &&
    !anyNA(value$private_files) &&
    all(vapply(
      value$private_files,
      .pathWithin,
      logical(1),
      parent = private_root
    ))
  auth_valid <- identical(
    value$auth_enabled,
    isTRUE(expectation$auth$enabled)
  ) &&
    if (isTRUE(expectation$auth$enabled)) {
      identical(
        value$auth_database,
        file.path(
          expectation$app_dir,
          "private-data",
          "auth",
          "credentials.sqlite"
        )
      )
    } else {
      is.null(value$auth_database)
    }
  if (
    !identical(names(value), required) ||
      !isTRUE(value$valid) ||
      !identical(value$contract_version, expectation$contract_version) ||
      !identical(value$app_dir, expectation$app_dir) ||
      !identical(value$selector_order, expectation$dataset_ids) ||
      !identical(value$dataset_labels, expectation$dataset_labels) ||
      !identical(value$initial_dataset, expectation$initial_dataset) ||
      !identical(value$show_upload_ui, expectation$show_upload_ui) ||
      !identical(value$colors, expectation$colors) ||
      !identical(value$backend_plan, expectation$backend_plan) ||
      !isTRUE(value$legacy_data_absent) ||
      !auth_valid ||
      !private_files_valid ||
      !diagnostic_valid
  ) {
    stop(
      "App verification evidence differs from the frozen plan.",
      call. = FALSE
    )
  }
  value
}

.builder_coordinator_request_matches <- function(request, expectation) {
  identical(request$contract_version, expectation$contract_version) &&
    identical(request$selector_order, expectation$dataset_ids) &&
    identical(request$dataset_labels, expectation$dataset_labels) &&
    identical(request$initial_dataset, expectation$initial_dataset) &&
    identical(request$initial_page, expectation$initial_page) &&
    identical(
      request$initial_dataset_mode,
      expectation$initial_dataset_mode
    ) &&
    identical(request$show_upload_ui, expectation$show_upload_ui) &&
    identical(request$welcome_message, expectation$welcome_message) &&
    identical(request$point_size, expectation$point_size) &&
    identical(request$variable_to_compare, expectation$variable_to_compare) &&
    identical(request$host, expectation$host) &&
    identical(request$port, expectation$port) &&
    identical(request$max_request_size, expectation$max_request_size) &&
    identical(request$display_mode, expectation$display_mode) &&
    identical(request$launch_browser, expectation$launch_browser) &&
    identical(request$auth, expectation$auth) &&
    identical(request$colors, expectation$colors) &&
    identical(request$backend_plan, expectation$backend_plan)
}

.builder_coordinator_assert_input_closure <- function(
  handle,
  built,
  parent_request,
  phase = "after parent verification"
) {
  unchanged <- isTRUE(tryCatch(
    {
      .builder_app_assert_input_identities(parent_request)
      TRUE
    },
    error = function(error) FALSE
  ))
  if (
    !unchanged ||
      !identical(unname(parent_request$cerebro_data), unname(built)) ||
      !identical(
        names(parent_request$cerebro_data),
        handle$app_expectation$dataset_ids
      )
  ) {
    stop("The App input closure changed ", phase, ".", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_coordinator_seed_digest_cache <- function(value, digest_cache) {
  fields <- c(
    "permissions",
    "size",
    "device_id",
    "inode",
    "hard_links",
    "modification_time",
    "change_time"
  )
  if (
    is.list(value) &&
      all(c(fields, "md5") %in% names(value)) &&
      .builder_release_payload_md5_valid(value$md5)
  ) {
    snapshot <- c(list(type = "file"), value[fields])
    key <- .builder_release_digest_cache_key(snapshot)
    collision_key <- .builder_release_digest_cache_collision_key(key)
    if (exists(collision_key, envir = digest_cache, inherits = FALSE)) {
      return(invisible(NULL))
    }
    if (exists(key, envir = digest_cache, inherits = FALSE)) {
      cached <- get(key, envir = digest_cache, inherits = FALSE)
      if (!identical(tolower(cached), tolower(value$md5))) {
        rm(list = key, envir = digest_cache)
        assign(collision_key, TRUE, envir = digest_cache)
      }
    } else {
      assign(key, value$md5, envir = digest_cache)
    }
    return(invisible(NULL))
  }
  if (is.list(value)) {
    lapply(value, .builder_coordinator_seed_digest_cache, digest_cache)
  }
  invisible(NULL)
}

.builder_coordinator_app_payload_summary <- function(identity) {
  paths <- vapply(identity$entries, `[[`, character(1), "path")
  prefix <- "cerebro_app/"
  selected <- startsWith(paths, prefix)
  entries <- identity$entries[selected]
  relative <- substring(paths[selected], nchar(prefix) + 1L)
  entries <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    if (identical(entry$type, "directory")) {
      return(list(path = relative[[index]], type = "directory"))
    }
    list(
      path = relative[[index]],
      type = "file",
      size = entry$size,
      md5 = entry$md5
    )
  })
  names(entries) <- relative
  .builder_app_tree_summary(list(entries = entries))
}

.builder_coordinator_utf16_length <- function(path) {
  vapply(
    path,
    function(value) {
      encoded <- tryCatch(
        iconv(value, from = "", to = "UTF-16LE", toRaw = TRUE)[[1L]],
        error = function(error) NULL
      )
      if (is.null(encoded)) {
        return(as.double(nchar(value, type = "chars")))
      }
      as.double(length(encoded) / 2L)
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}

.builder_coordinator_viewer_relative_paths <- function() {
  source_root <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = "")
  candidates <- c(
    if (nzchar(source_root)) file.path(source_root, "inst", "viewer"),
    system.file("viewer", package = "CerebroNexus")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  viewer <- candidates[dir.exists(candidates)][1L]
  if (!length(viewer) || is.na(viewer)) {
    return(file.path("viewer", "shiny_server.R"))
  }
  relative <- list.files(
    viewer,
    all.files = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  relative <- relative[!is.na(relative) & nzchar(relative)]
  unique(file.path("viewer", relative))
}

.builder_coordinator_windows_path_candidates <- function(
  plan,
  stage,
  app_expected,
  .tempfile = tempfile,
  .viewer_relative = NULL
) {
  items <- .subset2(plan, "items") %||% list()
  artifacts <- unique(unlist(
    lapply(items, function(item) {
      c(
        .subset2(item, "filename") %||% character(),
        .subset2(item, "sidecars") %||% character()
      )
    }),
    use.names = FALSE
  ))
  artifacts <- artifacts[
    !is.na(artifacts) & nzchar(artifacts) & !grepl("[/\\\\]", artifacts)
  ]
  export_stage <- .tempfile(pattern = ".crb-stage-", tmpdir = stage)
  report_temporary <- .tempfile(
    pattern = ".build-report-",
    tmpdir = stage,
    fileext = ".json"
  )
  stages <- dirname(stage)
  control <- dirname(stages)
  release_token_reserve <- strrep(
    "t",
    .builder_release_token_max_length("release")
  )
  atomic_token_reserve <- strrep(
    "a",
    .builder_release_token_max_length("a")
  )
  candidates <- c(
    stage,
    file.path(stage, artifacts),
    file.path(export_stage, artifacts),
    file.path(stage, "build-report.json"),
    report_temporary,
    file.path(
      stage,
      paste0(".o-", release_token_reserve, ".tmp")
    ),
    file.path(
      control,
      "diagnostics",
      paste0(".r-", atomic_token_reserve, ".tmp")
    ),
    file.path(
      control,
      "diagnostics",
      paste0(".l-", atomic_token_reserve)
    )
  )
  out_dir <- .subset2(plan, "out_dir")
  if (.builder_release_text(out_dir)) {
    candidates <- c(
      candidates,
      out_dir,
      file.path(out_dir, artifacts),
      file.path(out_dir, "build-report.json")
    )
  }
  for (item in items) {
    images <- .subset2(item, "images") %||% list()
    for (section_id in names(images)) {
      records <- images[[section_id]] %||% list()
      for (label in names(records)) {
        record <- records[[label]] %||% list()
        candidates <- c(
          candidates,
          file.path(
            stage,
            ".spatial",
            "d-ffffffffffff",
            "s-ffffffffffff",
            "i-ffffffffffff.jpg"
          )
        )
      }
    }
  }
  if (isTRUE(app_expected)) {
    app_dir <- file.path(stage, "cerebro_app")
    app_stage <- .tempfile(
      pattern = ".cerebro_app-stage-",
      tmpdir = stage
    )
    if (is.null(.viewer_relative)) {
      .viewer_relative <- .builder_coordinator_viewer_relative_paths()
    }
    app_relative <- c(
      "app.R",
      "config.yml",
      "build-manifest.rds",
      file.path("private-data", artifacts),
      file.path("spatial-assets", paste0("u", strrep("f", 32L), ".tiff")),
      .viewer_relative
    )
    candidates <- c(
      candidates,
      file.path(app_dir, app_relative),
      file.path(app_stage, app_relative)
    )
    if (.builder_release_text(out_dir)) {
      candidates <- c(
        candidates,
        file.path(out_dir, "cerebro_app", app_relative)
      )
    }
  }
  unique(gsub("\\", "/", candidates, fixed = TRUE))
}

.builder_coordinator_assert_windows_path_budget <- function(
  plan,
  stage,
  app_expected,
  os_type = .Platform$OS.type,
  limit = 259L,
  .tempfile = tempfile,
  .viewer_relative = NULL
) {
  if (!identical(os_type, "windows")) {
    return(invisible(TRUE))
  }
  candidates <- .builder_coordinator_windows_path_candidates(
    plan,
    stage,
    app_expected,
    .tempfile = .tempfile,
    .viewer_relative = .viewer_relative
  )
  lengths <- .builder_coordinator_utf16_length(candidates)
  longest <- which.max(lengths)
  if (lengths[[longest]] > as.double(limit)) {
    stop(
      "The selected output folder is too deep for this Windows R runtime. ",
      "The longest planned Builder path is ",
      lengths[[longest]],
      " UTF-16 characters (supported limit: ",
      limit,
      "). ",
      "Choose a shorter output folder, for example C:/CerebroBuild. ",
      "Builder's internal workspace names are already bounded; the ",
      "remaining path shown below must fit the Windows runtime. ",
      "Planned path: ",
      candidates[[longest]],
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_coordinator_publication_tree_matches <- function(
  current,
  parent,
  portable,
  phase
) {
  if (
    is.null(current) ||
      !identical(.builder_app_tree_summary(current), portable)
  ) {
    return(FALSE)
  }
  if (identical(phase, "after_in_place")) {
    current_relocated <- current
    parent_relocated <- parent
    current_relocated$root_fingerprint[c(
      "modification_time",
      "change_time"
    )] <- NULL
    parent_relocated$root_fingerprint[c(
      "modification_time",
      "change_time"
    )] <- NULL
    return(identical(current_relocated, parent_relocated))
  }
  identical(current, parent)
}

builder_coordinator_output_preflight <- function(plan, prior_state = NULL) {
  plan_class <- attr(plan, "class", exact = TRUE)
  if (
    !identical(typeof(plan), "list") ||
      (!is.null(plan_class) &&
        !identical(plan_class, c("builder_build_plan", "list")))
  ) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  out_dir <- .subset2(plan, "out_dir")
  if (!.builder_release_text(.builder_release_or(out_dir, NULL))) {
    stop("A frozen plan with an output release is required.", call. = FALSE)
  }
  output_release <- .builder_release_or(
    .subset2(plan, "output_release"),
    list()
  )
  expected <- .builder_release_or(
    .subset2(output_release, "targets"),
    character()
  )
  relative <- vapply(
    expected,
    function(path) {
      .builder_release_relative(path, out_dir)
    },
    ""
  )
  expected <- sort(unique(relative), method = "radix")
  declared_prior <- .subset2(plan, "expected_prior_identity")
  if (
    !is.null(declared_prior) && !.builder_release_identity_valid(declared_prior)
  ) {
    stop("The expected prior release identity is invalid.", call. = FALSE)
  }
  if (is.null(prior_state)) {
    prior_state <- builder_release_state(
      out_dir,
      exact_record = FALSE,
      allow_abandoned = TRUE
    )
  }
  valid_prior_state <- is.list(prior_state) &&
    identical(prior_state$schema_version, 1L) &&
    .builder_release_identity_valid(prior_state$identity)
  if (!valid_prior_state) {
    stop("The verified prior release state is invalid.", call. = FALSE)
  }
  prior <- declared_prior %||% prior_state$identity
  if (!identical(prior_state$identity, prior)) {
    stop(
      "The release changed after Review; nothing was published.",
      call. = FALSE
    )
  }
  prior_paths <- vapply(prior$entries, `[[`, character(1), "path")
  record <- prior_state$record
  expected_roots <- unique(sub("/.*$", "", expected))
  if (is.null(record)) {
    prior_roots <- unique(sub("/.*$", "", prior_paths))
    foreign <- setdiff(prior_roots, expected_roots)
  } else {
    foreign <- record$foreign
  }
  list(
    out_dir = out_dir,
    expected = expected,
    prior = prior,
    prior_state = prior_state,
    prior_paths = prior_paths,
    record = record,
    foreign = foreign
  )
}

builder_coordinator_prepare <- function(plan, build_id, prior_state = NULL) {
  app_contract <- .builder_coordinator_app_contract(plan)
  preflight <- builder_coordinator_output_preflight(plan, prior_state)
  out_dir <- preflight$out_dir
  expected <- preflight$expected
  prior <- preflight$prior
  prior_state <- preflight$prior_state
  prior_paths <- preflight$prior_paths
  record <- preflight$record
  foreign <- preflight$foreign
  if (length(foreign)) {
    stop(
      "The output directory contains foreign release entries: ",
      paste(foreign, collapse = ", "),
      ". They were preserved.",
      call. = FALSE
    )
  }
  if (
    length(prior_paths) &&
      !isTRUE(record$abandoned) &&
      !isTRUE(.subset2(plan, "overwrite"))
  ) {
    stop(
      "Known outputs already exist. Enable Replace existing outputs.",
      call. = FALSE
    )
  }
  handle <- builder_prepare_release(
    out_dir,
    build_id,
    expected_prior = prior,
    expected_prior_state = prior_state,
    .stage_guard = function(stage) {
      .builder_coordinator_assert_windows_path_budget(
        plan,
        stage,
        app_expected = isTRUE(app_contract$expectation$expected)
      )
    }
  )
  handle$expected_payload_targets <- expected
  handle$transient_app_inputs <- if (
    isTRUE(app_contract$expectation$expected)
  ) {
    sort(
      unique(unlist(
        lapply(plan$items, function(item) {
          c(item$filename, item$sidecars %||% character())
        }),
        use.names = FALSE
      )),
      method = "radix"
    )
  } else {
    character()
  }
  handle$expected_build_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      handle$transient_app_inputs
    )),
    method = "radix"
  )
  handle$expected_final_targets <- NULL
  handle$legacy_prior_members <- if (is.null(record)) {
    .builder_release_identity_members(prior)
  } else {
    NULL
  }
  app_contract$expectation$app_dir <- if (
    isTRUE(app_contract$expectation$expected)
  ) {
    file.path(handle$stage, "cerebro_app")
  } else {
    NULL
  }
  handle$app_plan <- app_contract$plan
  handle$report_plan <- .builder_coordinator_report_plan(plan)
  handle$app_expectation <- .builder_coordinator_freeze(
    app_contract$expectation
  )
  class(handle) <- c("builder_release_coordinator", class(handle))
  handle
}

.builder_coordinator_handle <- function(handle) {
  if (!inherits(handle, "builder_release_coordinator")) {
    stop("A Builder release coordinator is required.", call. = FALSE)
  }
  .builder_release_handle(handle)
}

.builder_coordinator_stage_identity <- function(
  handle,
  expected = handle$expected_payload_targets,
  exact = FALSE,
  .digest_cache = NULL,
  .on_file = NULL
) {
  identity <- builder_release_identity(
    handle$stage,
    .digest_cache = .digest_cache,
    .on_file = .on_file
  )
  if (!isTRUE(identity$exists)) {
    stop("The coordinator-assigned stage is missing.", call. = FALSE)
  }
  present <- vapply(identity$entries, `[[`, character(1), "path")
  if (length(expected)) {
    missing <- setdiff(expected, present)
    if (length(missing)) {
      stop(
        "The verified stage is missing planned release entries: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    if (isTRUE(exact)) {
      planned <- vapply(
        present,
        function(path) {
          any(
            path %in% expected,
            startsWith(path, paste0(expected, "/"))
          )
        },
        logical(1)
      )
      unplanned <- present[!planned]
    } else {
      expected_roots <- unique(sub("/.*$", "", expected))
      present_roots <- unique(sub("/.*$", "", present))
      unplanned <- setdiff(present_roots, expected_roots)
    }
    if (length(unplanned)) {
      stop(
        "The verified stage contains unplanned release entries: ",
        paste(unplanned, collapse = ", "),
        call. = FALSE
      )
    }
  }
  identity
}

.builder_coordinator_validate_build_auth <- function(handle, build_result) {
  expected_enabled <- isTRUE(handle$app_expectation$expected) &&
    isTRUE(handle$app_expectation$auth$enabled)
  expected_env_file <- if (expected_enabled) {
    file.path(handle$stage, "cerebro_app", "viewer-auth.env")
  } else {
    NULL
  }
  if (
    !is.list(build_result) ||
      !identical(build_result$auth_enabled, expected_enabled) ||
      !identical(build_result$auth_env_file, expected_env_file)
  ) {
    stop(
      "The build result authentication evidence differs from the frozen plan.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_coordinator_auth_env_identity <- function(
  handle,
  release_root = handle$stage
) {
  if (
    !isTRUE(handle$app_expectation$expected) ||
      !isTRUE(handle$app_expectation$auth$enabled)
  ) {
    return(NULL)
  }
  env_file <- file.path(
    release_root,
    "cerebro_app",
    "viewer-auth.env"
  )
  secret <- builder_auth_read_env_file(env_file)
  secret <- NULL
  database <- file.path(
    release_root,
    "cerebro_app",
    "private-data",
    "auth",
    "credentials.sqlite"
  )
  if (!isTRUE(builder_auth_verify_database_pair(database, env_file))) {
    stop("The authentication database could not be verified.", call. = FALSE)
  }
  fingerprint <- function() {
    .builder_release_file_fingerprint(
      fs::file_info(env_file, fail = TRUE, follow = FALSE)
    )
  }
  first <- fingerprint()
  md5 <- .builder_release_payload_md5(env_file)
  second <- fingerprint()
  if (!identical(first, second)) {
    stop(
      "The authentication environment changed during inspection.",
      call. = FALSE
    )
  }
  c(first, list(md5 = md5))
}

.builder_coordinator_remove_app_inputs <- function(
  handle,
  built,
  parent_request,
  .unlink = unlink,
  .sleep = Sys.sleep,
  .retry_delays = if (identical(.Platform$OS.type, "windows")) {
    c(0, 0.05, 0.1, 0.2, 0.4, 0.8)
  } else {
    0
  }
) {
  expected <- handle$transient_app_inputs
  built_relative <- vapply(
    unname(built),
    .builder_release_relative,
    character(1),
    root = handle$stage
  )
  frozen <- vapply(handle$app_plan$items, `[[`, character(1), "filename")
  if (!setequal(built_relative, frozen)) {
    stop("The staged App inputs differ from the frozen plan.", call. = FALSE)
  }
  .builder_coordinator_assert_input_closure(
    handle,
    built,
    parent_request,
    phase = "before temporary input removal"
  )
  for (relative in expected) {
    path <- file.path(handle$stage, relative)
    if (
      !.pathWithin(path, handle$stage) ||
        .builder_release_link(path) ||
        !.builder_release_exists(path)
    ) {
      stop("A temporary App input is missing or unsafe.", call. = FALSE)
    }
    .builder_coordinator_remove_app_input(
      path,
      relative,
      handle$stage,
      .unlink = .unlink,
      .sleep = .sleep,
      .retry_delays = .retry_delays
    )
  }
  invisible(TRUE)
}

.builder_coordinator_unlink_attempt <- function(path, recursive, .unlink) {
  messages <- character()
  value <- tryCatch(
    withCallingHandlers(
      .unlink(path, recursive = recursive, force = TRUE),
      warning = function(warning) {
        messages <<- c(messages, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) {
      messages <<- c(messages, conditionMessage(error))
      NA_integer_
    }
  )
  status <- suppressWarnings(as.integer(value))
  if (length(status) != 1L || is.na(status)) {
    status <- NA_integer_
  }
  list(status = status, messages = unique(messages[nzchar(messages)]))
}

.builder_coordinator_remove_app_input <- function(
  path,
  relative,
  stage,
  .unlink = unlink,
  .sleep = Sys.sleep,
  .retry_delays = if (identical(.Platform$OS.type, "windows")) {
    c(0, 0.05, 0.1, 0.2, 0.4, 0.8)
  } else {
    0
  }
) {
  delays <- suppressWarnings(as.numeric(.retry_delays))
  if (
    !length(delays) ||
      anyNA(delays) ||
      any(!is.finite(delays)) ||
      any(delays < 0)
  ) {
    stop("Temporary App input retry delays are invalid.", call. = FALSE)
  }
  last <- list(status = NA_integer_, messages = character())
  for (attempt in seq_along(delays)) {
    if (attempt > 1L && delays[[attempt]] > 0) {
      .sleep(delays[[attempt]])
    }
    exists <- .builder_release_exists(path)
    linked <- .builder_release_link(path)
    if (!exists && !linked) {
      return(invisible(TRUE))
    }
    if (!.pathWithin(path, stage) || linked) {
      stop(
        paste0(
          "A temporary App input became unsafe during removal: ",
          encodeString(path, quote = '"'),
          "."
        ),
        call. = FALSE
      )
    }
    last <- .builder_coordinator_unlink_attempt(
      path,
      recursive = dir.exists(path),
      .unlink = .unlink
    )
    exists <- .builder_release_exists(path)
    linked <- .builder_release_link(path)
    if (!exists && !linked) {
      return(invisible(TRUE))
    }
    if (linked) {
      stop(
        paste0(
          "A temporary App input became unsafe during removal: ",
          encodeString(path, quote = '"'),
          "."
        ),
        call. = FALSE
      )
    }
  }
  status <- if (is.na(last$status)) "unavailable" else as.character(last$status)
  detail <- if (length(last$messages)) {
    paste(last$messages, collapse = " | ")
  } else {
    "No operating-system error text was reported."
  }
  label <- normalizePath(path, winslash = "/", mustWork = FALSE)
  status_label <- if (identical(.Platform$OS.type, "windows")) {
    "Windows delete status"
  } else {
    "Delete status"
  }
  stop(
    paste0(
      "A temporary App input could not be removed after ",
      length(delays),
      " attempts. Path: ",
      encodeString(label, quote = '"'),
      ". Relative path: ",
      encodeString(relative, quote = '"'),
      ". ",
      status_label,
      ": ",
      status,
      ". Last system error: ",
      detail
    ),
    call. = FALSE
  )
}

.builder_coordinator_register_spatial_molecule_inputs <- function(
  handle,
  built,
  app_expected,
  verifications = list()
) {
  plan_items <- handle$app_plan$items %||% handle$report_plan$items %||% list()
  item_ids <- vapply(
    plan_items,
    function(item) as.character(item$id %||% ""),
    character(1)
  )
  names(plan_items) <- item_ids
  locations <- vapply(
    seq_along(built),
    function(index) {
      built_names <- names(built)
      item <- if (!is.null(built_names) && nzchar(built_names[[index]])) {
        plan_items[[built_names[[index]]]] %||% NULL
      } else {
        matches <- which(vapply(
          plan_items,
          function(candidate) {
            identical(
              basename(built[[index]]),
              candidate$filename %||% ""
            )
          },
          logical(1)
        ))
        if (length(matches) == 1L) plan_items[[matches]] else NULL
      }
      verification <- if (is.list(item)) {
        verifications[[item$id]] %||%
          if (length(verifications) >= index) verifications[[index]] else NULL
      } else {
        NULL
      }
      backend <- verification$spatial_molecule_backend %||% NULL
      if (is.null(backend)) {
        return("")
      }
      if (
        !is.list(backend) ||
          !identical(backend$type, "directory") ||
          !.builder_release_text(backend$location)
      ) {
        return(NA_character_)
      }
      backend$location
    },
    character(1)
  )
  if (
    anyNA(locations) ||
      anyDuplicated(tolower(locations[nzchar(locations)])) ||
      any(grepl("[/\\\\]", locations)) ||
      any(locations %in% c(".", ".."))
  ) {
    stop(
      "The staged spatial molecule sidecar closure is invalid.",
      call. = FALSE
    )
  }
  locations <- sort(unique(locations[nzchar(locations)]), method = "radix")
  if (!length(locations)) {
    return(handle)
  }
  if (isTRUE(app_expected)) {
    handle$transient_app_inputs <- sort(
      unique(c(handle$transient_app_inputs, locations)),
      method = "radix"
    )
  } else {
    handle$expected_payload_targets <- sort(
      unique(c(handle$expected_payload_targets, locations)),
      method = "radix"
    )
  }
  handle$expected_build_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      handle$transient_app_inputs
    )),
    method = "radix"
  )
  handle
}

## Sub-steps of "Verifying output" for the Build progress card, sent as the
## verify phase's detail. Only callbacks that take a detail receive them.
## Identity passes visit every staged file, so repeats of one step are
## throttled; the last item of a count is always reported.
.builder_coordinator_verify_reporter <- function(
  on_progress,
  interval = 0.25,
  .now = function() as.numeric(Sys.time())
) {
  arguments <- names(formals(on_progress))
  if (length(arguments) < 2L && !"..." %in% arguments) {
    return(function(label, index = NULL, count = NULL) invisible(FALSE))
  }
  last_label <- NULL
  last_at <- -Inf
  function(label, index = NULL, count = NULL) {
    now <- .now()
    last_item <- !is.null(index) && identical(index, count)
    if (
      identical(label, last_label) &&
        !last_item &&
        now - last_at < interval
    ) {
      return(invisible(FALSE))
    }
    last_label <<- label
    last_at <<- now
    detail <- list(step_label = label, step_index = index, step_count = count)
    # Progress is cosmetic; a failed write never stops verification.
    try(on_progress("verify", Filter(Negate(is.null), detail)), silent = TRUE)
    invisible(TRUE)
  }
}

builder_coordinator_publish <- function(
  handle,
  build_result,
  .record_move = file.rename,
  .verify_app = builder_verify_app,
  .write_report = builder_write_build_report,
  .publish = builder_publish_release,
  .on_progress = function(phase) invisible(NULL)
) {
  handle <- .builder_coordinator_handle(handle)
  .on_progress("verify")
  step <- .builder_coordinator_verify_reporter(.on_progress)
  # Name each file pass before it starts: listing the stage takes longer than
  # hashing files whose digests are already cached.
  file_step <- function(label) {
    step(label)
    function(index, count) step(label, as.integer(index), as.integer(count))
  }
  step("Checking build result")
  digest_cache <- new.env(parent = emptyenv())
  if (
    !is.list(build_result) ||
      !identical(build_result$state, "success") ||
      !isTRUE(build_result$publishable)
  ) {
    stop("Only a verified successful build can be published.", call. = FALSE)
  }
  .builder_coordinator_validate_build_auth(handle, build_result)
  result_stage <- tryCatch(
    .canonicalTargetPath(build_result$stage),
    error = function(error) ""
  )
  if (!identical(result_stage, .canonicalTargetPath(handle$stage))) {
    stop(
      "The build result did not come from the assigned stage.",
      call. = FALSE
    )
  }
  built <- .builder_release_or(build_result$built, character())
  if (
    !length(built) ||
      !all(vapply(
        built,
        .pathWithin,
        logical(1),
        parent = handle$stage
      ))
  ) {
    stop("Verified build artifacts escaped the assigned stage.", call. = FALSE)
  }
  app_expected <- isTRUE(handle$app_expectation$expected)
  handle <- .builder_coordinator_register_spatial_molecule_inputs(
    handle,
    built,
    app_expected,
    build_result$verifications %||% list()
  )
  parent_verification <- NULL
  staged_app <- file.path(handle$stage, "cerebro_app")
  if (!app_expected) {
    if (
      !is.null(build_result$app_dir) ||
        !is.null(build_result$app_verification) ||
        !is.null(build_result$app_tree_identity) ||
        .builder_release_exists(staged_app)
    ) {
      stop(
        "A CRB-only plan returned an unexpected App.",
        call. = FALSE
      )
    }
  } else {
    if (!identical(build_result$build_id, handle$build_id)) {
      stop(
        "The App build identity does not match its coordinator.",
        call. = FALSE
      )
    }
    returned_app_dir <- tryCatch(
      .canonicalTargetPath(build_result$app_dir),
      error = function(error) ""
    )
    expected_app_dir <- .canonicalTargetPath(
      handle$app_expectation$app_dir
    )
    if (
      !identical(returned_app_dir, expected_app_dir) ||
        !dir.exists(returned_app_dir) ||
        .builder_release_link(returned_app_dir)
    ) {
      stop(
        "The build result did not return the assigned App directory.",
        call. = FALSE
      )
    }
    build_result$app_dir <- returned_app_dir
    worker_tree_identity <- build_result$app_tree_identity
    if (
      !is.list(worker_tree_identity) ||
        is.object(worker_tree_identity) ||
        .builder_app_has_reference(worker_tree_identity)
    ) {
      stop("The staged App identity is missing or forged.", call. = FALSE)
    }
    if (!identical(build_result$labels, handle$app_expectation$labels)) {
      stop("The App labels differ from the frozen plan.", call. = FALSE)
    }
    parent_request <- tryCatch(
      builder_app_bundle_request(
        handle$app_plan,
        built,
        handle$app_expectation$labels,
        .on_progress = function(label, index, count) {
          step(label, as.integer(index), as.integer(count))
        }
      ),
      error = function(error) error
    )
    if (
      inherits(parent_request, "condition") ||
        !.builder_coordinator_request_matches(
          parent_request,
          handle$app_expectation
        )
    ) {
      stop(
        "The staged App inputs differ from the frozen plan.",
        call. = FALSE
      )
    }
    # Test doubles may not take the progress callback.
    verify_app <- if (".on_progress" %in% names(formals(.verify_app))) {
      function(...) .verify_app(..., .on_progress = function(label) step(label))
    } else {
      .verify_app
    }
    parent_verification <- tryCatch(
      verify_app(
        build_result$app_dir,
        parent_request,
        auth_env_file = if (isTRUE(handle$app_expectation$auth$enabled)) {
          expected_env <- file.path(
            handle$stage,
            "cerebro_app",
            "viewer-auth.env"
          )
          if (!identical(build_result$auth_env_file, expected_env)) {
            stop(
              "The build result returned an invalid authentication environment."
            )
          }
          expected_env
        } else {
          if (
            !is.null(build_result$auth_env_file) ||
              isTRUE(build_result$auth_enabled)
          ) {
            stop("A public build returned authentication material.")
          }
          NULL
        },
        .retain_tree_identity = TRUE
      ),
      error = function(error) error
    )
    if (inherits(parent_verification, "condition")) {
      stop(
        "Parent App verification failed: ",
        conditionMessage(parent_verification),
        call. = FALSE
      )
    }
    parent_tree_identity <- attr(
      parent_verification,
      "parent_tree_identity",
      exact = TRUE
    )
    attr(parent_verification, "parent_tree_identity") <- NULL
    parent_verification <- .builder_coordinator_app_verification(
      parent_verification,
      handle$app_expectation
    )
    if (
      !identical(
        worker_tree_identity,
        parent_verification$diagnostic_tree_identity
      )
    ) {
      stop("The staged App changed after worker assembly.", call. = FALSE)
    }
    # Cache hits bind device, inode, and change time; mutations are rehashed.
    .builder_coordinator_seed_digest_cache(
      list(
        parent_tree_identity,
        parent_request$crb_identities,
        parent_request$backend_identities,
        parent_request$spatial_molecule_identities,
        parent_request$spatial_image_identities
      ),
      digest_cache
    )
  }
  parent_env_identity <- NULL
  if (app_expected) {
    on_file <- file_step("Checking files")
    payload_identity <- .builder_coordinator_stage_identity(
      handle,
      expected = handle$expected_build_targets,
      .digest_cache = digest_cache,
      .on_file = on_file
    )
    parent_env_identity <- .builder_coordinator_auth_env_identity(handle)
    .builder_coordinator_assert_input_closure(
      handle,
      built,
      parent_request
    )
    if (
      !identical(
        .builder_coordinator_app_payload_summary(payload_identity),
        parent_verification$diagnostic_tree_identity
      )
    ) {
      stop(
        "The staged App changed after parent verification.",
        call. = FALSE
      )
    }
  }
  step("Writing build report")
  report_result <- build_result
  if (app_expected) {
    report_result$app_verification <- parent_verification
  }
  report <- builder_build_report(handle$report_plan, report_result)
  report_path <- .write_report(handle$stage, report)
  expected_report_path <- file.path(handle$stage, "build-report.json")
  if (
    !identical(
      normalizePath(report_path, winslash = "/", mustWork = TRUE),
      normalizePath(expected_report_path, winslash = "/", mustWork = TRUE)
    )
  ) {
    stop(
      "The build report was not written to the assigned stage.",
      call. = FALSE
    )
  }
  if (app_expected) {
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop("The staged App changed after parent verification.", call. = FALSE)
    }
    .builder_coordinator_remove_app_inputs(
      handle,
      built,
      parent_request
    )
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop(
        "The authentication environment changed during temporary input removal.",
        call. = FALSE
      )
    }
  }
  final_payload_targets <- sort(
    unique(c(
      handle$expected_payload_targets,
      "build-report.json"
    )),
    method = "radix"
  )
  on_file <- file_step("Checking final files")
  payload_identity <- .builder_coordinator_stage_identity(
    handle,
    expected = final_payload_targets,
    exact = TRUE,
    .digest_cache = digest_cache,
    .on_file = on_file
  )
  if (
    app_expected &&
      !identical(
        .builder_coordinator_app_payload_summary(payload_identity),
        parent_verification$diagnostic_tree_identity
      )
  ) {
    stop(
      "The staged App changed during temporary input removal.",
      call. = FALSE
    )
  }
  if (length(handle$legacy_prior_members)) {
    payload_members <- .builder_release_identity_members(payload_identity)
    payload_keys <- vapply(
      payload_members,
      function(member) paste(member$type, member$path, sep = "\t"),
      ""
    )
    legacy_keys <- vapply(
      handle$legacy_prior_members,
      function(member) paste(member$type, member$path, sep = "\t"),
      ""
    )
    missing_legacy <- setdiff(legacy_keys, payload_keys)
    if (length(missing_legacy)) {
      stop(
        "The verified stage would shrink the legacy release topology: ",
        paste(sub("^[FD]\t", "", missing_legacy), collapse = ", "),
        call. = FALSE
      )
    }
  }
  handle$expected_payload_targets <- final_payload_targets
  on_file <- file_step("Recording release")
  ownership <- .builder_release_write_record(
    handle$stage,
    payload_identity,
    handle$token,
    .move = .record_move,
    .digest_cache = digest_cache,
    .on_file = on_file
  )
  if (app_expected) {
    if (
      !identical(
        .builder_coordinator_app_payload_summary(ownership$identity),
        parent_verification$diagnostic_tree_identity
      )
    ) {
      stop(
        "The staged App changed during ownership record commit.",
        call. = FALSE
      )
    }
    if (
      !identical(
        .builder_coordinator_auth_env_identity(handle),
        parent_env_identity
      )
    ) {
      stop(
        "The authentication environment changed during ownership record commit.",
        call. = FALSE
      )
    }
  }
  final_payload_identity <- ownership$identity
  final_paths <- vapply(
    final_payload_identity$entries,
    `[[`,
    character(1),
    "path"
  )
  final_payload_identity$entries <- final_payload_identity$entries[
    !final_paths %in% .builder_release_record_name
  ]
  if (!identical(final_payload_identity, payload_identity)) {
    stop(
      "The verified payload changed during ownership record commit.",
      call. = FALSE
    )
  }
  handle$expected_final_targets <- c(
    handle$expected_payload_targets,
    .builder_release_record_name
  )
  handle$expected_stage_identity <- ownership$identity
  publication_guard <- function(root, phase) {
    if (!app_expected) {
      return(TRUE)
    }
    app_dir <- file.path(root, "cerebro_app")
    current <- tryCatch(
      .builder_app_tree_identity(
        app_dir,
        .previous = parent_tree_identity
      ),
      error = function(error) NULL
    )
    current_env <- tryCatch(
      .builder_coordinator_auth_env_identity(handle, release_root = root),
      error = function(error) NULL
    )
    paired <- if (isTRUE(handle$app_expectation$auth$enabled)) {
      database <- file.path(
        app_dir,
        "private-data",
        "auth",
        "credentials.sqlite"
      )
      tryCatch(
        builder_auth_verify_database_pair(
          database,
          file.path(app_dir, "viewer-auth.env")
        ),
        error = function(error) FALSE
      )
    } else {
      is.null(current_env)
    }
    artifact_root <- if (app_expected) {
      file.path(root, "cerebro_app", "private-data")
    } else {
      root
    }
    verification_relative <- vapply(
      build_result$verifications,
      function(value) {
        if (
          !is.list(value) ||
            !.builder_release_text(value$path) ||
            !.pathWithin(value$path, handle$stage)
        ) {
          return(NA_character_)
        }
        basename(.builder_release_relative(value$path, handle$stage))
      },
      character(1)
    )
    artifact_paths <- c(
      file.path(artifact_root, basename(unname(built))),
      file.path(artifact_root, verification_relative)
    )
    isTRUE(paired) &&
      .builder_coordinator_publication_tree_matches(
        current,
        parent_tree_identity,
        parent_verification$diagnostic_tree_identity,
        phase
      ) &&
      identical(current_env, parent_env_identity) &&
      !anyNA(artifact_paths) &&
      all(file.exists(artifact_paths))
  }
  .on_progress("publish")
  published <- .publish(
    handle,
    .verify_payload = publication_guard,
    .digest_cache = digest_cache
  )
  relative_built <- vapply(
    built,
    .builder_release_relative,
    "",
    root = handle$stage
  )
  mapped_built <- if (app_expected) {
    file.path(
      published$target,
      "cerebro_app",
      "private-data",
      basename(unname(built))
    )
  } else {
    file.path(published$target, relative_built)
  }
  build_result$built <- stats::setNames(mapped_built, names(built))
  if (app_expected) {
    relative_app <- .builder_release_relative(
      build_result$app_dir,
      handle$stage
    )
    build_result$app_dir <- file.path(published$target, relative_app)
    if (isTRUE(handle$app_expectation$auth$enabled)) {
      build_result$auth_enabled <- TRUE
      build_result$auth_env_file <- file.path(
        published$target,
        "cerebro_app",
        "viewer-auth.env"
      )
    } else {
      build_result$auth_enabled <- FALSE
      build_result$auth_env_file <- NULL
    }
  } else {
    build_result$app_dir <- NULL
    build_result$auth_enabled <- FALSE
    build_result$auth_env_file <- NULL
  }
  if (length(build_result$verifications)) {
    build_result$verifications <- lapply(
      build_result$verifications,
      function(verification) {
        if (
          is.list(verification) &&
            .builder_release_text(verification$path) &&
            .pathWithin(verification$path, handle$stage)
        ) {
          relative <- .builder_release_relative(verification$path, handle$stage)
          verification$path <- if (app_expected) {
            file.path(
              published$target,
              "cerebro_app",
              "private-data",
              basename(relative)
            )
          } else {
            file.path(published$target, relative)
          }
        }
        if (
          is.list(verification) &&
            .builder_release_text(verification$spatial_molecule_path)
        ) {
          location <- basename(verification$spatial_molecule_path)
          verification$spatial_molecule_path <- if (app_expected) {
            file.path(
              published$target,
              "cerebro_app",
              "private-data",
              location
            )
          } else {
            file.path(published$target, location)
          }
        }
        verification
      }
    )
  }
  verification_paths <- if (length(build_result$verifications)) {
    vapply(
      build_result$verifications,
      function(verification) {
        if (
          !is.list(verification) || !.builder_release_text(verification$path)
        ) {
          return(NA_character_)
        }
        verification$path
      },
      character(1)
    )
  } else {
    character()
  }
  mapped_paths <- c(unname(build_result$built), verification_paths)
  spatial_molecule_paths <- unlist(
    lapply(
      build_result$verifications,
      function(verification) {
        verification$spatial_molecule_path %||% character()
      }
    ),
    use.names = FALSE
  )
  mapped_paths <- c(mapped_paths, spatial_molecule_paths)
  if (
    !length(mapped_paths) ||
      anyNA(mapped_paths) ||
      !all(file.exists(mapped_paths))
  ) {
    stop("Published build artifacts are missing.", call. = FALSE)
  }
  build_result$app_verification <- NULL
  build_result$app_tree_identity <- NULL
  build_result$app_verified <- app_expected
  build_result$report_path <- file.path(published$target, "build-report.json")
  build_result$stage <- NULL
  build_result$published <- TRUE
  build_result$publishable <- FALSE
  build_result$release <- published
  build_result
}

builder_coordinator_abort <- function(handle) {
  handle <- .builder_coordinator_handle(handle)
  builder_abort_release(handle)
}

.builder_coordinator_stage_files <- function(root, relative) {
  path <- file.path(root, relative)
  if (.builder_release_link(path) || !.pathWithin(path, root)) {
    stop("A retained build artifact is unsafe.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    if (!file.exists(path)) {
      stop("A retained build artifact is missing.", call. = FALSE)
    }
    return(relative)
  }
  children <- list.files(
    path,
    all.files = TRUE,
    recursive = TRUE,
    no.. = TRUE
  )
  file.path(relative, children)
}

# A needs-decision result discards the stage, including every dataset that
# already succeeded. Before the abort, move (or copy across volumes) those
# verified CRBs and their sidecars into a session-owned directory so a retry
# reuses them instead of rebuilding.
builder_coordinator_retain_partial <- function(
  handle,
  build_result,
  retention_root,
  .move = file.rename,
  .copy = file.copy
) {
  handle <- .builder_coordinator_handle(handle)
  if (
    !is.list(build_result) ||
      !identical(build_result$state, "needs_decision") ||
      !.builder_release_text(retention_root) ||
      !dir.exists(retention_root) ||
      .builder_release_link(retention_root)
  ) {
    return(list())
  }
  built <- build_result$built %||% character()
  if (!length(built) || is.null(names(built))) {
    return(list())
  }
  items <- handle$report_plan$items %||% list()
  names(items) <- vapply(
    items,
    function(item) as.character(item$id %||% ""),
    character(1)
  )
  stage <- .canonicalTargetPath(handle$stage)
  retained <- list()
  for (id in names(built)) {
    item <- items[[id]]
    path <- built[[id]]
    verification <- build_result$verifications[[id]] %||% NULL
    if (
      !is.list(item) ||
        !is.list(verification) ||
        !isTRUE(verification$valid) ||
        !.builder_release_text(path) ||
        !.pathWithin(path, stage) ||
        !identical(dirname(.canonicalTargetPath(path)), stage) ||
        !identical(basename(path), item$filename)
    ) {
      next
    }
    record <- tryCatch(
      {
        spatial <- verification$spatial_molecule_backend$location %||%
          character()
        relative <- unique(c(
          item$filename,
          unlist(
            lapply(
              c(item$sidecars %||% character(), spatial),
              function(name) .builder_coordinator_stage_files(stage, name)
            ),
            use.names = FALSE
          )
        ))
        destination <- file.path(
          retention_root,
          paste0(
            "r-",
            substr(gsub("[^A-Za-z0-9]", "", handle$build_id), 1L, 24L),
            "-",
            length(retained) + 1L
          )
        )
        if (
          .builder_release_exists(destination) ||
            !dir.create(destination, mode = "0700")
        ) {
          stop("The retention directory could not be created.", call. = FALSE)
        }
        files <- lapply(relative, function(name) {
          source <- file.path(stage, name)
          target <- file.path(destination, name)
          dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
          moved <- isTRUE(suppressWarnings(.move(source, target))) ||
            isTRUE(.copy(source, target, overwrite = FALSE))
          if (!moved || !file.exists(target) || dir.exists(target)) {
            stop("A retained build artifact could not be moved.", call. = FALSE)
          }
          list(
            target = name,
            resolved_path = normalizePath(
              target,
              winslash = "/",
              mustWork = TRUE
            ),
            fingerprint = list(
              md5 = .builder_release_payload_md5(target),
              size = as.double(file.size(target))
            )
          )
        })
        primary <- files[[1L]]
        verification$path <- primary$resolved_path
        if (.builder_release_text(verification$spatial_molecule_path)) {
          verification$spatial_molecule_path <- file.path(
            dirname(primary$resolved_path),
            basename(verification$spatial_molecule_path)
          )
        }
        list(
          path = primary$resolved_path,
          fingerprint = primary$fingerprint,
          members = lapply(files[-1L], function(member) {
            member[c("resolved_path", "target", "fingerprint")]
          }),
          verification = verification
        )
      },
      error = function(error) NULL
    )
    if (is.list(record)) {
      retained[[id]] <- record
    }
  }
  retained
}

builder_coordinator_claim <- function(handle, pid = Sys.getpid()) {
  handle <- .builder_coordinator_handle(handle)
  pid <- as.integer(pid)
  .builder_release_transfer_lock(
    handle$lock,
    handle$token,
    from_pid = as.integer(handle$pid),
    to_pid = pid
  )
  handle$pid <- pid
  handle$host <- .builder_release_host()
  handle$record$pid <- pid
  handle$record$host <- handle$host
  handle
}

builder_coordinator_settle <- function(
  handle,
  build_result,
  .publish = builder_coordinator_publish,
  .abort = builder_coordinator_abort,
  .recovery = builder_coordinator_recovery
) {
  target <- if (
    is.list(handle) && .builder_release_text(handle$target %||% "")
  ) {
    handle$target
  } else {
    NULL
  }
  build_error <- NULL
  if (is.list(build_result)) {
    for (field in c("error", "message")) {
      candidate <- build_result[[field]]
      if (.builder_release_text(candidate)) {
        build_error <- candidate
        break
      }
    }
  }
  failed <- function(
    message,
    cleanup_error = NULL,
    settlement_error = NULL
  ) {
    recovery <- NULL
    recovery_error <- NULL
    if (!is.null(target)) {
      discovered <- tryCatch(.recovery(target), error = function(error) error)
      if (inherits(discovered, "condition")) {
        recovery_error <- conditionMessage(discovered)
      } else {
        recovery <- discovered
      }
    }
    list(
      ok = FALSE,
      value = NULL,
      error = message,
      cleanup_error = cleanup_error,
      settlement_error = settlement_error,
      recovery_error = recovery_error,
      target = target,
      recovery = recovery
    )
  }
  if (is.null(target)) {
    coordinator_error <- "The parent release coordinator identity was lost."
    return(failed(
      build_error %||% coordinator_error,
      settlement_error = if (is.null(build_error)) NULL else coordinator_error
    ))
  }
  if (
    is.list(build_result) &&
      identical(build_result$state, "success") &&
      isTRUE(build_result$publishable)
  ) {
    published <- tryCatch(
      .publish(handle, build_result),
      error = function(error) error
    )
    if (!inherits(published, "condition")) {
      return(list(
        ok = TRUE,
        value = published,
        error = NULL,
        target = target,
        recovery = NULL
      ))
    }
    publication_error <- conditionMessage(published)
    cleaned <- tryCatch(.abort(handle), error = function(error) error)
    cleanup_error <- if (inherits(cleaned, "condition")) {
      conditionMessage(cleaned)
    } else if (!isTRUE(cleaned$aborted)) {
      "The assigned build stage could not be cleaned."
    } else {
      NULL
    }
    return(failed(publication_error, cleanup_error = cleanup_error))
  }
  cleaned <- tryCatch(.abort(handle), error = function(error) error)
  if (inherits(cleaned, "condition") || !isTRUE(cleaned$aborted)) {
    cleanup_error <- if (inherits(cleaned, "condition")) {
      conditionMessage(cleaned)
    } else {
      "The assigned build stage could not be cleaned."
    }
    return(failed(
      build_error %||% cleanup_error,
      cleanup_error = cleanup_error
    ))
  }
  list(
    ok = TRUE,
    value = build_result,
    error = NULL,
    target = target,
    recovery = NULL
  )
}

builder_coordinator_recovery <- function(target) {
  builder_discover_recovery(target)
}

builder_coordinator_recover <- function(
  target,
  action = c("restore", "abort")
) {
  builder_recover_release(target, action = match.arg(action))
}
