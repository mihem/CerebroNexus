.generated_app_e2e_cache <- new.env(parent = emptyenv())
.generated_app_e2e_cache$bundle <- NULL
.generated_app_e2e_cache$server <- NULL
.generated_app_e2e_cache$driver <- NULL
.generated_app_e2e_cache$root <- NULL
.generated_app_e2e_cache$teardown_registered <- FALSE

generated_app_e2e_reset_driver <- function() {
  cache <- .generated_app_e2e_cache
  if (is.null(cache$driver)) {
    return(invisible(NULL))
  }
  driver <- cache$driver
  session_error <- NULL
  chromote_session <- tryCatch(
    driver$get_chromote_session(),
    error = function(error) {
      session_error <<- error
      NULL
    }
  )
  stop_error <- tryCatch(
    {
      driver$stop()
      NULL
    },
    error = identity
  )
  still_active <- if (is.null(chromote_session)) {
    NA
  } else {
    tryCatch(
      isTRUE(chromote_session$is_active()),
      error = function(error) NA
    )
  }
  if (isTRUE(still_active)) {
    try(chromote_session$close(), silent = TRUE)
    still_active <- tryCatch(
      isTRUE(chromote_session$is_active()),
      error = function(error) NA
    )
  }
  unverified <- is.na(still_active) && !is.null(stop_error)
  if (isTRUE(still_active) || unverified) {
    stop(
      paste0(
        "AppDriver remained active after reset",
        if (is.null(stop_error)) {
          ""
        } else {
          paste0(": ", conditionMessage(stop_error))
        },
        if (is.null(session_error)) {
          ""
        } else {
          paste0("; session check failed: ", conditionMessage(session_error))
        }
      ),
      call. = FALSE
    )
  }
  cache$driver <- NULL
  invisible(NULL)
}

generated_app_e2e_reset_server <- function() {
  cache <- .generated_app_e2e_cache
  if (is.null(cache$server)) {
    return(invisible(NULL))
  }
  server <- cache$server
  stop_error <- tryCatch(
    {
      privacy_stop_app(server)
      NULL
    },
    error = identity
  )
  alive <- tryCatch(
    isTRUE(server$process$is_alive()),
    error = function(error) NA
  )
  if (isTRUE(alive)) {
    try(server$process$kill_tree(), silent = TRUE)
    try(server$process$wait(timeout = 5000), silent = TRUE)
    alive <- tryCatch(
      isTRUE(server$process$is_alive()),
      error = function(error) NA
    )
  }
  if (isTRUE(alive) || is.na(alive)) {
    stop(
      paste0(
        "Generated App process could not be verified as stopped",
        if (is.null(stop_error)) {
          ""
        } else {
          paste0(": ", conditionMessage(stop_error))
        },
        "\n",
        privacy_app_logs(server)
      ),
      call. = FALSE
    )
  }
  cache$server <- NULL
  invisible(NULL)
}

generated_app_e2e_reset_runtime <- function() {
  failures <- character()
  for (reset in list(
    generated_app_e2e_reset_driver,
    generated_app_e2e_reset_server
  )) {
    reset_error <- tryCatch(
      {
        reset()
        NULL
      },
      error = identity
    )
    if (!is.null(reset_error)) {
      failures <- c(failures, conditionMessage(reset_error))
    }
  }
  if (length(failures)) {
    stop(paste(failures, collapse = "\n"), call. = FALSE)
  }
  invisible(NULL)
}

generated_app_e2e_stop <- function() {
  cache <- .generated_app_e2e_cache
  root <- cache$root
  failures <- character()

  for (reset in list(
    generated_app_e2e_reset_driver,
    generated_app_e2e_reset_server
  )) {
    reset_error <- tryCatch(
      {
        reset()
        NULL
      },
      error = identity
    )
    if (!is.null(reset_error)) {
      failures <- c(failures, conditionMessage(reset_error))
    }
  }
  if (length(failures)) {
    stop(paste(failures, collapse = "\n"), call. = FALSE)
  }
  if (!is.null(root) && dir.exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }
  cache$root <- NULL
  cache$bundle <- NULL
  cache$teardown_registered <- FALSE
  invisible(NULL)
}

.generated_app_e2e_register_teardown <- function() {
  cache <- .generated_app_e2e_cache
  if (isTRUE(cache$teardown_registered)) {
    return(invisible(NULL))
  }
  teardown <- tryCatch(
    testthat::teardown_env(),
    error = function(error) environment(generated_app_e2e_stop)
  )
  withr::defer(generated_app_e2e_stop(), envir = teardown)
  cache$teardown_registered <- TRUE
  invisible(NULL)
}

.generated_app_e2e_root <- function() {
  cache <- .generated_app_e2e_cache
  if (is.null(cache$root)) {
    cache$root <- tempfile("cerebro-generated-app-e2e-")
    dir.create(cache$root, recursive = TRUE, showWarnings = FALSE)
    .generated_app_e2e_register_teardown()
  }
  cache$root
}

.generated_app_e2e_id <- function(name) {
  paste0("e2e-", gsub("_", "-", name, fixed = TRUE))
}

.generated_app_e2e_record <- function(name, fixture) {
  object <- unserialize(serialize(fixture$object, NULL, version = 3L))
  list(
    id = .generated_app_e2e_id(name),
    label = fixture$expected$dataset_name,
    make = local({
      value <- object
      function() {
        list(
          object = unserialize(serialize(value, NULL, version = 3L)),
          format = "Generated App E2E fixture"
        )
      }
    })
  )
}

.generated_app_e2e_images <- function(fixture) {
  if (!length(fixture$attachments)) {
    return(list())
  }
  Map(
    function(attachment, section) {
      inspected <- builder_read_image(attachment$path)
      record <- builder_alignment_record(
        source = list(
          name = basename(attachment$path),
          type = inspected$mime,
          size = inspected$bytes
        ),
        base_bounds = as.list(attachment$bounds),
        section = list(id = section, kind = "spatial"),
        source_path = inspected$source_path
      )
      record$source_content_md5 <- inspected$source_content_md5
      record
    },
    fixture$attachments,
    names(fixture$attachments)
  )
}

.generated_app_e2e_entry <- function(name, fixture, snapshot_root) {
  record <- .generated_app_e2e_record(name, fixture)
  entry <- builder_e2e_entry(record)
  overrides <- fixture$builder_settings
  entry$settings <- utils::modifyList(entry$settings, overrides)
  entry$settings$metadata_policy <- builder_e2e_review_metadata_policy(
    entry$settings$recommendations$metadata,
    overrides$groups
  )
  entry$settings$groups <- overrides$groups
  entry$settings$included_groups <- overrides$groups
  entry$settings$reductions <- overrides$reductions
  entry$settings$included_projections <- overrides$reductions
  entry$settings$analyses <- character()
  entry$settings$tables <- list()
  entry$settings$images <- .generated_app_e2e_images(fixture)
  entry$settings$spatial_image_storage <- "external"
  snapshot <- builder_snapshot_seurat(
    fixture$object,
    file.path(snapshot_root, record$id),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot
  list(entry = entry, snapshot = snapshot)
}

.generated_app_e2e_build_bundle <- function() {
  generated_app_fixture_source_runtime()
  root <- .generated_app_e2e_root()
  fixtures <- generated_app_fixture_matrix()
  snapshot_root <- file.path(root, "snapshots")
  dir.create(snapshot_root, recursive = TRUE, showWarnings = FALSE)

  prepared <- lapply(names(fixtures), function(name) {
    .generated_app_e2e_entry(name, fixtures[[name]], snapshot_root)
  })
  names(prepared) <- names(fixtures)
  entries <- lapply(prepared, `[[`, "entry")
  snapshots <- lapply(prepared, `[[`, "snapshot")
  names(snapshots) <- vapply(entries, `[[`, character(1), "id")

  app_settings <- fixtures$basic$expected$app_settings
  release <- file.path(root, "release")
  plan <- builder_freeze_plan(
    unname(entries),
    release,
    make_app = TRUE,
    app_options = c(app_settings, list(launch_browser = FALSE)),
    app_auth = list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
  )
  if (!is.null(plan$error)) {
    stop(
      "Generated App E2E BuildPlan failed: ",
      plan$error,
      call. = FALSE
    )
  }

  coordinator <- builder_coordinator_prepare(plan, "generated-app-e2e")
  result <- builder_execute_plan(plan, coordinator$stage, snapshots)
  result$build_id <- coordinator$build_id
  if (!identical(result$state, "success") || !isTRUE(result$publishable)) {
    stop(
      "Generated App E2E build failed: ",
      result$error %||% paste(result$failures, collapse = "; "),
      call. = FALSE
    )
  }
  published <- builder_coordinator_publish(coordinator, result)
  if (!isTRUE(published$published)) {
    stop("Generated App E2E release was not published.", call. = FALSE)
  }

  labels <- vapply(
    fixtures,
    function(fixture) fixture$expected$dataset_name,
    character(1)
  )
  built_by_label <- stats::setNames(published$built, labels)
  crbs <- lapply(unname(built_by_label), readCerebro)
  names(crbs) <- names(fixtures)
  config_path <- file.path(published$app_dir, "cerebro_config.rds")
  if (!file.exists(config_path)) {
    stop("Generated App E2E config is missing.", call. = FALSE)
  }

  list(
    root = root,
    fixtures = fixtures,
    entries = entries,
    plan = plan,
    result = result,
    published = published,
    crb_paths = stats::setNames(unname(built_by_label), names(fixtures)),
    crbs = crbs,
    app_dir = published$app_dir,
    config = readRDS(config_path)
  )
}

generated_app_e2e_bundle <- function() {
  cache <- .generated_app_e2e_cache
  if (is.null(cache$bundle)) {
    cache$bundle <- tryCatch(
      .generated_app_e2e_build_bundle(),
      error = function(error) {
        generated_app_e2e_stop()
        stop(error)
      }
    )
  }
  cache$bundle
}

generated_app_e2e_server <- function() {
  cache <- .generated_app_e2e_cache
  if (!is.null(cache$server)) {
    return(cache$server)
  }
  bundle <- generated_app_e2e_bundle()
  runtime_root <- file.path(bundle$root, "runtime")
  dir.create(runtime_root, recursive = TRUE, showWarnings = FALSE)
  library <- privacy_hermetic_library(bundle$root)
  if (is.null(library) || !dir.exists(library)) {
    stop("A package-free test library could not be created.", call. = FALSE)
  }
  port <- httpuv::randomPort(host = "127.0.0.1")
  cache$server <- privacy_start_app(
    bundle$app_dir,
    port,
    runtime_root,
    libpath = library,
    exclude_package = TRUE,
    test_mode = TRUE
  )
  cache$server$library <- library
  privacy_wait_for_app(cache$server)
  cache$server
}

generated_app_e2e_request <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("One generated App URL path is required.", call. = FALSE)
  }
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }
  privacy_get_from_app(generated_app_e2e_server(), path)
}

generated_app_e2e_document_urls <- function(html) {
  if (!is.character(html) || length(html) != 1L || is.na(html)) {
    stop("One generated App HTML document is required.", call. = FALSE)
  }
  matches <- gregexpr(
    "(?:src|href)=[\\\"'][^\\\"']+[\\\"']",
    html,
    perl = TRUE
  )
  tokens <- regmatches(html, matches)[[1L]]
  if (!length(tokens) || identical(tokens, character(0))) {
    return(character())
  }
  urls <- sub("^[^=]+=[\\\"']", "", tokens)
  urls <- sub("[\\\"']$", "", urls)
  unique(gsub("&amp;", "&", urls, fixed = TRUE))
}

generated_app_e2e_asset_urls <- function(html) {
  expected <- c(
    "custom.css",
    "trekker.css",
    "hla_motifs.css",
    "fill_height.js",
    "trekker.js",
    "hla_motifs.js",
    "projection_layouts.js",
    "projection_scatter.js"
  )
  urls <- generated_app_e2e_document_urls(html)
  paths <- sub("[?].*$", "", urls)
  names_by_path <- basename(paths)
  selected <- match(expected, names_by_path, nomatch = 0L)
  found <- selected > 0L
  stats::setNames(urls[selected[found]], expected[found])
}

generated_app_e2e_driver <- function() {
  cache <- .generated_app_e2e_cache
  if (!is.null(cache$driver)) {
    return(cache$driver)
  }
  server <- generated_app_e2e_server()
  cache$driver <- shinytest2::AppDriver$new(
    server$base_url,
    name = "generated_app_e2e_matrix",
    width = 1440,
    height = 960,
    load_timeout = 60000
  )
  cache$driver$wait_for_js(
    paste0(
      "window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && ",
      "Shiny.shinyapp.$socket.readyState === 1"
    ),
    timeout = 60000
  )
  cache$driver$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 60000
  )
  cache$driver
}
