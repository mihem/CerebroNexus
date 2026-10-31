##----------------------------------------------------------------------------##
## Build Cerebro data sets from Seurat objects, by pointing and clicking.
##
## Several objects per session: each becomes a .crb, and they are bundled into
## one app with the data set switcher, which is how a lab usually wants to hand
## a project over -- one link, every sample behind a dropdown.
##
## Objects are opened only in an isolated worker process. This is still a tool
## for your own machine or RStudio Server session, not a service to deploy for
## untrusted users.
##----------------------------------------------------------------------------##

.builder_app_shiny_was_attached <- "package:shiny" %in% search()
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The shiny package is required to run Builder.", call. = FALSE)
}
## Import Shiny's public API into this App environment without attaching the
## package to the caller's search path.
.builder_app_environment <- environment()
for (.builder_shiny_export in getNamespaceExports("shiny")) {
  if (
    !exists(
      .builder_shiny_export,
      envir = .builder_app_environment,
      inherits = FALSE
    )
  ) {
    assign(
      .builder_shiny_export,
      getExportedValue("shiny", .builder_shiny_export),
      envir = .builder_app_environment
    )
  }
}
rm(.builder_shiny_export, .builder_app_environment)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Bootstrap the UTF-8 loader without changing the caller's process-wide
## encoding option. launchCerebroBuilder() runs in the user's R session, so a
## global option change here would survive after the App stops.
base::source(
  "prerequisite.R",
  local = TRUE,
  encoding = "UTF-8"
)
source <- function(file, local = FALSE, ...) {
  envir <- if (isTRUE(local)) {
    parent.frame()
  } else if (is.environment(local)) {
    local
  } else {
    globalenv()
  }
  builder_source_utf8(file, envir = envir)
}
builder_source_package <- builder_source_package_root()
builder_runtime_version <- builder_runtime_package_version()
builder_app_directory <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)
runtime_capability <- NULL

.builder_app_process_state <- new.env(parent = emptyenv())
.builder_app_process_state$active <- FALSE
.builder_app_process_state$prewarmed_worker <- NULL
.builder_app_process_state$prewarm_scheduled <- FALSE

builder_app_stop_prewarmed_worker <- function(
  .stop = builder_worker_stop
) {
  worker <- .builder_app_process_state$prewarmed_worker
  .builder_app_process_state$prewarmed_worker <- NULL
  if (is.null(worker)) {
    return(invisible(FALSE))
  }
  try(.stop(worker, grace_ms = 5000L), silent = TRUE)
  invisible(TRUE)
}

builder_app_poll_prewarmed_worker <- function(
  .poll = builder_worker_poll_startup,
  .schedule = later::later,
  .stop = builder_worker_stop
) {
  worker <- .builder_app_process_state$prewarmed_worker
  if (!isTRUE(.builder_app_process_state$active) || is.null(worker)) {
    return(invisible(FALSE))
  }
  startup <- try(.poll(worker), silent = TRUE)
  if (inherits(startup, "try-error")) {
    builder_app_stop_prewarmed_worker(.stop = .stop)
    return(invisible(FALSE))
  }
  .builder_app_process_state$prewarmed_worker <- startup$worker
  if (identical(startup$state, "starting")) {
    .schedule(builder_app_poll_prewarmed_worker, delay = 0.1)
    return(invisible(TRUE))
  }
  if (!identical(startup$state, "ready")) {
    builder_app_stop_prewarmed_worker(.stop = .stop)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

builder_app_prewarm_worker <- function(
  .start = builder_worker_start,
  .schedule = later::later
) {
  if (!is.null(.builder_app_process_state$prewarmed_worker)) {
    return(invisible(FALSE))
  }
  worker <- try(
    .start(builder_app_directory, .async = TRUE),
    silent = TRUE
  )
  if (
    inherits(worker, "try-error") ||
      !is.null(worker$error) ||
      !inherits(worker, "builder_worker")
  ) {
    return(invisible(FALSE))
  }
  .builder_app_process_state$prewarmed_worker <- worker
  .schedule(builder_app_poll_prewarmed_worker, delay = 0)
  invisible(TRUE)
}

builder_app_schedule_prewarm_worker <- function(
  delay = 1,
  .schedule = later::later,
  .prewarm = builder_app_prewarm_worker
) {
  if (
    !isTRUE(.builder_app_process_state$active) ||
      isTRUE(.builder_app_process_state$prewarm_scheduled) ||
      !is.null(.builder_app_process_state$prewarmed_worker)
  ) {
    return(invisible(FALSE))
  }
  .builder_app_process_state$prewarm_scheduled <- TRUE
  .schedule(
    function() {
      .builder_app_process_state$prewarm_scheduled <- FALSE
      if (
        isTRUE(.builder_app_process_state$active) &&
          is.null(.builder_app_process_state$prewarmed_worker)
      ) {
        try(.prewarm(), silent = TRUE)
      }
    },
    delay = delay
  )
  invisible(TRUE)
}

builder_app_take_prewarmed_worker <- function(
  .alive = function(worker) worker$process$is_alive(),
  .stop = builder_worker_stop,
  .replenish = builder_app_schedule_prewarm_worker
) {
  worker <- .builder_app_process_state$prewarmed_worker
  .builder_app_process_state$prewarmed_worker <- NULL
  if (is.null(worker)) {
    return(NULL)
  }
  alive <- try(.alive(worker), silent = TRUE)
  if (!isTRUE(alive)) {
    try(.stop(worker, grace_ms = 5000L), silent = TRUE)
    return(NULL)
  }
  if (identical(worker$state %||% NULL, "ready")) {
    try(.replenish(), silent = TRUE)
  }
  worker
}

builder_app_restore_process_state <- function() {
  if (!isTRUE(.builder_app_process_state$active)) {
    return(invisible(FALSE))
  }
  builder_app_stop_prewarmed_worker()
  .builder_app_process_state$prewarm_scheduled <- FALSE
  options(.builder_app_process_state$request_size)
  source_package <- .builder_app_process_state$source_package
  if (is.na(source_package)) {
    Sys.unsetenv("CEREBRO_PACKAGE_SOURCE")
  } else {
    do.call(
      Sys.setenv,
      stats::setNames(list(source_package), "CEREBRO_PACKAGE_SOURCE")
    )
  }
  if (
    !isTRUE(.builder_app_process_state$shiny_was_attached) &&
      "package:shiny" %in% search()
  ) {
    try(
      detach("package:shiny", unload = FALSE, character.only = TRUE),
      silent = TRUE
    )
  }
  .builder_app_process_state$active <- FALSE
  invisible(TRUE)
}

builder_app_start <- function(
  .capability = builder_runtime_capability(),
  .prewarm = builder_app_prewarm_worker
) {
  if (isTRUE(.builder_app_process_state$active)) {
    return(invisible(FALSE))
  }
  if (!isTRUE(.capability$available)) {
    stop(.capability$reason, call. = FALSE)
  }
  runtime_capability <<- .capability
  .builder_app_process_state$request_size <- options("shiny.maxRequestSize")
  .builder_app_process_state$source_package <- Sys.getenv(
    "CEREBRO_PACKAGE_SOURCE",
    unset = NA_character_
  )
  .builder_app_process_state$shiny_was_attached <-
    .builder_app_shiny_was_attached
  .builder_app_process_state$active <- TRUE
  shiny::onStop(builder_app_restore_process_state)
  ## Local Seurat objects and microscopy images can be arbitrarily large.
  ## Respect an explicit launcher-supplied cap; otherwise impose none.
  if (is.null(getOption("shiny.maxRequestSize"))) {
    options(shiny.maxRequestSize = Inf)
  }
  if (!is.null(builder_source_package)) {
    builder_activate_source_package(builder_source_package)
  }
  ## Start the empty isolated worker before the browser creates its first
  ## session. Windows can spend several seconds starting a cold R process; by
  ## overlapping that work with browser launch and asset loading, the first
  ## session normally receives an already-ready worker without sharing any
  ## dataset state between sessions.
  try(
    builder_app_schedule_prewarm_worker(delay = 0, .prewarm = .prewarm),
    silent = TRUE
  )
  invisible(TRUE)
}

source(
  file.path("core", "bundle_path_contract.R"),
  local = TRUE
)
source("publish.R", local = TRUE)
source("app_bundle.R", local = TRUE)
source("report.R", local = TRUE)
source("coordinator.R", local = TRUE)

## runApp() sets the working directory to the app directory.
source("notify.R", local = TRUE)
source("io.R", local = TRUE)
source("project.R", local = TRUE)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "viewer_content_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  local = TRUE
)
source("spatial.R", local = TRUE)
## The HLA core lives in the package R/ directory: source it from a
## checkout, or take it from the installed namespace.
.builder_hla_core_files <- file.path(
  "..",
  "..",
  "R",
  c("hla_typing.R", "hla_motif_core.R", "hla_association_core.R")
)
if (all(file.exists(.builder_hla_core_files))) {
  for (.builder_hla_core_file in .builder_hla_core_files) {
    source(.builder_hla_core_file, local = TRUE)
  }
} else {
  list2env(
    utils::getFromNamespace(".viewerHLACore", "CerebroNexus")(),
    envir = environment()
  )
}
source("manifest.R", local = TRUE)
source("content_tables.R", local = TRUE)
source("content_immune.R", local = TRUE)
source("content_spatial.R", local = TRUE)
source("content.R", local = TRUE)
source("profile.R", local = TRUE)
source("inspect.R", local = TRUE)
source("recommend.R", local = TRUE)
source("adapters.R", local = TRUE)
source("preview.R", local = TRUE)
source("extras.R", local = TRUE)
source("analysis.R", local = TRUE)
source("marker_import.R", local = TRUE)
source("build.R", local = TRUE)
source("state.R", local = TRUE)
source("review.R", local = TRUE)
source("workflow.R", local = TRUE)
source("loading.R", local = TRUE)
source("diagnostics.R", local = TRUE)
source("async.R", local = TRUE)
source(file.path("ui", "dataset_rail.R"), local = TRUE)
source("plan.R", local = TRUE)
source(file.path("ui", "inspect_stage.R"), local = TRUE)
source(file.path("ui", "core_stage.R"), local = TRUE)
source(file.path("ui", "marker_import.R"), local = TRUE)
source(file.path("ui", "enhance_stage.R"), local = TRUE)
source(file.path("ui", "review_stage.R"), local = TRUE)
source(file.path("ui", "workflow.R"), local = TRUE)
source(file.path("ui", "build_status.R"), local = TRUE)
source(file.path("ui", "project.R"), local = TRUE)
source("worker.R", local = TRUE)
source("session.R", local = TRUE)
source("spatial_alignment_server.R", local = TRUE)

builder_projection_preview_contract <- function(entry, projections) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    group = entry$settings$default_group %||% NULL,
    projections = projections
  )
}

builder_trajectory_preview_contract <- function(entry, trajectories) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    trajectories = trajectories
  )
}

builder_table_inventory_owner <- function(entry, project_epoch) {
  if (!is.list(entry) || !builder_has_text(entry$id)) {
    return(NULL)
  }
  snapshot_identity <- .builder_worker_identity(entry$snapshot)
  epoch <- suppressWarnings(as.double(project_epoch))
  if (
    !builder_has_text(snapshot_identity) ||
      length(epoch) != 1L ||
      is.na(epoch) ||
      !is.finite(epoch)
  ) {
    return(NULL)
  }
  list(
    dataset = as.character(entry$id),
    snapshot_identity = snapshot_identity,
    project_epoch = epoch
  )
}

builder_table_inventory_owner_is_current <- function(
  owner,
  entry,
  project_epoch
) {
  !is.null(owner) &&
    identical(
      owner,
      builder_table_inventory_owner(entry, project_epoch)
    )
}

builder_preview_cache_hit <- function(cache, id, contract) {
  record <- cache[[id]] %||% NULL
  is.list(record) && identical(record$contract, contract)
}

builder_preview_cache_begin <- function(cache, id, contract) {
  cache[[id]] <- list(
    contract = contract,
    frames = list(),
    status = "pending"
  )
  cache
}

builder_preview_cache_store <- function(cache, id, frames) {
  record <- cache[[id]] %||% list(contract = NULL)
  record$frames <- frames %||% list()
  record$status <- "ready"
  cache[[id]] <- record
  cache
}

builder_preview_cache_frames <- function(cache, id) {
  record <- cache[[id]] %||% NULL
  if (is.list(record)) record$frames %||% list() else list()
}

builder_preview_revision_independent <- function(kind) {
  kind %in%
    c(
      "projection_previews",
      "trajectory_previews",
      "spatial_preview"
    )
}

app_capability <- builder_app_capability()
auth_capability <- builder_auth_capability

## The viewer's wordmark, inlined. The builder and the viewer are one product
## and should look like it; inlining avoids a resource path that would have to
## resolve both from the installed package and from a source checkout.
cerebro_wordmark <- local({
  candidates <- c(
    system.file(
      "viewer/www/cerebronexus.svg",
      package = "CerebroNexus"
    ),
    file.path("..", "viewer", "www", "cerebronexus.svg")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) {
    HTML(paste(readLines(hit[1], warn = FALSE), collapse = "\n"))
  } else {
    NULL
  }
})

## Stamp the assets so a browser that already has them does not keep them past
## an upgrade. Shiny serves www/ with caching on, and a cached stylesheet
## against new markup is worse than either alone -- the classes change and the
## rules that give them meaning do not arrive. The stamp is the file's own
## mtime, so it changes exactly when the file does, including while editing.
asset_stamp <- function(file) {
  mt <- tryCatch(file.mtime(file), error = function(e) NA)
  if (is.na(mt)) {
    return("")
  }
  paste0("?v=", as.integer(as.numeric(mt)))
}

builder_stylesheet_files <- c(
  "builder.tokens.css",
  "builder.base.css",
  "builder.layout.css",
  "builder.components.css",
  "builder.features.css",
  "builder.editorial.css"
)

builder_stylesheet_tags <- function(
  files = builder_stylesheet_files
) {
  tagList(lapply(files, function(file) {
    tags$link(
      rel = "stylesheet",
      href = paste0(file, asset_stamp(file.path("www", file)))
    )
  }))
}

builder_example_buttons_ui <- function(examples = builder_example_directory()) {
  tagList(lapply(examples, function(ex) {
    members <- ex$examples %||% list(list(id = ex$id, label = ex$label))
    tags$button(
      class = "builder-data-source example-btn",
      type = "button",
      `data-ex` = ex$id,
      `data-label` = ex$label,
      `data-examples` = jsonlite::toJSON(
        members,
        auto_unbox = TRUE
      ),
      `aria-disabled` = "false",
      tags$span(
        class = "builder-data-source-icon",
        `aria-hidden` = "true",
        shiny::icon("wand-magic-sparkles")
      ),
      tags$span(
        class = "builder-data-source-copy",
        tags$strong(class = "ex-label", ex$label),
        tags$small(class = "ex-detail", ex$detail)
      ),
      tags$span(
        class = "builder-data-source-tag is-example",
        "Load Example"
      )
    )
  }))
}

ui <- shiny::bootstrapPage(
  lang = "en",
  tags$head(
    builder_stylesheet_tags(),
    tags$script(src = paste0("icons.js", asset_stamp("www/icons.js"))),
    tags$script(
      src = paste0(
        "builder-spatial-canvas.js",
        asset_stamp("www/builder-spatial-canvas.js")
      )
    ),
    tags$script(src = paste0("builder.js", asset_stamp("www/builder.js"))),
    tags$title("Cerebro Dataset Builder")
  ),
  tags$a(
    class = "builder-skip-link",
    href = "#builder-workspace",
    "Skip to workspace"
  ),
  tags$header(
    class = "topbar builder-project-header",
    div(
      class = "builder-project-brand",
      div(class = "wordmark", cerebro_wordmark),
      if (!is.null(builder_runtime_version)) {
        tags$span(
          class = "builder-runtime-version",
          `aria-label` = paste("CerebroNexus version", builder_runtime_version),
          paste0("v", builder_runtime_version)
        )
      },
      tags$h1(class = "visually-hidden", "CerebroNexus Builder")
    ),
    uiOutput("busy", inline = TRUE),
    ## Worker start-up status sits in the header, not a floating card.
    div(
      id = "builder-worker-status",
      class = "builder-worker-status is-starting",
      `aria-hidden` = "false",
      span(class = "builder-worker-status-dot", `aria-hidden` = "true"),
      span(
        class = "builder-worker-status-copy",
        strong(
          id = "builder-worker-status-title",
          "Starting Builder\u2026"
        ),
        span(
          id = "builder-worker-status-detail",
          "Preparing your workspace\u2026"
        )
      )
    ),
    builder_project_toolbar_ui()
  ),
  uiOutput("workflow_progress"),
  ## Global notices (builder_notify with no target) slide in here.
  div(
    id = "builder-notice-bar",
    class = "builder-notice-bar",
    role = "region",
    `aria-label` = "Notices"
  ),
  div(
    class = "shell builder-shell",
    div(
      class = "rail",
      div(
        class = "rail-head",
        span("Datasets"),
        span(
          textOutput("ds_count", inline = TRUE),
          uiOutput("rail_undo", inline = TRUE)
        )
      ),
      div(
        id = "ds_list",
        class = "ds-picker",
        div(
          id = "ds_ready_list",
          class = "shiny-html-output",
          builder_dataset_rail_ui(builder_state())
        ),
        div(
          id = "ds_import_list",
          class = "shiny-html-output"
        ),
        div(
          id = "ds_client_import_queue",
          class = "builder-client-import-queue",
          `aria-live` = "polite",
          `aria-relevant` = "additions text"
        )
      ),
      div(
        class = "rail-add-actions",
        tags$button(
          type = "button",
          class = "btn builder-add-datasets",
          `aria-label` = "Add dataset files",
          tags$span(
            class = "rail-add-title",
            tags$span(
              class = "rail-add-icon",
              `aria-hidden` = "true",
              shiny::icon("folder-open")
            ),
            tags$strong(class = "builder-add-label", "Add files")
          ),
          tags$small(
            class = "rail-add-detail",
            paste(builder_advertised_format_labels(), collapse = ", ")
          )
        ),
        builder_deprecated_format_note_ui(),
        builder_format_unavailable_ui(builder_format_capabilities()),
        tags$input(
          id = "dataset_files",
          name = "dataset_files",
          class = "shiny-input-file builder-upload-transport",
          type = "file",
          accept = builder_file_accept(
            builder_dataset_extensions(available_only = TRUE)
          ),
          hidden = "hidden"
        ),
        # Shiny reports upload progress into this bar; builder.js mirrors it
        # into the client import queue row.
        tags$div(
          id = "dataset_files_progress",
          class = "shiny-file-input-progress builder-upload-transport-progress",
          hidden = "hidden",
          tags$div(class = "progress-bar")
        )
      )
    ),
    tags$main(
      id = "builder-workspace",
      class = "builder-content",
      tabindex = "-1",
      div(
        id = "pane",
        div(
          id = "workbench",
          class = "shiny-html-output",
          tabindex = "-1",
          builder_empty_workbench_ui(
            formats = builder_format_capabilities(),
            examples = builder_example_directory()
          )
        )
      )
    ),
  ),
  div(
    id = "builder-operation-overlay",
    class = "builder-operation-overlay",
    role = "dialog",
    `aria-modal` = "true",
    `aria-hidden` = "true",
    `aria-labelledby` = "builder-operation-overlay-title",
    `aria-describedby` = paste(
      "builder-operation-overlay-detail",
      "builder-operation-overlay-message"
    ),
    tabindex = "-1",
    div(
      class = "builder-operation-overlay-card",
      span(
        class = "builder-operation-overlay-icon",
        `aria-hidden` = "true",
        span(class = "spinner"),
        span(class = "builder-operation-success-mark", "\u2713"),
        span(class = "builder-operation-error-mark", "!")
      ),
      div(
        class = "builder-operation-overlay-copy",
        div(
          class = "builder-operation-overlay-heading",
          strong(
            id = "builder-operation-overlay-title",
            "Working on your Builder project"
          ),
          span(
            id = "builder-build-progress-summary",
            class = "builder-build-progress-summary",
            hidden = "hidden"
          )
        ),
        span(
          id = "builder-operation-overlay-detail",
          class = "builder-operation-overlay-detail",
          role = "status",
          `aria-live` = "polite",
          `aria-atomic` = "true"
        ),
        span(
          id = "builder-operation-overlay-message",
          "Keep this page open."
        ),
        tags$div(
          id = "builder-build-progress",
          class = "builder-build-progress",
          hidden = "hidden",
          builder_build_progress_detail_ui("builder-build-progress-detail"),
          tags$ol(
            class = "builder-build-progress-steps",
            tags$li(`data-build-phase` = "preflight", "Check release"),
            tags$li(`data-build-phase` = "stage", "Create safe stage"),
            tags$li(`data-build-phase` = "dispatch", "Start worker"),
            tags$li(`data-build-phase` = "datasets", "Build datasets"),
            tags$li(`data-build-phase` = "viewer", "Package Viewer"),
            tags$li(`data-build-phase` = "verify", "Verify output"),
            tags$li(`data-build-phase` = "publish", "Publish release")
          )
        )
      ),
      div(
        id = "builder-operation-overlay-actions",
        class = "builder-operation-overlay-actions",
        `aria-hidden` = "true"
      )
    )
  ),
  builder_auth_dialog_ui(),
  builder_marker_dialog_ui(),
  div(
    id = "builder-live-status",
    class = "visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  )
)

.builder_server_sources <- c(
  "server/foundation.R",
  "server/imports.R",
  "server/datasets.R",
  "server/enhancements.R",
  "server/review.R",
  "server/workflow.R",
  "server/build.R",
  "server/project.R"
)
.builder_server_programs <- lapply(.builder_server_sources, function(file) {
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(lines)) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }
  parse(
    text = lines,
    srcfile = srcfilecopy(path, lines, isFile = TRUE),
    encoding = "UTF-8",
    keep.source = FALSE
  )
})
names(.builder_server_programs) <- .builder_server_sources

## Compile hot state/UI closures before accepting sessions. Otherwise R's JIT
## compiles them together during the first import/restore, blocking UI updates.
## Do not alter process-wide JIT settings or functions owned by other packages.
.builder_compile_environment <- environment()
for (.builder_compile_name in ls(
  .builder_compile_environment,
  all.names = TRUE,
  pattern = paste0(
    "^([.]builder_(state_|store_|manifest_|page_)|builder_(dataset_state|reduce_state|",
    "group_catalog|projection_catalog|upgrade_viewer_content|",
    "core_stage_ui|enhance_model|specialized_content_model|viewer_|content_|",
    "analysis_results_model|immune_source_choices_model|build_stage_status|activity_|project_(cached_configuration|",
    "file_fingerprint|fingerprint|checked|live_dirty)))"
  )
)) {
  if (bindingIsActive(.builder_compile_name, .builder_compile_environment)) {
    next
  }
  .builder_compile_value <- get(
    .builder_compile_name,
    .builder_compile_environment
  )
  if (
    is.function(.builder_compile_value) &&
      identical(
        environment(.builder_compile_value),
        .builder_compile_environment
      )
  ) {
    assign(
      .builder_compile_name,
      compiler::cmpfun(.builder_compile_value),
      envir = .builder_compile_environment
    )
  }
}
.builder_server_programs <- lapply(.builder_server_programs, function(program) {
  compiler::compile(
    as.call(c(list(as.name("{")), as.list(program))),
    env = .builder_compile_environment
  )
})
rm(.builder_compile_name, .builder_compile_value, .builder_compile_environment)

server <- function(input, output, session) {
  server_environment <- environment()
  for (program in .builder_server_programs) {
    eval(program, envir = server_environment)
  }
  rm(server_environment, program)
}

shinyApp(ui, server, onStart = builder_app_start)
