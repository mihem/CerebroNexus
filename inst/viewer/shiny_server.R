##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##

## plotting_functions.R holds only pure plot-builder functions (no reactive /
## input / output / session references), so source it ONCE at process startup
## instead of re-evaluating every definition on every new browser session. It
## lands in this file's environment (which encloses server()), so the server
## reactives still reach the functions by lexical scope. color_setup.R and
## utility_functions.R stay inside server(): the former defines a top-level
## reactive (reactive_colors); the latter's helpers close over session-scope
## reactives (data_set(), preferences, ...).
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/plotting_functions.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/color_config.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/source_cache.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/core/viewer_pack.R"),
  local = TRUE
)

## Generated Extra material tables are immutable. Share their lazy cache across
## sessions instead of reading the same sheet again for every browser tab.
.extra_material_process_cache <- new.env(parent = emptyenv())

## Process-owned query/cache functions do not capture a browser session.
source(
  file.path(Cerebro.options[["cerebro_root"]], "viewer", "msigdb.R"),
  local = TRUE
)
source(
  file.path(Cerebro.options[["cerebro_root"]], "viewer", "crb_cache.R"),
  local = TRUE
)
.viewer_pack_process_cache <- new.env(parent = emptyenv())
.crb_launch_state <- new.env(parent = emptyenv())
.crb_launch_state$prototypes <- Cerebro.options[[".dataset_prototypes"]]
if (!is.list(.crb_launch_state$prototypes)) {
  .crb_launch_state$prototypes <- list()
}
if (length(.crb_launch_state$prototypes)) {
  for (prototype_path in names(.crb_launch_state$prototypes)) {
    viewerPackOpenCached(
      prototype_path,
      .crb_launch_state$prototypes[[prototype_path]],
      .viewer_pack_process_cache
    )
  }
}

server <- function(input, output, session) {
  source <- function(file, local = FALSE, ...) {
    dots <- list(...)
    if (identical(local, TRUE) && !length(dots)) {
      return(viewerSource(file, parent.frame()))
    }
    base::source(file, local = local, ...)
  }

  ##--------------------------------------------------------------------------##
  ## Load color setup and utility functions.
  ##--------------------------------------------------------------------------##
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/color_setup.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/utility_functions.R"
    ),
    local = TRUE
  )
  if (length(.crb_launch_state$prototypes)) {
    backend_plan <- Cerebro.options[[".bundle_backend_plan"]]
    configured_paths <- unname(
      Cerebro.options[["crb_file_to_load"]] %||% character()
    )
    for (prototype_path in names(.crb_launch_state$prototypes)) {
      effective_backend <- .configuredRuntimeBackendPlan(
        prototype_path,
        backend_plan,
        configured_paths
      )
      .cacheCrbPrototype(
        prototype_path,
        .crb_launch_state$prototypes[[prototype_path]],
        .runtimeBackendCacheIdentity(effective_backend)
      )
    }
    .crb_launch_state$prototypes <- NULL
  }
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/coordinated_views/config.R"
    ),
    local = TRUE
  )
  observeEvent(
    input[["cell_view_aux_request"]],
    {
      request <- input[["cell_view_aux_request"]]
      cerebroCellViewAuxRequest(request)
    },
    ignoreInit = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/clone_contract.R"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  preferences <- reactiveValues(
    cell_point_size = list(
      min = 1,
      max = 20,
      step = 1
    ),
    cell_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1
    ),
    cell_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10
    ),
    use_webgl = TRUE,
    show_hover_info_in_projections = ifelse(
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_show_hover_info']]),
      Cerebro.options[['projections_show_hover_info']],
      TRUE
    )
  )

  ## Outputs inside collapsed boxes may stay active while their owning sidebar
  ## page is current. Re-suspend them on every page switch so inactive modules
  ## cannot initialise expensive data or expression backends in the background.
  viewer_hidden_output_options <- list()
  outputOptions <- function(output, x, ...) {
    options <- list(...)
    owner <- viewerOutputTab(x)
    if (identical(options$suspendWhenHidden, FALSE) && !is.na(owner)) {
      viewer_hidden_output_options[[x]] <<- options
      options$suspendWhenHidden <- !identical(
        isolate(input[["sidebar"]]),
        owner
      )
    }
    do.call(shiny::outputOptions, c(list(x = output, name = x), options))
  }
  observeEvent(input[["sidebar"]], {
    tab <- input[["sidebar"]]
    ids <- names(viewer_hidden_output_options)
    for (id in ids) {
      options <- viewer_hidden_output_options[[id]]
      options$suspendWhenHidden <- !identical(viewerOutputTab(id), tab)
      do.call(
        shiny::outputOptions,
        c(list(x = output, name = id), options)
      )
    }
  })

  viewer_initial_page_tabs <- c(
    data_info = "loadData",
    projection = "overview",
    linked_views = "coordinated_views",
    groups = "groups",
    marker_genes = "markerGenes",
    most_expressed_genes = "mostExpressedGenes",
    enriched_pathways = "enrichedPathways",
    extra_material = "extra_material",
    immune_repertoire = "immune_repertoire",
    trajectory = "trajectory",
    spatial = "spatial",
    trekker = "trekker",
    hla_tcr_motifs = "hla_tcr_motifs",
    gene_expression = "geneExpression",
    gene_id_conversion = "geneIdConversion",
    color_management = "color_management",
    about = "about"
  )
  initial_page <- Cerebro.options[["initial_page"]]
  initial_tab <- if (
    is.character(initial_page) &&
      length(initial_page) == 1L &&
      initial_page %in% names(viewer_initial_page_tabs)
  ) {
    unname(viewer_initial_page_tabs[[initial_page]])
  } else {
    NULL
  }
  conditional_tabs <- unname(viewer_initial_page_tabs[c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectory",
    "spatial",
    "trekker",
    "hla_tcr_motifs"
  )])
  initial_page_applied <- reactiveVal(FALSE)
  if (!is.null(initial_tab) && !initial_tab %in% conditional_tabs) {
    session$onFlushed(
      function() {
        updateTabItems(session, "sidebar", selected = initial_tab)
        initial_page_applied(TRUE)
      },
      once = TRUE
    )
  }

  ##--------------------------------------------------------------------------##
  ## Load data set.
  ##--------------------------------------------------------------------------##

  ## reactive values holding available .crb files and the current selection.
  ## In single-file mode only 'selected' is used. For configured datasets,
  ## `files` is keyed by stable dataset id, `labels` is display-only, and
  ## `selected` stores the id rather than a label or path.
  available_crb_files <- reactiveValues(
    files = NULL,
    selected = NULL,
    names = NULL,
    labels = NULL
  )
  dataset_load_requested <- reactiveVal(FALSE)

  ## One immutable context identifies every load attempt. `epoch` changes with
  ## the Shiny session; `generation` advances before the public selection is
  ## changed. Browser messages never receive the path/object stored in target.
  viewer_dataset_epoch <- paste0(
    sample(c(letters, LETTERS, 0:9), 24L, replace = TRUE),
    collapse = ""
  )
  viewer_dataset_generation <- 0
  viewer_upload_serial <- 0L
  viewer_dataset_request <- reactiveVal(NULL)

  viewer_send_dataset_phase <- function(
    phase,
    dataset_context,
    request = isolate(viewer_dataset_request())
  ) {
    session$sendCustomMessage(
      "cerebro_dataset_context",
      list(
        phase = as.character(phase),
        dataset_context = dataset_context,
        selection = as.character(request$selection %||% ""),
        selection_nonce = request$selection_nonce %||% 0
      )
    )
  }

  viewer_clear_cell_view_aux <- function() {
    pending <- ls(envir = .cerebro_cell_view_aux_pending, all.names = TRUE)
    if (length(pending)) {
      rm(list = pending, envir = .cerebro_cell_view_aux_pending)
    }
  }

  viewer_request_dataset <- function(
    selection,
    target,
    dataset_key,
    source,
    force = FALSE,
    selection_nonce = 0
  ) {
    current <- isolate(viewer_dataset_request())
    same_request <- is.list(current) &&
      identical(current$selection, selection) &&
      identical(current$target, target) &&
      identical(current$source, source) &&
      identical(current$selection_nonce, selection_nonce) &&
      identical(current$dataset_context$dataset_key, dataset_key)
    if (same_request && !isTRUE(force)) {
      if (!identical(isolate(available_crb_files$selected), selection)) {
        available_crb_files$selected <- selection
      }
      return(invisible(current))
    }

    viewer_dataset_generation <<- viewer_dataset_generation + 1
    dataset_context <- viewerDatasetContext(
      viewer_dataset_epoch,
      dataset_key,
      viewer_dataset_generation
    )
    request <- list(
      selection = selection,
      target = target,
      source = source,
      selection_nonce = selection_nonce,
      dataset_context = dataset_context
    )

    ## Publish the invalidation before changing the selector-backed state. This
    ## makes the browser reject every previous generation while the new object
    ## is loading, and prevents old progressive aux data crossing the boundary.
    viewer_clear_cell_view_aux()
    viewer_send_dataset_phase("pending", dataset_context, request)
    viewer_dataset_request(request)
    if (!identical(isolate(available_crb_files$selected), selection)) {
      available_crb_files$selected <- selection
    }
    invisible(request)
  }

  viewer_requested_dataset_context <- reactive({
    request <- viewer_dataset_request()
    req(is.list(request), is.list(request$dataset_context))
    request$dataset_context
  })

  current_scatter_defaults <- reactive({
    viewerScatterDefaults(
      Cerebro.options,
      viewer_requested_dataset_context()$dataset_key
    )
  })

  current_expression_scatter_defaults <- reactive({
    viewerScatterDefaults(
      Cerebro.options,
      viewer_requested_dataset_context()$dataset_key,
      page = "expression"
    )
  })

  ## listen to selected 'input_file', initialize before UI element is loaded
  observeEvent(input[['input_file']], ignoreNULL = FALSE, {
    selection <- viewerUploadPath(input[["input_file"]], Cerebro.options)
    dataset_key <- NULL
    dataset_source <- NULL
    dataset_target <- NULL
    if (nzchar(selection)) {
      viewer_upload_serial <<- viewer_upload_serial + 1L
      dataset_key <- paste0("__upload_", viewer_upload_serial, "__")
      dataset_source <- "upload"
      dataset_target <- selection
      ## an uploaded file replaces the pre-configured data sets, so clear the
      ## switcher state — otherwise the dropdown keeps offering the old data
      ## sets, which no longer match what is loaded.
      available_crb_files$files <- NULL
      available_crb_files$names <- NULL
      available_crb_files$labels <- NULL
      ## take path or object from 'Cerebro.options' if it is set and points to an
      ## existing file or object
    } else if (
      exists('Cerebro.options') &&
        !is.null(Cerebro.options[["crb_file_to_load"]])
    ) {
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      ## Older launch configurations could provide more than one unnamed path.
      ## Give those entries deterministic opaque ids rather than falling back to
      ## paths as browser-visible identity.
      file_names <- names(file_to_load)
      valid_file_names <- is.character(file_names) &&
        length(file_names) == length(file_to_load) &&
        !anyNA(file_names) &&
        all(nzchar(file_names)) &&
        !anyDuplicated(file_names)
      if (length(file_to_load) > 1L && !valid_file_names) {
        file_names <- paste0("legacy-", seq_along(file_to_load))
        names(file_to_load) <- file_names
      }
      ## multiple files (or a single named file) -> enable dataset switcher
      if (length(file_to_load) > 1 || !is.null(names(file_to_load))) {
        available_crb_files$files <- file_to_load
        file_names <- names(file_to_load)
        if (
          !is.null(file_names) &&
            length(file_names) == length(file_to_load)
        ) {
          available_crb_files$names <- file_names
          configured_labels <- Cerebro.options[["dataset_labels"]]
          if (
            !is.character(configured_labels) ||
              is.null(names(configured_labels)) ||
              !identical(names(configured_labels), file_names) ||
              anyNA(configured_labels) ||
              any(!nzchar(configured_labels))
          ) {
            configured_labels <- stats::setNames(file_names, file_names)
          }
          available_crb_files$labels <- unname(configured_labels[file_names])
        } else {
          available_crb_files$names <- NULL
          available_crb_files$labels <- NULL
        }

        ##--------------------------------------------------------------------##
        ## Check for a dataset specified in the URL (query string or path),
        ## e.g. '?dataset=sampleA' or '/sampleA'.
        ##--------------------------------------------------------------------##
        url_dataset <- NULL

        ## 1. query string (?dataset=...)
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query$dataset)) {
          url_dataset <- query$dataset
        }

        ## 2. pathname (e.g. /dataset_name or /app/dataset_name). Use only the
        ## LAST path segment as the token, so the app still resolves it when
        ## mounted under a sub-path (e.g. shiny-server at /app/TCR -> "TCR").
        if (
          is.null(url_dataset) &&
            !is.null(session$clientData$url_pathname)
        ) {
          path_val <- session$clientData$url_pathname
          path_val <- gsub("/$", "", path_val) # drop trailing slash
          segments <- strsplit(path_val, "/", fixed = TRUE)[[1]]
          segments <- segments[nzchar(segments)]
          if (length(segments) > 0) {
            ## URL-decode so links with encoded names (e.g. %20) still match
            url_dataset <- utils::URLdecode(segments[length(segments)])
          }
        }

        ## URL/state identity is always the stable dataset id. Filename matching
        ## remains as a compatibility fallback for old shared links.
        if (!is.null(url_dataset)) {
          selection <- match_dataset_id_by_url(
            url_dataset,
            available_crb_files$files
          )
          if (selection != '') {
            print(glue::glue(
              "[{Sys.time()}] Dataset selected via URL: {url_dataset} -> {selection}"
            ))
          }
        }

        ## if not chosen via URL: keep current selection, else pick default.
        ## crb_pick_smallest_file TRUE/NULL -> smallest file; FALSE -> first.
        if (selection != '') {
          ## already set by URL logic
        } else if (
          !is.null(available_crb_files$selected) &&
            available_crb_files$selected %in% names(file_to_load)
        ) {
          selection <- available_crb_files$selected
        } else if (
          !is.null(Cerebro.options[["initial_dataset"]]) &&
            Cerebro.options[["initial_dataset"]] %in% names(file_to_load)
        ) {
          selection <- Cerebro.options[["initial_dataset"]]
        } else {
          pick_smallest <- TRUE
          if (!is.null(Cerebro.options[["crb_pick_smallest_file"]])) {
            pick_smallest <- as.logical(
              Cerebro.options[["crb_pick_smallest_file"]]
            )
          }
          if (isTRUE(pick_smallest)) {
            file_sizes <- sapply(file_to_load, function(f) {
              if (file.exists(f)) {
                file.size(f)
              } else {
                Inf ## variable/object -> infinite size, skipped
              }
            })
            selection <- names(file_to_load)[[which.min(file_sizes)]]
          } else {
            selection <- names(file_to_load)[[1L]]
          }
        }
      } else {
        ## single unnamed file
        available_crb_files$files <- NULL
        available_crb_files$names <- NULL
        available_crb_files$labels <- NULL
        if (file.exists(file_to_load) || exists(file_to_load)) {
          selection <- file_to_load
          dataset_key <- "__single__"
          dataset_source <- "single"
          dataset_target <- selection
        }
      }
    }
    ## assign path to example file if none of the above apply
    if (length(selection) == 0 || all(selection == '')) {
      ## Resolve relative to cerebro_root, never via the package: the exported
      ## bundle, inst/app.R and the installed launcher all point cerebro_root at
      ## a directory that carries extdata/, so this fallback stays self-contained
      ## even when the app runs without CerebroNexus installed (a package
      ## lookup would then resolve to "" and silently break the fallback).
      selection <- file.path(
        Cerebro.options[["cerebro_root"]],
        "extdata/examples/example.crb"
      )
      dataset_key <- "__example__"
      dataset_source <- "example"
      dataset_target <- selection
    }
    if (is.null(dataset_key)) {
      dataset_key <- viewerDatasetName(
        available_crb_files$files,
        selection
      )
      dataset_source <- "configured"
      dataset_target <- viewerDatasetPath(
        available_crb_files$files,
        selection
      )
    }
    viewer_request_dataset(
      selection = selection,
      target = dataset_target,
      dataset_key = dataset_key,
      source = dataset_source
    )
  })

  ## renderUI for the dataset switcher; shown only when >1 .crb files are
  ## available, inert (returns NULL) in single-file mode.
  output[["crb_file_selector_UI"]] <- renderUI({
    if (
      !is.null(available_crb_files$files) &&
        length(available_crb_files$files) > 1
    ) {
      labels <- available_crb_files$labels
      if (is.null(labels) && !is.null(available_crb_files$names)) {
        labels <- available_crb_files$names
      }
      choices <- viewerDatasetChoices(available_crb_files$files, labels)
      selected <- isolate(available_crb_files$selected)
      if (is.null(selected)) {
        selected <- unname(choices[[1L]])
      }
      selectInput(
        inputId = "crb_file_selector",
        label = "Sample data set",
        choices = choices,
        selected = selected,
        width = "72ch"
      )
    }
  })

  ## listen to the dataset switcher and update the current selection
  observeEvent(
    list(input[['crb_file_selector']], input[['viewer_dataset_selection']]),
    {
      if (
        !is.null(input[['crb_file_selector']]) &&
          !is.null(available_crb_files$files)
      ) {
        pending <- input[['viewer_dataset_selection']]
        nonce <- if (
          is.list(pending) &&
            identical(pending$value, input[['crb_file_selector']])
        ) {
          as.numeric(pending$nonce)
        } else {
          0
        }
        viewer_request_dataset(
          selection = input[['crb_file_selector']],
          target = viewerDatasetPath(
            available_crb_files$files,
            input[['crb_file_selector']]
          ),
          dataset_key = as.character(input[['crb_file_selector']]),
          source = "configured",
          selection_nonce = nonce
        )
      }
    }
  )

  ## Keep the lightweight Data Info page independent of the full CRB. Opening
  ## another page, or selecting a dataset absent from the catalog, enables the
  ## ordinary data_set() chain.
  observeEvent(
    available_crb_files$selected,
    {
      catalog <- if (exists("Cerebro.options")) {
        Cerebro.options[[".dataset_catalog"]]
      } else {
        NULL
      }
      info <- viewerDatasetInfo(
        catalog,
        viewerDatasetPath(
          available_crb_files$files,
          available_crb_files$selected
        )
      )
      dataset_load_requested(viewerDatasetLoadRequired(
        isolate(input[["sidebar"]]),
        info,
        initial_tab,
        isolate(initial_page_applied())
      ))
      selected <- available_crb_files$selected
      if (!is.null(info) && length(selected) == 1L) {
        session$onFlushed(
          function() {
            if (session$isClosed()) {
              return()
            }
            withReactiveDomain(session, {
              current <- isolate(available_crb_files$selected)
              if (
                length(current) == 1L &&
                  identical(as.character(current), as.character(selected))
              ) {
                path <- isolate(viewerDatasetPath(
                  available_crb_files$files,
                  selected
                ))
                start_crb_prefetch(
                  path,
                  .runtimeBackendCacheIdentity(
                    .configuredRuntimeBackendPlan(
                      path,
                      Cerebro.options[[".bundle_backend_plan"]],
                      unname(
                        Cerebro.options[["crb_file_to_load"]] %||% character()
                      )
                    )
                  )
                )
              }
            })
          },
          once = TRUE
        )
      }
    },
    ignoreNULL = FALSE,
    priority = 100
  )

  observeEvent(
    input[["sidebar"]],
    {
      dataset_load_requested(viewerDatasetLoadOnTab(
        isolate(dataset_load_requested()),
        input[["sidebar"]]
      ))
    },
    ignoreNULL = FALSE
  )

  current_dataset_info <- reactive({
    req(!is.null(available_crb_files$selected))
    selected <- available_crb_files$selected
    catalog <- if (exists("Cerebro.options")) {
      Cerebro.options[[".dataset_catalog"]]
    } else {
      NULL
    }
    info <- viewerDatasetInfo(
      catalog,
      viewerDatasetPath(available_crb_files$files, selected)
    )
    if (!is.null(info)) {
      return(info)
    }

    req(isTRUE(dataset_load_requested()))
    data <- data_set()
    experiment <- data$getExperiment()
    scalar <- function(value) {
      if (is.null(value) || length(value) != 1L || is.na(value)) {
        return(NA_character_)
      }
      as.character(value)
    }
    list(
      label = viewerDatasetName(available_crb_files$files, selected),
      path = selected,
      cells = .runtimeCerebroCellCount(data),
      organism = scalar(experiment$organism),
      date = scalar(experiment$date_of_export)
    )
  })

  datasetLoadProgress <- function(label, percent, detail, done = FALSE) {
    session$sendCustomMessage(
      "cerebro_dataset_load",
      list(
        label = as.character(label %||% "Dataset"),
        percent = max(0, min(100, as.numeric(percent))),
        detail = as.character(detail %||% ""),
        done = isTRUE(done)
      )
    )
  }

  viewer_dataset_snapshot <- reactive({
    request <- viewer_dataset_request()
    req(
      is.list(request),
      is.list(request$dataset_context),
      !is.null(request$target)
    )
    req(isTRUE(dataset_load_requested()))
    dataset_to_load <- request$target
    dataset_label <- request$dataset_context$dataset_key
    datasetLoadProgress(dataset_label, 8, "Preparing dataset")
    tryCatch(
      {
        if (exists(dataset_to_load)) {
          print(glue::glue(
            "[{Sys.time()}] Load data set from variable: {dataset_to_load}"
          ))
          data <- get(dataset_to_load)
        } else {
          ## Clone the process-cached prototype and attach session-local backends.
          ## Configured bundle CRBs consume the exact backend plan validated during
          ## createShinyApp(). Uploads and older configurations fall back to the
          ## ordinary expression_backend field; serialized getters are not called.
          backend_plan <- if (exists("Cerebro.options")) {
            Cerebro.options[[".bundle_backend_plan"]]
          } else {
            NULL
          }
          configured_paths <- if (
            exists("Cerebro.options") &&
              !is.null(Cerebro.options[["crb_file_to_load"]])
          ) {
            unname(Cerebro.options[["crb_file_to_load"]])
          } else {
            character()
          }
          cache_key <- normalizePath(
            dataset_to_load,
            winslash = "/",
            mustWork = FALSE
          )
          if (is.null(.crb_process_cache[[cache_key]])) {
            prefetch_task <- crb_prefetch_tasks[[cache_key]]
            if (!is.null(prefetch_task)) {
              status <- prefetch_task$status()
              if (status %in% c("initial", "running")) {
                datasetLoadProgress(
                  dataset_label,
                  24,
                  "Reading dataset in the background"
                )
                prefetch_task$result()
              } else if (identical(status, "success")) {
                prefetched <- prefetch_task$result()
                req(identical(prefetched$path, as.character(dataset_to_load)))
                effective_backend <- .configuredRuntimeBackendPlan(
                  dataset_to_load,
                  backend_plan,
                  configured_paths
                )
                .cacheCrbPrototype(
                  dataset_to_load,
                  prefetched$prototype,
                  .runtimeBackendCacheIdentity(effective_backend)
                )
                print(glue::glue(
                  "[{Sys.time()}] CRB background prefetch ready: ",
                  "{.crbLogLabel(dataset_to_load)}"
                ))
              } else {
                tryCatch(
                  prefetch_task$result(),
                  error = function(error) {
                    warning(
                      "CRB background prefetch failed; loading synchronously: ",
                      conditionMessage(error),
                      call. = FALSE
                    )
                  }
                )
              }
            }
          }
          datasetLoadProgress(dataset_label, 42, "Opening cells and metadata")
          data <- get_or_load_crb(
            dataset_to_load,
            backend_plan,
            configured_paths
          )
        }
        datasetLoadProgress(dataset_label, 78, "Preparing viewer data")
        expression_is_deferred <- is.environment(data) &&
          exists("expression", envir = data, inherits = FALSE) &&
          isTRUE(rlang::env_binding_are_lazy(data, "expression"))
        if (expression_is_deferred) {
          print(glue::glue(
            "[{Sys.time()}] Data loaded; expression backend will attach on first use."
          ))
        } else {
          message(data$print())
          if (!is.null(data$expression)) {
            print(glue::glue(
              "[{Sys.time()}] Format of expression data: {class(data$expression)}"
            ))
          }
        }
        viewer_pack <- if (
          is.character(dataset_to_load) &&
            length(dataset_to_load) == 1L &&
            file.exists(dataset_to_load)
        ) {
          viewerPackOpenCached(
            dataset_to_load,
            data,
            .viewer_pack_process_cache
          )
        } else {
          NULL
        }
        datasetLoadProgress(dataset_label, 94, "Finalizing the workspace")
        attr(data, "cerebro_viewer_pack") <- viewer_pack
        datasetLoadProgress(dataset_label, 100, "Ready", done = TRUE)
        list(ok = TRUE, data = data, dataset_context = request$dataset_context)
      },
      error = function(error) {
        if (inherits(error, "shiny.silent.error")) {
          stop(error)
        }
        list(
          ok = FALSE,
          error = error,
          dataset_context = request$dataset_context
        )
      }
    )
  })

  data_set <- reactive({
    snapshot <- viewer_dataset_snapshot()
    if (!isTRUE(snapshot$ok)) {
      stop(snapshot$error)
    }
    snapshot$data
  })

  viewer_loaded_dataset_context <- reactive({
    snapshot <- viewer_dataset_snapshot()
    req(isTRUE(snapshot$ok), is.list(snapshot$dataset_context))
    snapshot$dataset_context
  })

  viewer_current_dataset_key <- reactive({
    viewer_loaded_dataset_context()$dataset_key
  })

  observe(
    {
      snapshot <- viewer_dataset_snapshot()
      if (isTRUE(snapshot$ok)) {
        viewer_send_dataset_phase("ready", snapshot$dataset_context)
      } else {
        warning(
          "Viewer data set failed to load: ",
          conditionMessage(snapshot$error),
          call. = FALSE
        )
        viewer_send_dataset_phase("error", snapshot$dataset_context)
      }
    },
    priority = 1000
  )

  ## A websocket reconnect may resume the same Shiny session, in which case the
  ## snapshot reactive does not invalidate by itself. The browser enters a
  ## fail-closed pending phase on every connection and asks the session to replay
  ## the exact loaded snapshot before any cached view becomes usable again.
  observeEvent(
    input[["viewer_dataset_context_sync"]],
    {
      request <- isolate(viewer_dataset_request())
      if (!is.list(request) || !is.list(request$dataset_context)) {
        return()
      }
      snapshot <- isolate(viewer_dataset_snapshot())
      if (!is.list(snapshot) || !is.list(snapshot$dataset_context)) {
        return()
      }
      viewer_send_dataset_phase(
        if (isTRUE(snapshot$ok)) "ready" else "error",
        snapshot$dataset_context
      )
    },
    ignoreInit = TRUE,
    priority = 1001
  )

  ## The page benchmark must separate full dataset materialization from page
  ## readiness. Data Info intentionally uses the lightweight catalog and does
  ## not force data_set(), so a benchmark-only input provides an explicit,
  ## request-bound load boundary without opening (and warming) any analysis
  ## page. There is no UI control that sends this input in the product.
  observeEvent(
    input[["cerebro_benchmark_dataset_load_request"]],
    {
      request <- as.character(
        input[["cerebro_benchmark_dataset_load_request"]] %||% ""
      )
      dataset_load_requested(TRUE)
      loaded <- data_set()
      identity <- viewerDatasetIdentity()
      session$sendCustomMessage(
        "cerebro_benchmark_dataset_ready",
        list(
          request = request,
          cell_count = as.integer(identity$cell_count),
          dataset_fingerprint = as.character(identity$fingerprint),
          order_fingerprint = as.character(identity$order_fingerprint %||% "")
        )
      )
      invisible(loaded)
    },
    ignoreInit = TRUE,
    priority = 10000
  )

  ## Large projection coordinates already exist as interleaved Float32 assets
  ## in the validated Viewer Pack. Expose only that projection directory under
  ## a session-specific resource prefix so specialist views can fetch the file
  ## directly instead of copying the same 8 MB through R and Shiny's websocket.
  viewer_resource_prefixes <- character()
  viewer_resource_counts <- new.env(parent = emptyenv())
  viewerStaticResourcePrefix <- function(kind, directory) {
    count <- if (
      exists(kind, envir = viewer_resource_counts, inherits = FALSE)
    ) {
      get(kind, envir = viewer_resource_counts, inherits = FALSE) + 1L
    } else {
      1L
    }
    prefix <- paste0(
      "cerebro-",
      kind,
      "-",
      gsub("[^A-Za-z0-9_-]", "", session$token),
      "-",
      count
    )
    shiny::addResourcePath(prefix, directory)
    assign(kind, count, envir = viewer_resource_counts)
    viewer_resource_prefixes <<- c(viewer_resource_prefixes, prefix)
    prefix
  }
  viewerCanonicalResourceIdentity <- function(pack) {
    identity <- tryCatch(
      viewerDatasetIdentity(),
      error = function(error) NULL
    )
    list(
      dataset_fingerprint = as.character(identity$fingerprint %||% ""),
      cell_order_fingerprint = as.character(
        pack$manifest$cell_order_fingerprint %||% ""
      ),
      pack_dataset_fingerprint = as.character(
        pack$manifest$dataset_fingerprint %||% ""
      )
    )
  }
  viewer_projection_resources <- new.env(parent = emptyenv())
  viewerProjectionAsset <- function(name, cell_indices = NULL) {
    pack <- viewerPackCurrent()
    if (
      !is.list(pack) ||
        !isTRUE(pack$canonical_order) ||
        (!is.null(cell_indices) &&
          !identical(as.integer(cell_indices), seq_len(pack$cell_count)))
    ) {
      return(NULL)
    }
    projection_names <- as.character(pack$manifest$projection_names)
    projection_index <- match(as.character(name), projection_names)
    if (is.na(projection_index)) {
      return(NULL)
    }
    asset_path <- file.path(
      "projections",
      sprintf("%03d.bin", projection_index)
    )
    key <- paste(pack$path, asset_path, sep = "::")
    if (exists(key, envir = viewer_projection_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_projection_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    asset_row <- which(as.character(assets$path) == asset_path)
    dimensions <- if (length(asset_row) == 1L) {
      suppressWarnings(as.integer(strsplit(
        as.character(assets$dimensions[[asset_row]]),
        "x",
        fixed = TRUE
      )[[1L]]))
    } else {
      integer()
    }
    file <- file.path(pack$path, asset_path)
    valid <- length(asset_row) == 1L &&
      identical(as.character(assets$dtype[[asset_row]]), "float32") &&
      length(dimensions) == 2L &&
      identical(dimensions[[1L]], as.integer(pack$cell_count)) &&
      dimensions[[2L]] %in% c(2L, 3L) &&
      file.exists(file) &&
      !dir.exists(file) &&
      identical(
        as.numeric(file.info(file)$size),
        as.numeric(assets$bytes[[asset_row]])
      ) &&
      identical(
        unname(tools::md5sum(file)),
        as.character(assets$checksum[[asset_row]])
      )
    if (!valid) {
      return(NULL)
    }
    prefix <- viewerStaticResourcePrefix(
      "projection",
      dirname(file)
    )
    descriptor <- c(
      list(
        protocol = "canonical-projection-v1",
        projection_name = as.character(name),
        url = paste0(prefix, "/", basename(file)),
        cells = dimensions[[1L]],
        dimensions = dimensions[[2L]],
        dtype = "float32",
        bytes = as.numeric(assets$bytes[[asset_row]]),
        checksum = as.character(assets$checksum[[asset_row]])
      ),
      viewerCanonicalResourceIdentity(pack)
    )
    assign(key, descriptor, envir = viewer_projection_resources)
    descriptor
  }
  viewerProjectionCatalog <- function() {
    pack <- viewerPackCurrent()
    names <- if (is.list(pack)) {
      as.character(pack$manifest$projection_names)
    } else {
      character()
    }
    Filter(
      Negate(is.null),
      lapply(names, viewerProjectionAsset)
    )
  }
  ## Trajectory frames may bind an exact row subset to a canonical projection.
  ## The binding is published only by the Viewer Pack builder after exact
  ## coordinate comparison. Older packs and non-matching frames retain their
  ## dedicated geometry asset.
  viewer_trajectory_resources <- new.env(parent = emptyenv())
  viewerTrajectoryFrameAsset <- function(method, name) {
    pack <- viewerPackCurrent()
    if (!is.list(pack) || !isTRUE(pack$canonical_order)) {
      return(NULL)
    }
    frame <- viewerPackTrajectoryFrame(pack, method, name)
    if (is.null(frame)) {
      return(NULL)
    }
    key <- paste(pack$path, method, name, sep = "::")
    if (exists(key, envir = viewer_trajectory_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_trajectory_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    asset_row <- function(path) {
      which(as.character(assets$path) == as.character(path))
    }
    codes_row <- asset_row(frame$state_codes_path)
    dictionary_row <- asset_row(frame$state_dictionary_path)
    if (length(codes_row) != 1L || length(dictionary_row) != 1L) {
      return(NULL)
    }
    code_dimensions <- suppressWarnings(as.integer(
      as.character(assets$dimensions[[codes_row]])
    ))
    dtype <- as.character(assets$dtype[[codes_row]])
    bytes_per_code <- switch(
      dtype,
      uint8 = 1L,
      uint16 = 2L,
      uint32 = 4L,
      NA_integer_
    )
    paths <- c(frame$state_codes_path, frame$state_dictionary_path)
    rows <- c(codes_row, dictionary_row)
    files <- file.path(pack$path, paths)
    valid <- identical(code_dimensions, frame$cells) &&
      !is.na(bytes_per_code) &&
      identical(
        as.numeric(assets$bytes[[codes_row]]),
        as.numeric(frame$cells) * bytes_per_code
      ) &&
      all(file.exists(files)) &&
      !any(dir.exists(files)) &&
      all(
        as.numeric(file.info(files)$size) == as.numeric(assets$bytes[rows])
      ) &&
      all(
        unname(tools::md5sum(files)) == as.character(assets$checksum[rows])
      )
    if (!isTRUE(valid)) {
      return(NULL)
    }
    levels <- tryCatch(
      as.character(jsonlite::read_json(
        files[[2L]],
        simplifyVector = TRUE
      )),
      error = function(error) NULL
    )
    if (
      is.null(levels) ||
        length(levels) !=
          suppressWarnings(as.integer(
            assets$dimensions[[dictionary_row]]
          ))
    ) {
      return(NULL)
    }
    geometry_kind <- as.character(frame$geometry_kind)
    projection <- NULL
    subset <- NULL
    if (identical(geometry_kind, "canonical_projection")) {
      projection <- viewerProjectionAsset(frame$projection_name)
      canonical_cells <- as.integer(pack$cell_count)
      if (is.null(projection) || frame$cells > canonical_cells) {
        return(NULL)
      }
      subset_kind <- as.character(frame$subset_kind)
      subset_count <- switch(
        subset_kind,
        identity = 0L,
        include_uint32 = frame$cells,
        exclude_uint32 = canonical_cells - frame$cells,
        NA_integer_
      )
      if (is.na(subset_count)) {
        return(NULL)
      }
      subset <- list(
        protocol = "canonical-subset-v1",
        kind = subset_kind,
        canonical_cells = canonical_cells,
        cells = frame$cells,
        index_base = 0L,
        dtype = "uint32",
        bytes = 0,
        checksum = ""
      )
      if (!identical(subset_kind, "identity")) {
        subset_row <- asset_row(frame$subset_path)
        if (length(subset_row) != 1L) {
          return(NULL)
        }
        subset_dimensions <- suppressWarnings(as.integer(
          as.character(assets$dimensions[[subset_row]])
        ))
        subset_file <- file.path(pack$path, frame$subset_path)
        subset_valid <- identical(
          as.character(assets$dtype[[subset_row]]),
          "uint32"
        ) &&
          identical(subset_dimensions, subset_count) &&
          identical(
            as.numeric(assets$bytes[[subset_row]]),
            as.numeric(subset_count) * 4
          ) &&
          file.exists(subset_file) &&
          !dir.exists(subset_file) &&
          identical(
            as.numeric(file.info(subset_file)$size),
            as.numeric(assets$bytes[[subset_row]])
          ) &&
          identical(
            unname(tools::md5sum(subset_file)),
            as.character(assets$checksum[[subset_row]])
          )
        if (!isTRUE(subset_valid)) {
          return(NULL)
        }
        paths <- c(paths, frame$subset_path)
        files <- c(files, subset_file)
        rows <- c(rows, subset_row)
        subset$bytes <- as.numeric(assets$bytes[[subset_row]])
        subset$checksum <- as.character(assets$checksum[[subset_row]])
      }
    } else {
      geometry_row <- asset_row(frame$geometry_path)
      if (length(geometry_row) != 1L) {
        return(NULL)
      }
      geometry_dimensions <- suppressWarnings(as.integer(strsplit(
        as.character(assets$dimensions[[geometry_row]]),
        "x",
        fixed = TRUE
      )[[1L]]))
      geometry_file <- file.path(pack$path, frame$geometry_path)
      geometry_valid <- length(geometry_dimensions) == 2L &&
        identical(geometry_dimensions, c(frame$cells, 2L)) &&
        identical(as.character(assets$dtype[[geometry_row]]), "float32") &&
        identical(
          as.numeric(assets$bytes[[geometry_row]]),
          as.numeric(frame$cells) * 2 * 4
        ) &&
        file.exists(geometry_file) &&
        !dir.exists(geometry_file) &&
        identical(
          as.numeric(file.info(geometry_file)$size),
          as.numeric(assets$bytes[[geometry_row]])
        ) &&
        identical(
          unname(tools::md5sum(geometry_file)),
          as.character(assets$checksum[[geometry_row]])
        )
      if (!isTRUE(geometry_valid)) {
        return(NULL)
      }
      paths <- c(paths, frame$geometry_path)
      files <- c(files, geometry_file)
      rows <- c(rows, geometry_row)
    }
    prefix <- viewerStaticResourcePrefix(
      "trajectory",
      dirname(files[[1L]])
    )
    resource_url <- function(file) paste0(prefix, "/", basename(file))
    if (!is.null(subset) && !identical(subset$kind, "identity")) {
      subset$url <- resource_url(subset_file)
    }
    descriptor <- c(
      list(
        protocol = "trajectory-frame-v1",
        cells = frame$cells,
        canonical_cells = as.integer(pack$cell_count),
        geometry_kind = geometry_kind,
        projection = if (identical(geometry_kind, "canonical_projection")) {
          projection
        } else {
          list(
            url = resource_url(geometry_file),
            cells = frame$cells,
            dimensions = 2L,
            dtype = "float32",
            bytes = as.numeric(assets$bytes[[geometry_row]]),
            checksum = as.character(assets$checksum[[geometry_row]])
          )
        },
        subset = subset,
        state = list(
          url = resource_url(file.path(pack$path, frame$state_codes_path)),
          cells = frame$cells,
          dtype = dtype,
          bytes = as.numeric(assets$bytes[[codes_row]]),
          checksum = as.character(assets$checksum[[codes_row]]),
          levels = I(levels)
        )
      ),
      viewerCanonicalResourceIdentity(pack)
    )
    assign(key, descriptor, envir = viewer_trajectory_resources)
    descriptor
  }
  ## Spatial coordinates have their own physical coordinate system and row
  ## order. Publish only a Viewer Pack asset whose canonical cell mapping is an
  ## exact match for the current unrotated frame; every other case retains the
  ## established websocket path.
  viewer_spatial_resources <- new.env(parent = emptyenv())
  viewerSpatialGeometryAsset <- function(name, cell_indices, rotation = 0) {
    pack <- viewerPackCurrent()
    if (!is.list(pack) || !isTRUE(pack$canonical_order)) {
      return(NULL)
    }
    frame <- viewerPackSpatialFrame(pack, name)
    stored_index <- viewerPackSpatialIndex(pack, name)
    rotation <- suppressWarnings(as.numeric(rotation))
    if (
      is.null(frame) ||
        length(rotation) != 1L ||
        is.na(rotation) ||
        !identical(rotation, 0) ||
        !identical(as.integer(cell_indices), stored_index) ||
        !identical(length(stored_index), frame$cells)
    ) {
      return(NULL)
    }
    key <- paste(pack$path, frame$geometry_path, sep = "::")
    if (exists(key, envir = viewer_spatial_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_spatial_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    asset_row <- which(
      as.character(assets$path) == as.character(frame$geometry_path)
    )
    dimensions <- if (length(asset_row) == 1L) {
      suppressWarnings(as.integer(strsplit(
        as.character(assets$dimensions[[asset_row]]),
        "x",
        fixed = TRUE
      )[[1L]]))
    } else {
      integer()
    }
    file <- file.path(pack$path, frame$geometry_path)
    valid <- length(asset_row) == 1L &&
      identical(as.character(assets$dtype[[asset_row]]), "float32") &&
      identical(dimensions, c(frame$cells, 2L)) &&
      identical(
        as.numeric(assets$bytes[[asset_row]]),
        as.numeric(frame$cells) * 2 * 4
      ) &&
      file.exists(file) &&
      !dir.exists(file) &&
      identical(
        as.numeric(file.info(file)$size),
        as.numeric(assets$bytes[[asset_row]])
      ) &&
      identical(
        unname(tools::md5sum(file)),
        as.character(assets$checksum[[asset_row]])
      )
    if (!isTRUE(valid)) {
      return(NULL)
    }
    prefix <- viewerStaticResourcePrefix("spatial", dirname(file))
    descriptor <- c(
      list(
        protocol = "spatial-geometry-v1",
        spatial_name = as.character(name),
        url = paste0(prefix, "/", basename(file)),
        cells = frame$cells,
        dimensions = 2L,
        dtype = "float32",
        bytes = as.numeric(assets$bytes[[asset_row]]),
        checksum = as.character(assets$checksum[[asset_row]])
      ),
      viewerCanonicalResourceIdentity(pack)
    )
    assign(key, descriptor, envir = viewer_spatial_resources)
    descriptor
  }
  viewerSpatialGeometryCatalog <- function() {
    pack <- viewerPackCurrent()
    frames <- if (is.list(pack)) pack$manifest$spatial_frames else NULL
    if (!is.data.frame(frames) || !nrow(frames)) {
      return(list())
    }
    dataset <- spatial_dataset_name(
      available_crb_files$files,
      available_crb_files$selected
    )
    resources <- lapply(unique(as.character(frames$name)), function(name) {
      viewerSpatialGeometryAsset(
        name,
        viewerPackSpatialIndex(pack, name),
        spatialPlotRotation(Cerebro.options, dataset, name)
      )
    })
    Filter(Negate(is.null), resources)
  }
  ## Categorical metadata codes are also stored in canonical cell order. The
  ## first Linked views frame needs one such column, which can otherwise be the
  ## largest remaining websocket vector. Keep the small level/color contract in
  ## the bundle and let the browser fetch the validated packed codes directly.
  viewer_metadata_resources <- new.env(parent = emptyenv())
  viewerMetadataCodesAsset <- function(name, levels = NULL) {
    pack <- viewerPackCurrent()
    if (
      !is.list(pack) ||
        !isTRUE(pack$canonical_order) ||
        !is.character(name) ||
        length(name) != 1L ||
        is.na(name) ||
        !nzchar(name)
    ) {
      return(NULL)
    }
    metadata_names <- as.character(pack$manifest$metadata_names)
    metadata_index <- match(name, metadata_names)
    if (is.na(metadata_index)) {
      return(NULL)
    }
    prefix_path <- file.path("metadata", sprintf("%03d", metadata_index))
    codes_path <- paste0(prefix_path, ".codes.bin")
    dictionary_path <- paste0(prefix_path, ".dictionary.json")
    requested_levels <- if (is.null(levels)) NULL else as.character(levels)
    key <- paste(
      pack$path,
      codes_path,
      paste(requested_levels, collapse = "\r"),
      sep = "::"
    )
    if (exists(key, envir = viewer_metadata_resources, inherits = FALSE)) {
      return(get(key, envir = viewer_metadata_resources, inherits = FALSE))
    }
    assets <- pack$manifest$assets
    codes_row <- which(as.character(assets$path) == codes_path)
    dictionary_row <- which(as.character(assets$path) == dictionary_path)
    if (length(codes_row) != 1L || length(dictionary_row) != 1L) {
      return(NULL)
    }
    dtype <- as.character(assets$dtype[[codes_row]])
    dimensions <- suppressWarnings(as.integer(
      as.character(assets$dimensions[[codes_row]])
    ))
    codes_file <- file.path(pack$path, codes_path)
    dictionary_file <- file.path(pack$path, dictionary_path)
    bytes_per_code <- if (identical(dtype, "uint8")) {
      1L
    } else if (identical(dtype, "uint16")) {
      2L
    } else {
      NA_integer_
    }
    valid_files <- !is.na(bytes_per_code) &&
      length(dimensions) == 1L &&
      identical(dimensions, as.integer(pack$cell_count)) &&
      file.exists(codes_file) &&
      !dir.exists(codes_file) &&
      file.exists(dictionary_file) &&
      !dir.exists(dictionary_file) &&
      identical(
        as.numeric(file.info(codes_file)$size),
        as.numeric(assets$bytes[[codes_row]])
      ) &&
      identical(
        as.numeric(file.info(codes_file)$size),
        as.numeric(pack$cell_count) * bytes_per_code
      ) &&
      identical(
        as.numeric(file.info(dictionary_file)$size),
        as.numeric(assets$bytes[[dictionary_row]])
      ) &&
      identical(
        unname(tools::md5sum(codes_file)),
        as.character(assets$checksum[[codes_row]])
      ) &&
      identical(
        unname(tools::md5sum(dictionary_file)),
        as.character(assets$checksum[[dictionary_row]])
      )
    if (!valid_files) {
      return(NULL)
    }
    dictionary <- tryCatch(
      as.character(jsonlite::read_json(dictionary_file, simplifyVector = TRUE)),
      error = function(error) NULL
    )
    target_levels <- if (is.null(requested_levels)) {
      sort(unique(dictionary))
    } else {
      requested_levels
    }
    remap <- match(dictionary, target_levels) - 1L
    if (
      is.null(dictionary) ||
        length(dictionary) != as.integer(assets$dimensions[[dictionary_row]]) ||
        anyNA(remap)
    ) {
      return(NULL)
    }
    resource_prefix <- viewerStaticResourcePrefix(
      "metadata",
      dirname(codes_file)
    )
    descriptor <- c(
      list(
        protocol = "canonical-metadata-codes-v1",
        url = paste0(resource_prefix, "/", basename(codes_file)),
        cells = as.integer(pack$cell_count),
        dtype = dtype,
        bytes = as.numeric(assets$bytes[[codes_row]]),
        checksum = as.character(assets$checksum[[codes_row]])
      ),
      viewerCanonicalResourceIdentity(pack),
      list(
        metadata_name = as.character(name),
        levels = I(target_levels),
        ## Viewer Pack uses 0 for missing and 1..K for dictionary entries.
        ## Linked views uses -1 for missing and 0..K-1 for its displayed levels.
        code_map = I(c(-1L, as.integer(remap)))
      )
    )
    assign(key, descriptor, envir = viewer_metadata_resources)
    descriptor
  }
  session$onSessionEnded(function() {
    for (prefix in viewer_resource_prefixes) {
      try(shiny::removeResourcePath(prefix), silent = TRUE)
    }
  })

  cv_saved_view_cells <- reactive({
    metadata <- getMetaData()
    if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata$cell_barcode)
    } else {
      rownames(metadata)
    }
  })

  viewerDatasetIdentity <- reactive({
    dataset <- data_set()
    pack <- attr(dataset, "cerebro_viewer_pack", exact = TRUE)
    pack_fingerprint <- if (is.list(pack)) {
      as.character(pack$manifest$dataset_fingerprint %||% "")
    } else {
      ""
    }
    order_fingerprint <- if (is.list(pack)) {
      as.character(pack$manifest$cell_order_fingerprint %||% "")
    } else {
      ""
    }
    stored_fingerprint <- tryCatch(
      dataset$cell_fingerprint,
      error = function(error) NULL
    )
    if (
      is.character(stored_fingerprint) &&
        length(stored_fingerprint) == 1L &&
        !is.na(stored_fingerprint) &&
        grepl("^md5-cell-set-v1:[[:xdigit:]]{32}$", stored_fingerprint)
    ) {
      return(list(
        cell_count = getNumberOfCells(),
        fingerprint = stored_fingerprint,
        order_fingerprint = order_fingerprint,
        pack_fingerprint = pack_fingerprint
      ))
    }
    cells <- cv_saved_view_cells()
    list(
      cell_count = length(cells),
      fingerprint = cv_config_dataset_fingerprint(cells, stored_fingerprint),
      order_fingerprint = order_fingerprint,
      pack_fingerprint = pack_fingerprint
    )
  })

  cv_saved_view_dataset <- reactive({
    cells <- cv_saved_view_cells()
    list(
      cells = cells,
      fingerprint = viewerDatasetIdentity()$fingerprint,
      dataset_context = viewer_loaded_dataset_context()
    )
  })

  observe({
    input[["sidebar"]]
    identity <- viewerDatasetIdentity()
    dataset_identity <- list(
      dataset_context = viewer_loaded_dataset_context(),
      dataset_id = viewer_current_dataset_key(),
      cell_count = as.integer(identity$cell_count),
      cell_fingerprint = as.character(identity$fingerprint),
      cell_order_fingerprint = as.character(identity$order_fingerprint %||% ""),
      pack_dataset_fingerprint = as.character(identity$pack_fingerprint %||% "")
    )
    session$sendCustomMessage(
      "cerebro_saved_view_dataset",
      dataset_identity
    )
    resources <- viewerSpatialGeometryCatalog()
    if (length(resources)) {
      spatial_names <- availableSpatial()
      spatial_defaults <- current_scatter_defaults()
      default_resource_name <- if (
        length(spatial_names) > 0L &&
          isTRUE(all.equal(
            as.numeric(spatial_defaults$percentage_cells_to_show),
            100
          )) &&
          spatial_names[[1L]] %in%
            vapply(
              resources,
              `[[`,
              character(1),
              "spatial_name"
            )
      ) {
        spatial_names[[1L]]
      } else {
        NULL
      }
      session$sendCustomMessage(
        "cell_view_resource_catalog",
        list(
          id = "spatial_projection",
          resources = unname(resources),
          default_resource_name = default_resource_name,
          dataset_context = viewer_loaded_dataset_context(),
          dataset_identity = dataset_identity
        )
      )
    }
    ## Spatial can begin its immutable geometry download as soon as its catalog
    ## reaches the browser. Build the Overview category assets afterwards so
    ## they cannot delay that prefetch on a first Spatial visit.
    projection_resources <- viewerProjectionCatalog()
    if (length(projection_resources)) {
      category_resources <- Filter(
        Negate(is.null),
        lapply(getGroups(), function(group) {
          viewerMetadataCodesAsset(group, getGroupLevels(group))
        })
      )
      session$sendCustomMessage(
        "cell_view_resource_catalog",
        list(
          id = "overview_projection",
          resources = unname(projection_resources),
          category_resources = unname(category_resources),
          dataset_context = viewer_loaded_dataset_context(),
          dataset_identity = dataset_identity
        )
      )
    }
  })

  # list of available trajectories
  available_trajectories <- reactive({
    req(!is.null(data_set()))
    ## collect available trajectories across all methods and create selectable
    ## options
    available_trajectories <- c()
    available_trajectory_method <- getMethodsForTrajectories()
    ## check if at least 1 trajectory method exists
    if (length(available_trajectory_method) > 0) {
      ## cycle through trajectory methods
      for (i in seq_along(available_trajectory_method)) {
        ## get current method and names of trajectories for this method
        current_method <- available_trajectory_method[i]
        available_trajectories_for_this_method <- getNamesOfTrajectories(
          current_method
        )
        ## check if at least 1 trajectory is available for this method
        if (length(available_trajectories_for_this_method) > 0) {
          ## cycle through trajectories for this method
          for (j in seq_along(available_trajectories_for_this_method)) {
            ## create selectable combination of method and trajectory name and add
            ## it to the available trajectories
            current_trajectory <- available_trajectories_for_this_method[j]
            available_trajectories <- c(
              available_trajectories,
              glue::glue("{current_method} // {current_trajectory}")
            )
          }
        }
      }
    }
    # message(str(available_trajectories))
    return(available_trajectories)
  })

  # available genes
  list_of_genes <- reactive({
    req(data_set())
    getGeneNames()
  })

  ## Dynamic sidebar: conditional tabs are shown or hidden based on dataset
  ## content (see toggleConditionalTab() below).
  ##--------------------------------------------------------------------------##

  ##--------------------------------------------------------------------------##
  ## Print message when session is closed due to inactivity.
  ##--------------------------------------------------------------------------##
  observeEvent(input$timeOut, {
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste(
        "Session timeout due to",
        input$timeOut,
        "inactivity -",
        Sys.time()
      ),
      footer = NULL
    ))
    session$close()
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/module/group_filters/group_filters_widget.R"
    ),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/load_data/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/overview/server.R"),
    local = TRUE
  )
  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/groups/server.R"),
    local = TRUE
  )
  source(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/gene_expression/server.R"
    ),
    local = TRUE
  )
  ##--------------------------------------------------------------------------##
  ## Dynamic sidebar: show/hide conditional tabs based on dataset content.
  ##--------------------------------------------------------------------------##
  toggleConditionalTab <- function(tab_name, check_fn, catalog_field = NULL) {
    item_id <- paste0("sidebar_item_", tab_name)
    show_reactive <- reactive({
      if (!is.null(catalog_field)) {
        info <- current_dataset_info()
        catalog_value <- info[[catalog_field]]
        if (is.null(catalog_value) && is.list(info$capabilities)) {
          catalog_value <- info$capabilities[[catalog_field]]
        }
        if (is.logical(catalog_value) && length(catalog_value) == 1L) {
          return(isTRUE(catalog_value))
        }
      }
      req(data_set())
      result <- tryCatch(check_fn(), error = function(e) FALSE)
      if (is.logical(result)) {
        return(result)
      }
      length(result) > 0
    })
    observe({
      should_show <- show_reactive()
      shinyjs::toggle(id = item_id, condition = should_show)
      decision <- viewerInitialPageDecision(
        initial_tab,
        tab_name,
        should_show,
        isolate(initial_page_applied())
      )
      if (!is.null(decision)) {
        initial_page_applied(decision$applied)
        if (!is.null(decision$selected)) {
          updateTabItems(session, "sidebar", selected = decision$selected)
        }
      }
    })
  }

  toggleConditionalTab(
    "markerGenes",
    function() getMethodsForMarkerGenes(),
    catalog_field = "marker_genes"
  )
  toggleConditionalTab(
    "mostExpressedGenes",
    function() getGroupsWithMostExpressedGenes(),
    catalog_field = "most_expressed_genes"
  )
  toggleConditionalTab(
    "enrichedPathways",
    function() getMethodsForEnrichedPathways(),
    catalog_field = "enriched_pathways"
  )
  toggleConditionalTab(
    "extra_material",
    function() {
      length(getExtraMaterialCategories()) > 0L
    },
    catalog_field = "extra_material"
  )
  toggleConditionalTab(
    "immune_repertoire",
    function() {
      getImmuneRepertoireSummary()$available
    },
    catalog_field = "immune_repertoire"
  )
  toggleConditionalTab(
    "trajectory",
    ## Only supported methods should surface the tab; an unsupported
    ## method would otherwise render a blank tab instead of the empty state.
    function() {
      viewerSupportedTrajectoryMethods(
        getMethodsForTrajectories()
      )
    },
    catalog_field = "trajectory"
  )
  toggleConditionalTab(
    "spatial",
    function() availableSpatial(),
    catalog_field = "spatial"
  )
  ## Trekker single-cell spatial mapping: its own bespoke page (not the generic
  ## Spatial tab). Shown only when the loaded .crb carries a `trekker` slot.
  toggleConditionalTab(
    "trekker",
    function() {
      tk <- tryCatch(data_set()$getTrekker(), error = function(e) NULL)
      !is.null(tk)
    },
    catalog_field = "trekker"
  )
  toggleConditionalTab(
    "hla_tcr_motifs",
    ## Show only when the data set actually carries a TCR (TRA/TRB). HLA typing
    ## is NOT required — the motif network works without it, and the Data & QC
    ## tab is where a user would add HLA, so the page must be reachable first.
    ##
    ## The lightweight gate scans every sample; the older IR page helper stops
    ## after three and could hide a valid cohort whose TCR starts at sample four.
    function() {
      summary <- getImmuneRepertoireSummary()
      chains <- summary$chains
      if (length(chains)) {
        return(any(chains %in% c("TRA", "TRB")))
      }
      if (!summary$available) {
        return(FALSE)
      }
      tryCatch(
        viewerHasTcrRepertoire(getImmuneRepertoire()),
        error = function(e) FALSE
      )
    },
    catalog_field = "tcr_repertoire"
  )

  source(
    paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial/server.R"),
    local = TRUE
  )

  deferred_viewer_server_files <- c(
    markerGenes = "marker_genes/server.R",
    geneIdConversion = "gene_id_conversion/server.R",
    color_management = "color_management/server.R",
    about = "about/server.R",
    mostExpressedGenes = "most_expressed_genes/server.R",
    enrichedPathways = "enriched_pathways/server.R",
    extra_material = "extra_material/server.R",
    immune_repertoire = "immune_repertoire/server.R",
    trajectory = "trajectory/server.R",
    trekker = "trekker/server.R",
    coordinated_views = "coordinated_views/server.R",
    hla_tcr_motifs = "hla_tcr_motifs/server.R"
  )
  server_scope <- environment()
  deferred_viewer_server_loaded <- new.env(parent = emptyenv())
  load_deferred_viewer_server <- function(server_file) {
    if (
      exists(
        server_file,
        envir = deferred_viewer_server_loaded,
        inherits = FALSE
      )
    ) {
      return(invisible(FALSE))
    }
    sys.source(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "viewer",
        server_file
      ),
      envir = server_scope
    )
    assign(server_file, TRUE, envir = deferred_viewer_server_loaded)
    invisible(TRUE)
  }
  observeEvent(
    input[["coordviews_visible"]],
    {
      if (isTRUE(input[["coordviews_visible"]])) {
        load_deferred_viewer_server("coordinated_views/server.R")
      }
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input[["sidebar"]],
    {
      server_file <- unname(deferred_viewer_server_files[input[["sidebar"]]])
      if (length(server_file) && !is.na(server_file)) {
        load_deferred_viewer_server(server_file)
      }
    },
    ignoreInit = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## Export reactive values for testing (shinytest2).
  ##--------------------------------------------------------------------------##
  exportTestValues(
    overview_cells_to_show = {
      if (is.null(data_set())) {
        NULL
      } else {
        overview_projection_cells_to_show()
      }
    },
    expression_levels = {
      if (is.null(data_set())) {
        NULL
      } else {
        expression_projection_expression_levels()
      }
    },
    ## Lazy-load regression guard: app startup must NOT load scRepertoire. Its
    ## ~90-package tree is exactly what deferred loading avoids, so if this is
    ## TRUE at startup the settings/tab gates have regressed into eager loading.
    ## Only a real repertoire plot render should pull scRepertoire in.
    scRepertoire_loaded = "scRepertoire" %in% loadedNamespaces(),
    ## Representative heavy deps unique to the scRepertoire tree (immApex / iNEXT
    ## are pulled in by nothing else). A prewarm-like implementation would load
    ## these too, so asserting they stay absent — with a delayed re-check in the
    ## test, past the former 1s prewarm timer — catches a deferred loader that
    ## was merely sampled before its callback ran.
    ir_heavy_deps_loaded = any(
      c("scRepertoire", "immApex", "iNEXT") %in% loadedNamespaces()
    )
  )
}
