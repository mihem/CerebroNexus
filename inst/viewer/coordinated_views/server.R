##----------------------------------------------------------------------------##
## Tab: Linked views — server.
##
## Sourced into the main server scope (source(..., local = TRUE)), so
## `input`, `output`, `session` and `data_set` are in scope.
##
## Responsibility: build ONE per-dataset bundle describing the cells, their
## categorical groupings, and every available "space" (a named 2-D layout of the
## SAME cells: umap / spatial / clone), plus per-cell clone identity, and push it
## to www/cell_views.js. All interaction (linked brushing, highlight, readout) is
## then client-side. The engine keys selection on cell index, so a brush in any
## panel highlights the same cells in every other panel — across modalities.
##----------------------------------------------------------------------------##

## Per-dataset bundle builders (pure functions; see bundle.R). Sourced first so
## cv_build_bundle() and every cv_build_* / cv_* helper is in scope for the
## reactive below. Kept in a separate file so the builders can be unit-tested
## without a running session (tests/testthat/test-coordinated-views.R).
## Both pages resolve ROI membership, image scope and coordinate pivots through
## the same pure Spatial helpers, even when Spatial has not been opened yet.
source(
  file.path(
    Cerebro.options[["cerebro_root"]],
    "viewer/spatial/func_spatial_helpers.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/coordinated_views/bundle.R"
  ),
  local = TRUE
)

## Always resolves to something sendable: the bundle, or a list(error = <text>)
## describing why this data set has no linked views. Never NULL — see the observe
## below for why silence is the one outcome we cannot afford.
## How many times the bundle has actually been built this session. A plain
## environment rather than a reactiveVal: it is written from inside the reactive
## that it counts, and a reactive value would make that a dependency on itself.
## Read back through exportTestValues so tests can distinguish the primary and
## full progressive stages from accidental builds while the page is hidden.
coordviews_build_log <- new.env(parent = emptyenv())
coordviews_build_log$n <- 0L
coordviews_build_log$primary_n <- 0L
coordviews_build_log$sent_n <- 0L
coordviews_build_log$sent_palette <- NULL
coordviews_build_log$sent_primary_n <- 0L
coordviews_build_log$supplemented_primary_n <- 0L
coordviews_build_log$clone_started_key <- ""
coordviews_build_log$clone_sent_primary_n <- 0L
coordviews_build_log$color_observer_started <- FALSE
coordviews_background_ready <- reactiveVal(FALSE)
coordviews_image_spaces <- reactiveVal(NULL)
coordviews_color_source <- reactiveVal(NULL)
coordviews_assets <- reactiveVal(list())
coordviews_clone_details <- reactiveVal(NULL)
coordviews_sent_primary <- reactiveVal(0L)
coordviews_requested_modalities <- reactiveVal(character())

cv_async_clone_spec <- function(crb, primary) {
  pack <- attr(crb, "cerebro_viewer_pack", exact = TRUE)
  receptors <- if (is.list(pack)) {
    as.character(pack$manifest$immune_receptors %||% character())
  } else {
    character()
  }
  if (
    !isTRUE(pack$canonical_order) ||
      !length(receptors) ||
      !identical(as.integer(pack$cell_count), as.integer(primary$n))
  ) {
    return(NULL)
  }
  list(
    pack_path = pack$path,
    manifest = pack$manifest,
    receptor = receptors[[1L]],
    n = as.integer(primary$n)
  )
}

cv_clone_task_key <- function(spec) {
  if (is.null(spec)) {
    ""
  } else {
    paste(
      spec$pack_path,
      spec$receptor,
      spec$manifest$dataset_fingerprint %||% "",
      sep = "\r"
    )
  }
}

coordviews_clone_task <- shiny::ExtendedTask$new(function(
  spec,
  target,
  bundle_file,
  clone_contract_file,
  viewer_pack_file
) {
  mirai::mirai(
    {
      started <- proc.time()[["elapsed"]]
      scope <- new.env(parent = globalenv())
      sys.source(clone_contract_file, envir = scope)
      sys.source(viewer_pack_file, envir = scope)
      sys.source(bundle_file, envir = scope)
      pack <- list(
        path = spec$pack_path,
        manifest = spec$manifest,
        cells = NULL,
        cell_count = spec$n,
        canonical_order = TRUE,
        cache = new.env(parent = emptyenv())
      )
      packed <- scope$viewerPackImmuneIndex(pack, spec$receptor)
      clone <- if (is.null(packed)) {
        NULL
      } else {
        scope$cv_build_clone_rows(
          as.character(packed$clone),
          as.character(packed$ctaa),
          as.character(packed$receptor),
          as.integer(packed$cell_index),
          spec$n,
          TRUE
        )
      }
      list(
        target = target,
        clone = clone,
        server_prepare_ms = (proc.time()[["elapsed"]] - started) * 1000
      )
    },
    spec = spec,
    target = target,
    bundle_file = bundle_file,
    clone_contract_file = clone_contract_file,
    viewer_pack_file = viewer_pack_file
  )
})

cv_start_clone_task <- function(spec) {
  key <- cv_clone_task_key(spec)
  if (!nzchar(key) || identical(coordviews_build_log$clone_started_key, key)) {
    return(invisible(FALSE))
  }
  root <- normalizePath(
    Cerebro.options[["cerebro_root"]],
    winslash = "/",
    mustWork = TRUE
  )
  coordviews_build_log$clone_started_key <- key
  coordviews_clone_task$invoke(
    spec,
    list(
      key = key,
      pack_path = spec$pack_path,
      receptor = spec$receptor,
      n = spec$n,
      cell_order_fingerprint = spec$manifest$cell_order_fingerprint %||% ""
    ),
    file.path(root, "viewer/coordinated_views/bundle.R"),
    file.path(root, "viewer/clone_contract.R"),
    file.path(root, "viewer/core/viewer_pack.R")
  )
  invisible(TRUE)
}

observeEvent(input[["coordviews_spatial_image_load_error"]], {
  event <- input[["coordviews_spatial_image_load_error"]]
  if (
    !is.list(event) ||
      !viewerDatasetContextEqual(
        event$dataset_context,
        viewer_loaded_dataset_context()
      )
  ) {
    return()
  }
  space <- as.character(event$space %||% "Spatial")
  image <- as.character(event$image %||% "background image")
  if (
    length(space) != 1L ||
      is.na(space) ||
      !nzchar(space) ||
      length(image) != 1L ||
      is.na(image) ||
      !nzchar(image)
  ) {
    return()
  }
  showNotification(
    paste0(
      "Could not display “",
      image,
      "” in “",
      space,
      "”. The browser could not decode the image or retrieve its local file."
    ),
    type = "error",
    duration = 8
  )
})

cv_build_bundle_safe <- function(primary_only = FALSE) {
  dataset_context <- viewer_loaded_dataset_context()
  tryCatch(
    {
      started <- proc.time()[["elapsed"]]
      first_frame <- if (isTRUE(primary_only)) {
        viewerProjectionFirstFrameCache()
      } else {
        NULL
      }
      resource_started <- proc.time()[["elapsed"]]
      projection_resource <- NULL
      group_resource <- NULL
      if (
        is.list(first_frame) &&
          is.list(first_frame$projections) &&
          length(first_frame$projections)
      ) {
        configured <- cv_selected_viewer_content()[["default_projection"]]
        projection_name <- cv_preferred_projection_name(
          names(first_frame$projections),
          configured
        )
        descriptor <- viewerProjectionAsset(projection_name)
        if (is.list(descriptor)) {
          projection_resource <- list(
            name = projection_name,
            descriptor = descriptor
          )
        }
      }
      if (is.list(first_frame) && is.data.frame(first_frame$meta_data)) {
        group_resource <- cv_primary_group_resource(
          data_set(),
          first_frame$meta_data,
          viewerMetadataCodesAsset
        )
      }
      resource_prepare_ms <-
        (proc.time()[["elapsed"]] - resource_started) * 1000
      bundle_started <- proc.time()[["elapsed"]]
      b <- cv_build_bundle(
        data_set(),
        primary_only,
        first_frame = first_frame,
        primary_projection_resource = projection_resource,
        dataset_context = dataset_context,
        primary_group_resource = group_resource
      )
      bundle_build_ms <-
        (proc.time()[["elapsed"]] - bundle_started) * 1000
      if (is.null(b)) {
        list(
          dataset_context = dataset_context,
          error = paste(
            "This data set carries no dimensional reduction, so there is",
            "nothing to link its modalities on."
          )
        )
      } else {
        b$dataset_fingerprint <- viewerDatasetIdentity()$fingerprint
        attr(b, "server_prepare_ms") <-
          (proc.time()[["elapsed"]] - started) * 1000
        attr(b, "server_resource_ms") <- resource_prepare_ms
        attr(b, "server_bundle_ms") <- bundle_build_ms
        b
      }
    },
    error = function(e) {
      ## The message goes to the console for debugging; the client gets a
      ## generic one (an internal error string is neither useful nor safe to
      ## render in the browser).
      warning(
        "Linked views bundle failed: ",
        conditionMessage(e),
        call. = FALSE
      )
      list(
        dataset_context = dataset_context,
        error = "Linked views could not be built for this data set."
      )
    }
  )
}

coordviews_bundle <- reactive({
  req(!is.null(data_set()))
  coordviews_build_log$n <- coordviews_build_log$n + 1L
  cv_build_bundle_safe()
})

coordviews_primary_bundle <- reactive({
  req(!is.null(data_set()))
  coordviews_build_log$primary_n <- coordviews_build_log$primary_n + 1L
  cv_build_bundle_safe(primary_only = TRUE)
})

cv_prepare_progressive_supplement <- function(primary, primary_n, modalities) {
  started <- proc.time()[["elapsed"]]
  wants_clone <- "all" %in% modalities || "clone" %in% modalities
  clone_spec <- if (wants_clone) {
    cv_async_clone_spec(data_set(), primary)
  } else {
    NULL
  }
  projection_names <- tryCatch(
    data_set()$availableProjections(),
    error = function(error) character()
  )
  projection_resources <- Filter(
    is.list,
    lapply(stats::setNames(projection_names, projection_names), function(name) {
      tryCatch(viewerProjectionAsset(name), error = function(error) NULL)
    })
  )
  supplement <- tryCatch(
    {
      value <- cv_build_compact_supplement(
        data_set(),
        primary,
        viewerProjectionFirstFrameCache(),
        include_clone = wants_clone && is.null(clone_spec),
        projection_resources = projection_resources
      )
      if (is.null(value)) {
        value <- cv_build_progressive_supplement(
          data_set(),
          primary,
          include_clone = wants_clone && is.null(clone_spec),
          projection_resources = projection_resources
        )
      }
      value
    },
    error = function(error) NULL
  )
  if (is.null(supplement)) {
    return(NULL)
  }
  supplement <- cv_reference_trajectory_geometry(
    supplement,
    primary,
    data_set()
  )
  attr(supplement, "server_prepare_ms") <-
    (proc.time()[["elapsed"]] - started) * 1000
  list(primary_n = primary_n, value = supplement, clone_spec = clone_spec)
}

cv_send_progressive_supplement <- function(prepared) {
  primary_n <- prepared$primary_n
  if (identical(coordviews_build_log$supplemented_primary_n, primary_n)) {
    return(invisible(FALSE))
  }
  deferred <- cv_defer_assets(prepared$value)
  supplement <- deferred$value
  coordviews_assets(deferred$assets)
  current_colors <- coordviews_color_source()
  current_colors$groups <- c(
    current_colors$groups %||% list(),
    supplement$groups %||% list()
  )
  current_colors$cat_extra <- c(
    current_colors$cat_extra %||% list(),
    supplement$cat_extra %||% list()
  )
  current_colors$fields <- c(
    current_colors$fields %||% list(),
    supplement$fields %||% list()
  )
  coordviews_color_source(current_colors)
  supplement$transport_profile <- list(
    server_prepare_ms = attr(supplement, "server_prepare_ms") %||% NA_real_,
    sent_at_ms = as.numeric(Sys.time()) * 1000
  )
  session$sendBinaryMessage(
    "coordviews_supplement",
    cv_wire_pack_message(supplement)
  )
  coordviews_image_spaces(supplement$spaces %||% list())
  coordviews_background_ready(TRUE)
  coordviews_build_log$supplemented_primary_n <- primary_n
  invisible(TRUE)
}

observe({
  sent_primary_n <- coordviews_sent_primary()
  req(sent_primary_n > 0L)
  requested <- coordviews_requested_modalities()
  req("all" %in% requested || "clone" %in% requested)
  req(identical(coordviews_clone_task$status(), "success"))
  result <- coordviews_clone_task$result()
  target <- result$target
  clone <- result$clone
  source <- isolate(coordviews_color_source())
  pack <- attr(isolate(data_set()), "cerebro_viewer_pack", exact = TRUE)
  req(
    is.list(target),
    is.list(clone),
    is.list(source),
    is.list(pack),
    isTRUE(pack$canonical_order),
    identical(
      as.character(target$key),
      cv_clone_task_key(list(
        pack_path = pack$path,
        receptor = target$receptor,
        manifest = pack$manifest
      ))
    ),
    identical(
      normalizePath(pack$path, winslash = "/", mustWork = TRUE),
      as.character(target$pack_path)
    ),
    identical(as.integer(target$n), as.integer(source$n)),
    identical(
      as.integer(sent_primary_n),
      as.integer(coordviews_build_log$sent_primary_n)
    ),
    identical(
      as.character(target$cell_order_fingerprint),
      as.character(source$canonical_order_id %||% "")
    )
  )
  if (
    identical(
      coordviews_build_log$clone_sent_primary_n,
      as.integer(sent_primary_n)
    )
  ) {
    return()
  }
  deferred_clone <- cv_defer_clone_details(clone)
  clone <- deferred_clone$value
  coordviews_clone_details(list(
    primary_n = as.integer(sent_primary_n),
    dataset_fingerprint = source$dataset_fingerprint,
    dataset_context = source$dataset_context,
    label = deferred_clone$details$label,
    n_cdr3 = deferred_clone$details$n_cdr3
  ))
  supplement <- list(
    dataset_id = source$dataset_id,
    dataset_context = source$dataset_context,
    dataset_fingerprint = source$dataset_fingerprint,
    progressive_token = sent_primary_n,
    groups = list(clone_expansion = clone$group),
    cat_extra = list(),
    fields = list(),
    projections = list(),
    spaces = list(clone$space),
    clone = clone$bundle,
    trekker = NULL,
    progressive_complete = FALSE,
    transport_profile = list(
      server_prepare_ms = result$server_prepare_ms,
      sent_at_ms = as.numeric(Sys.time()) * 1000
    )
  )
  current_colors <- isolate(coordviews_color_source())
  current_colors$groups <- c(
    current_colors$groups %||% list(),
    supplement$groups
  )
  coordviews_color_source(current_colors)
  session$sendBinaryMessage(
    "coordviews_supplement",
    cv_wire_pack_message(supplement)
  )
  coordviews_build_log$clone_sent_primary_n <- as.integer(sent_primary_n)
})

observeEvent(
  input[["coordviews_clone_details_request"]],
  {
    request <- input[["coordviews_clone_details_request"]]
    details <- isolate(coordviews_clone_details())
    req(
      is.list(request),
      is.list(details),
      viewerDatasetContextEqual(
        request$dataset_context,
        details$dataset_context
      ),
      viewerDatasetContextEqual(
        details$dataset_context,
        viewer_loaded_dataset_context()
      ),
      identical(
        as.character(request$dataset_fingerprint %||% ""),
        as.character(details$dataset_fingerprint)
      ),
      identical(
        as.integer(details$primary_n),
        as.integer(coordviews_build_log$sent_primary_n)
      )
    )
    ids <- unique(suppressWarnings(as.integer(request$ids)))
    ids <- ids[
      !is.na(ids) & ids >= 0L & ids < length(details$label)
    ]
    req(length(ids) > 0L, length(ids) <= 32L)
    at <- ids + 1L
    session$sendCustomMessage(
      "coordviews_clone_details",
      list(
        dataset_context = details$dataset_context,
        dataset_fingerprint = details$dataset_fingerprint,
        ids = I(ids),
        labels = I(details$label[at]),
        n_cdr3 = I(details$n_cdr3[at])
      )
    )
  },
  ignoreInit = TRUE
)

## The bundle when it actually built; NULL otherwise. Server-side consumers
## (gene vectors, histology controls) need real cells, not an error payload.
cv_ok <- function(b) {
  if (is.null(b) || !is.null(b$error)) NULL else b
}

cv_context_payload <- function(payload, dataset_context) {
  if (!is.list(payload)) {
    payload <- list(value = payload)
  }
  payload$dataset_context <- dataset_context
  payload
}

## Palette edits do not change cells, coordinates, or available spaces. Keep the
## expensive bundle reactive independent of Colour management and send only the
## categorical colours that changed.
coordviews_color_patch <- reactive({
  b <- coordviews_color_source()
  req(!is.null(b))
  colors <- tryCatch(reactive_colors(), error = function(e) NULL)
  cv_color_patch(b, colors)
})
##----------------------------------------------------------------------------##
## Portable Linked views configuration transport.
##
## The browser owns the high-frequency workspace state, but it never turns an
## arbitrary object into a downloadable or applicable document on its own. The
## R contract above validates the full snapshot and current cell population at
## this boundary. Only canonical JSON reaches the clipboard/download path, and
## only normalized state reaches the browser after upload.
##----------------------------------------------------------------------------##
cv_config_send_result <- function(
  nonce,
  action,
  ok,
  dataset_context,
  ...
) {
  if (is.null(nonce)) {
    nonce <- ""
  }
  if (is.null(action)) {
    action <- ""
  }
  session$sendCustomMessage(
    "coordviews_config_result",
    c(
      list(
        nonce = as.character(nonce),
        action = as.character(action),
        ok = isTRUE(ok),
        dataset_context = dataset_context
      ),
      list(...)
    )
  )
}

cv_config_log_failure <- function(error) {
  code <- if (inherits(error, "cv_config_error")) error$code else "internal"
  warning(
    "Linked views configuration failed [",
    code,
    "]: ",
    conditionMessage(error),
    call. = FALSE
  )
}

cv_config_validate_genes <- function(config, cells) {
  if (!identical(config$schema, CV_CONFIG_SCHEMA)) {
    return(NULL)
  }
  colour <- config$view$colour
  requested <- if (identical(colour$mode, "__gene__")) {
    colour$gene
  } else if (identical(colour$mode, "__gene_panels__")) {
    colour$genes
  } else if (identical(colour$mode, "__rgb__")) {
    colour$rgb_genes
  } else {
    character()
  }
  if (!length(requested)) {
    return(NULL)
  }
  available <- tryCatch(
    enc2utf8(as.character(getGeneNames())),
    error = function(error) character()
  )
  if (!length(available) || length(setdiff(requested, available))) {
    cv_config_abort(
      "missing_gene",
      "The configuration uses a gene that is unavailable here."
    )
  }
  values <- lapply(requested, cv_gene_values, cells = cells)
  if (any(vapply(values, is.null, logical(1)))) {
    cv_config_abort(
      "missing_gene",
      "The configuration uses a gene that is unavailable here."
    )
  }
  if (identical(colour$mode, "__gene_panels__")) {
    return(cv_gene_panels_payload(requested, values))
  }
  scaled <- lapply(values, cv_scale_gene_values)
  if (identical(colour$mode, "__gene__")) {
    return(list(
      mode = "__gene__",
      gene = requested[[1L]],
      v = I(scaled[[1L]]$v),
      max = scaled[[1L]]$max
    ))
  }
  list(
    mode = "__rgb__",
    genes = I(requested),
    r = I(scaled[[1L]]$v),
    g = I(scaled[[2L]]$v),
    b = I(scaled[[3L]]$v)
  )
}

cv_config_materialize_selection <- function(config, cells) {
  if (
    is.list(config) &&
      identical(config$schema, CV_CONFIG_SCHEMA) &&
      is.list(config$selection) &&
      !is.null(config$selection$indices)
  ) {
    resolved <- cv_cells_at_indices(cells, config$selection$indices)
    config$selection$cells <- if (is.null(resolved)) character() else resolved
    config$selection$indices <- NULL
  }
  config
}

observeEvent(
  input[["coordviews_config_request"]],
  {
    request <- input[["coordviews_config_request"]]
    request_context <- if (is.list(request)) {
      request$dataset_context
    } else {
      NULL
    }
    current_context <- viewer_loaded_dataset_context()
    if (!viewerDatasetContextEqual(request_context, current_context)) {
      return()
    }
    dataset_context <- current_context
    raw_nonce <- if (is.list(request)) request$nonce else NULL
    raw_action <- if (is.list(request)) request$action else NULL
    nonce <- if (
      is.character(raw_nonce) &&
        length(raw_nonce) == 1L &&
        !is.na(raw_nonce) &&
        nchar(enc2utf8(raw_nonce), type = "bytes") <= 128L
    ) {
      raw_nonce
    } else {
      ""
    }
    action <- if (
      is.character(raw_action) &&
        length(raw_action) == 1L &&
        !is.na(raw_action) &&
        identical(raw_action, "prepare")
    ) {
      raw_action
    } else {
      "invalid"
    }
    tryCatch(
      {
        cv_config_check_node_limit(request)
        request <- cv_config_record(
          request,
          c("nonce", "action", "dataset_context", "config"),
          required = c("nonce", "action"),
          path = "$.request"
        )
        nonce <- cv_config_string(request$nonce, "$.request.nonce", 128L)
        action <- cv_config_choice(
          request$action,
          "$.request.action",
          "prepare"
        )
        dataset <- cv_saved_view_dataset()
        if (
          !viewerDatasetContextEqual(
            dataset$dataset_context,
            dataset_context
          )
        ) {
          return()
        }
        prepared <- cv_config_prepare(
          cv_config_materialize_selection(request$config, dataset$cells),
          cells = dataset$cells,
          fingerprint = dataset$fingerprint
        )
        cv_config_send_result(
          nonce,
          action,
          TRUE,
          dataset_context,
          json = prepared$json,
          filename = paste0(
            if (identical(prepared$config$schema, CV_CONFIG_SCHEMA)) {
              "linked-views-"
            } else {
              paste0(prepared$config$page$id, "-")
            },
            format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"),
            ".json"
          ),
          selected_cells = length(prepared$config$selection$cells)
        )
      },
      error = function(error) {
        cv_config_log_failure(error)
        cv_config_send_result(
          nonce,
          action,
          FALSE,
          dataset_context,
          code = if (inherits(error, "cv_config_error")) {
            error$code
          } else {
            "internal"
          },
          message = cv_config_safe_message(error)
        )
      }
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["coordviews_config_upload_request"]],
  {
    upload <- input[["coordviews_config_upload_request"]]
    request_context <- if (is.list(upload)) {
      upload$dataset_context
    } else {
      NULL
    }
    current_context <- viewer_loaded_dataset_context()
    if (!viewerDatasetContextEqual(request_context, current_context)) {
      return()
    }
    dataset_context <- current_context
    raw_nonce <- if (is.list(upload)) upload$nonce else NULL
    nonce <- if (
      is.character(raw_nonce) &&
        length(raw_nonce) == 1L &&
        !is.na(raw_nonce) &&
        nchar(enc2utf8(raw_nonce), type = "bytes") <= 128L
    ) {
      raw_nonce
    } else {
      ""
    }
    tryCatch(
      {
        cv_config_check_node_limit(upload)
        upload <- cv_config_record(
          upload,
          c(
            "nonce",
            "action",
            "dataset_context",
            "name",
            "size",
            "text"
          ),
          path = "$.upload"
        )
        nonce <- cv_config_string(raw_nonce, "$.upload.nonce", 128L)
        action <- cv_config_choice(upload$action, "$.upload.action", "apply")
        text <- cv_config_read_upload(upload)
        dataset <- cv_saved_view_dataset()
        if (
          !viewerDatasetContextEqual(
            dataset$dataset_context,
            dataset_context
          )
        ) {
          return()
        }
        normalized <- cv_config_decode(
          enc2utf8(text),
          cells = dataset$cells,
          fingerprint = dataset$fingerprint
        )
        colour_data <- cv_config_validate_genes(normalized, dataset$cells)
        cv_config_send_result(
          nonce,
          action,
          TRUE,
          dataset_context,
          config = cv_config_json_document(normalized),
          colour_data = colour_data,
          selection_indices = I(as.integer(
            match(normalized$selection$cells, dataset$cells) - 1L
          )),
          selected_cells = length(normalized$selection$cells)
        )
      },
      error = function(error) {
        cv_config_log_failure(error)
        cv_config_send_result(
          nonce,
          "apply",
          FALSE,
          dataset_context,
          code = if (inherits(error, "cv_config_error")) {
            error$code
          } else {
            "internal"
          },
          message = cv_config_safe_message(error)
        )
      }
    )
  },
  ignoreInit = TRUE
)


## Nothing is sent until the user opens the tab. Large datasets first receive
## one projection and one colour field; the browser requests the expensive
## trajectories/spatial/clone supplement only after that primary frame paints.
##
## The client reports whether the workspace is on screen (`coordviews_visible`)
## -- see www/cell_views.js for why that signal rather than the sidebar's
## active-tab input. The gate is CURRENT visibility, not "was opened once":
## a sticky flag stopped the first build but left every later one, so opening
## the tab, going to Colour management and changing a colour rebuilt and re-sent
## the whole bundle to a hidden page.
coordviews_visible <- reactiveVal(FALSE)
observeEvent(input[["coordviews_visible"]], {
  coordviews_visible(isTRUE(input[["coordviews_visible"]]))
})

## Push the primary bundle while visible. Generation catches same-path reloads.
##
## The req() has to come FIRST. It is what keeps this observer from taking a
## dependency on the bundle while hidden -- nothing is built until the user
## returns to the workspace.
observe(
  {
    req(coordviews_visible())
    progressive <- isTRUE(input[["coordviews_wire_supported"]])
    if (progressive) {
      primary <- coordviews_primary_bundle()
      primary_n <- coordviews_build_log$primary_n
      if (identical(coordviews_build_log$sent_primary_n, primary_n)) {
        return()
      }
      if (!is.null(primary$error)) {
        session$sendCustomMessage("coordviews_data", primary)
        coordviews_background_ready(TRUE)
        coordviews_build_log$sent_primary_n <- primary_n
        return()
      }
      shared <- input[["coordviews_shared_base"]]
      if (
        is.list(shared) &&
          viewerDatasetContextEqual(
            shared$dataset_context,
            primary$dataset_context
          ) &&
          identical(
            as.character(shared$dataset_fingerprint %||% ""),
            primary$dataset_fingerprint
          ) &&
          identical(as.integer(shared$cell_count), as.integer(primary$n)) &&
          nzchar(primary$canonical_order_id %||% "") &&
          identical(
            as.character(shared$canonical_order_id %||% ""),
            primary$canonical_order_id
          ) &&
          as.character(shared$projection %||% "") %in%
            names(primary$projections)
      ) {
        projection <- as.character(shared$projection)
        primary$shared_projection <- projection
        primary$projections[[projection]]$x <- NULL
        primary$projections[[projection]]$y <- NULL
        primary$projections[[projection]]$z <- NULL
        primary$projections[[projection]]$projection_resource <- NULL
      }
      if (is.null(primary$shared_projection)) {
        projection <- primary$default_projection
        resource <- primary$projections[[projection]]$projection_resource
        if (is.null(resource)) {
          resource <- viewerProjectionAsset(projection, primary$cells)
        }
        if (
          is.list(resource) &&
            projection %in% names(primary$projections)
        ) {
          primary$projections[[projection]]$projection_resource <- resource
          primary$projections[[projection]]$x <- NULL
          primary$projections[[projection]]$y <- NULL
          primary$projections[[projection]]$z <- NULL
        }
      }
      coordviews_background_ready(FALSE)
      coordviews_image_spaces(NULL)
      coordviews_assets(list())
      coordviews_clone_details(NULL)
      coordviews_sent_primary(0L)
      coordviews_requested_modalities(character())
      coordviews_color_source(primary)
      coordviews_build_log$sent_palette <- list(
        context = viewer_loaded_dataset_context(),
        patch = isolate(coordviews_color_patch())
      )
      primary$progressive <- TRUE
      primary$progressive_token <- primary_n
      primary$transport_profile <- list(
        server_prepare_ms = attr(primary, "server_prepare_ms") %||% NA_real_,
        server_resource_ms = attr(primary, "server_resource_ms") %||% NA_real_,
        server_bundle_ms = attr(primary, "server_bundle_ms") %||% NA_real_,
        sent_at_ms = as.numeric(Sys.time()) * 1000
      )
      session$sendBinaryMessage(
        "coordviews_binary",
        cv_wire_pack_bundle(primary, include_cells = FALSE)
      )
      coordviews_build_log$sent_primary_n <- primary_n
      coordviews_sent_primary(primary_n)
    } else {
      colors <- tryCatch(reactive_colors(), error = function(e) NULL)
      bundle <- coordviews_bundle()
      bundle_n <- coordviews_build_log$n
      if (identical(coordviews_build_log$sent_n, bundle_n)) {
        return()
      }
      if (is.null(bundle$error)) {
        bundle <- cv_apply_color_patch(bundle, cv_color_patch(bundle, colors))
      }
      session$sendCustomMessage("coordviews_data", bundle)
      coordviews_image_spaces(bundle$spaces %||% list())
      coordviews_assets(list())
      coordviews_color_source(bundle)
      coordviews_build_log$sent_palette <- list(
        context = viewer_loaded_dataset_context(),
        patch = isolate(coordviews_color_patch())
      )
      coordviews_background_ready(TRUE)
      coordviews_build_log$sent_n <- bundle_n
    }
    if (!isTRUE(coordviews_build_log$color_observer_started)) {
      coordviews_build_log$color_observer_started <- TRUE
      observe({
        ## Gate before evaluating the patch: hidden palette changes must not
        ## hydrate or redraw the million-cell Linked workspace.
        req(coordviews_visible())
        palette <- list(
          context = viewer_loaded_dataset_context(),
          patch = coordviews_color_patch()
        )
        if (identical(palette, coordviews_build_log$sent_palette)) {
          return()
        }
        session$sendCustomMessage("coordviews_colors", palette$patch)
        coordviews_build_log$sent_palette <- palette
      })
    }
  },
  priority = 1
)

observeEvent(
  input[["coordviews_primary_ready"]],
  {
    request <- input[["coordviews_primary_ready"]]
    primary <- isolate(coordviews_primary_bundle())
    primary_n <- coordviews_build_log$primary_n
    modalities <- unique(as.character(request$modalities %||% character()))
    req(
      !is.null(request$dataset_id),
      is.list(request$dataset_context),
      !is.null(request$dataset_fingerprint),
      !is.null(request$progressive_token),
      viewerDatasetContextEqual(
        request$dataset_context,
        primary$dataset_context
      ),
      identical(as.character(request$dataset_id), primary$dataset_id),
      identical(
        as.character(request$dataset_fingerprint),
        primary$dataset_fingerprint
      ),
      identical(as.integer(request$progressive_token), primary_n),
      length(modalities) > 0L,
      all(
        modalities %in%
          c(
            "all",
            "attributes",
            "projections",
            "trajectory",
            "spatial",
            "trekker",
            "clone"
          )
      )
    )
    if (identical(coordviews_build_log$supplemented_primary_n, primary_n)) {
      return()
    }
    coordviews_requested_modalities(modalities)
    primary$progressive_token <- primary_n
    prepared <- cv_prepare_progressive_supplement(
      primary,
      primary_n,
      modalities
    )
    req(!is.null(prepared))
    cv_send_progressive_supplement(prepared)
    ## Clone preparation can saturate CPU and disk long enough to delay the GPU
    ## frame. The client reaches this observer only after the primary frame is
    ## correct and visible, so progressive work cannot compete with first paint.
    cv_start_clone_task(prepared$clone_spec)
    session$sendCustomMessage(
      "coordviews_colors",
      isolate(coordviews_color_patch())
    )
    coordviews_build_log$sent_palette <- list(
      context = viewer_loaded_dataset_context(),
      patch = isolate(coordviews_color_patch())
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["coordviews_attribute_request"]],
  {
    request <- input[["coordviews_attribute_request"]]
    source <- isolate(coordviews_color_source())
    req(
      is.list(request),
      !is.null(source),
      viewerDatasetContextEqual(
        request$dataset_context,
        source$dataset_context
      ),
      viewerDatasetContextEqual(
        source$dataset_context,
        viewer_loaded_dataset_context()
      ),
      identical(as.character(request$dataset_id), source$dataset_id),
      identical(
        as.character(request$dataset_fingerprint),
        viewerDatasetIdentity()$fingerprint
      )
    )
    kind <- as.character(request$kind %||% "")
    name <- as.character(request$name %||% "")
    req(length(kind) == 1L, length(name) == 1L, nzchar(name))
    descriptor <- source[[kind]][[name]]
    req(!is.null(descriptor), isTRUE(descriptor$deferred))
    first_frame <- viewerProjectionFirstFrameCache()
    metadata <- if (is.data.frame(first_frame$meta_data)) {
      first_frame$meta_data
    } else {
      cv_canonical_metadata(getMetaData())
    }
    value <- cv_build_attribute(
      data_set(),
      metadata,
      kind,
      name,
      function(group_name, levels) {
        tryCatch(
          cerebro_group_colors(length(levels)),
          error = function(error) cv_colors_for(levels)
        )
      }
    )
    req(!is.null(value))
    session$sendBinaryMessage(
      "coordviews_attribute",
      cv_wire_pack_message(list(
        dataset_id = source$dataset_id,
        dataset_context = source$dataset_context,
        dataset_fingerprint = viewerDatasetIdentity()$fingerprint,
        kind = kind,
        name = name,
        value = value
      ))
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["coordviews_asset_request"]],
  {
    request <- input[["coordviews_asset_request"]]
    req(
      is.list(request),
      viewerDatasetContextEqual(
        request$dataset_context,
        viewer_loaded_dataset_context()
      )
    )
    key <- as.character(request$key %||% "")
    assets <- isolate(coordviews_assets())
    req(
      length(key) == 1L,
      nzchar(key),
      identical(
        as.character(request$dataset_fingerprint %||% ""),
        viewerDatasetIdentity()$fingerprint
      ),
      !is.null(assets[[key]])
    )
    uri <- cv_asset_data_uri(assets[[key]])
    req(!is.null(uri))
    if (!is.character(assets[[key]])) {
      assets[[key]] <- uri
      coordviews_assets(assets)
    }
    session$sendCustomMessage(
      "coordviews_asset",
      list(
        dataset_context = request$dataset_context,
        dataset_fingerprint = viewerDatasetIdentity()$fingerprint,
        key = key,
        uri = uri
      )
    )
  },
  ignoreInit = TRUE
)

observeEvent(input[["coordviews_wire_fallback"]], {
  req(coordviews_visible())
  request <- input[["coordviews_wire_fallback"]]
  bundle <- cv_ok(coordviews_bundle())
  req(!is.null(bundle))
  requested_dataset <- as.character(request$dataset_id %||% "")
  requested_fingerprint <- as.character(request$dataset_fingerprint %||% "")
  req(
    viewerDatasetContextEqual(
      request$dataset_context,
      bundle$dataset_context
    ),
    !nzchar(requested_dataset) ||
      identical(requested_dataset, bundle$dataset_id),
    !nzchar(requested_fingerprint) ||
      identical(requested_fingerprint, bundle$dataset_fingerprint)
  )
  session$sendCustomMessage(
    "coordviews_data",
    cv_apply_color_patch(bundle, isolate(coordviews_color_patch()))
  )
  coordviews_image_spaces(bundle$spaces %||% list())
  coordviews_background_ready(TRUE)
  coordviews_build_log$supplemented_primary_n <- coordviews_build_log$primary_n
})

##----------------------------------------------------------------------------##
## Selected-cell detail views — mirror the Overview/Projection tab. When cells
## are selected in ANY linked panel, the client reports their barcodes in
## `coordviews_selection`; we render a "Plot of selected cells" (bar chart for a
## categorical variable / violin for a numeric one, selected vs. rest) and a
## "Table of selected cells" (their meta data), exactly like the Projection tab.
## The client-side composition + top-clonotype readout is unaffected and stays.
##----------------------------------------------------------------------------##
coordviews_selected_barcodes <- reactive({
  indices <- input[["coordviews_selection_indices"]]
  current <- viewer_loaded_dataset_context()
  if (
    is.list(indices) &&
      viewerDatasetContextEqual(indices$dataset_context, current)
  ) {
    return(cv_cells_at_indices(cv_saved_view_cells(), indices$indices))
  }
  selection <- input[["coordviews_selection"]]
  if (
    !is.list(selection) ||
      !viewerDatasetContextEqual(selection$dataset_context, current)
  ) {
    return(NULL)
  }
  sel <- selection$cells
  if (is.null(sel) || !length(sel)) NULL else as.character(sel)
})

## Boxes appear only while a selection exists (same gating as the Overview tab).
## Shiny re-runs this renderUI on every selection change (same as Overview), which
## would reset the "Variable to compare" dropdown each lasso — so seed it from the
## current input via isolate(): the user's choice survives the rebuild, and reading
## it isolated adds no extra dependency (no rebuild when only the variable changes).
## The `cv-rise` class + its delay continue the stagger the client-side readout
## starts (composition 0ms, clonotypes 60ms), so everything a selection produces
## arrives as one motion instead of three boxes popping in independently. It is a
## CSS animation rather than a transition because Shiny rebuilds these two from
## scratch on every selection change — a new element has nothing to transition
## from. Definition: www/coordviews.css, @keyframes cvRise.
output[["coordviews_selected_cells_UI"]] <- renderUI({
  ## Drop dataset/selection dependencies as soon as this page is left. The
  ## small visibility gate must still run while hidden (see outputOptions
  ## below), so a pending result cannot be invalidated and then suspended
  ## before Shiny flushes it during a dataset transition.
  req(identical(input[["sidebar"]], "coordinated_views"))
  req(coordviews_selected_barcodes())
  meta_cols <- colnames(getMetaData())
  tagList(
    fluidRow(
      class = "cv-rise",
      style = "--cv-rise-delay:120ms",
      cerebroBox(
        title = tagList(
          boxTitle("Plot of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_plot_info")
        ),
        tagList(
          selectInput(
            "coordviews_selected_cells_plot_variable",
            label = "Variable to compare:",
            choices = meta_cols[!meta_cols %in% c("cell_barcode")],
            selected = isolate(input[[
              "coordviews_selected_cells_plot_variable"
            ]])
          ),
          plotly::plotlyOutput("coordviews_selected_cells_plot")
        )
      )
    ),
    fluidRow(
      class = "cv-rise",
      style = "--cv-rise-delay:180ms",
      cerebroBox(
        title = tagList(
          boxTitle("Table of selected cells"),
          cerebroInfoButton("coordviews_selected_cells_table_info")
        ),
        tagList(
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_number_formatting",
            label = "Automatically format numbers:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          shinyWidgets::materialSwitch(
            inputId = "coordviews_selected_cells_table_color_highlighting",
            label = "Highlight values with colours:",
            value = TRUE,
            status = "primary",
            inline = TRUE
          ),
          DT::dataTableOutput("coordviews_selected_cells_table")
        )
      )
    )
  )
})

## Deliberately bypass the page-scoped outputOptions wrapper: only the gate
## above stays awake; metadata and the plot/table outputs remain demand-driven.
shiny::outputOptions(
  output,
  "coordviews_selected_cells_UI",
  suspendWhenHidden = FALSE
)

## Plot: categorical variable -> bar of selected-cell counts per group (coloured
## by the SAME assignColorsToGroups() the Projection tab uses); numeric variable
## -> violin/box of selected vs. not-selected. Barcode-keyed, so it needs no
## projection coordinates.
output[["coordviews_selected_cells_plot"]] <- plotly::renderPlotly({
  sel <- coordviews_selected_barcodes()
  req(sel, input[["coordviews_selected_cells_plot_variable"]])
  cells_df <- cv_canonical_metadata(getMetaData())
  var <- input[["coordviews_selected_cells_plot_variable"]]
  req(var %in% colnames(cells_df))
  is_selected <- cells_df[["cell_barcode"]] %in% sel
  ## categorical -> bar chart of counts within the selection
  if (is.factor(cells_df[[var]]) || is.character(cells_df[[var]])) {
    sub <- cells_df[is_selected, , drop = FALSE]
    if (nrow(sub) > 0) {
      counts <- sub %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(var))) %>%
        dplyr::tally() %>%
        dplyr::ungroup()
    } else {
      lv <- if (var %in% getGroups()) {
        getGroupLevels(var)
      } else {
        unique(cells_df[[var]])
      }
      counts <- data.frame(x = lv, n = 0L)
      colnames(counts)[1] <- var
    }
    colors_for_groups <- assignColorsToGroups(counts, var)
    x_vals <- as.character(counts[[1]])
    plot <- plotly::plot_ly(
      x = x_vals,
      y = counts[[2]],
      type = "bar",
      color = x_vals,
      colors = colors_for_groups,
      showlegend = FALSE,
      hoverinfo = "y"
    )
    y_axis_title <- "Number of cells"
    ## numeric -> violin/box of selected vs. not selected
  } else if (is.numeric(cells_df[[var]])) {
    grp <- factor(
      ifelse(is_selected, "selected", "not selected"),
      levels = c("selected", "not selected")
    )
    violin_data <- compactViolinData(
      data.frame(group = grp, value = cells_df[[var]]),
      "value",
      "group"
    )
    plot <- plotly::plot_ly(
      x = violin_data[["group"]],
      y = violin_data[["value"]],
      type = "violin",
      box = list(visible = FALSE),
      meanline = list(visible = FALSE),
      color = violin_data[["group"]],
      colors = setNames(
        c("#e74c3c", "#7f8c8d"),
        c("selected", "not selected")
      ),
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(size = 5)
    )
    plot <- addExactViolinSummary(plot, violin_data)
    y_axis_title <- var
  } else {
    return(NULL)
  }
  plot %>%
    plotly::layout(
      title = "",
      xaxis = list(title = "", mirror = TRUE, showline = TRUE),
      yaxis = list(
        title = y_axis_title,
        tickformat = ",.0f",
        hoverformat = ",.0f",
        mirror = TRUE,
        showline = TRUE
      ),
      hovermode = "compare"
    ) %>%
    cerebro_plotly_toolbar()
})

## Table: meta data of the selected cells (same prettifyTable options as the
## Projection tab's table). Filtered by barcode; empty skeleton when nothing hit.
output[["coordviews_selected_cells_table"]] <- DT::renderDataTable({
  sel <- coordviews_selected_barcodes()
  if (is.null(sel)) {
    return(
      cv_canonical_metadata(getMetaData()) %>%
        dplyr::slice(0) %>%
        prepareEmptyTable()
    )
  }
  cells_df <- cv_selected_metadata(getMetaData(), sel) %>%
    dplyr::select(cell_barcode, dplyr::everything())
  if (nrow(cells_df) == 0) {
    cv_canonical_metadata(getMetaData()) %>%
      dplyr::slice(0) %>%
      prepareEmptyTable()
  } else {
    prettifyTable(
      cells_df,
      filter = list(position = "top", clear = TRUE),
      dom = "Brtlip",
      show_buttons = TRUE,
      number_formatting = input[[
        "coordviews_selected_cells_table_number_formatting"
      ]],
      color_highlighting = input[[
        "coordviews_selected_cells_table_color_highlighting"
      ]],
      hide_long_columns = TRUE,
      download_file_name = "linked_views_selected_cells"
    )
  }
})

## Info modals for the two panels.
observeEvent(input[["coordviews_selected_cells_plot_info"]], {
  showModal(modalDialog(
    title = "Plot of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Depending on the variable chosen, this plot summarises the cells you ",
      "selected across the linked panels. A categorical variable (e.g. ",
      "'cluster' or 'sample') gives a bar chart of how many selected cells fall ",
      "in each group, coloured exactly as in the panels. A continuous variable ",
      "(e.g. number of transcripts) gives a violin/box plot comparing its ",
      "distribution in the selected vs. non-selected cells."
    )
  ))
})
observeEvent(input[["coordviews_selected_cells_table_info"]], {
  showModal(modalDialog(
    title = "Table of selected cells",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    p(
      "Meta data for the cells selected across the linked panels (some columns ",
      "may be hidden — check the 'Column visibility' button). The table can be ",
      "downloaded as CSV or Excel for further analysis."
    )
  ))
})

##----------------------------------------------------------------------------##
## Gene-expression colouring (the richer "Colour by"). The gene pickers are
## whole-transcriptome server-side searches (same helper the Spatial/Gene tabs
## use); on change the server returns a 0-255 vector aligned to the bundle's
## cell order, and the client colours the points (viridis, or RGB blend).
##----------------------------------------------------------------------------##
cv_has_expression <- function() {
  isTRUE(tryCatch(nrow(data_set()$expression) > 0, error = function(e) FALSE))
}

cv_expression_cells <- function() {
  identity <- viewerDatasetIdentity()
  if (nzchar(identity$order_fingerprint %||% "")) {
    return(NULL)
  }
  cv_saved_view_cells()
}

## Pull one gene aligned to `cells`. Returns NULL if unavailable.
cv_gene_values <- function(gene, cells = NULL, n = NULL) {
  if (is.null(gene) || !nzchar(gene)) {
    return(NULL)
  }
  if (!is.null(cells)) {
    cells <- as.character(cells)
  }
  m <- tryCatch(
    data_set()$getExpressionMatrix(cells = cells, genes = gene),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(NULL)
  }
  if (is.null(dim(m))) {
    v <- as.numeric(m)
  } else {
    cn <- colnames(m)
    v <- if (!is.null(cells) && !is.null(cn)) {
      as.numeric(m[1, match(cells, cn)])
    } else {
      as.numeric(m[1, ])
    }
  }
  if (!is.null(n) && length(v) != as.integer(n)) {
    return(NULL)
  }
  v[is.na(v)] <- 0
  v
}

cv_scale_gene_values <- function(v) {
  mx <- suppressWarnings(max(v, na.rm = TRUE))
  q <- if (is.finite(mx) && mx > 0) {
    as.integer(round(v / mx * 255))
  } else {
    rep(0L, length(v))
  }
  q[is.na(q)] <- 0L
  list(v = q, max = round(mx, 3))
}

coordviews_gene_names <- reactive({
  req(coordviews_visible())
  req(input[["coordviews_gene_controls_active"]] %in% c("gene", "rgb"))
  sort(getGeneNames())
})

serverSideGeneSelector(
  session,
  "coordviews_gene",
  active = function() {
    coordviews_visible() &&
      identical(input[["coordviews_gene_controls_active"]], "gene") &&
      cv_has_expression()
  },
  choices = function() coordviews_gene_names()
)
lapply(
  c("coordviews_gene_r", "coordviews_gene_g", "coordviews_gene_b"),
  function(channel_id) {
    serverSideGeneSelector(
      session,
      channel_id,
      active = function() {
        coordviews_visible() &&
          identical(input[["coordviews_gene_controls_active"]], "rgb") &&
          cv_has_expression()
      },
      choices = function() coordviews_gene_names()
    )
  }
)

cv_expression_feedback <- function(ok, rgb = FALSE) {
  shiny::isolate({
    ids <- if (rgb) {
      paste0("coordviews_gene_", c("r", "g", "b"))
    } else {
      c("coordviews_gene", "coordviews_expression_mode")
    }
    controls <- stats::setNames(
      lapply(ids, function(id) as.character(input[[id]])),
      ids
    )
    session$sendCustomMessage(
      "cerebro_linked_expression_feedback",
      list(
        controls = controls,
        ok = isTRUE(ok),
        rgb = rgb,
        dataset_context = viewer_loaded_dataset_context()
      )
    )
  })
}

observeEvent(
  list(
    input[["coordviews_gene"]],
    input[["coordviews_expression_mode"]]
  ),
  {
    req(coordviews_visible())
    dataset_context <- viewer_loaded_dataset_context()
    genes <- unique(input[["coordviews_gene"]])
    genes <- genes[!is.na(genes) & nzchar(genes)]
    if (length(genes) == 0) {
      session$sendCustomMessage(
        "coordviews_geneval",
        cv_context_payload(
          list(gene = "", ok = FALSE),
          dataset_context
        )
      )
      session$sendCustomMessage(
        "coordviews_genepanels",
        cv_context_payload(list(ok = FALSE), dataset_context)
      )
      cv_expression_feedback(FALSE)
      return()
    }
    cells <- cv_expression_cells()
    n <- getNumberOfCells()
    values <- lapply(genes, cv_gene_values, cells = cells, n = n)
    keep <- !vapply(values, is.null, logical(1))
    genes <- genes[keep]
    values <- values[keep]
    if (length(values) == 0) {
      session$sendCustomMessage(
        "coordviews_geneval",
        cv_context_payload(
          list(gene = "", ok = FALSE),
          dataset_context
        )
      )
      session$sendCustomMessage(
        "coordviews_genepanels",
        cv_context_payload(list(ok = FALSE), dataset_context)
      )
      cv_expression_feedback(FALSE)
      return()
    }
    mode <- input[["coordviews_expression_mode"]]
    if (identical(mode, "panels")) {
      session$sendCustomMessage(
        "coordviews_genepanels",
        cv_context_payload(
          cv_gene_panels_payload(genes, values),
          dataset_context
        )
      )
    } else {
      mean_values <- Reduce(`+`, values) / length(values)
      gv <- cv_scale_gene_values(mean_values)
      session$sendCustomMessage(
        "coordviews_geneval",
        cv_context_payload(
          cv_gene_message(
            if (length(genes) == 1) {
              genes[[1]]
            } else {
              paste0("Mean expression (", length(genes), " genes)")
            },
            gv$v,
            gv$max
          ),
          dataset_context
        )
      )
    }
    cv_expression_feedback(TRUE)
  }
)

## RGB co-expression: one gene per channel, each scaled independently; an empty
## channel is all-zero. Recompute whenever any of the three genes changes.
## ignoreInit: the event value is a LIST, and a list of three NULLs is not NULL,
## so ignoreNULL does not suppress the initial run the way it does for the
## single-gene selector above. Without it this observer built the whole bundle
## on connect -- the one place the laziness leaked, and invisible from outside
## because the bundle was built but never sent.
observeEvent(
  list(
    input[["coordviews_gene_r"]],
    input[["coordviews_gene_g"]],
    input[["coordviews_gene_b"]]
  ),
  {
    req(coordviews_visible())
    cells <- cv_expression_cells()
    n <- getNumberOfCells()
    zero <- rep(0L, n)
    chan <- function(id) {
      g <- input[[id]]
      if (is.null(g) || !nzchar(g)) {
        return(list(v = zero, gene = ""))
      }
      gv <- cv_gene_values(g, cells = cells, n = n)
      if (!is.null(gv)) {
        gv <- cv_scale_gene_values(gv)
      }
      if (is.null(gv)) list(v = zero, gene = "") else list(v = gv$v, gene = g)
    }
    r <- chan("coordviews_gene_r")
    g <- chan("coordviews_gene_g")
    bl <- chan("coordviews_gene_b")
    if (!nzchar(r$gene) && !nzchar(g$gene) && !nzchar(bl$gene)) {
      session$sendCustomMessage(
        "coordviews_rgbval",
        cv_context_payload(list(ok = FALSE), b$dataset_context)
      )
      cv_expression_feedback(FALSE, rgb = TRUE)
      return()
    }
    session$sendCustomMessage(
      "coordviews_rgbval",
      cv_context_payload(cv_rgb_message(r, g, bl), b$dataset_context)
    )
    cv_expression_feedback(TRUE, rgb = TRUE)
  },
  ignoreInit = TRUE
)

##----------------------------------------------------------------------------##
## Spatial histology-image controls — shown only when the current data set's
## spatial entry carries an embedded image. The controls are client-owned
## (cv-img- ids), wired by cell_views.js; adjusting them re-styles the image on
## the canvas instantly and never round-trips to the server.
##----------------------------------------------------------------------------##
output[["coordviews_image_ui"]] <- renderUI({
  req(coordviews_visible())
  spaces <- coordviews_image_spaces()
  req(!is.null(spaces))
  ## Two separate questions, and conflating them is what went wrong before.
  ##
  ## DOES a bar exist? Any section carrying an image is enough. A space's own
  ## `image` is its FIRST section's, so a data set whose first section has no
  ## histology used to render no bar at all, and switching to one that does
  ## revealed an empty box.
  ##
  ## What does it open SHOWING? The section that is on screen, which is the
  ## first one. Scanning for "any image" and keeping the last one found seeded
  ## the controls -- values, and the slider ranges built from the coordinate
  ## span -- from a section the user is not looking at.
  img <- NULL
  seed <- NULL
  for (s in spaces) {
    if (!is.null(s$image)) {
      img <- s$image
      if (is.null(seed)) {
        seed <- s$image
      }
    }
    for (smp in (s$samples %||% list())) {
      if (!is.null(smp$image)) {
        img <- smp$image
      }
    }
  }
  if (is.null(img)) {
    return(NULL)
  }
  ## The displayed section's image when it has one; otherwise any, purely so the
  ## controls exist for the client to re-seed on the first switch.
  img <- seed %||% img
  ## Seed the controls from the alignment preset so an external image (Visium
  ## H&E) opens PRE-ALIGNED, exactly as the Spatial tab does. Move sliders are in
  ## DATA units, ranged to the coordinate span so the nudge is meaningful.
  pr <- img$preset
  span <- img$coord_span
  if (is.null(span) || length(span) < 2) {
    span <- c(400, 400)
  }
  rng <- function(id, label, mn, mx, val, step) {
    div(
      class = "cv-img-range",
      tags$input(
        type = "range",
        id = id,
        `aria-label` = label,
        min = mn,
        max = mx,
        value = val,
        step = step
      ),
      tags$input(
        type = "number",
        id = paste0(id, "-number"),
        class = "cv-img-number",
        min = mn,
        max = mx,
        value = val,
        step = step,
        `aria-label` = paste(label, "value")
      )
    )
  }
  chk <- function(id, label, on) {
    tags$label(
      class = "cv-chk",
      if (isTRUE(on)) {
        tags$input(type = "checkbox", id = id, checked = "checked")
      } else {
        tags$input(type = "checkbox", id = id)
      },
      label
    )
  }
  ## The range has to CONTAIN the value it is being asked to show. A preset is a
  ## calibration someone measured; a slider ranged on the coordinate span alone
  ## clamps anything outside it, and because the whole bar is read back together
  ## the clamped number is then written into the state by an unrelated nudge --
  ## the alignment silently becoming one nobody chose.
  sx <- signif(max(span[1] * 1.2, abs(pr$offsetX %||% 0) * 1.1), 3)
  sy <- signif(max(span[2] * 1.2, abs(pr$offsetY %||% 0) * 1.1), 3)
  scale_lo <- min(
    0.3,
    (pr$scaleX %||% 1) * 0.9,
    (pr$scaleY %||% pr$scaleX %||% 1) * 0.9
  )
  scale_hi <- max(
    3,
    (pr$scaleX %||% 1) * 1.1,
    (pr$scaleY %||% pr$scaleX %||% 1) * 1.1
  )
  div(
    class = "cv-imgbar",
    div(
      class = "cv-imgbar-heading",
      tags$span(class = "cv-imgbar-title", "Alignment")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-opacity-number", "Opacity"),
      rng("cv-img-opacity", "Opacity", 0, 1, pr$opacity %||% 0.5, "any")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-offx-number", "Horizontal"),
      rng("cv-img-offx", "Move X", -sx, sx, pr$offsetX %||% 0, "any")
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-offy-number", "Vertical"),
      rng("cv-img-offy", "Move Y", -sy, sy, pr$offsetY %||% 0, "any")
    ),
    ## Two scales, not one. A preset can carry scaleX != scaleY -- a calibration
    ## that is genuinely non-uniform -- and a single slider had to pick a number
    ## for both, so touching ANY control in this bar silently squared the image
    ## up and threw that calibration away. Locked together by default, since a
    ## uniform scale is the common case and two sliders to drag is a worse one.
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-scalex-number", "Scale X"),
      rng(
        "cv-img-scalex",
        "Scale X",
        scale_lo,
        scale_hi,
        pr$scaleX %||% 1,
        "any"
      )
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-scaley-number", "Scale Y"),
      rng(
        "cv-img-scaley",
        "Scale Y",
        scale_lo,
        scale_hi,
        pr$scaleY %||% pr$scaleX %||% 1,
        "any"
      )
    ),
    div(
      class = "cv-img-ctl",
      tags$label(`for` = "cv-img-rotate-number", "Rotation"),
      rng(
        "cv-img-rotate",
        "Rotate",
        -180,
        180,
        pr$rotation %||% 0,
        "any"
      )
    ),
    div(
      class = "cv-img-checks",
      chk("cv-img-show", "Show this image", TRUE),
      chk(
        "cv-img-lock",
        "Lock aspect ratio",
        isTRUE(is.null(pr$scaleY) || identical(pr$scaleY, pr$scaleX))
      ),
      chk("cv-img-flipx", "Flip horizontally", isTRUE(pr$flipX)),
      chk("cv-img-flipy", "Flip vertically", isTRUE(pr$flipY))
    ),
    ## Alignment is fiddly and easy to lose; the preset is the state the data set
    ## shipped with, so there has to be a way back to it that is not "reload".
    tags$button(
      type = "button",
      id = "cv-img-reset",
      class = "cv-imgbar-reset",
      "Reset to preset"
    )
  )
})
outputOptions(output, "coordviews_image_ui", suspendWhenHidden = FALSE)

##----------------------------------------------------------------------------##
## Info modal
##----------------------------------------------------------------------------##
observeEvent(input[["coordinated_views_info"]], {
  showModal(modalDialog(
    title = "Linked views",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    tagList(
      tags$p(
        "Every modality is a layout of the ",
        tags$b("same cells"),
        ": the ",
        tags$b("selected projections"),
        " (UMAP, t-SNE, PCA or any other embedding), the ",
        tags$b("Spatial"),
        " map (physical positions, when the data set carries them), and ",
        tags$b("Clonal expansion"),
        " (each receptor-bearing cell placed by its clone's rank and size)."
      ),
      tags$p(
        tags$b("Coordinated selection"),
        " — lasso-drag in any ",
        tags$i("2-D"),
        " panel and the same cells highlight in ",
        tags$i("every"),
        " panel, because the selection is keyed on the cell, not on a panel's ",
        "coordinates. Select a cluster in any projection to see where those cells sit in ",
        "tissue and which clonotypes they carry; select an expanded clone to see ",
        "where its cells fall across every selected embedding. A 3-D panel is for navigating: with ",
        "depth on screen, what a lasso encloses depends on the viewing angle, so ",
        "those panels display a selection rather than make one."
      ),
      tags$p(
        tags$b("Readout"),
        " — the selection's cell-type composition and its top clonotypes ",
        "(CDR3, clone size, share of the selection) update live. Click a ",
        "clonotype row to select all of its cells across every panel."
      ),
      tags$p(
        tags$b("Colouring"),
        " — every meta data column is available, exactly as on the Projection ",
        "tab: the grouping variables, any other categorical column, and every ",
        "numeric one (number of transcripts, percent mitochondrial, scores) on a ",
        "continuous scale. Single genes and three-gene co-expression are there too."
      ),
      tags$p(
        tags$b("Navigating"),
        " — zoom with each panel's toolbar buttons, and drag with the hand tool ",
        "(or shift-drag / middle-drag from any tool) to pan. The toolbar also ",
        "has reset and PNG download. A 3-D embedding is ",
        "marked as such in the multi-projection picker and gains a rotate tool: drag ",
        "to turn it, and nearer cells are drawn larger so the depth reads."
      ),
      tags$p(
        style = "color:#6b6b70;",
        tags$b("Why this is different. "),
        "General coordinated viewers link expression and space, but have no ",
        "concept of a clonotype, so the immune repertoire can never enter their ",
        "linked loop. Here it is a first-class space, and the composition and ",
        "clonotype readouts are computed, not just recoloured."
      )
    )
  ))
})
