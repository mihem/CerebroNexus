##----------------------------------------------------------------------------##
## The worker process that actually holds the Seurat objects.
##
## Reading a real object into the Shiny process has two costs, and the second
## one is worse than it sounds: the memory is obvious, but a multi-GB readRDS
## or a marker-gene run also *blocks* R, and a blocked R process cannot answer
## the browser at all. The page stops responding, the progress bar stops
## moving, and a Stop button would be a lie because there is nothing left to
## service the click.
##
## So the objects live in a separate R process and this file is the only thing
## that talks to it. The Shiny side sees profiles, coordinate samples and log
## lines -- all small -- and never an object.
##
## The worker owns immutable snapshots keyed by data set id and reopens a
## bounded decoded working set on demand. Calls are queued one at a time; the
## page polls for the answer instead of waiting for it.
##----------------------------------------------------------------------------##

#' Start the worker, or explain why it cannot start.
builder_session_start <- function(
  builder_dir,
  snapshot_root = NULL,
  snapshot_registry = list(),
  .async = FALSE,
  .bootstrap = NULL
) {
  worker <- builder_worker_start(
    builder_dir = builder_dir,
    snapshot_root = snapshot_root,
    snapshot_registry = snapshot_registry,
    .async = .async,
    .bootstrap = .bootstrap
  )
  if (!is.null(worker$error)) {
    return(list(error = worker$error))
  }
  list(worker = worker, session = worker$process)
}

builder_session_poll_startup <- function(worker, timeout = 0) {
  builder_worker_poll_startup(worker, timeout = timeout)
}

.builder_session_process <- function(worker) {
  if (inherits(worker, "builder_worker")) {
    return(worker$process)
  }
  worker
}

.builder_session_collect_failed_import <- function(.gc = gc) {
  invisible(.gc(full = TRUE))
}
.builder_session_collect_failed_import <- utils::removeSource(
  .builder_session_collect_failed_import
)
environment(.builder_session_collect_failed_import) <- baseenv()

# Known worker callbacks resolve their dependencies in the worker. Carrying the
# Builder environment as an argument serializes the entire live app on dispatch.
builder_session_worker_callback <- function(callback) {
  stopifnot(is.function(callback))
  callback <- utils::removeSource(callback)
  environment(callback) <- globalenv()
  callback
}

## Run one dataset import in the worker. `load(id, source, progress, args)`
## returns the dataset descriptor, which is answered with the worker's backend
## capabilities. A failed or interrupted import releases its memory with one
## full collection. `load` runs in the worker's global environment, so it may
## refer only to what the worker defines and to `args`.
.builder_session_import_call <- function(
  worker,
  id,
  source,
  request,
  progress_path,
  import_generation,
  load,
  args
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(
      id,
      source,
      request,
      progress_path,
      import_generation,
      load,
      args,
      collect_failed_import
    ) {
      progress <- .builder_import_progress_callback(
        progress_path,
        import_generation
      )
      completed <- FALSE
      on.exit(
        {
          if (!completed) {
            collect_failed_import()
          }
        },
        add = TRUE
      )
      response <- tryCatch(
        {
          value <- load(id, source, progress, args)
          backends <- builder_backend_capabilities()
          value$backend_capabilities <- list(
            build = backends,
            viewer = backends
          )
          response <- builder_worker_response(request, value)
          completed <- TRUE
          response
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
      response
    },
    args = list(
      id = id,
      source = source,
      request = request,
      progress_path = progress_path,
      import_generation = import_generation,
      load = builder_session_worker_callback(load),
      args = args,
      collect_failed_import = .builder_session_collect_failed_import
    )
  )
}

#' Load one of the built-in examples, without a file.
builder_session_example <- function(
  worker,
  id,
  example_id,
  request = NULL,
  progress_path = NULL,
  import_generation = 1L,
  .importer = NULL
) {
  .builder_session_import_call(
    worker,
    id,
    example_id,
    request,
    progress_path,
    import_generation,
    function(id, example_id, progress, args) {
      if (is.function(args$importer)) {
        return(args$importer(id, example_id, progress))
      }
      ex <- Filter(
        function(e) identical(e$id, example_id),
        builder_examples()
      )
      if (!length(ex)) {
        stop("This example does not exist.", call. = FALSE)
      }
      made <- ex[[1]]$make()
      if (!is.null(made$error)) {
        stop(made$error, call. = FALSE)
      }
      .builder_register_adapter(
        .builder_example_source_adapter(
          builder_example_adapter(example_id, made$object),
          made$source_file,
          made$source_md5
        ),
        id,
        progress = progress
      )
    },
    list(importer = .importer)
  )
}

#' Ask the worker to load a file and describe it.
#'
#' The immutable snapshot stays there; what comes back is a small descriptor.
builder_session_load <- function(
  worker,
  id,
  path,
  request = NULL,
  progress_path = NULL,
  import_generation = 1L,
  expected_md5 = NULL,
  .importer = NULL,
  .copy = NULL,
  .transport_writer = NULL
) {
  .builder_session_import_call(
    worker,
    id,
    path,
    request,
    progress_path,
    import_generation,
    function(id, path, progress, args) {
      if (!is.function(args$importer)) {
        return(.builder_register_adapter(
          .builder_seurat_file_adapter(
            path,
            expected_md5 = args$expected_md5,
            snapshot_reusable = TRUE
          ),
          id,
          progress = progress
        ))
      }
      if (is.function(args$copy) && is.function(args$transport_writer)) {
        args$importer(
          id,
          path,
          progress,
          .copy = args$copy,
          .transport_writer = args$transport_writer
        )
      } else {
        args$importer(id, path, progress)
      }
    },
    list(
      expected_md5 = expected_md5,
      importer = .importer,
      copy = .copy,
      transport_writer = .transport_writer
    )
  )
}

## Run `compute(object, args)` on dataset `id` in the worker once
## `capabilities` are loaded, and answer `request` with its value or its error
## message. `compute` runs in the worker's global environment, so it may refer
## only to what the worker defines and to `args`.
.builder_session_object_call <- function(
  worker,
  id,
  request,
  capabilities,
  compute,
  args
) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, request, capabilities, compute, args) {
      tryCatch(
        {
          for (capability in capabilities) {
            builder_worker_require_capability(capability)
          }
          object <- .builder_worker_object(id)
          builder_worker_response(request, compute(object, args))
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(
      id = id,
      request = request,
      capabilities = capabilities,
      compute = builder_session_worker_callback(compute),
      args = args
    )
  )
}

#' Coordinates for the projection preview: two columns and a label, sampled.
builder_session_preview <- function(
  worker,
  id,
  reduction,
  group,
  max_cells,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    "analysis",
    function(object, args) {
      builder_preview_frame(object, args$reduction, args$group, args$max_cells)
    },
    list(reduction = reduction, group = group, max_cells = max_cells)
  )
}

#' Ask the worker for all bounded projection thumbnails in one pass.
builder_session_projection_previews <- function(
  worker,
  id,
  projections,
  group,
  max_cells = BUILDER_PREVIEW_MAX,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    "analysis",
    function(object, args) {
      builder_projection_preview_catalog(
        object,
        projections = args$projections,
        group = args$group,
        max_cells = args$max_cells
      )
    },
    list(projections = projections, group = group, max_cells = max_cells)
  )
}

#' Ask the worker for bounded trajectory coordinates and edges.
builder_session_trajectory_previews <- function(
  worker,
  id,
  trajectories,
  max_cells = BUILDER_PREVIEW_MAX,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    "analysis",
    function(object, args) {
      builder_trajectory_preview_catalog(
        object,
        trajectories = args$trajectories,
        max_cells = args$max_cells
      )
    },
    list(trajectories = trajectories, max_cells = max_cells)
  )
}

#' Spatial coordinates, for deciding where a background image sits.
builder_session_coords <- function(
  worker,
  id,
  image = NULL,
  max_cells = BUILDER_PREVIEW_MAX,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    "spatial",
    function(object, args) {
      co <- builder_spatial_coords(object, args$image)
      if (is.null(co)) {
        return(NULL)
      }
      ## Sampled: the alignment preview needs a shape, not every cell.
      n <- length(co[[1]])
      keep <- if (n > args$max_cells) {
        set.seed(7)
        sort(sample.int(n, args$max_cells))
      } else {
        seq_len(n)
      }
      list(
        x = co[[1]],
        y = co[[2]],
        sx = co[[1]][keep],
        sy = co[[2]][keep]
      )
    },
    list(image = image, max_cells = max_cells)
  )
}

#' Paired transcriptome and physical coordinates for the alignment workbench.
builder_session_spatial_preview <- function(
  worker,
  id,
  default_projection = NULL,
  group = NULL,
  roi = NULL,
  section_id = NULL,
  assay = NULL,
  layer = "data",
  base_coordinate_transform = NULL,
  max_cells = BUILDER_PREVIEW_MAX,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    c("analysis", "spatial"),
    function(object, args) {
      builder_alignment_preview_model(
        object,
        default_projection = args$default_projection,
        group = args$group,
        roi = args$roi,
        section_id = args$section_id,
        assay = args$assay,
        layer = args$layer,
        base_coordinate_transform = args$base_coordinate_transform,
        max_cells = args$max_cells
      )
    },
    list(
      default_projection = default_projection,
      group = group,
      roi = roi,
      section_id = section_id,
      assay = assay,
      layer = layer,
      base_coordinate_transform = base_coordinate_transform,
      max_cells = max_cells
    )
  )
}

#' Where one slide scan lands on EVERY section, section by section.
#'
#' "Apply to all" cannot copy the extent it computed for the section on screen.
#' In bounding-box mode the extent *means* "fit this section's cells", and
#' sections sit at different offsets -- copying section one's numbers puts the
#' other slides thousands of units away, selected but invisible. So the extent
#' is re-derived per section here, where the coordinates live, and only the
#' geometry travels: the encoded picture stays in the Shiny process rather than
#' making a round trip per section.
#'
#' @return A named list, one entry per section, each `list(bounds =, outside =,
#'   total =)`.
builder_session_section_bounds <- function(
  worker,
  id,
  sections,
  mode,
  extent_width,
  extent_height,
  um_per_px = 1,
  dx = 0,
  dy = 0,
  scale = 1,
  request = NULL
) {
  .builder_session_object_call(
    worker,
    id,
    request,
    "spatial",
    function(object, args) {
      out <- list()
      for (nm in args$sections) {
        co <- builder_spatial_coords(object, nm)
        if (is.null(co)) {
          next
        }
        b0 <- builder_image_bounds(
          args$mode,
          co,
          list(
            extent_width = args$extent_width,
            extent_height = args$extent_height
          ),
          um_per_px = args$um_per_px
        )
        if (!is.null(b0$error)) {
          next
        }
        bounds <- builder_adjust_bounds(
          b0,
          dx = args$dx,
          dy = args$dy,
          scale = args$scale
        )
        out[[nm]] <- list(
          bounds = bounds,
          cover = builder_bounds_cover(bounds, co)
        )
      }
      out
    },
    list(
      sections = sections,
      mode = mode,
      extent_width = extent_width,
      extent_height = extent_height,
      um_per_px = um_per_px,
      dx = dx,
      dy = dy,
      scale = scale
    )
  )
}

.builder_session_plan_snapshot_error <- function(plan, snapshot_registry) {
  if (!is.list(plan) || !is.list(plan$items) || !length(plan$items)) {
    return("BuildPlan does not contain any frozen dataset snapshots.")
  }
  if (!is.list(snapshot_registry)) {
    return("The worker snapshot registry is unavailable.")
  }
  for (item in plan$items) {
    id <- item$id
    label <- item$name
    if (
      !is.character(id) ||
        length(id) != 1L ||
        is.na(id) ||
        !nzchar(id)
    ) {
      return("BuildPlan contains a dataset without a stable id.")
    }
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
      label <- id
    }
    if (is.list(item$reused_artifact)) {
      next
    }
    snapshot <- snapshot_registry[[id]]
    if (is.null(snapshot)) {
      return(paste0("The frozen snapshot is missing for ", label, "."))
    }
    identity <- item$source_snapshot_identity
    expected <- if (is.list(identity)) identity$snapshot else NULL
    if (
      !is.list(identity) ||
        !isTRUE(identity$available) ||
        !is.list(expected)
    ) {
      return(paste0(
        "BuildPlan does not contain an owned frozen snapshot for ",
        label,
        "."
      ))
    }
    mirrored <- all(vapply(
      names(expected),
      function(field) identical(identity[[field]], expected[[field]]),
      logical(1)
    ))
    if (!identical(snapshot, expected) || !mirrored) {
      return(paste0(
        "The worker snapshot for ",
        label,
        " does not match the frozen BuildPlan. Rebuild the plan and retry."
      ))
    }
  }
  NULL
}

#' Execute a frozen plan inside a coordinator-assigned private stage.
builder_session_build <- function(
  worker,
  plan,
  request = NULL,
  coordinator = NULL,
  auth_material = NULL,
  progress_path = NULL,
  runtime_files = NULL,
  runtime_identity = NULL
) {
  on.exit(auth_material <- NULL, add = TRUE)
  rs <- .builder_session_process(worker)
  validate_snapshots <- inherits(worker, "builder_worker")
  snapshots <- if (validate_snapshots) worker$snapshot_registry else list()
  if (isTRUE(plan$make_app)) {
    current_contract <- builder_app_capability()$version
    if (
      !identical(plan$app_contract_version, 1L) ||
        !identical(current_contract, 1L)
    ) {
      return(rs$call(
        function(request) {
          error <- paste0(
            "Private app publication contract v1 is not available. ",
            "Build CRB-only output for now."
          )
          if (is.null(request)) {
            return(list(error = error))
          }
          builder_worker_response(request, error = error)
        },
        args = list(request = request)
      ))
    }
  }
  if (validate_snapshots) {
    snapshot_error <- .builder_session_plan_snapshot_error(plan, snapshots)
    if (!is.null(snapshot_error)) {
      rs$call(
        function(request, error) {
          builder_worker_response(request, error = error)
        },
        args = list(request = request, error = snapshot_error)
      )
      return(invisible(NULL))
    }
  }

  assigned_stage <- !is.null(coordinator)
  if (assigned_stage) {
    coordinator <- tryCatch(
      .builder_coordinator_handle(coordinator),
      error = function(error) error
    )
    if (
      inherits(coordinator, "condition") ||
        !dir.exists(coordinator$stage) ||
        !.pathWithin(coordinator$stage, coordinator$control)
    ) {
      stop(
        "The parent release coordinator assigned an invalid stage.",
        call. = FALSE
      )
    }
    stage <- coordinator$stage
  } else {
    stage_root <- if (validate_snapshots) worker$snapshot_root else tempdir()
    stage <- tempfile("builder-build-stage-", tmpdir = stage_root)
  }
  if (
    !assigned_stage &&
      !dir.create(stage, mode = "0700", showWarnings = FALSE)
  ) {
    rs$call(
      function(request) {
        builder_worker_response(
          request,
          error = "The coordinator could not create the assigned build stage."
        )
      },
      args = list(request = request)
    )
    return(invisible(NULL))
  }
  if (validate_snapshots && !assigned_stage) {
    owner <- tryCatch(.builder_stage_owner(stage), error = function(error) {
      error
    })
    if (inherits(owner, "condition")) {
      unlink(stage, recursive = TRUE, force = TRUE)
      rs$call(
        function(request, error) {
          builder_worker_response(request, error = error)
        },
        args = list(request = request, error = conditionMessage(owner))
      )
      return(invisible(NULL))
    }
  }
  if (isTRUE(plan$app_auth$enabled)) {
    auth_material <- builder_auth_validate_material(auth_material, stage)
  } else if (!is.null(auth_material)) {
    stop("A public build cannot use authentication material.", call. = FALSE)
  }
  runtime_guard <- !is.null(runtime_identity)
  runtime_identity_fn <- get0(
    "builder_runtime_file_identity",
    mode = "function",
    inherits = TRUE
  )
  runtime_assert_fn <- get0(
    "builder_runtime_assert_identity",
    mode = "function",
    inherits = TRUE
  )
  if (
    isTRUE(runtime_guard) &&
      (!is.character(runtime_files) ||
        !length(runtime_files) ||
        !is.function(runtime_identity_fn) ||
        !is.function(runtime_assert_fn))
  ) {
    stop("The frozen Builder runtime is invalid.", call. = FALSE)
  }

  rs$call(
    function(
      plan,
      stage,
      request,
      validate_snapshots,
      snapshot_validator,
      auth_material,
      progress_path,
      runtime_files,
      runtime_identity,
      runtime_identity_fn,
      runtime_assert_fn
    ) {
      on.exit(auth_material <- NULL, add = TRUE)
      build_diagnostics <- NULL
      tryCatch(
        withCallingHandlers(
          {
            builder_worker_require_capability("build")
            worker_runtime_identity <- if (is.null(runtime_identity)) {
              NULL
            } else {
              current <- runtime_identity_fn(runtime_files)
              runtime_assert_fn(
                runtime_identity,
                current,
                context = "worker build"
              )
              current
            }
            dependency_capability <- if (isTRUE(validate_snapshots)) {
              builder_build_dependency_capability(plan)
            } else {
              list(available = TRUE, reason = NULL)
            }
            if (!isTRUE(dependency_capability$available)) {
              return(builder_worker_response(
                request,
                error = dependency_capability$reason
              ))
            }
            object_ids <- if (isTRUE(validate_snapshots)) {
              vapply(
                Filter(
                  function(item) !is.list(item$reused_artifact),
                  plan$items
                ),
                `[[`,
                character(1),
                "id"
              )
            } else {
              character()
            }
            registry <- if (length(object_ids)) {
              snapshot_environment <- get(
                ".builder_snapshots",
                envir = globalenv()
              )
              ids <- intersect(
                object_ids,
                ls(snapshot_environment, all.names = TRUE)
              )
              if (length(ids)) {
                mget(
                  ids,
                  envir = snapshot_environment,
                  inherits = FALSE
                )
              } else {
                list()
              }
            } else {
              list()
            }
            if (isTRUE(validate_snapshots)) {
              snapshot_error <- snapshot_validator(plan, registry)
              if (!is.null(snapshot_error)) {
                return(builder_worker_response(
                  request,
                  error = snapshot_error
                ))
              }
            }
            objects <- if (isTRUE(validate_snapshots)) {
              object_environment <- get(".builder_objects", envir = globalenv())
              object_ids <- intersect(
                object_ids,
                ls(object_environment, all.names = TRUE)
              )
              if (length(object_ids)) {
                mget(object_ids, envir = object_environment, inherits = FALSE)
              } else {
                list()
              }
            } else {
              list()
            }
            value <- builder_execute_plan(
              plan,
              stage,
              registry,
              auth_material = auth_material,
              objects = objects,
              on_progress = .builder_build_progress_callback(progress_path),
              check_cancel = .builder_build_cancel_callback(progress_path)
            )
            builder_build_check_cancel(progress_path)
            if (
              isTRUE(plan$make_app) &&
                is.list(value) &&
                is.list(request)
            ) {
              value$build_id <- request$build_id
            }
            if (is.list(value) && !is.null(worker_runtime_identity)) {
              value$runtime_identity <- worker_runtime_identity
            }
            builder_worker_response(request, value = value)
          },
          error = function(error) {
            captured <- try(
              .builder_worker_condition_diagnostics(error, sys.calls()),
              silent = TRUE
            )
            if (!inherits(captured, "try-error")) {
              build_diagnostics <<- list(build = captured)
            }
            invisible(NULL)
          }
        ),
        error = function(error) {
          if (is.null(build_diagnostics)) {
            captured <- try(
              .builder_worker_condition_diagnostics(error, list()),
              silent = TRUE
            )
            if (!inherits(captured, "try-error")) {
              build_diagnostics <- list(build = captured)
            }
          }
          builder_worker_response(
            request,
            error = conditionMessage(error),
            diagnostics = build_diagnostics
          )
        }
      )
    },
    args = list(
      plan = plan,
      stage = stage,
      request = request,
      validate_snapshots = validate_snapshots,
      snapshot_validator = .builder_session_plan_snapshot_error,
      auth_material = auth_material,
      progress_path = progress_path,
      runtime_files = runtime_files,
      runtime_identity = runtime_identity,
      runtime_identity_fn = runtime_identity_fn,
      runtime_assert_fn = runtime_assert_fn
    )
  )
}

#' Forget one object, so removing a data set actually frees its memory.
builder_session_drop <- function(worker, id, request = NULL) {
  rs <- .builder_session_process(worker)
  rs$call(
    function(id, request) {
      tryCatch(
        {
          store <- get(".builder_objects", envir = globalenv())
          snapshots <- get(".builder_snapshots", envir = globalenv())
          if (exists(id, envir = store)) {
            rm(list = id, envir = store)
            gc(FALSE)
          }
          if (exists(id, envir = snapshots, inherits = FALSE)) {
            rm(list = id, envir = snapshots)
          }
          gc(FALSE)
          builder_worker_response(request, TRUE)
        },
        error = function(error) {
          builder_worker_response(request, error = conditionMessage(error))
        }
      )
    },
    args = list(id = id, request = request)
  )
}

#' Has the pending call finished? Returns NULL while it is still running.
#'
#' `read()` also delivers interim messages -- anything the worker printed --
#' before the one carrying the result. Treating those as the answer is how a
#' panel ends up permanently empty while the worker has in fact finished, so
#' only the completion code counts.
builder_session_poll <- function(worker, timeout = 0) {
  builder_worker_poll(worker, timeout = timeout)
}
