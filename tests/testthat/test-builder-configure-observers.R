## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

## The Configure page observers in inst/builder/server/datasets.R, driven
## through the namespaced `core-*` inputs: each one is pinned with an accepted
## input and with the inputs it must ignore (another dataset rendered, a value
## out of range, an unknown action, capability or disposition).

configure_observer_app <- function(envir = parent.frame()) {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = envir)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this Configure observer test.")
  }
  app_env
}

configure_observer_entry <- function(
  app_env,
  id = "dataset-a",
  load_state = "loaded"
) {
  metadata <- list(
    cell_type = list(
      name = "cell_type",
      classification = "categorical",
      group_eligible = TRUE
    ),
    sample = list(
      name = "sample",
      classification = "categorical",
      group_eligible = TRUE
    ),
    Phase = list(
      name = "Phase",
      classification = "categorical",
      group_eligible = TRUE
    ),
    nUMI = list(
      name = "nUMI",
      classification = "continuous",
      group_eligible = FALSE,
      group_reason = "Continuous values"
    )
  )
  projection <- function(id, available = TRUE) {
    list(
      id = id,
      name = toupper(id),
      kind = if (identical(id, "pca")) "pca" else "other",
      dimensions = 2L,
      cell_count = 100L,
      available = available
    )
  }
  app_env$builder_upgrade_viewer_content_entry(list(
    id = id,
    revision = 0L,
    load_state = load_state,
    snapshot = list(
      path = paste0("/private/", id),
      owner_token = paste0("owner-", id),
      object_md5 = strrep("a", 32L)
    ),
    profile = list(
      marker = id,
      extras = list(),
      n_cells = 100L,
      reductions = c("umap", "pca", "tsne"),
      viewer_content = list(
        metadata = metadata,
        projections = list(
          umap = projection("umap"),
          pca = projection("pca"),
          tsne = projection("tsne", available = FALSE)
        ),
        trajectories = list(
          list(method = "monocle", name = "traj1", selectable = TRUE),
          list(method = "monocle", name = "traj2", selectable = FALSE),
          list(method = "slingshot", name = "lin1", selectable = TRUE)
        )
      ),
      content = list(
        immune_repertoire = list(
          candidates = list(
            unified_misc = list(detected = TRUE, full_ir_ready = TRUE),
            metadata = list(detected = TRUE, full_ir_ready = TRUE)
          )
        )
      )
    ),
    levels = list(
      cell_type = c("T", "B"),
      sample = c("s1", "s2"),
      Phase = c("G1", "S")
    ),
    settings = list(
      name = paste("Dataset", id),
      included_projections = "umap",
      reductions = "umap",
      default_projection = "umap",
      initial_projections = "umap",
      overview_point_size = 5,
      overview_point_opacity = 1,
      overview_percentage_cells_to_show = 100,
      included_groups = c("cell_type", "sample"),
      groups = c("cell_type", "sample"),
      default_group = "cell_type",
      group_color_overrides = list(
        cell_type = c(T = "#112233"),
        sample = c(s1 = "#445566")
      ),
      metadata_policy = list(
        columns = list(
          cell_barcode = list(
            disposition = "included",
            retain_in_crb = TRUE,
            forced = TRUE
          ),
          cell_type = list(
            disposition = "included",
            retain_in_crb = TRUE,
            group_enabled = TRUE
          ),
          sample = list(
            disposition = "included",
            retain_in_crb = TRUE,
            group_enabled = TRUE
          )
        )
      ),
      included_trajectories = list(monocle = "traj1"),
      default_trajectory = list(method = "monocle", name = "traj1"),
      cell_cycle_columns = character()
    )
  ))
}

## Record every custom message the server sends from here on.
configure_observer_messages <- function(session) {
  log <- new.env(parent = emptyenv())
  log$messages <- list()
  original <- session$sendCustomMessage
  session$sendCustomMessage <- function(type, message) {
    log$messages[[length(log$messages) + 1L]] <- list(
      type = type,
      message = message
    )
    original(type, message)
  }
  log
}

configure_observer_sent <- function(log, type) {
  lapply(
    Filter(function(item) identical(item$type, type), log$messages),
    `[[`,
    "message"
  )
}

configure_observer_reset <- function(log) {
  log$messages <- list()
  invisible(log)
}

## The settings of a stored dataset, read back from the Builder state.
configure_observer_stored <- function(sets, id = "dataset-a") {
  hit <- Filter(function(entry) identical(entry$id, id), isolate(sets()))
  hit[[1L]]
}

test_that("overview point controls accept only in-range values for the rendered dataset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    log <- configure_observer_messages(session)
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()
    expect_identical(current(), "dataset-a")

    controls <- list(
      list(
        input = "core-point_size",
        setting = "overview_point_size",
        field = "point_size",
        rejected = list(0.5, 20.5, "abc", NA, c(3, 4), Inf),
        accepted = list(1, 20, "7")
      ),
      list(
        input = "core-point_opacity",
        setting = "overview_point_opacity",
        field = "point_opacity",
        rejected = list(0.05, 1.01, "abc", NA, c(0.3, 0.4), -Inf),
        accepted = list(0.1, "0.5", 1)
      ),
      list(
        input = "core-percentage_cells_to_show",
        setting = "overview_percentage_cells_to_show",
        field = "percentage_cells_to_show",
        rejected = list(9.99, 100.5, "abc", NA, c(20, 30), Inf),
        accepted = list(10, "55", 100)
      )
    )

    ## Another dataset in the Configure pane: nothing is applied.
    session$setInputs(`core-rendered_for` = "dataset-b")
    for (control in controls) {
      configure_observer_reset(log)
      do.call(session$setInputs, stats::setNames(list(15), control$input))
      session$flushReact()
      stored <- configure_observer_stored(sets)
      expect_identical(stored$revision, 0L, info = control$input)
      expect_length(
        configure_observer_sent(log, "builder_projection_state"),
        0L
      )
      expect_length(
        configure_observer_sent(log, "builder_dataset_action_owner"),
        0L
      )
    }

    session$setInputs(`core-rendered_for` = "dataset-a")
    revision <- 0L
    for (control in controls) {
      before <- configure_observer_stored(sets)$settings[[control$setting]]
      for (value in control$rejected) {
        configure_observer_reset(log)
        do.call(session$setInputs, stats::setNames(list(value), control$input))
        session$flushReact()
        stored <- configure_observer_stored(sets)
        expect_identical(stored$revision, revision, info = control$input)
        expect_identical(
          stored$settings[[control$setting]],
          before,
          info = paste(control$input, format(value))
        )
        expect_length(
          configure_observer_sent(log, "builder_projection_state"),
          0L
        )
      }
      for (value in control$accepted) {
        configure_observer_reset(log)
        do.call(session$setInputs, stats::setNames(list(value), control$input))
        session$flushReact()
        revision <- revision + 1L
        stored <- configure_observer_stored(sets)
        expect_identical(stored$revision, revision, info = control$input)
        expect_identical(
          stored$settings[[control$setting]],
          as.numeric(value),
          info = control$input
        )
        sent <- configure_observer_sent(log, "builder_projection_state")
        expect_length(sent, 1L)
        expect_identical(sent[[1L]]$dataset, "dataset-a")
        expect_identical(sent[[1L]]$generation, revision)
        expect_identical(sent[[1L]][[control$field]], as.numeric(value))
        expect_identical(sent[[1L]]$included, "umap")
        expect_null(sent[[1L]]$message)
        owner <- configure_observer_sent(log, "builder_dataset_action_owner")
        expect_length(owner, 1L)
        expect_identical(owner[[1L]]$dataset, "dataset-a")
        expect_identical(owner[[1L]]$generation, as.double(revision))
      }
    }
    final <- configure_observer_stored(sets)$settings
    expect_identical(final$overview_point_size, 7)
    expect_identical(final$overview_point_opacity, 1)
    expect_identical(final$overview_percentage_cells_to_show, 100)
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("projection and trajectory actions keep only selectable choices", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    log <- configure_observer_messages(session)
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()

    ## Not the rendered dataset, or not a "set" action: ignored.
    session$setInputs(`core-rendered_for` = "dataset-b")
    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = list("pca"),
        default = "pca"
      ),
      `core-trajectory_action` = list(
        action = "set",
        included = list(list(method = "slingshot", name = "lin1"))
      )
    )
    session$flushReact()
    session$setInputs(`core-rendered_for` = "dataset-a")
    session$setInputs(
      `core-projection_action` = list(action = "reset", included = list("pca")),
      `core-trajectory_action` = list(
        action = "clear",
        included = list(list(method = "slingshot", name = "lin1"))
      )
    )
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 0L)
    expect_length(configure_observer_sent(log, "builder_projection_state"), 0L)
    expect_length(configure_observer_sent(log, "builder_trajectory_state"), 0L)

    ## Only unavailable projections: the state is re-sent with a message.
    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = list("tsne", "missing"),
        default = "tsne"
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 0L)
    expect_identical(stored$settings$included_projections, "umap")
    sent <- configure_observer_sent(log, "builder_projection_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$dataset, "dataset-a")
    expect_identical(sent[[1L]]$generation, 0L)
    expect_identical(sent[[1L]]$included, "umap")
    expect_identical(
      sent[[1L]]$message,
      "Keep at least one projection selected."
    )

    configure_observer_reset(log)
    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = list("pca", "tsne", "umap"),
        default = "tsne",
        initial = list("pca", "tsne")
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 1L)
    expect_identical(stored$settings$included_projections, c("umap", "pca"))
    expect_identical(stored$settings$reductions, c("umap", "pca"))
    expect_identical(stored$settings$default_projection, "umap")
    expect_identical(stored$settings$initial_projections, c("umap", "pca"))
    sent <- configure_observer_sent(log, "builder_projection_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$generation, 1L)
    expect_identical(sent[[1L]]$included, c("umap", "pca"))
    expect_identical(sent[[1L]]$default, "umap")
    expect_identical(sent[[1L]]$initial, c("umap", "pca"))
    expect_null(sent[[1L]]$message)

    configure_observer_reset(log)
    session$setInputs(
      `core-projection_action` = list(
        action = "set",
        included = list("pca"),
        default = "pca"
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 2L)
    expect_identical(stored$settings$default_projection, "pca")
    expect_identical(stored$settings$initial_projections, "pca")

    ## Trajectories: only selectable records survive; a default that is not
    ## included falls back to the first included trajectory.
    configure_observer_reset(log)
    session$setInputs(
      `core-trajectory_action` = list(
        action = "set",
        included = list(
          list(method = "slingshot", name = "lin1"),
          list(method = "monocle", name = "traj2"),
          list(method = "monocle", name = "traj1"),
          list(method = "unknown", name = "x"),
          "junk"
        ),
        default = list(method = "monocle", name = "traj2")
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 3L)
    expect_identical(
      stored$settings$included_trajectories,
      list(slingshot = "lin1", monocle = "traj1")
    )
    expect_identical(
      stored$settings$default_trajectory,
      list(method = "slingshot", name = "lin1")
    )
    sent <- configure_observer_sent(log, "builder_trajectory_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$dataset, "dataset-a")
    expect_identical(sent[[1L]]$generation, 3L)
    expect_identical(
      sent[[1L]]$included,
      list(
        list(method = "slingshot", name = "lin1"),
        list(method = "monocle", name = "traj1")
      )
    )
    expect_identical(
      sent[[1L]]$default,
      list(method = "slingshot", name = "lin1")
    )

    configure_observer_reset(log)
    session$setInputs(
      `core-trajectory_action` = list(
        action = "set",
        included = list(list(method = "monocle", name = "traj2"))
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 4L)
    expect_identical(stored$settings$included_trajectories, list())
    expect_true("default_trajectory" %in% names(stored$settings))
    expect_null(stored$settings$default_trajectory)
    sent <- configure_observer_sent(log, "builder_trajectory_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$included, list())
    expect_null(sent[[1L]]$default)
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("group, metadata and cell-cycle actions keep only eligible columns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    log <- configure_observer_messages(session)
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()
    policy <- configure_observer_stored(sets)$settings$metadata_policy

    session$setInputs(`core-rendered_for` = "dataset-b")
    session$setInputs(
      `core-group_action` = list(action = "set", included = list("sample")),
      `core-metadata_action` = list(
        action = "set-retention",
        retained = list("cell_barcode")
      ),
      `core-cell_cycle` = "Phase"
    )
    session$flushReact()
    session$setInputs(`core-rendered_for` = "dataset-a")
    session$setInputs(
      `core-group_action` = list(action = "select", included = list("sample")),
      `core-metadata_action` = list(
        action = "set",
        retained = list("cell_barcode")
      )
    )
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 0L)
    expect_length(configure_observer_sent(log, "builder_group_state"), 0L)
    expect_identical(group_preview_ui_revision(), 0L)

    session$setInputs(
      `core-group_action` = list(
        action = "set",
        included = list("nUMI", "missing")
      )
    )
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 0L)
    sent <- configure_observer_sent(log, "builder_group_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$dataset, "dataset-a")
    expect_identical(sent[[1L]]$generation, 0L)
    expect_identical(sent[[1L]]$included, c("cell_type", "sample"))
    expect_identical(
      sent[[1L]]$message,
      "Keep at least one Viewer Group selected."
    )
    expect_identical(group_preview_ui_revision(), 0L)

    configure_observer_reset(log)
    session$setInputs(
      `core-group_action` = list(
        action = "set-groups",
        included = list("nUMI", "sample", "Phase"),
        default = "cell_type"
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 1L)
    expect_identical(stored$settings$included_groups, c("sample", "Phase"))
    expect_identical(stored$settings$groups, c("sample", "Phase"))
    expect_identical(stored$settings$default_group, "sample")
    expect_identical(
      stored$settings$group_color_overrides,
      list(sample = c(s1 = "#445566"))
    )
    expect_identical(
      stored$settings$metadata_policy,
      app_env$builder_metadata_policy_set_groups(policy, c("sample", "Phase"))
    )
    expect_identical(group_preview_ui_revision(), 1L)
    sent <- configure_observer_sent(log, "builder_group_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$generation, 1L)
    expect_identical(sent[[1L]]$included, c("sample", "Phase"))
    expect_identical(sent[[1L]]$default, "sample")
    expect_null(sent[[1L]]$message)

    configure_observer_reset(log)
    policy <- stored$settings$metadata_policy
    session$setInputs(
      `core-metadata_action` = list(
        action = "set-retention",
        retained = list("cell_barcode", "cell_type", "missing")
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 2L)
    expect_identical(
      stored$settings$metadata_policy,
      app_env$builder_metadata_policy_set_retained(policy, "cell_barcode")
    )
    expect_identical(group_preview_ui_revision(), 2L)
    sent <- configure_observer_sent(log, "builder_group_state")
    expect_length(sent, 1L)
    expect_identical(sent[[1L]]$generation, 2L)

    ## Cell-cycle columns: only eligible phase-like columns; NULL clears.
    session$setInputs(`core-cell_cycle` = c("cell_type", "Phase"))
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 3L)
    expect_identical(stored$settings$cell_cycle_columns, "Phase")
    session$setInputs(`core-cell_cycle` = c("Phase", "missing"))
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 3L)
    session$setInputs(`core-cell_cycle` = NULL)
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 4L)
    expect_identical(stored$settings$cell_cycle_columns, character())
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("group colour edits apply only to included groups and known levels", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()

    session$setInputs(`core-rendered_for` = "dataset-b")
    session$setInputs(
      `core-group_color` = list(
        group = "cell_type",
        level = "B",
        color = "#aabbcc"
      ),
      `core-group_focus` = "cell_type",
      `core-reset_colors` = 1L
    )
    session$flushReact()
    session$setInputs(`core-rendered_for` = "dataset-a")
    for (change in list(
      list(group = "Phase", level = "G1", color = "#aabbcc"),
      list(group = "cell_type", level = "NK", color = "#aabbcc"),
      list(group = "cell_type", level = "", color = "#aabbcc"),
      list(group = "cell_type", level = "T", color = "#112233"),
      list(group = "cell_type", level = "B", color = "not-a-colour")
    )) {
      session$setInputs(`core-group_color` = change)
      session$flushReact()
    }
    session$setInputs(`core-group_focus` = "Phase", `core-reset_colors` = 2L)
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 0L)
    expect_identical(group_color_ui_revision(), 0L)

    session$setInputs(
      `core-group_color` = list(
        group = "cell_type",
        level = "B",
        color = "#AABBCC"
      )
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 1L)
    expect_identical(
      stored$settings$group_color_overrides,
      app_env$builder_update_color_override(
        list(cell_type = c(T = "#112233"), sample = c(s1 = "#445566")),
        "cell_type",
        "B",
        "#AABBCC"
      )
    )
    expect_false("colors" %in% names(stored$settings))
    expect_identical(group_color_ui_revision(), 0L)

    session$setInputs(
      `core-group_focus` = list(group = "cell_type"),
      `core-reset_colors` = 3L
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 2L)
    expect_identical(
      stored$settings$group_color_overrides,
      list(sample = c(s1 = "#445566"))
    )
    expect_identical(group_color_ui_revision(), 1L)

    ## Nothing left to reset for that group.
    session$setInputs(`core-reset_colors` = 4L)
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 2L)
    expect_identical(group_color_ui_revision(), 1L)
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("immune source choices apply only a listed source for the rendered dataset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()

    session$setInputs(`core-rendered_for` = "dataset-b")
    session$setInputs(`core-immune_source_immune_repertoire` = "metadata")
    session$flushReact()
    session$setInputs(`core-rendered_for` = "dataset-a")
    session$setInputs(`core-immune_source_immune_repertoire` = "unknown")
    session$flushReact()
    session$setInputs(`core-immune_source_immune_repertoire` = "")
    session$flushReact()
    session$setInputs(`core-immune_source_hla_tcr_motifs` = "metadata")
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 0L)
    expect_identical(
      length(configure_observer_stored(sets)$settings$content_sources),
      0L
    )

    session$setInputs(`core-immune_source_immune_repertoire` = "metadata")
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 1L)
    expect_identical(
      stored$settings$content_sources,
      list(immune_repertoire = "metadata")
    )
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("content acknowledgements and dispositions need a current owner", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  real_state <- app_env$builder_dataset_state
  app_env$builder_dataset_state <- function(entry) {
    state <- real_state(entry)
    state$manifest$spatial <- list(
      status = "attention",
      required_action = list(type = "acknowledge", token = "spatial:token-1"),
      evidence = list(detected = TRUE)
    )
    state$manifest$hla <- list(
      status = "attention",
      evidence = list(detected = FALSE)
    )
    ## Detected, but not a capability a disposition may name.
    state$manifest$bogus_capability <- list(
      status = "attention",
      required_action = list(type = "acknowledge", token = "bogus:token"),
      evidence = list(detected = TRUE)
    )
    state
  }
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()
    session$setInputs(`core-rendered_for` = "dataset-a")
    owner <- list(
      generation = 0,
      project_epoch = 0,
      owner_token = "owner-dataset-a"
    )
    acknowledge <- function(...) {
      utils::modifyList(
        c(
          list(
            dataset = "dataset-a",
            capability = "spatial",
            token = "spatial:token-1"
          ),
          owner
        ),
        list(...)
      )
    }
    disposition <- function(...) {
      utils::modifyList(
        c(
          list(
            dataset = "dataset-a",
            capability = "spatial",
            disposition = "filtered"
          ),
          owner
        ),
        list(...)
      )
    }
    ## The observers clear the surface on success; the surface observer then
    ## recomputes it in the same flush, so count the resets as they happen.
    surface_resets <- 0L
    observe(
      {
        if (is.null(configure_workbench_surface())) {
          surface_resets <<- surface_resets + 1L
        }
      },
      priority = 1e6
    )
    nonce <- 0L
    send <- function(input, request) {
      if (is.list(request)) {
        nonce <<- nonce + 1L
        request$nonce <- nonce
      }
      configure_workbench_surface("spatial")
      session$flushReact()
      surface_resets <<- 0L
      do.call(session$setInputs, stats::setNames(list(request), input))
      session$flushReact()
    }

    rejected_acknowledgements <- list(
      "not-a-list",
      acknowledge(dataset = "dataset-b"),
      acknowledge(dataset = c("dataset-a", "dataset-a")),
      acknowledge(capability = ""),
      acknowledge(token = NA_character_),
      acknowledge(token = "spatial:other"),
      acknowledge(capability = "hla"),
      acknowledge(capability = "unknown"),
      acknowledge(generation = 1),
      acknowledge(project_epoch = 1),
      acknowledge(owner_token = "owner-dataset-b")
    )
    for (request in rejected_acknowledgements) {
      send("core-acknowledge_content", request)
      stored <- configure_observer_stored(sets)
      expect_identical(stored$revision, 0L)
      expect_identical(surface_resets, 0L)
      expect_identical(configure_workbench_surface(), "spatial")
    }
    session$setInputs(`core-rendered_for` = "dataset-b")
    send("core-acknowledge_content", acknowledge())
    expect_identical(configure_observer_stored(sets)$revision, 0L)
    expect_identical(surface_resets, 0L)
    expect_identical(configure_workbench_surface(), "spatial")

    session$setInputs(`core-rendered_for` = "dataset-a")
    send("core-acknowledge_content", acknowledge())
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 1L)
    expect_identical(stored$settings$acknowledgements, "spatial:token-1")
    expect_identical(surface_resets, 1L)
    ## The same token again changes nothing.
    owner$generation <- 1
    send("core-acknowledge_content", acknowledge())
    expect_identical(configure_observer_stored(sets)$revision, 1L)
    expect_identical(surface_resets, 0L)
    expect_identical(configure_workbench_surface(), "spatial")
    ## Acknowledgements are not limited to the disposition capabilities.
    send(
      "core-acknowledge_content",
      acknowledge(capability = "bogus_capability", token = "bogus:token")
    )
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 2L)
    expect_identical(
      stored$settings$acknowledgements,
      c("spatial:token-1", "bogus:token")
    )
    expect_identical(surface_resets, 1L)
    owner$generation <- 2

    rejected_dispositions <- list(
      list(),
      disposition(dataset = "dataset-b"),
      disposition(capability = "unknown"),
      disposition(capability = "bogus_capability"),
      disposition(capability = c("spatial", "hla")),
      disposition(disposition = "hidden"),
      disposition(disposition = ""),
      disposition(capability = "hla"),
      disposition(generation = 0),
      disposition(owner_token = "")
    )
    for (request in rejected_dispositions) {
      send("core-content_disposition", request)
      stored <- configure_observer_stored(sets)
      expect_identical(stored$revision, 2L)
      expect_identical(length(stored$settings$content_dispositions), 0L)
      expect_identical(surface_resets, 0L)
      expect_identical(configure_workbench_surface(), "spatial")
    }
    session$setInputs(`core-rendered_for` = "dataset-b")
    send("core-content_disposition", disposition())
    expect_identical(configure_observer_stored(sets)$revision, 2L)

    session$setInputs(`core-rendered_for` = "dataset-a")
    send("core-content_disposition", disposition())
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 3L)
    expect_identical(
      stored$settings$content_dispositions,
      list(spatial = "filtered")
    )
    expect_identical(surface_resets, 1L)

    owner$generation <- 3
    send(
      "core-content_disposition",
      disposition(capability = "hla_tcr_motifs", disposition = "auto")
    )
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 3L)
    expect_identical(surface_resets, 0L)
    expect_identical(configure_workbench_surface(), "spatial")
    send("core-content_disposition", disposition(disposition = "auto"))
    stored <- configure_observer_stored(sets)
    expect_identical(stored$revision, 4L)
    expect_identical(length(stored$settings$content_dispositions), 0L)
    expect_identical(surface_resets, 1L)
    expect_identical(configure_observer_stored(sets, "dataset-b")$revision, 0L)
  })
})

test_that("Configure previews render only for the rendered dataset", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()
    null_outputs <- c(
      "core-group_detail",
      "core-group_colors",
      "core-projection_gallery",
      "core-trajectory_gallery"
    )

    for (rendered in list(NULL, "dataset-b")) {
      if (!is.null(rendered)) {
        session$setInputs(`core-rendered_for` = rendered)
        session$flushReact()
      }
      for (name in null_outputs) {
        expect_null(output[[name]], info = name)
      }
      expect_error(
        output[["core-metadata_preview"]],
        class = "shiny.silent.error"
      )
    }

    session$setInputs(`core-rendered_for` = "dataset-a")
    session$flushReact()
    html <- function(name) {
      value <- output[[name]]
      expect_type(value, "list")
      value$html
    }
    expect_match(html("core-group_detail"), "cell_type", fixed = TRUE)
    expect_match(html("core-group_colors"), "#112233", ignore.case = TRUE)
    expect_match(html("core-projection_gallery"), "UMAP", fixed = TRUE)
    expect_match(html("core-trajectory_gallery"), "traj1", fixed = TRUE)
    expect_true(nzchar(html("core-metadata_preview")))

    ## A focused group that is not included: no colour editor.
    session$setInputs(`core-group_focus` = "Phase")
    session$flushReact()
    expect_null(output[["core-group_colors"]])
  })
})

test_that("projection and trajectory previews are queued once per contract", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    queued <- function() {
      Filter(
        function(request) {
          request$kind %in% c("projection_previews", "trajectory_previews")
        },
        Filter(
          Negate(is.null),
          c(list(protocol()$pending), protocol()$queue)
        )
      )
    }
    ready <- configure_observer_entry(app_env, "dataset-a")
    artifact <- configure_observer_entry(
      app_env,
      "dataset-b",
      load_state = "artifact_ready"
    )
    empty <- configure_observer_entry(app_env, "dataset-c")
    empty$profile$viewer_content$projections$umap$available <- FALSE
    empty$profile$viewer_content$projections$pca$available <- FALSE
    empty$profile$viewer_content$trajectories <- list()
    use_state_only_fixture(list(ready, artifact, empty))
    session$flushReact()
    expect_length(projection_previews(), 0L)

    ## No worker process: the dispatcher leaves the first request pending.
    protocol(app_env$builder_request_protocol("worker-configure-previews"))
    worker_available(TRUE)
    session$flushReact()

    requests <- queued()
    kinds <- vapply(requests, `[[`, character(1), "kind")
    expect_setequal(kinds, c("projection_previews", "trajectory_previews"))
    projection <- requests[[which(kinds == "projection_previews")]]
    expect_identical(projection$dataset, "dataset-a")
    expect_identical(projection$slot, "viewer-projection-gallery")
    expect_identical(
      projection$payload[c(
        "kind",
        "id",
        "dataset_revision",
        "projections",
        "group",
        "max_cells",
        "replaces"
      )],
      list(
        kind = "projection_previews",
        id = "dataset-a",
        dataset_revision = 0L,
        projections = c("umap", "pca"),
        group = "cell_type",
        max_cells = app_env$BUILDER_PREVIEW_MAX,
        replaces = "viewer-projection-gallery"
      )
    )
    trajectory <- requests[[which(kinds == "trajectory_previews")]]
    expect_identical(trajectory$dataset, "dataset-a")
    expect_identical(trajectory$slot, "viewer-trajectory-gallery")
    expect_identical(
      trajectory$payload[c(
        "kind",
        "id",
        "dataset_revision",
        "trajectories",
        "max_cells",
        "replaces"
      )],
      list(
        kind = "trajectory_previews",
        id = "dataset-a",
        dataset_revision = 0L,
        trajectories = list(monocle = "traj1", slingshot = "lin1"),
        max_cells = app_env$BUILDER_PREVIEW_MAX,
        replaces = "viewer-trajectory-gallery"
      )
    )
    expect_identical(
      projection_previews()[["dataset-a"]],
      list(
        contract = app_env$builder_projection_preview_contract(
          ready,
          c("umap", "pca")
        ),
        frames = list(),
        status = "pending"
      )
    )
    expect_identical(
      trajectory_previews()[["dataset-a"]]$contract,
      app_env$builder_trajectory_preview_contract(
        ready,
        list(monocle = "traj1", slingshot = "lin1")
      )
    )
    expect_identical(trajectory_previews()[["dataset-a"]]$status, "pending")

    ## A new revision with the same contract is a cache hit: nothing is
    ## queued for it.
    renamed <- configure_observer_stored(sets)
    renamed$settings$name <- "Renamed dataset"
    expect_true(replace_entry(renamed))
    session$flushReact()
    expect_identical(configure_observer_stored(sets)$revision, 1L)
    expect_false(any(vapply(
      queued(),
      function(request) identical(request$payload$dataset_revision, 1L),
      logical(1)
    )))
    count <- length(queued())

    ## Artifact-backed datasets are never previewed.
    current("dataset-b")
    session$flushReact()
    expect_length(queued(), count)
    expect_null(projection_previews()[["dataset-b"]])
    expect_null(trajectory_previews()[["dataset-b"]])

    ## Nothing to preview: the empty result is stored without a request.
    current("dataset-c")
    session$flushReact()
    expect_length(queued(), count)
    expect_identical(
      projection_previews()[["dataset-c"]],
      list(
        contract = app_env$builder_projection_preview_contract(
          empty,
          character()
        ),
        frames = list(),
        status = "ready"
      )
    )
    expect_identical(
      trajectory_previews()[["dataset-c"]],
      list(
        contract = app_env$builder_trajectory_preview_contract(empty, list()),
        frames = list(),
        status = "ready"
      )
    )
  })
})

test_that("rail patches are sent when they change and in full on a resync", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    log <- configure_observer_messages(session)
    use_state_only_fixture(list(
      configure_observer_entry(app_env, "dataset-a"),
      configure_observer_entry(app_env, "dataset-b")
    ))
    session$flushReact()
    dataset_patches <- function() {
      configure_observer_sent(log, "builder_dataset_rail_patch")
    }
    import_patches <- function() {
      configure_observer_sent(log, "builder_import_rail_patch")
    }
    row_ids <- function(patch) vapply(patch$rows, `[[`, character(1), "id")
    has_html <- function(patch) {
      vapply(patch$rows, function(row) is.character(row$html), logical(1))
    }
    sent <- dataset_patches()
    expect_gte(length(sent), 1L)
    expect_identical(sent[[length(sent)]], last_dataset_rail_patch())
    expect_identical(
      row_ids(last_dataset_rail_patch()),
      c("dataset-a", "dataset-b")
    )
    expect_gte(length(import_patches()), 1L)
    expect_identical(
      import_patches()[[length(import_patches())]],
      last_import_rail_patch()
    )

    ## A renamed dataset: one dataset patch, re-rendering only that row; the
    ## unchanged import rail is not sent again.
    configure_observer_reset(log)
    renamed <- configure_observer_stored(sets)
    renamed$settings$name <- "Renamed dataset"
    expect_true(replace_entry(renamed))
    session$flushReact()
    sent <- dataset_patches()
    expect_length(sent, 1L)
    expect_identical(sent[[1L]], last_dataset_rail_patch())
    expect_identical(has_html(sent[[1L]]), c(TRUE, FALSE))
    expect_match(sent[[1L]]$rows[[1L]]$html, "Renamed dataset", fixed = TRUE)
    expect_length(import_patches(), 0L)

    ## Two more edits that leave every row as it is: the first sends the
    ## rows without html, the second the same patch, so nothing.
    configure_observer_reset(log)
    for (size in c(9, 8)) {
      changed <- configure_observer_stored(sets)
      changed$settings$overview_point_size <- size
      expect_true(replace_entry(changed))
      session$flushReact()
    }
    sent <- dataset_patches()
    expect_length(sent, 1L)
    expect_identical(has_html(sent[[1L]]), c(FALSE, FALSE))
    expect_identical(sent[[1L]], last_dataset_rail_patch())

    ## A resync resends every row in full, without touching the last patch.
    before <- last_dataset_rail_patch()
    configure_observer_reset(log)
    session$setInputs(builder_dataset_rail_sync = list(nonce = 1))
    session$flushReact()
    sent <- dataset_patches()
    expect_length(sent, 1L)
    expect_identical(row_ids(sent[[1L]]), c("dataset-a", "dataset-b"))
    expect_identical(has_html(sent[[1L]]), c(TRUE, TRUE))
    expect_identical(last_dataset_rail_patch(), before)
    expect_length(import_patches(), 0L)
  })
})

test_that("a dataset without QC columns keeps applying later Core edits", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- configure_observer_app()
  shiny::testServer(app_env$server, {
    entry <- configure_observer_entry(app_env, "dataset-a")
    entry$profile$default_assay <- "RNA"
    entry$profile$assays <- "RNA"
    entry$profile$assay_profiles <- list(
      RNA = list(
        layers = "data",
        default_layer = "data",
        nUMI_choices = character(),
        nGene_choices = character(),
        nUMI = NA_character_,
        nGene = NA_character_
      )
    )
    entry$settings$organism <- "hg"
    entry$settings$assay <- "RNA"
    entry$settings$layer <- "data"
    entry$settings$expression_backend <- "embedded"
    entry$settings[c("nUMI", "nGene")] <- list(NULL)
    use_state_only_fixture(list(entry))
    session$flushReact()

    ## The empty QC selects send "" as Shiny selectize does.
    session$setInputs(
      `core-rendered_for` = "dataset-a",
      `core-name` = "Renamed",
      `core-organism` = "mm",
      `core-assay` = "RNA",
      `core-layer` = "data",
      `core-nUMI` = "",
      `core-nGene` = "",
      `core-backend` = "embedded"
    )
    session$flushReact()
    stored <- configure_observer_stored(sets)
    expect_identical(stored$settings$name, "Renamed")
    expect_identical(stored$settings$organism, "mm")
    expect_true(all(c("nUMI", "nGene") %in% names(stored$settings)))
    expect_null(stored$settings$nUMI)
    expect_null(stored$settings$nGene)

    session$setInputs(`core-name` = "Renamed again")
    session$flushReact()
    expect_identical(
      configure_observer_stored(sets)$settings$name,
      "Renamed again"
    )
  })
})
