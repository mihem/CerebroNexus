## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

builder_stage_contract_source_runtime(environment())
sys.source(
  builder_profile_inst_path("builder", "analysis.R"),
  envir = environment()
)

test_that("Core waits for source assay profiles and repairs stale selections", {
  profile <- list(
    assays = c("SCT", "RNA"),
    assay_profiles = list(
      SCT = list(
        layers = c("data", "counts"),
        default_layer = "data",
        nUMI_choices = "nCount_SCT",
        nUMI = "nCount_SCT",
        nGene_choices = "nFeature_SCT",
        nGene = "nFeature_SCT"
      ),
      RNA = list(
        layers = "counts",
        default_layer = "counts",
        nUMI_choices = "nCount_RNA",
        nUMI = "nCount_RNA",
        nGene_choices = "nFeature_RNA",
        nGene = "nFeature_RNA"
      )
    )
  )

  expect_identical(builder_core_resolve_assay(profile, "RNA"), "RNA")
  expect_null(builder_core_resolve_assay(profile, "missing"))
  expect_identical(
    builder_core_resolve_assay(profile, "missing", fallback = "SCT"),
    "SCT"
  )
  expect_null(builder_core_resolve_assay(list(), "RNA"))
  expect_null(builder_core_assay_controls(profile, list(), "missing"))

  controls <- builder_core_assay_controls(
    profile,
    list(layer = "missing", nUMI = NULL, nGene = NULL),
    builder_core_resolve_assay(profile, "missing", fallback = "SCT")
  )
  expect_identical(controls$layer$selected, "data")
  expect_identical(controls$nUMI$selected, "nCount_SCT")
  expect_identical(controls$nGene$selected, "nFeature_SCT")
})

test_that("the organism selector starts empty and required", {
  model <- list(
    id = "ds1",
    name = "Dataset",
    organism = NULL,
    organism_choices = c("Human (hg)" = "hg", "Mouse (mm)" = "mm"),
    group_choices = "cluster",
    suggested_groups = "cluster",
    included_groups = "cluster",
    default_group = "cluster",
    metadata_catalog = list(),
    metadata_policy = list(),
    levels = list(cluster = c("A", "B")),
    content_manifest = list(),
    analysis_manifest = list(),
    content_sources = list(),
    projection_catalog = list(
      umap = list(id = "umap", name = "umap", available = TRUE)
    ),
    projection_choices = "umap",
    included_projections = "umap",
    default_projection = "umap",
    initial_projections = "umap",
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    nUMI_choices = "nCount_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = c(Embedded = "embedded")
  )

  html <- builder_stage_html(builder_core_stage_ui("core", model))

  expect_match(html, ">Organism (required)<", fixed = TRUE)
  expect_match(
    html,
    '<option value="" selected>Select organism</option>',
    fixed = TRUE
  )
})

test_that("the organism selector starts on a detected guess, marked as detected", {
  model <- list(
    id = "ds1",
    name = "Dataset",
    organism = "hg",
    organism_detected = "hg",
    organism_choices = c("Human (hg)" = "hg", "Mouse (mm)" = "mm"),
    group_choices = "cluster",
    included_groups = "cluster",
    default_group = "cluster",
    projection_choices = "umap",
    included_projections = "umap",
    default_projection = "umap",
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data"
  )

  html <- builder_stage_html(builder_core_stage_ui("core", model))
  expect_match(
    html,
    '<option value="hg" selected>Human (hg)</option>',
    fixed = TRUE
  )
  expect_match(html, 'data-builder-detected-organism="hg"', fixed = TRUE)
  expect_match(html, "Detected from gene names", fixed = TRUE)

  model$organism <- NULL
  model$organism_detected <- NULL
  html <- builder_stage_html(builder_core_stage_ui("core", model))
  expect_no_match(html, "data-builder-detected-organism", fixed = TRUE)
  expect_no_match(html, "Detected from gene names", fixed = TRUE)
})

test_that("imported Marker sidecars are explicit in Optional analyses", {
  modules <- builder_enhance_modules(
    profile = list(organism_guess = "hg"),
    settings = list(
      organism = "hg",
      analyses = character(),
      marker_imports = list(
        fixture_sidecars = list(
          method = "Fixture sidecars",
          sources = rep(list(list()), 5L)
        )
      )
    )
  )
  marker <- Filter(
    function(module) identical(module$id, "marker_genes"),
    modules
  )[[1L]]

  expect_true(marker$selected)
  expect_identical(
    marker$consequence,
    "Imported: Fixture sidecars · 5 sources."
  )
})

test_that("gene lists are visible in Inspect and existing-content summaries", {
  tag <- builder_inspect_content_tag(list(id = "gene_lists", status = "valid"))
  expect_identical(tag, list(label = "Gene lists", tone = "analysis"))

  content <- builder_specialized_content_model(list(
    content_manifest = list(
      gene_lists = list(
        status = "valid",
        disposition = "preserved",
        evidence = list(
          detected = TRUE,
          normalized = list(n_lists = 3L, n_genes = 12L)
        )
      )
    )
  ))
  expect_identical(content$total_count, 1L)
  expect_identical(content$items[[1L]]$id, "gene_lists")
  expect_identical(content$items[[1L]]$label, "Gene lists")
  expect_identical(content$items[[1L]]$metrics, c("3 lists", "12 genes"))
  expect_identical(
    content$items[[1L]]$message,
    "Available in Analysis info."
  )
})

test_that("attention content exposes a scoped acknowledgement action", {
  token <- "builder:extra_material:attention-v1:review"
  owner <- list(
    generation = 4,
    project_epoch = 7,
    owner_token = "snapshot-owner-a"
  )
  record <- list(
    status = "attention",
    disposition = "preserved",
    required_action = list(type = "acknowledge", token = token),
    evidence = list(
      detected = TRUE,
      normalized = list(tables = list(), plots = list())
    )
  )
  pending <- builder_specialized_content_model(list(
    id = "ds1",
    content_manifest = list(extra_material = record),
    content_acknowledgements = character(),
    content_action_owner = owner
  ))
  html <- builder_stage_html(builder_specialized_content_ui(pending, "core"))

  expect_identical(pending$attention_count, 1L)
  expect_match(html, "data-builder-acknowledge=\"true\"", fixed = TRUE)
  expect_match(html, "data-dataset=\"ds1\"", fixed = TRUE)
  expect_match(html, "data-capability=\"extra_material\"", fixed = TRUE)
  expect_match(html, "data-token=\"builder:extra_material", fixed = TRUE)
  expect_match(html, "data-generation=\"4\"", fixed = TRUE)
  expect_match(html, "data-project-epoch=\"7\"", fixed = TRUE)
  expect_match(html, "data-owner-token=\"snapshot-owner-a\"", fixed = TRUE)
  expect_match(html, "data-builder-content-disposition=\"true\"", fixed = TRUE)
  expect_match(html, "data-disposition=\"filtered\"", fixed = TRUE)

  acknowledged <- builder_specialized_content_model(list(
    id = "ds1",
    content_manifest = list(extra_material = record),
    content_acknowledgements = token,
    content_action_owner = owner
  ))
  acknowledged_html <- builder_stage_html(
    builder_specialized_content_ui(acknowledged, "core")
  )
  expect_identical(acknowledged$included_count, 1L)
  expect_false(grepl(
    "data-builder-acknowledge",
    acknowledged_html,
    fixed = TRUE
  ))

  excluded <- record
  excluded$status <- "valid"
  excluded$disposition <- "filtered"
  excluded$required_action <- NULL
  excluded_model <- builder_specialized_content_model(list(
    id = "ds1",
    content_manifest = list(extra_material = excluded),
    content_action_owner = owner
  ))
  excluded_html <- builder_stage_html(
    builder_specialized_content_ui(excluded_model, "core")
  )
  expect_match(excluded_html, "data-disposition=\"auto\"", fixed = TRUE)
  expect_match(excluded_html, "Include in CRB", fixed = TRUE)
})

test_that("content actions reject stale same-id dataset owners", {
  project_a <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(owner_token = "snapshot-owner-a")
  )
  request <- builder_content_action_owner(project_a, 7)
  expect_true(builder_content_action_owner_is_current(request, project_a, 7))

  revised <- project_a
  revised$revision <- 5L
  expect_false(builder_content_action_owner_is_current(request, revised, 7))

  same_id_project_b <- project_a
  same_id_project_b$snapshot$owner_token <- "snapshot-owner-b"
  expect_false(builder_content_action_owner_is_current(
    request,
    same_id_project_b,
    7
  ))
  expect_false(builder_content_action_owner_is_current(request, project_a, 8))
  expect_false(builder_content_action_owner_is_current(
    request[names(request) != "owner_token"],
    project_a,
    7
  ))
})

test_that("Inspect uses actionable issue labels and removes duplicate content", {
  expect_identical(
    builder_inspect_issue_label("settings_organism"),
    "Choose an organism in Essentials."
  )

  model <- builder_inspect_model(
    profile = list(n_cells = 12L, n_genes = 8L),
    state = list(
      attention_ids = "settings_organism",
      blocking_ids = character(),
      manifest = list(
        list(id = "spatial", status = "valid"),
        list(id = "reduction:spatial", status = "valid")
      )
    ),
    format = "RDS",
    dataset_id = "ds1"
  )
  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_identical(length(model$content_tags), 1L)
  expect_match(html, "Choose an organism in Essentials.", fixed = TRUE)
  expect_false(grepl("settings_organism", html, fixed = TRUE))
  expect_match(html, "1 content type", fixed = TRUE)
  ## Cell and gene counts render as stat figures: the number over its unit.
  expect_match(
    html,
    '<span class="builder-stat-value">12</span>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<span class="builder-stat-label">genes</span>',
    fixed = TRUE
  )
})

test_that("Inspect notes a source cell_barcode column the CRB renames", {
  reason <- "The CRB keeps that column as cell_barcode_source."
  model <- builder_inspect_model(
    profile = list(n_cells = 12L, n_genes = 8L),
    state = list(
      attention_ids = character(),
      blocking_ids = character(),
      metadata_policy = list(
        columns = list(
          cell_barcode = list(
            disposition = "included",
            reason = reason,
            renamed_to = "cell_barcode_source"
          )
        )
      ),
      manifest = list()
    ),
    format = "RDS",
    dataset_id = "ds1"
  )
  expect_identical(model$notes, reason)
  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))
  expect_match(html, "builder-inspect-notes", fixed = TRUE)
  expect_match(html, reason, fixed = TRUE)
  expect_false(grepl("Needs attention", html, fixed = TRUE))
})

test_that("Inspect names the metadata column that blocks the policy", {
  reason <- "The object has a metadata column named cell_barcode. Rename it."
  policy <- list(
    columns = list(
      cell_barcode = list(disposition = "blocking", reason = reason),
      cluster = list(disposition = "included", reason = "Fine.")
    )
  )
  expect_identical(
    builder_inspect_issue_label("metadata_policy", policy),
    reason
  )
  model <- builder_inspect_model(
    profile = list(n_cells = 12L, n_genes = 8L),
    state = list(
      attention_ids = character(),
      blocking_ids = "metadata_policy",
      metadata_policy = policy,
      manifest = list()
    ),
    format = "RDS",
    dataset_id = "ds1"
  )
  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))
  expect_match(html, "named cell_barcode", fixed = TRUE)

  ## Columns waiting for confirmation are named too.
  policy$columns$cell_barcode$disposition <- "included"
  policy$columns$donor <- list(disposition = "attention", reason = "Check.")
  expect_match(
    builder_inspect_issue_label("metadata_policy", policy),
    "donor",
    fixed = TRUE
  )
  ## Without a policy the generic label stays.
  expect_identical(
    builder_inspect_issue_label("metadata_policy"),
    "Metadata Policy"
  )
  expect_identical(
    builder_inspect_issue_label("settings_name"),
    "Type a dataset name in Essentials."
  )
})

test_that("group catalog lists eligible columns before ineligible columns", {
  metadata <- list(
    score = list(name = "Score", group_eligible = FALSE),
    cluster = list(name = "Cluster", group_eligible = TRUE),
    barcode = list(name = "Barcode", group_eligible = FALSE),
    sample = list(name = "Sample", group_eligible = TRUE)
  )
  model <- builder_group_catalog_model(list(
    metadata_catalog = metadata,
    metadata_policy = list(),
    included_groups = "cluster",
    default_group = "cluster"
  ))

  expect_identical(
    vapply(model$items, `[[`, character(1), "id"),
    c("cluster", "sample", "score", "barcode")
  )
})

test_that("group catalog searches visible labels and keeps controls compact", {
  withr::local_package("shiny")
  metadata <- list(
    stable_sample_id = list(
      name = "Visible sample label",
      classification = "categorical",
      group_eligible = TRUE
    ),
    barcode = list(
      name = "Cell barcode",
      classification = "character",
      group_eligible = FALSE,
      group_reason = "Unique values"
    ),
    empty_cluster = list(
      name = "Empty cluster",
      classification = "categorical",
      group_eligible = FALSE,
      group_reason = "All values are missing"
    )
  )
  model <- builder_group_catalog_model(list(
    metadata_catalog = metadata,
    metadata_policy = list(),
    included_groups = "stable_sample_id",
    default_group = "stable_sample_id"
  ))
  html <- htmltools::renderTags(
    builder_group_catalog_ui("enhance", model)
  )$html

  expect_match(model$items[[1L]]$search, "visible sample label", fixed = TRUE)
  expect_match(html, 'class="viewer-group-unavailable"', fixed = TRUE)
  expect_match(html, "Unavailable metadata", fixed = TRUE)
  expect_match(html, 'class="viewer-group-unavailable-count">2<', fixed = TRUE)
  expect_false(grepl(">Group<", html, fixed = TRUE))
})

builder_open_app_runtime_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  app_dir <- file.path(root, "cerebro_app")
  database <- file.path(
    app_dir,
    "private-data",
    "auth",
    "credentials.sqlite"
  )
  dir.create(dirname(database), recursive = TRUE)
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    root,
    .capability = function() list(available = TRUE, reason = NULL)
  )
  expect_true(file.copy(material$credentials, database))
  env_file <- file.path(app_dir, "viewer-auth.env")
  expect_true(file.copy(material$env_file, env_file, copy.mode = TRUE))
  Sys.chmod(env_file, mode = "0600", use_umask = FALSE)
  list(
    root = root,
    app_dir = app_dir,
    database = database,
    env_file = env_file,
    result = builder_result_success(
      published = TRUE,
      app_dir = app_dir,
      app_verified = TRUE,
      auth_enabled = TRUE,
      auth_env_file = env_file,
      release = list(target = root)
    )
  )
}

## Runs each `list(path, env_file, previous)` in turn inside one callr child.
builder_open_app_child_in_callr <- function(runs) {
  app_bundle <- builder_profile_inst_path("builder", "app_bundle.R")
  build_status <- builder_profile_inst_path("builder", "ui", "build_status.R")
  viewer_auth <- test_path("..", "..", "R", "viewer-auth.R")
  if (!file.exists(viewer_auth)) {
    viewer_auth <- NULL
  }
  callr::r(
    function(app_bundle, build_status, viewer_auth, runs) {
      app_bundle <- normalizePath(app_bundle)
      build_status <- normalizePath(build_status)
      if (!is.null(viewer_auth)) {
        viewer_auth <- normalizePath(viewer_auth)
      }
      # `callr` starts outside the package checkout; make the assembly loader's
      # documented checkout-relative fallback available in this isolated child.
      setwd(dirname(dirname(dirname(app_bundle))))
      runtime <- new.env(parent = globalenv())
      sys.source(app_bundle, envir = runtime)
      if (is.null(viewer_auth)) {
        runtime$.viewerAuthValidateDatabase <- getFromNamespace(
          ".viewerAuthValidateDatabase",
          "CerebroNexus"
        )
      } else {
        sys.source(viewer_auth, envir = runtime)
      }
      sys.source(build_status, envir = runtime)
      lapply(runs, function(run) {
        launched <- FALSE
        if (is.null(run$previous)) {
          Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE")
        } else {
          do.call(
            Sys.setenv,
            stats::setNames(list(run$previous), "CEREBRO_AUTH_PASSPHRASE")
          )
        }
        before <- Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
        outcome <- tryCatch(
          {
            seen <- runtime$.builder_open_app_child(
              run$path,
              run$env_file,
              "CEREBRO_AUTH_PASSPHRASE",
              validate_database = runtime$.viewerAuthValidateDatabase,
              run_app = function(...) {
                launched <<- TRUE
                Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
              }
            )
            list(ok = TRUE, seen = seen, error = NULL)
          },
          error = function(error) {
            list(ok = FALSE, seen = NULL, error = conditionMessage(error))
          }
        )
        list(
          outcome = outcome,
          launched = launched,
          before = before,
          after = Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
        )
      })
    },
    args = list(
      app_bundle = app_bundle,
      build_status = build_status,
      viewer_auth = viewer_auth,
      runs = runs
    ),
    spinner = FALSE
  )
}

test_that("Open App child launches only an unchanged pair and restores its environment", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  cases <- list(
    multiline = function(fixture) {
      writeLines(
        c(
          paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("a", 64L)),
          "another=value"
        ),
        fixture$env_file
      )
      Sys.chmod(fixture$env_file, mode = "0600", use_umask = FALSE)
    },
    symlink = function(fixture) {
      target <- file.path(fixture$root, "alternate.env")
      expect_true(file.copy(fixture$env_file, target))
      unlink(fixture$env_file)
      expect_true(file.symlink(target, fixture$env_file))
    },
    hardlink = function(fixture) {
      target <- file.path(fixture$root, "alternate.env")
      expect_true(file.copy(fixture$env_file, target))
      unlink(fixture$env_file)
      expect_true(file.link(target, fixture$env_file))
    },
    mode = function(fixture) {
      Sys.chmod(fixture$env_file, mode = "0644", use_umask = FALSE)
    },
    mismatch = function(fixture) {
      writeLines(
        paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("b", 64L)),
        fixture$env_file
      )
      Sys.chmod(fixture$env_file, mode = "0600", use_umask = FALSE)
    },
    database = function(fixture) {
      replacement <- file.path(fixture$root, "replacement.sqlite")
      writeBin(as.raw(seq_len(8L)), replacement)
      unlink(fixture$database)
      expect_true(file.rename(replacement, fixture$database))
    }
  )
  # Each race changes the pair after the parent verified it.
  runs <- list()
  for (name in names(cases)) {
    fixture <- builder_open_app_runtime_fixture()
    opened <- builder_open_final_app(
      fixture$result,
      .open = function(path, env_file) {
        cases[[name]](fixture)
        runs[[name]] <<- list(path = path, env_file = env_file)
        FALSE
      }
    )
    expect_false(opened, info = name)
  }
  matching <- builder_open_app_runtime_fixture()
  runs$matching <- list(
    path = matching$app_dir,
    env_file = matching$env_file,
    previous = "preexisting-auth-environment-value"
  )
  children <- builder_open_app_child_in_callr(runs)

  for (name in names(cases)) {
    child <- children[[name]]
    expect_false(child$outcome$ok, info = name)
    expect_false(child$launched, info = name)
    expect_identical(
      child$outcome$error,
      "The authentication environment is invalid.",
      info = name
    )
    expect_true(is.na(child$after), info = name)
  }
  child <- children$matching
  expect_true(child$outcome$ok)
  expect_true(child$launched)
  expect_identical(
    child$outcome$seen,
    builder_auth_read_env_file(matching$env_file)
  )
  expect_identical(child$before, "preexisting-auth-environment-value")
  expect_identical(child$after, "preexisting-auth-environment-value")
})

test_that("Open App uses its default r_bg launcher without parent helpers", {
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  fixture <- builder_open_app_runtime_fixture()
  process <- NULL

  expect_true(builder_open_final_app(
    fixture$result,
    .run_app = function(...) {
      Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
    },
    .on_open = function(value) {
      process <<- value$process
    }
  ))
  expect_s3_class(process, "r_process")
  withr::defer(if (process$is_alive()) try(process$kill(), silent = TRUE))
  process$wait(120000)
  expect_false(process$is_alive())
  expect_identical(
    process$get_result(),
    builder_auth_read_env_file(fixture$env_file)
  )
})

test_that("Open App keeps the App running on its port until Builder stops", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  root <- withr::local_tempdir()
  app_dir <- normalizePath(
    file.path(root, "cerebro_app"),
    winslash = "/",
    mustWork = FALSE
  )
  dir.create(app_dir)
  configured <- httpuv::randomPort(host = "127.0.0.1")
  saveRDS(
    list(
      .bundle_run_options = list(
        shiny_app_options = list(port = configured, host = "127.0.0.1")
      )
    ),
    file.path(app_dir, "cerebro_config.rds")
  )
  result <- builder_result_success(
    published = TRUE,
    app_dir = app_dir,
    app_verified = TRUE,
    release = list(target = root)
  )
  # Stands in for shiny::runApp(): serves on the port it is given.
  serve <- function(path, port, host, ...) {
    httpuv::runServer(
      host,
      port,
      list(call = function(request) {
        list(status = 200L, headers = list(), body = "ok")
      })
    )
  }
  environment(serve) <- globalenv()
  withr::defer(builder_stop_opened_apps())
  wait_ready <- function(app) {
    for (i in seq_len(240L)) {
      state <- builder_opened_app_state(app)
      if (!identical(state, "starting")) {
        return(state)
      }
      Sys.sleep(0.25)
    }
    state
  }
  opened <- list()
  remember <- function(app) {
    opened[[length(opened) + 1L]] <<- app
  }

  expect_true(builder_open_final_app(
    result,
    .run_app = serve,
    .on_open = remember
  ))
  first <- opened[[1L]]
  expect_identical(first$port, as.integer(configured))
  # No reference outside the registry: a collected handle would kill the App.
  opened <- list()
  invisible(gc(full = TRUE))
  expect_identical(wait_ready(first), "ready")
  expect_identical(
    builder_opened_app_message(first, "ready")[c("ok", "port", "host")],
    list(ok = TRUE, port = as.integer(configured), host = "127.0.0.1")
  )

  # A second click reuses the running App.
  expect_true(builder_open_final_app(
    result,
    .run_app = serve,
    .on_open = remember
  ))
  expect_identical(opened[[1L]]$process$get_pid(), first$process$get_pid())

  # Builder stopping stops the App.
  builder_stop_opened_apps()
  first$process$wait(10000)
  expect_false(first$process$is_alive())
  expect_identical(builder_opened_app_state(first), "stopped")

  # A taken port falls back to a free one.
  blocker <- httpuv::startServer("127.0.0.1", configured, list())
  withr::defer(httpuv::stopServer(blocker))
  opened <- list()
  expect_true(builder_open_final_app(
    result,
    .run_app = serve,
    .on_open = remember
  ))
  second <- opened[[1L]]
  expect_false(identical(second$port, as.integer(configured)))
  expect_identical(second$configured_port, as.integer(configured))
  expect_identical(wait_ready(second), "ready")
})

test_that("Apply to all targets loaded unchecked datasets", {
  entries <- list(
    list(id = "a", settings = list(organism = NULL)),
    list(
      id = "b",
      settings = list(organism = "hg", analyses = c("percent_mt_ribo", "x"))
    ),
    list(id = "c", settings = list(organism = NULL)),
    list(id = "d", load_state = "artifact_ready", settings = list()),
    list(id = "e", settings = list(organism = "zebrafish"))
  )

  targets <- builder_core_organism_targets(entries, "zebrafish", checked = "c")

  expect_identical(vapply(targets, `[[`, character(1), "id"), c("a", "b"))
  expect_identical(targets[[2L]]$settings$analyses, "x")
  expect_identical(
    builder_core_organism_targets(entries, "hg", checked = "c")[[2L]]$id,
    "e"
  )
  expect_identical(
    builder_core_organism_targets(entries, "hg", checked = "c")[[1L]]$settings,
    list(organism = "hg")
  )
})

test_that("the group colours show the cells of every level, not only the first 12", {
  levels <- paste0("level_", 1:15)
  cells <- stats::setNames(seq(1500, by = -100, length.out = 15), levels)
  profile <- list(
    group_counts = list(
      cluster = as.table(cells)
    )
  )
  full <- builder_group_full_distribution(profile, "cluster")
  expect_length(full, 15L)
  expect_identical(full[[15L]], list(value = "level_15", count = 100))

  ## The catalog lists 12 levels: the rest would show no cells.
  listed <- builder_group_colors_model(
    "cluster",
    levels,
    distribution = full[1:12]
  )
  expect_identical(
    vapply(listed$items, `[[`, numeric(1), "count")[13:15],
    c(0, 0, 0)
  )

  shown <- builder_group_colors_model("cluster", levels, distribution = full)
  expect_identical(
    vapply(shown$items, `[[`, numeric(1), "count"),
    unname(cells)
  )

  ## No profile, no counts of its own, or another group: nothing to add.
  expect_identical(builder_group_full_distribution(NULL, "cluster"), list())
  expect_identical(builder_group_full_distribution(profile, "other"), list())
})
