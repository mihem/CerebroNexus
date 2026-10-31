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

  datasets_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "datasets.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  guards <- gregexpr(
    "builder_content_action_owner_is_current(",
    datasets_server,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(guards > 0L), 2L)
  refreshes <- gregexpr(
    "configure_workbench_surface(NULL)",
    datasets_server,
    fixed = TRUE
  )[[1L]]
  expect_identical(sum(refreshes > 0L), 2L)
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

  checkbox <- regexpr('class="viewer-group-include"', html, fixed = TRUE)[[1L]]
  focus <- regexpr('class="viewer-group-focus"', html, fixed = TRUE)[[1L]]
  default <- regexpr('class="viewer-group-default"', html, fixed = TRUE)[[1L]]
  expect_true(checkbox < focus)
  expect_true(focus < default)
})

test_that("group colors align with the distribution rows", {
  withr::local_package("shiny")
  model <- builder_group_colors_model(
    "cell_type",
    c("Short", "A substantially longer cell type")
  )
  html <- htmltools::renderTags(
    builder_group_colors_ui("enhance", model)
  )$html

  expect_match(html, "viewer-group-distribution-list", fixed = TRUE)
  expect_match(html, "viewer-group-distribution-row", fixed = TRUE)
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    css,
    "grid-template-columns: 2rem minmax(5rem, .8fr) minmax(5rem, 1fr) 5rem;",
    fixed = TRUE
  )
  expect_match(css, "padding: .2rem 0;", fixed = TRUE)
  expect_false(grepl("group-color-toggle", html, fixed = TRUE))
  expect_false(grepl("Show all", html, fixed = TRUE))
  expect_false(grepl("Show fewer", html, fixed = TRUE))
})

test_that("group distribution reserves a fixed count column", {
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    "grid-template-columns: minmax(5rem, .8fr) minmax(5rem, 1fr) 6rem;",
    fixed = TRUE
  )
  expect_match(css, ".viewer-group-count {", fixed = TRUE)
  expect_match(css, "text-align: right;", fixed = TRUE)
})

test_that("dataset source titles use the full interface font size", {
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.layout.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    paste(
      ".rail-add-title strong {",
      "  color: var(--c-text);",
      "  font-size: var(--builder-type-subtitle);",
      sep = "\n"
    ),
    fixed = TRUE
  )
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

builder_open_app_child_in_callr <- function(path, env_file, previous = NULL) {
  app_bundle <- builder_profile_inst_path("builder", "app_bundle.R")
  build_status <- builder_profile_inst_path("builder", "ui", "build_status.R")
  viewer_auth <- test_path("..", "..", "R", "viewer-auth.R")
  if (!file.exists(viewer_auth)) {
    viewer_auth <- NULL
  }
  callr::r(
    function(app_bundle, build_status, viewer_auth, path, env_file, previous) {
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
      launched <- FALSE
      if (!is.null(previous)) {
        do.call(
          Sys.setenv,
          stats::setNames(list(previous), "CEREBRO_AUTH_PASSPHRASE")
        )
      }
      before <- Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
      outcome <- tryCatch(
        {
          seen <- runtime$.builder_open_app_child(
            path,
            env_file,
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
    },
    args = list(
      app_bundle = app_bundle,
      build_status = build_status,
      viewer_auth = viewer_auth,
      path = path,
      env_file = env_file,
      previous = previous
    ),
    spinner = FALSE
  )
}

test_that("Open App child rejects post-verification authentication races", {
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
  for (name in names(cases)) {
    fixture <- builder_open_app_runtime_fixture()
    child <- NULL
    opened <- builder_open_final_app(
      fixture$result,
      .open = function(path, env_file) {
        cases[[name]](fixture)
        child <<- builder_open_app_child_in_callr(path, env_file)
        FALSE
      }
    )
    expect_false(opened, info = name)
    expect_false(child$outcome$ok, info = name)
    expect_false(child$launched, info = name)
    expect_identical(
      child$outcome$error,
      "The authentication environment is invalid.",
      info = name
    )
    expect_true(is.na(child$after), info = name)
  }
})

test_that("Open App child launches only a matching pair and restores its environment", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if_not_installed("shinymanager")
  fixture <- builder_open_app_runtime_fixture()
  child <- builder_open_app_child_in_callr(
    fixture$app_dir,
    fixture$env_file,
    previous = "preexisting-auth-environment-value"
  )

  expect_true(child$outcome$ok)
  expect_true(child$launched)
  expect_identical(
    child$outcome$seen,
    builder_auth_read_env_file(fixture$env_file)
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
      process <<- value
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
