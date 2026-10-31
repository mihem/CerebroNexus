builder_profile_source_runtime()

builder_expected_content_ids <- function() {
  c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectory",
    "gene_lists",
    "extra_material",
    "immune_repertoire",
    "hla",
    "spatial",
    "trekker"
  )
}

test_that("dataset profiles expose versioned optional-content facts", {
  skip_if_not_installed("SeuratObject")

  profile <- builder_dataset_profile(
    builder_profile_pbmc(),
    builder_profile_source_fixture()
  )

  expect_identical(profile$schema_version, 2L)
  expect_s3_class(profile$content, "builder_content_profile")
  expect_named(
    profile$content,
    builder_expected_content_ids(),
    ignore.order = FALSE
  )

  for (id in names(profile$content)) {
    fact <- profile$content[[id]]
    required <- c(
      "detected",
      "valid",
      "normalized",
      "diagnostics",
      "requirements",
      "page_candidates"
    )
    expect_true(all(required %in% names(fact)), info = id)
    expect_type(fact$detected, "logical")
    expect_length(fact$detected, 1L)
    expect_false(is.na(fact$detected))
    expect_type(fact$valid, "logical")
    expect_length(fact$valid, 1L)
    expect_false(is.na(fact$valid))
    expect_type(fact$diagnostics, "character")
    expect_false(anyNA(fact$diagnostics))
    expect_type(fact$requirements, "character")
    expect_false(anyNA(fact$requirements))
    expect_type(fact$page_candidates, "character")
    expect_false(anyNA(fact$page_candidates))
    expect_false(anyDuplicated(fact$page_candidates) > 0L)
  }
})

test_that("optional source facts do not make final plan decisions", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  object@misc$marker_genes <- list(malformed = "not a marker table")
  profile <- builder_dataset_profile(
    object,
    builder_profile_source_fixture()
  )

  expect_true(profile$content$marker_genes$detected)
  expect_false(profile$content$marker_genes$valid)
  expect_identical(
    intersect(names(profile$manifest), builder_expected_content_ids()),
    character()
  )
  expect_false("disposition" %in% names(profile$content$marker_genes))
})

test_that("application and worker load content contracts before profiles", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  session <- readLines(
    builder_profile_inst_path("builder", "worker.R"),
    warn = FALSE
  )

  app_hla <- grep(
    "hla_typing[.]R.*hla_motif_core[.]R.*hla_association_core[.]R",
    app
  )[1L]
  app_hla_namespace <- grep(".viewerHLACore", app, fixed = TRUE)[1L]
  app_spatial_contract <- grep(
    "spatial_coordinate_contract[.]R",
    app
  )[1L]
  app_builder_spatial <- grep(
    'source("spatial.R"',
    app,
    fixed = TRUE
  )[1L]
  app_tables <- grep('source("content_tables.R"', app, fixed = TRUE)[1L]
  app_immune <- grep('source("content_immune.R"', app, fixed = TRUE)[1L]
  app_spatial <- grep('source("content_spatial.R"', app, fixed = TRUE)[1L]
  app_content <- grep('source("content.R"', app, fixed = TRUE)[1L]
  app_profile <- grep('source("profile.R"', app, fixed = TRUE)[1L]
  expect_true(
    app_hla < app_hla_namespace &&
      app_spatial_contract < app_builder_spatial &&
      app_builder_spatial < app_spatial &&
      app_hla_namespace < app_tables &&
      app_tables < app_immune &&
      app_immune < app_spatial &&
      app_spatial < app_content &&
      app_content < app_profile
  )

  ## The HLA core comes with the package runtime, which the immune
  ## capability loads before the immune content contract.
  worker_immune_capability <- grep(
    "immune = function()",
    session,
    fixed = TRUE
  )[1L]
  worker_package <- grep(
    'builder_worker_ensure_capability(capabilities, "package")',
    session,
    fixed = TRUE
  )
  worker_package <- worker_package[worker_package > worker_immune_capability][
    1L
  ]
  worker_hla_namespace <- grep(".viewerHLACore", session, fixed = TRUE)[1L]
  worker_spatial_contract <- grep(
    "spatial_coordinate_contract[.]R",
    session
  )[1L]
  worker_builder_spatial <- grep(
    'source_utf8(file.path(dir, "spatial.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_tables <- grep(
    'source_utf8(file.path(dir, "content_tables.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_immune <- grep(
    'file.path(dir, "content_immune.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_spatial <- grep(
    'file.path(dir, "content_spatial.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_content <- grep(
    'source_utf8(file.path(dir, "content.R"',
    session,
    fixed = TRUE
  )[1L]
  worker_profile <- grep(
    'source_utf8(file.path(dir, "profile.R"',
    session,
    fixed = TRUE
  )[1L]
  expect_true(
    worker_immune_capability < worker_package &&
      worker_package < worker_hla_namespace &&
      worker_builder_spatial < worker_spatial_contract &&
      worker_spatial_contract < worker_spatial &&
      worker_tables < worker_content &&
      worker_content < worker_profile &&
      worker_hla_namespace < worker_immune &&
      worker_profile < worker_immune &&
      worker_profile < worker_spatial
  )
})

test_that("installed-layout runtime profiles tracked HLA and TCR together", {
  isolated <- new.env(parent = baseenv())
  files <- builder_profile_source_runtime(isolated)
  demo <- builder_immune_fixture_viewer_demo()
  skip_if(is.null(demo), "tracked HLA/TCR Viewer demo is unavailable")
  repertoire <- demo[["immune_repertoire"]]
  cells <- unique(unlist(lapply(repertoire, `[[`, "barcode")))

  candidate <- isolated$.builder_immune_candidate_from_tables(
    repertoire,
    cells,
    source_kind = "installed_layout_demo"
  )
  hla <- isolated$.builder_immune_profile_hla(
    list(hla_typing = demo[["hla_typing"]]),
    list(installed_layout_demo = candidate)
  )

  expect_true(all(nzchar(files)))
  expect_true(all(file.exists(files)))
  expect_true(candidate$full_ir_ready)
  expect_true(candidate$hla_tcr_ready)
  expect_true(hla$valid)
  expect_identical(hla$normalized$n_rows, 14L)
  expect_identical(hla$page_candidates, character())
  expect_identical(
    hla$unit_mappings$installed_layout_demo$analysis_unit_type,
    unique(
      isolated$hla_analysis_unit_map(
        hla$normalized$table_preview,
        names(repertoire)
      )$unit_type
    )
  )
})
