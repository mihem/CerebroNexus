builder_e2e_source_runtime()

test_that("new and restored projects use explicit spatial image storage", {
  profile <- list(
    default_assay = "RNA",
    assay_profiles = list(
      RNA = list(
        default_layer = "data",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA"
      )
    ),
    group_preselect = "cluster",
    reduction_preselect = "umap",
    organism_guess = "hg"
  )
  expect_identical(
    builder_default_settings(profile, "Dataset A")$spatial_image_storage,
    "external"
  )

  legacy <- list(settings = list(viewer_content_schema_version = 1L))
  expect_identical(
    builder_upgrade_viewer_content_entry(legacy)$settings$spatial_image_storage,
    "external"
  )
})

test_that("generated App smoke detects whether browser verification is available", {
  expect_true(builder_e2e_browser_available(list(
    path = "/path/to/chrome",
    error = "",
    .check = list(status = 0L)
  )))
  expect_false(builder_e2e_browser_available(list(
    path = "/path/to/chrome",
    error = "",
    .check = list(status = -6L)
  )))
  expect_false(builder_e2e_browser_available(list(
    path = "",
    error = "Chrome was not found",
    .check = list(status = 0L)
  )))
})

test_that("Builder release documentation matches the guided workflow", {
  builder_dir <- normalizePath(builder_profile_inst_path("builder"))
  repo <- dirname(dirname(builder_dir))
  readme_path <- file.path(repo, "README.md")
  vignette_path <- file.path(
    repo,
    "vignettes",
    "build_a_data_set_by_pointing.Rmd"
  )
  skip_if_not(
    file.exists(readme_path) && file.exists(vignette_path),
    "Builder source documentation not present (installed-package layout)"
  )

  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  readme_text <- gsub("\\s+", " ", readme)
  vignette_lines <- readLines(vignette_path, warn = FALSE)
  vignette <- paste(vignette_lines, collapse = "\n")
  vignette_text <- gsub("\\s+", " ", vignette)
  news_path <- file.path(repo, "NEWS.md")
  news <- readLines(news_path, warn = FALSE)
  news_text <- gsub("\\s+", " ", paste(news, collapse = "\n"))
  for (stage in c(
    "**Data**",
    "**Configure**",
    "**Review**",
    "**Build**"
  )) {
    expect_match(readme_text, stage, fixed = TRUE)
    expect_match(vignette_text, stage, fixed = TRUE)
  }
  expect_match(readme, "build_a_data_set_by_pointing", fixed = TRUE)

  for (contract in c(
    "CRB-only",
    "Public App",
    "Login App",
    "cerebro_app/viewer-auth.env",
    "private-data",
    "viewer_bundle_assets",
    "HTTP exposure",
    "snapshot disk cost",
    "Needs decision",
    "rollback",
    "recovery",
    "ownership migration",
    "Reveal Folder",
    "Copy Path",
    "Copy Report"
  )) {
    expect_match(vignette, contract, fixed = TRUE)
  }
  expect_false(grepl("logged and skipped", vignette, fixed = TRUE))
  expect_match(news_text, "planned payload targets", fixed = TRUE)
  for (contract in c(
    "distinct source adapters",
    "shared import pipeline",
    "planned payload targets",
    "snapshot payload once",
    "build-report.json",
    ".cerebro-builder-release-v1",
    "Reveal Folder"
  )) {
    expect_match(vignette_text, contract, fixed = TRUE)
  }
  combined_docs <- tolower(paste(readme_text, news_text, vignette_text))
  expect_false(grepl("exact release members", combined_docs, fixed = TRUE))
  expect_false(grepl("same adapter", combined_docs, fixed = TRUE))
  expect_false(grepl(
    "each card says whether its source is real or synthetic",
    combined_docs,
    fixed = TRUE
  ))
  expect_false(grepl(
    "disk estimate includes both sets",
    combined_docs,
    fixed = TRUE
  ))

  resource_start <- match("resource_files:", trimws(vignette_lines))
  expect_false(is.na(resource_start))
  resource_end <- which(
    trimws(vignette_lines) == "---" & seq_along(vignette_lines) > resource_start
  )[[1L]]
  resources <- trimws(vignette_lines[seq.int(
    resource_start + 1L,
    resource_end - 1L
  )])
  resources <- sub("^-\\s*", "", resources)
  resources <- resources[nzchar(resources)]
  expect_length(resources, 1L)
  expect_true(all(file.exists(file.path(dirname(vignette_path), resources))))

  builder_heading <- match("## Builder", news)
  next_release <- which(grepl("^# CerebroNexus ", news))[[2L]]
  expect_false(is.na(builder_heading))
  expect_lt(builder_heading, next_release)
})

test_that("the Builder example catalog is a stable product contract", {
  expect_true(exists("builder_example_catalog", mode = "function"))
  catalog <- builder_example_catalog()
  expected_ids <- c(
    "complete_viewer_data",
    "trekker_spatial",
    "spatial_alignment"
  )
  expect_identical(names(catalog), expected_ids)
  expect_identical(
    unname(vapply(catalog, `[[`, character(1), "id")),
    expected_ids
  )
  expect_identical(
    anyDuplicated(vapply(
      catalog,
      `[[`,
      character(1),
      "label"
    )),
    0L
  )
  required <- c(
    "id",
    "label",
    "detail",
    "provenance",
    "make",
    "serialized_path",
    "expected_manifest",
    "expected_dispositions",
    "expected_pages",
    "expected_supporting_content",
    "histology_images",
    "gallery_visible"
  )
  expect_true(all(vapply(
    catalog,
    function(record) {
      identical(names(record), required)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    catalog,
    function(record) {
      record$provenance %in%
        c("real", "synthetic") &&
        file.exists(record$serialized_path) &&
        !grepl("^[a-z]+://", record$serialized_path)
    },
    logical(1)
  )))
  expect_identical(
    unname(vapply(builder_examples(), `[[`, character(1), "id")),
    expected_ids
  )
})

test_that("sourced Builder resources stay in the io.R inst tree", {
  root <- withr::local_tempdir(pattern = "builder-resource-root-")
  source_inst <- file.path(root, "checkout", "inst")
  installed_inst <- file.path(root, "current-library", "CerebroNexus")
  stale_inst <- file.path(root, "stale-library", "CerebroNexus")
  unrelated <- file.path(root, "unrelated-working-directory")
  dir.create(file.path(source_inst, "builder"), recursive = TRUE)
  dir.create(file.path(installed_inst, "builder"), recursive = TRUE)
  dir.create(stale_inst, recursive = TRUE)
  dir.create(unrelated)

  io_source <- builder_profile_inst_path("builder", "io.R")
  io_copy <- file.path(source_inst, "builder", "io.R")
  installed_io <- file.path(installed_inst, "builder", "io.R")
  expect_true(file.copy(io_source, io_copy))
  expect_true(file.copy(io_source, installed_io))
  resources <- c(
    "builder/fixtures/complete_viewer/complete_viewer_data.rds",
    "builder/fixtures/trekker_spatial/trekker_4_tissues.qs2",
    "builder/fixtures/spatial_alignment/arrow_spatial.rds",
    "builder/fixtures/spatial_alignment/arrow_background.png",
    "extdata/examples/demo_trekker.crb"
  )
  for (relative in resources) {
    current <- file.path(source_inst, relative)
    installed <- file.path(installed_inst, relative)
    stale <- file.path(stale_inst, relative)
    dir.create(dirname(current), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(installed), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(stale), recursive = TRUE, showWarnings = FALSE)
    expect_true(file.copy(builder_profile_inst_path(relative), current))
    expect_true(file.copy(builder_profile_inst_path(relative), installed))
    writeBin(charToRaw("stale installed resource"), stale)
  }

  load_io <- function(loader, path) {
    runtime <- new.env(parent = globalenv())
    runtime$system.file <- local({
      root <- stale_inst
      function(..., package, lib.loc = NULL, mustWork = FALSE) {
        path <- file.path(root, ...)
        if (isTRUE(mustWork) && !file.exists(path)) {
          stop("No file found", call. = FALSE)
        }
        path
      }
    })
    if (identical(loader, "source")) {
      source(path, local = runtime, chdir = FALSE)
    } else {
      sys.source(path, envir = runtime)
    }
    runtime
  }

  withr::local_dir(unrelated)
  cases <- list(
    `direct runApp source` = list("source", io_copy, source_inst),
    `tests and data-raw sys.source` = list("sys.source", io_copy, source_inst),
    `installed app source` = list("source", installed_io, installed_inst)
  )
  for (case in names(cases)) {
    loader <- cases[[case]][[1L]]
    io_path <- cases[[case]][[2L]]
    expected_root <- cases[[case]][[3L]]
    runtime <- load_io(loader, io_path)
    catalog <- runtime$builder_example_catalog()
    expected_paths <- normalizePath(
      file.path(expected_root, resources),
      winslash = "/"
    )
    observed_paths <- c(
      catalog$complete_viewer_data$serialized_path,
      catalog$trekker_spatial$serialized_path,
      catalog$spatial_alignment$serialized_path,
      file.path(
        dirname(catalog$spatial_alignment$serialized_path),
        catalog$spatial_alignment$histology_images$arrow_background$path
      ),
      runtime$.builder_example_path("extdata/examples/demo_trekker.crb")
    )
    expect_identical(unname(observed_paths), expected_paths, info = case)
    expect_true(all(file.exists(observed_paths)), info = case)
    expect_true(
      all(startsWith(
        observed_paths,
        paste0(normalizePath(expected_root, winslash = "/"), "/")
      )),
      info = case
    )
    expect_s4_class(
      catalog$complete_viewer_data$make()$object,
      "Seurat"
    )
    expect_s4_class(catalog$trekker_spatial$make()$object, "Seurat")
    expect_s4_class(catalog$spatial_alignment$make()$object, "Seurat")
  }
})

test_that("example and file adapters inspect the same immutable objects", {
  catalog <- builder_example_catalog()
  set.seed(1401)
  caller_seed <- .Random.seed

  for (record in catalog) {
    first <- record$make()
    second <- record$make()
    expect_null(first$error, info = record$id)
    expect_identical(
      serialize(first$object, NULL),
      serialize(second$object, NULL),
      info = record$id
    )
    example <- builder_adapter_inspect(
      builder_example_adapter(record$id, first$object)
    )
    file <- builder_adapter_inspect(
      builder_seurat_file_adapter(record$serialized_path)
    )
    expect_identical(
      example$legacy_profile,
      file$legacy_profile,
      info = record$id
    )
    expect_identical(example$levels, file$levels, info = record$id)
    expect_identical(
      builder_e2e_without_source(example$profile),
      builder_e2e_without_source(file$profile),
      info = record$id
    )
  }

  expect_identical(.Random.seed, caller_seed)
})

test_that("immune and motif pages follow receptor semantics, not HLA presence", {
  repertoire <- function(chain) {
    list(
      sample_a = data.frame(
        barcode = "cell1",
        CTgene = paste0(chain, "V1.", chain, "J1"),
        CTnt = "ACGT",
        CTaa = "CASSLGQF",
        CTstrict = paste0(chain, "_clone"),
        stringsAsFactors = FALSE
      )
    )
  }
  crb <- function(ir = NULL, hla = NULL) {
    value <- new.env(parent = emptyenv())
    value$immune_repertoire <- ir
    value$hla_typing <- hla
    value
  }
  hla <- data.frame(sample = "sample_a", allele = "HLA-A*02:01")
  cases <- list(
    tcr_hla = list(
      crb(repertoire("TRB"), hla),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    tcr_only = list(
      crb(repertoire("TRB")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    hla_only = list(crb(hla = hla), character()),
    bcr_only = list(crb(repertoire("IGH")), "immune_repertoire"),
    metadata_tcr = list(
      crb(repertoire("TRA")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    ),
    legacy_tcr = list(
      crb(repertoire("TRB")),
      c(
        "immune_repertoire",
        "hla_tcr_motifs"
      )
    )
  )
  for (name in names(cases)) {
    expect_setequal(
      .builder_crb_visible_pages(cases[[name]][[1L]]),
      cases[[name]][[2L]]
    )
  }
})

test_that("complete fixture derives precise optional-content failure states", {
  source <- builder_example_catalog()$complete_viewer_data$make()$object

  no_hla <- builder_e2e_variant(source, function(object) {
    object@misc$hla_typing <- NULL
    object@misc$hla_typing_source_type <- NULL
    object
  })
  no_hla_state <- builder_dataset_state(
    builder_e2e_variant_entry(no_hla, "complete-no-hla")
  )
  expect_setequal(
    intersect(
      no_hla_state$page_expectations$visible_conditional,
      c("immune_repertoire", "hla_tcr_motifs")
    ),
    c("immune_repertoire", "hla_tcr_motifs")
  )

  no_tcr <- builder_e2e_variant(source, function(object) {
    object@misc$immune_repertoire <- NULL
    object
  })
  no_tcr_state <- builder_dataset_state(
    builder_e2e_variant_entry(no_tcr, "complete-no-tcr")
  )
  expect_false(
    "hla_tcr_motifs" %in% no_tcr_state$page_expectations$visible_conditional
  )
  expect_identical(no_tcr_state$manifest$hla$status, "valid")

  invalid_trekker <- builder_e2e_variant(source, function(object) {
    object@misc$trekker$barcodes[[1L]] <- "outside-dataset"
    object
  })
  invalid_trekker_state <- builder_dataset_state(
    builder_e2e_variant_entry(invalid_trekker, "complete-invalid-trekker")
  )
  expect_contains(invalid_trekker_state$blocking_ids, "trekker")
  expect_contains(
    invalid_trekker_state$manifest$trekker$evidence$diagnostics,
    "unknown_barcodes"
  )

  incomplete_trekker <- builder_e2e_variant(source, function(object) {
    object@misc$trekker$x <- NULL
    object
  })
  incomplete_trekker_state <- builder_dataset_state(
    builder_e2e_variant_entry(incomplete_trekker, "complete-missing-trekker-x")
  )
  expect_contains(incomplete_trekker_state$blocking_ids, "trekker")

  invalid_immune <- builder_e2e_variant(source, function(object) {
    object@misc$immune_repertoire[[1L]]$barcode[[1L]] <- "outside-dataset"
    object
  })
  invalid_immune_state <- builder_dataset_state(
    builder_e2e_variant_entry(invalid_immune, "complete-invalid-immune")
  )
  expect_contains(invalid_immune_state$attention_ids, "immune_repertoire")
  expect_contains(invalid_immune_state$attention_ids, "hla_tcr_motifs")

  unsupported <- builder_e2e_variant(source, function(object) {
    object@misc$trajectories$slingshot <- list(
      lineage = list(curves = matrix(1, 1, 1))
    )
    object
  })
  unsupported_entry <- builder_e2e_variant_entry(
    unsupported,
    "complete-unsupported-trajectory"
  )
  unsupported_state <- builder_dataset_state(unsupported_entry)
  expect_contains(unsupported_state$blocking_ids, "trajectory")
  unsupported_entry$settings$content_dispositions <- list(
    trajectory = "filtered"
  )
  filtered_state <- builder_dataset_state(unsupported_entry)
  expect_identical(
    filtered_state$manifest$trajectory$disposition,
    "filtered"
  )
  expect_false("trajectory" %in% filtered_state$blocking_ids)

  missing <- builder_marker_import_inventory(
    file.path(tempdir(), "missing-marker.csv"),
    "missing-marker.csv"
  )[[1L]]
  expect_identical(missing$error, "file_not_found")
})

test_that("Viewer defaults lead included group and projection order", {
  expect_identical(
    builder_default_first(c("sample", "cell_type", "condition"), "condition"),
    c("condition", "sample", "cell_type")
  )
  expect_identical(
    builder_default_first(c("pca", "umap", "tsne"), "tsne"),
    c("tsne", "pca", "umap")
  )
})

test_that("catalog expectations are derived from real inspection and Review", {
  for (record in builder_example_catalog()) {
    entry <- builder_e2e_entry(record)
    recommendations <- entry$settings$recommendations
    expect_type(recommendations, "list")
    expect_named(
      recommendations,
      c(
        "metadata",
        "groups",
        "projections",
        "organism",
        "nomenclature",
        "backend"
      )
    )
    expect_true(length(entry$settings$groups) > 0L, info = record$id)
    expect_true(
      all(entry$settings$groups %in% entry$settings$metadata_policy$included),
      info = record$id
    )
    expect_identical(
      entry$settings$reductions,
      recommendations$projections$included,
      info = record$id
    )
    expect_true(
      entry$settings$default_group %in% entry$settings$groups,
      info = record$id
    )
    expect_identical(
      entry$settings$default_projection,
      recommendations$projections$value,
      info = record$id
    )
    expect_identical(
      entry$settings$expression_backend,
      recommendations$backend$value,
      info = record$id
    )
    expect_false(
      identical(entry$settings$metadata_policy, recommendations$metadata),
      info = record$id
    )
    expect_length(entry$settings$metadata_policy$attention, 0L)
    expect_identical(
      recommendations$metadata$attention,
      names(Filter(
        function(column) identical(column$disposition, "attention"),
        recommendations$metadata$columns
      )),
      info = record$id
    )
    state <- builder_dataset_state(entry)
    observed <- vapply(
      state$manifest,
      function(entry) {
        entry$disposition %||% NA_character_
      },
      character(1)
    )
    observed <- observed[!is.na(observed)]
    expect_identical(
      names(observed),
      record$expected_manifest,
      info = record$id
    )
    expect_identical(observed, record$expected_dispositions, info = record$id)
    expect_identical(
      state$page_expectations$visible_conditional,
      record$expected_pages,
      info = record$id
    )
    supporting <- setdiff(record$expected_supporting_content, "extra_material")
    expect_true(
      all(file.exists(file.path(
        dirname(record$serialized_path),
        supporting
      ))),
      info = record$id
    )
  }
})

test_that("all valid examples build and reopen", {
  root <- withr::local_tempdir()
  for (record in builder_example_catalog()) {
    entry <- builder_e2e_entry(record)
    object <- record$make()$object
    snapshot <- builder_snapshot_seurat(
      object,
      file.path(root, paste0(record$id, "-snapshot")),
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    if (identical(record$id, "complete_viewer_data")) {
      entry <- builder_e2e_configure_complete_viewer_data(
        entry,
        record,
        object
      )
    }
    release <- file.path(root, paste0(record$id, "-release"))
    make_app <- TRUE
    plan <- builder_freeze_plan(
      list(entry),
      release,
      make_app = make_app,
      app_options = if (make_app) list(launch_browser = FALSE) else list()
    )
    expect_null(plan$error, info = record$id)
    expect_identical(
      plan$items[[1L]]$viewer_page_expectations$visible_conditional,
      record$expected_pages,
      info = record$id
    )
    stage <- file.path(root, paste0(record$id, "-stage"))
    dir.create(stage)
    result <- builder_execute_plan(
      plan,
      stage,
      stats::setNames(list(snapshot), record$id)
    )
    expect_identical(
      result$state,
      "success",
      info = paste(record$id, result$error %||% "")
    )
    expect_true(result$publishable, info = record$id)
    expect_length(result$built, 1L)
    if (make_app) {
      expect_true(is.list(result$app_tree_identity))
      expect_true(dir.exists(result$app_dir))
      config <- readRDS(file.path(result$app_dir, "cerebro_config.rds"))
      expect_identical(
        unname(config$crb_file_to_load),
        file.path("private-data", basename(result$built[[1L]]))
      )
    }
    reopened <- qs2::qs_read(result$built[[1L]])
    expect_setequal(
      .builder_crb_visible_pages(reopened),
      record$expected_pages
    )
    if (identical(record$id, "complete_viewer_data")) {
      expect_silent(builder_e2e_validate_complete_viewer_data(
        record,
        object,
        entry$settings,
        reopened
      ))
    }
  }
})

test_that("BuildPlan pages follow the final export selections", {
  root <- withr::local_tempdir()
  record <- builder_example_catalog()$complete_viewer_data
  object <- record$make()$object
  entry <- builder_e2e_entry(record)
  entry$settings$groups <- c("patient_id", "cluster")
  entry$settings$included_groups <- c("patient_id", "cluster")
  entry$settings$default_group <- "cluster"
  entry$settings$reductions <- "umap"
  entry$settings$included_projections <- "umap"
  entry$settings$default_projection <- "umap"
  entry$settings$initial_projections <- "umap"
  entry$settings$included_trajectories <- list()
  entry$settings$default_trajectory <- NULL
  snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot

  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  expected_pages <- setdiff(
    record$expected_pages,
    c("most_expressed_genes", "enriched_pathways", "trajectory")
  )

  expect_null(plan$error)
  expect_setequal(
    plan$items[[1L]]$viewer_page_expectations$visible_conditional,
    expected_pages
  )
})

test_that("valid Seurat content blockers stop before staging or publication", {
  root <- withr::local_tempdir()
  release <- file.path(root, "invalid-release")
  dir.create(release)
  sentinel <- file.path(release, "keep.txt")
  writeLines("unchanged", sentinel)
  prior <- builder_release_identity(release)
  control <- builder_release_control_path(release)
  expect_false(file.exists(control))

  invalid <- builder_e2e_invalid_content_entry()
  expect_s4_class(invalid$object, "Seurat")
  expect_s3_class(invalid$inspected$profile, "builder_dataset_profile")
  expect_type(invalid$entry$settings$recommendations, "list")
  review <- builder_dataset_state(invalid$entry)
  expect_identical(review$readiness, "blocked")
  expect_true("immune_repertoire" %in% review$blocking_ids)

  blocked <- builder_freeze_plan(
    list(invalid$entry),
    release,
    make_app = FALSE
  )
  expect_s3_class(blocked, "builder_plan_failure")
  expect_identical(blocked$error_code, "blocking_capability")
  stage <- file.path(root, "invalid-stage")
  expect_error(
    builder_coordinator_prepare(blocked, "invalid-content-build"),
    "frozen plan|output release|inert contract-v1 BuildPlan"
  )
  expect_error(
    builder_execute_plan(blocked, stage, list()),
    "frozen BuildPlan"
  )
  guarded_stage <- file.path(root, "guarded-stage")
  dir.create(guarded_stage)
  stage_sentinel <- file.path(guarded_stage, "keep.txt")
  writeLines("stage unchanged", stage_sentinel)
  stage_before <- tools::md5sum(stage_sentinel)
  expect_error(
    builder_execute_plan(blocked, guarded_stage, list()),
    "frozen BuildPlan"
  )

  expect_false(file.exists(stage))
  expect_identical(tools::md5sum(stage_sentinel), stage_before)
  expect_identical(list.files(guarded_stage), "keep.txt")
  expect_identical(builder_release_identity(release), prior)
  expect_identical(readLines(sentinel), "unchanged")
  expect_false(file.exists(control))
})

test_that("the 10 valid external-image artifact combinations build and relocate", {
  root <- withr::local_tempdir()
  hermetic_library <- privacy_hermetic_library(root)
  expect_true(
    !is.null(hermetic_library) && dir.exists(hermetic_library),
    info = "generated apps require a package-free test library"
  )
  runtime_results <- list()
  browser_available <- builder_e2e_browser_available()
  matrix <- expand.grid(
    backend = c("embedded", "h5"),
    content = c("plain", "histology", "trekker"),
    output = c("crb_only", "generated_app"),
    stringsAsFactors = FALSE
  )
  matrix <- matrix[
    !(matrix$content == "histology" & matrix$output == "crb_only"),
    ,
    drop = FALSE
  ]
  expect_identical(nrow(matrix), 10L)
  expect_identical(anyDuplicated(matrix), 0L)

  records <- builder_example_catalog()
  content_names <- c("plain", "histology", "trekker")
  fixtures <- lapply(content_names, function(content) {
    object <- records$complete_viewer_data$make()$object
    object@misc[c(
      "gene_lists",
      "marker_genes",
      "most_expressed_genes",
      "mean_expression",
      "enriched_pathways",
      "trajectories",
      "extra_material",
      "immune_repertoire",
      "hla_typing",
      "hla_typing_source_type"
    )] <- NULL
    if (content %in% c("plain", "trekker")) {
      for (section in SeuratObject::Images(object)) {
        object[[section]] <- NULL
      }
    }
    if (content %in% c("plain", "histology")) {
      object@misc$trekker <- NULL
    }
    record <- list(
      id = paste0("matrix_", content),
      label = paste("Matrix", content),
      make = function() list(object = object, format = "Built-in example")
    )
    entry <- builder_e2e_entry(record)
    snapshot <- builder_snapshot_seurat(
      object,
      file.path(root, paste0(content, "-snapshot")),
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    if (identical(content, "histology")) {
      image_records <- records$complete_viewer_data$histology_images
      image_sections <- vapply(
        image_records,
        function(image) image$fov_ids[[1L]],
        character(1)
      )
      default_images <- !duplicated(image_sections)
      image_sections <- image_sections[default_images]
      image_names <- vapply(
        image_records[default_images],
        `[[`,
        character(1),
        "path"
      )
      entry$settings$images <- stats::setNames(
        lapply(seq_along(image_sections), function(i) {
          image <- file.path(
            dirname(records$complete_viewer_data$serialized_path),
            image_names[[i]]
          )
          coordinates <- SeuratObject::GetTissueCoordinates(
            object[[image_sections[[i]]]]
          )
          inspected <- builder_read_image(image)
          record <- builder_alignment_record(
            source = list(
              name = basename(image),
              type = inspected$mime,
              size = inspected$bytes
            ),
            base_bounds = list(
              xmin = min(coordinates$x),
              xmax = max(coordinates$x),
              ymin = min(coordinates$y),
              ymax = max(coordinates$y)
            ),
            section = list(id = image_sections[[i]], kind = "spatial"),
            source_path = inspected$source_path
          )
          record$source_content_md5 <- inspected$source_content_md5
          record
        }),
        image_sections
      )
    }
    list(
      entry = entry,
      snapshot = snapshot,
      sections = SeuratObject::Images(object),
      gene = rownames(object)[[1L]]
    )
  })
  names(fixtures) <- content_names

  for (index in seq_len(nrow(matrix))) {
    coordinate <- matrix[index, , drop = FALSE]
    label <- paste(coordinate, collapse = "/")
    fixture <- fixtures[[coordinate$content]]
    entry <- unserialize(serialize(fixture$entry, NULL))
    entry$id <- paste0("matrix-", index)
    entry$settings$name <- paste("Matrix", index)
    entry$settings$expression_backend <- coordinate$backend
    make_app <- identical(coordinate$output, "generated_app")
    if (identical(coordinate$content, "histology")) {
      entry$settings$spatial_image_storage <- "external"
    }
    release <- file.path(root, paste0("release-", index))
    plan <- builder_freeze_plan(
      list(entry),
      release,
      make_app = make_app,
      app_options = if (make_app) list(show_upload_ui = FALSE) else list()
    )
    expect_null(plan$error, info = label)
    handle <- builder_coordinator_prepare(plan, paste0("matrix-build-", index))
    result <- builder_execute_plan(
      plan,
      handle$stage,
      stats::setNames(list(fixture$snapshot), entry$id)
    )
    result$build_id <- handle$build_id
    expect_identical(
      result$state,
      "success",
      info = paste(label, result$error %||% "")
    )
    expect_true(result$publishable, info = label)
    published <- builder_coordinator_publish(handle, result)
    expect_true(published$published, info = label)
    expect_true(
      all(startsWith(
        normalizePath(published$built, winslash = "/"),
        paste0(normalizePath(release, winslash = "/"), "/")
      )),
      info = label
    )

    crb <- published$built[[1L]]
    reopened <- qs2::qs_read(crb)
    descriptor <- reopened$getExpressionBackend()
    expect_identical(descriptor$type, coordinate$backend, info = label)
    if (identical(coordinate$backend, "embedded")) {
      expect_true(length(reopened$getExpressionMatrix()) > 0L, info = label)
    } else {
      sidecar <- file.path(dirname(crb), descriptor$location)
      expect_true(file.exists(sidecar) && !dir.exists(sidecar), info = label)
      lazy <- DelayedArray::t(
        HDF5Array::TENxMatrix(sidecar, group = "expression")
      )
      expect_true(length(lazy) > 0L, info = label)
    }
    if (identical(coordinate$content, "histology")) {
      spatial <- reopened$spatial
      expect_identical(names(spatial), fixture$sections, info = label)
      expect_true(
        all(names(entry$settings$images) %in% names(spatial)),
        info = label
      )
      for (section in names(entry$settings$images)) {
        expected <- entry$settings$images[[section]]
        expect_length(spatial[[section]]$histology_images, 0L)
        expect_identical(
          spatial[[section]]$histology_alignment,
          builder_alignment_payload(expected),
          info = section
        )
      }
      points_only <- setdiff(names(spatial), names(entry$settings$images))
      expect_setequal(
        points_only,
        c("section_b_2_fov_1", "section_b_3_fov_1", "section_c_1_fov_1")
      )
      for (section in points_only) {
        expect_length(spatial[[section]]$histology_images, 0L)
      }
    }
    if (identical(coordinate$content, "trekker")) {
      clusters <- reopened$getTrekker()$clusters
      expect_type(clusters, "integer")
      expect_gte(min(clusters), 0L)
    }
    if (make_app) {
      expect_true(dir.exists(published$app_dir), info = label)
      expect_true(
        file.exists(file.path(published$app_dir, "app.R")),
        info = label
      )
      expect_true(
        file.exists(file.path(published$app_dir, "cerebro_config.rds")),
        info = label
      )
      runtime_results[[label]] <- builder_e2e_run_generated_app(
        published$app_dir,
        hermetic_library,
        root,
        backend = coordinate$backend,
        content = coordinate$content,
        expected_images = entry$settings$images,
        label = label,
        gene = fixture$gene,
        browser_available = browser_available
      )
    } else {
      expect_null(published$app_dir, info = label)
      expect_false(dir.exists(file.path(release, "cerebro_app")), info = label)
    }
  }
  expect_length(runtime_results, 6L)
  expect_true(all(vapply(
    runtime_results,
    function(result) isTRUE(result$started),
    logical(1)
  )))
  browser_checked <- vapply(
    runtime_results,
    function(result) isTRUE(result$browser_checked),
    logical(1)
  )
  if (browser_available) {
    expect_true(all(browser_checked))
  } else {
    expect_false(any(browser_checked))
  }
})

test_that("default settings build a source with its own nUMI and nGene columns", {
  ## pbmc_SCE.rds keeps nUMI/nGene in colData; its Seurat conversion adds
  ## nCount_RNA/nFeature_RNA, which the defaults choose. The CRB's nUMI/nGene
  ## must hold the chosen columns, and the plan must expect exactly the
  ## exported metadata.
  root <- withr::local_tempdir()
  path <- builder_test_inst_path("extdata", "examples", "pbmc_SCE.rds")
  inspected <- builder_adapter_inspect(builder_seurat_file_adapter(path))
  object <- inspected$object
  expect_true(all(
    c("nUMI", "nGene", "nCount_RNA", "nFeature_RNA") %in%
      colnames(object@meta.data)
  ))
  ## As the import handler (server/imports.R) sets a new dataset up.
  settings <- builder_default_settings(
    inspected$legacy_profile,
    "PBMC SCE",
    dataset_profile = inspected$profile
  )
  metadata <- builder_recommend_metadata(inspected$profile)
  settings$recommendations <- list(metadata = metadata)
  settings$metadata_policy <- metadata
  settings$expression_backend <- "embedded"
  expect_identical(
    c(settings$nUMI, settings$nGene),
    c("nCount_RNA", "nFeature_RNA")
  )
  snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  entry <- list(
    id = "pbmc_sce",
    source_id = "pbmc_sce",
    output_id = "pbmc_sce",
    path = path,
    filename = basename(path),
    source_origin = "upload",
    format = inspected$format,
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    snapshot = snapshot,
    revision = 1L,
    levels = inspected$levels %||% list(),
    settings = settings
  )
  plan <- builder_freeze_plan(list(entry), file.path(root, "release"))
  expect_null(plan$error)
  stage <- file.path(root, "stage")
  dir.create(stage)
  result <- builder_execute_plan(plan, stage, list(pbmc_sce = snapshot))
  expect_identical(result$state, "success", info = result$error %||% "")
  meta <- qs2::qs_read(result$built[[1L]])$getMetaData()
  expect_false(any(c("nUMI.1", "nGene.1") %in% colnames(meta)))
  expect_equal(meta$nUMI, object@meta.data[meta$cell_barcode, "nCount_RNA"])
  expect_equal(meta$nGene, object@meta.data[meta$cell_barcode, "nFeature_RNA"])
})
