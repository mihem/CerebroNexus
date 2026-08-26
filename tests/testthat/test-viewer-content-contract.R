viewer_contract_path <- function(root = c("R", "core")) {
  root <- match.arg(root)
  if (identical(root, "R")) {
    return(testthat::test_path(
      "..",
      "..",
      "R",
      "viewer_content_contract.R"
    ))
  }
  relative <- file.path(
    "viewer",
    "core",
    "viewer_content_contract.R"
  )
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

viewer_contract_source_if_present <- function(local = parent.frame()) {
  path <- viewer_contract_path("core")
  if (nzchar(path) && file.exists(path)) {
    source(path, local = local)
  }
}

viewer_contract_source_if_present()

viewer_manifest_entry_record <- function(
  id,
  source,
  status,
  disposition,
  artifact_scope,
  summary = "",
  diagnostics = list(),
  compatibility = list(),
  pages = character(),
  required_action = NULL,
  verifier = NULL
) {
  values <- .builder_manifest_validate_values(
    id = id,
    source = source,
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = summary,
    diagnostics = diagnostics,
    compatibility = compatibility,
    pages = pages,
    required_action = required_action,
    verifier = verifier
  )
  structure(values, class = c("builder_manifest_entry", "list"))
}

viewer_content_manifest <- function(entries) {
  entries <- unname(entries)
  for (entry in entries) {
    .builder_manifest_validate_entry(entry)
  }
  ids <- vapply(entries, function(entry) entry$id, character(1))
  if (anyDuplicated(ids)) {
    .builder_manifest_abort("duplicate_id", "Manifest ids must be unique.")
  }
  names(entries) <- ids
  structure(entries, class = c("builder_content_manifest", "list"))
}

viewer_manifest_entry <- function(
  id,
  page,
  status = "valid",
  disposition = "preserved",
  artifact_scope = "both",
  required_action = NULL
) {
  viewer_manifest_entry_record(
    id = id,
    source = list(type = "fixture", location = id),
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    pages = page,
    required_action = required_action
  )
}

test_that("Viewer page catalog declares the current sidebar identities", {
  catalog <- builder_viewer_page_catalog()

  expect_named(catalog, c("always", "conditional"))
  expect_identical(
    catalog$always$id,
    c(
      "data_info",
      "projection",
      "groups",
      "gene_expression",
      "gene_id_conversion",
      "color_management",
      "about"
    )
  )
  expect_identical(
    catalog$always$tab_name,
    c(
      "loadData",
      "coordinated_views",
      "groups",
      "geneExpression",
      "geneIdConversion",
      "color_management",
      "about"
    )
  )
  expect_identical(
    catalog$always$label,
    c(
      "Data info",
      "Linked views",
      "Groups",
      "Gene expression",
      "Gene ID conversion",
      "Color management",
      "About"
    )
  )
  expect_identical(
    catalog$always$icon,
    c("info", "home", "layer-group", "signal", "barcode", "palette", "at")
  )

  expect_identical(
    catalog$conditional$id,
    c(
      "marker_genes",
      "most_expressed_genes",
      "enriched_pathways",
      "extra_material",
      "immune_repertoire",
      "trajectory",
      "spatial",
      "trekker",
      "hla_tcr_motifs"
    )
  )
  expect_identical(
    catalog$conditional$tab_name,
    c(
      "markerGenes",
      "mostExpressedGenes",
      "enrichedPathways",
      "extra_material",
      "immune_repertoire",
      "trajectory",
      "coordinated_views",
      "coordinated_views",
      "hla_tcr_motifs"
    )
  )
  expect_identical(
    catalog$conditional$label,
    c(
      "Marker genes",
      "Most expressed genes",
      "Enriched pathways",
      "Extra material",
      "Immune repertoire",
      "Trajectory",
      "Spatial",
      "Trekker",
      "HLA & TCR Motifs"
    )
  )
  expect_identical(
    catalog$conditional$icon,
    c(
      "list-alt",
      "bullhorn",
      "project-diagram",
      "gift",
      "dna",
      "route",
      "map-pin",
      "map-marked-alt",
      "project-diagram"
    )
  )
})

test_that("always pages stay visible with an empty manifest", {
  catalog <- builder_viewer_page_catalog()
  pages <- builder_viewer_page_contract(viewer_content_manifest(list()))

  expect_identical(pages$always, catalog$always)
  expect_identical(pages$conditional, catalog$conditional)
  expect_identical(pages$visible_conditional, character())
  expect_identical(
    pages$hidden_conditional,
    catalog$conditional$id
  )
})

test_that("only valid app-facing usable content opens conditional pages", {
  manifest <- viewer_content_manifest(list(
    viewer_manifest_entry(
      "markers_generated",
      "marker_genes",
      disposition = "generated"
    ),
    viewer_manifest_entry(
      "most_converted",
      "most_expressed_genes",
      disposition = "converted"
    ),
    viewer_manifest_entry(
      "pathways_attached",
      "enriched_pathways",
      disposition = "attached"
    ),
    viewer_manifest_entry(
      "trajectory_preserved",
      "trajectory",
      disposition = "preserved"
    ),
    viewer_manifest_entry(
      "extra_filtered",
      "extra_material",
      disposition = "filtered"
    ),
    viewer_manifest_entry(
      "immune_stored",
      "immune_repertoire",
      disposition = "stored_only"
    ),
    viewer_manifest_entry(
      "spatial_rejected",
      "spatial",
      disposition = "rejected"
    ),
    viewer_manifest_entry(
      "trekker_attention",
      "trekker",
      status = "attention",
      disposition = "attached",
      required_action = list(type = "provide")
    ),
    viewer_manifest_entry(
      "hla_blocking",
      "hla_tcr_motifs",
      status = "blocking",
      disposition = "rejected"
    ),
    viewer_manifest_entry(
      "spatial_crb_only",
      "spatial",
      artifact_scope = "crb"
    )
  ))

  pages <- builder_viewer_page_contract(manifest)
  expect_identical(
    pages$visible_conditional,
    c(
      "marker_genes",
      "most_expressed_genes",
      "enriched_pathways",
      "trajectory"
    )
  )
  expect_identical(
    pages$hidden_conditional,
    c(
      "extra_material",
      "immune_repertoire",
      "spatial",
      "trekker",
      "hla_tcr_motifs"
    )
  )
})

test_that("manifest entries reject unknown Viewer pages with a stable code", {
  error <- tryCatch(
    viewer_manifest_entry("unknown", "not_a_viewer_page"),
    builder_manifest_error = function(error) error
  )

  expect_s3_class(error, "builder_manifest_error")
  expect_identical(error$code, "unknown_page")
})

test_that("forged manifest classes cannot open conditional pages", {
  plain_entry <- list(
    id = "plain",
    source = list(type = "fixture", location = "plain"),
    status = "valid",
    disposition = "preserved",
    artifact_scope = "both",
    summary = "Forged entry",
    diagnostics = list(),
    compatibility = list(),
    pages = "spatial",
    required_action = NULL,
    verifier = NULL
  )
  forged <- structure(
    list(plain = plain_entry),
    class = c("builder_content_manifest", "list")
  )
  error <- tryCatch(
    builder_viewer_page_contract(forged),
    builder_manifest_error = function(error) error
  )

  expect_s3_class(error, "builder_manifest_error")
  expect_identical(error$code, "invalid_entry")
})

test_that("atomic manifest entries fail with a stable typed condition", {
  forged <- structure(
    list(atomic = 1L),
    class = c("builder_content_manifest", "list")
  )
  error <- tryCatch(
    builder_viewer_page_contract(forged),
    error = function(error) error
  )

  expect_s3_class(error, "builder_manifest_error")
  expect_identical(error$code, "invalid_entry")
})

test_that("manifest names must match their typed entry ids", {
  entry <- viewer_manifest_entry("spatial_payload", "spatial")
  forged <- structure(
    list(wrong_name = entry),
    class = c("builder_content_manifest", "list")
  )
  error <- tryCatch(
    builder_viewer_page_contract(forged),
    builder_manifest_error = function(error) error
  )

  expect_s3_class(error, "builder_manifest_error")
  expect_identical(error$code, "invalid_manifest")
})

test_that("Viewer contract core is byte-identical and bundle safe", {
  source_path <- viewer_contract_path("R")
  runtime_path <- viewer_contract_path("core")
  testthat::skip_if_not(
    file.exists(source_path),
    "R source tree not present (installed-package layout)"
  )

  expect_true(nzchar(runtime_path) && file.exists(runtime_path))
  source_bytes <- readBin(
    source_path,
    what = "raw",
    n = file.info(source_path)$size
  )
  runtime_bytes <- readBin(
    runtime_path,
    what = "raw",
    n = file.info(runtime_path)$size
  )
  expect_identical(runtime_bytes, source_bytes)

  text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("CerebroNexus|cerebroAppLite", text))
  expect_false(grepl(
    "library\\s*\\(|requireNamespace\\s*\\(|getFromNamespace\\s*\\(|::",
    text,
    perl = TRUE
  ))
})
