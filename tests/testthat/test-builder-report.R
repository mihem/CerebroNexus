builder_repo_source("core/bundle_path_contract.R")
builder_repo_source("publish.R")
builder_repo_source("report.R")

builder_report_fixture <- function(
  stage = withr::local_tempdir(.local_envir = parent.frame())
) {
  built <- file.path(stage, "dataset-a.crb")
  saveRDS(list(valid = TRUE), built)
  plan <- structure(
    list(
      revision = 12L,
      readiness = "ready",
      out_dir = "/Users/private/source/release",
      make_app = FALSE,
      dataset_order = "dataset-a",
      items = list(list(
        id = "dataset-a",
        name = "Dataset A",
        filename = "dataset-a.crb",
        organism = "hg",
        analyses = c("marker_genes"),
        included_groups = c("cluster"),
        included_projections = c("umap"),
        metadata_policy = list(
          retained = c("cell_barcode", "cluster"),
          excluded = "secret_note",
          forced = "cell_barcode"
        ),
        expression_backend = "embedded",
        spatial_image_storage = "external",
        spatial_alignment = list(
          section_count = 6L,
          image_count = 7L,
          saved_count = 7L,
          points_only = character()
        ),
        sidecars = character(),
        manifest = list(marker_genes = list(status = "valid"))
      )),
      manifest = list(marker_genes = list(status = "valid")),
      acknowledgements = list("Filtered orphan repertoire rows"),
      viewer_bundle_assets = character(),
      private_assets = "dataset-a.crb",
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      ),
      output_release = list(
        directory = "/Users/private/source/release",
        targets = "/Users/private/source/release/dataset-a.crb"
      )
    ),
    class = c("builder_build_plan", "list")
  )
  result <- list(
    state = "success",
    publishable = TRUE,
    stage = stage,
    built = stats::setNames(built, "Dataset A"),
    labels = "Dataset A",
    verifications = list(
      dataset.a = list(
        valid = TRUE,
        path = built,
        cells = c("private-cell-a", "private-cell-b"),
        features = c("private-gene-a", "private-gene-b"),
        groups = "cluster",
        projections = "umap",
        metadata = c("cell_barcode", "cluster")
      )
    ),
    app_dir = NULL,
    app_verification = NULL
  )
  list(stage = stage, plan = plan, result = result)
}

test_that("portable reports derive redacted identity from plan and verification", {
  fixture <- builder_report_fixture()
  report <- builder_build_report(fixture$plan, fixture$result)

  expect_s3_class(report, "builder_build_report")
  expect_identical(report$schema_version, 1L)
  expect_identical(report$plan_revision, 12L)
  expect_identical(report$artifact_mode, "crbs_only")
  expect_identical(report$dataset_order, "dataset-a")
  expect_identical(report$datasets[[1L]]$methods, "marker_genes")
  expect_identical(
    report$datasets[[1L]]$metadata_columns,
    c("cell_barcode", "cluster")
  )
  expect_identical(
    report$datasets[[1L]]$metadata,
    list(
      retained = c("cell_barcode", "cluster"),
      excluded = "secret_note",
      forced = "cell_barcode"
    )
  )
  expect_identical(
    report$datasets[[1L]]$expression_storage,
    list(mode = "embedded")
  )
  expect_identical(
    report$datasets[[1L]]$spatial_image_storage,
    list(mode = "external", image_count = 7L, section_count = 6L)
  )
  expect_identical(
    report$output_members,
    c("build-report.json", "dataset-a.crb")
  )
  expect_match(report$identity, "^[0-9a-f]{32}$")

  serialized <- jsonlite::toJSON(report, auto_unbox = TRUE, null = "null")
  expect_false(grepl(fixture$stage, serialized, fixed = TRUE))
  expect_false(grepl("/Users/private/source", serialized, fixed = TRUE))
  expect_false(grepl("private-cell|private-gene", serialized))
  expect_false(grepl(
    "host|pid|lock|backup|diagnostic",
    serialized,
    ignore.case = TRUE
  ))
})

test_that("portable report reader accepts the strict legacy private-App topology", {
  fixture <- builder_report_fixture()
  report <- builder_build_report(fixture$plan, fixture$result)
  report$artifact_mode <- "crbs_and_private_app"
  report$output_members <- sort(
    c(
      report$output_members,
      "cerebro_app"
    ),
    method = "radix"
  )
  report$identity <- .builder_report_identity(report)
  expect_silent(.builder_report_validate(report, require_class = TRUE))
  report$output_members <- sort(
    c(
      report$output_members,
      "cerebro_app/viewer-auth.env"
    ),
    method = "radix"
  )
  report$identity <- .builder_report_identity(report)
  expect_error(.builder_report_validate(report, require_class = TRUE), "schema")
})

test_that("report JSON writes atomically and rereads exact schema identity", {
  fixture <- builder_report_fixture()
  report <- builder_build_report(fixture$plan, fixture$result)
  path <- builder_write_build_report(fixture$stage, report)
  reread <- builder_read_build_report(path)

  expect_identical(
    path,
    file.path(
      normalizePath(fixture$stage, winslash = "/", mustWork = TRUE),
      "build-report.json"
    )
  )
  expect_true(file.exists(path))
  expect_identical(reread$identity, report$identity)
  expect_identical(reread$plan_revision, 12L)
  expect_false(file.exists(paste0(path, ".tmp")))
})

test_that("not-applicable manifest dispositions round-trip as portable null", {
  fixture <- builder_report_fixture()
  fixture$plan$manifest$marker_genes <- list(
    status = "not_applicable",
    disposition = NA_character_,
    pages = character()
  )

  report <- builder_build_report(fixture$plan, fixture$result)
  expect_null(report$content$marker_genes$disposition)
  expect_identical(report$identity, .builder_report_identity(report))

  path <- builder_write_build_report(fixture$stage, report)
  reread <- builder_read_build_report(path)
  expect_null(reread$content$marker_genes$disposition)
  expect_identical(reread$identity, report$identity)

  for (invalid in list(1L, list("preserved"), c("a", "b"))) {
    malformed <- builder_report_fixture()
    malformed$plan$manifest$marker_genes$disposition <- invalid
    expect_error(
      builder_build_report(malformed$plan, malformed$result),
      "schema"
    )
  }
})

test_that("reports with no documented warnings retain identity", {
  fixture <- builder_report_fixture()
  fixture$plan$acknowledgements <- list()
  report <- builder_build_report(fixture$plan, fixture$result)

  expect_identical(report$warnings, character())
  path <- builder_write_build_report(fixture$stage, report)
  expect_identical(builder_read_build_report(path)$identity, report$identity)
})

test_that("artifact-visible labels may use provenance-like words", {
  fixture <- builder_report_fixture()
  report <- builder_build_report(fixture$plan, fixture$result)
  report$datasets[[1L]]$name <- "Host response atlas"
  report$datasets[[1L]]$methods <- c("Source cohort", "Raw values QC")
  report$datasets[[1L]]$metadata_columns <- c(
    "host_response",
    "source_batch"
  )
  report$identity <- .builder_report_identity(report)

  expect_silent(.builder_report_validate(report, require_class = TRUE))
  path <- builder_write_build_report(fixture$stage, report)
  expect_true(file.exists(path))
})

test_that("sensitive structural keys fail closed at every depth", {
  fixture <- builder_report_fixture()
  base <- builder_build_report(fixture$plan, fixture$result)
  cases <- list(
    diagnostic = function(report) {
      report$datasets[[1L]]$diagnostic <- "already redacted"
      report
    },
    private_diagnostic = function(report) {
      report$content$private_diagnostic <- 1L
      report
    },
    host = function(report) {
      report$content$nested <- list(host = 12345L)
      report
    },
    pid = function(report) {
      report$datasets[[1L]]$nested <- list(pid = 12345L)
      report
    },
    source = function(report) {
      report$content$source <- TRUE
      report
    },
    raw_value = function(report) {
      report$content$one <- list(two = list(three = list(raw_value = 42)))
      report
    }
  )

  for (name in names(cases)) {
    leaked <- cases[[name]](base)
    leaked$identity <- .builder_report_identity(leaked)
    expect_error(
      .builder_report_validate(leaked, require_class = TRUE),
      "schema|redact|sensitive",
      info = name
    )
  }
})

test_that("report v1 rejects malformed nested schemas", {
  fixture <- builder_report_fixture()
  base <- builder_build_report(fixture$plan, fixture$result)
  malformed <- list(
    dataset_order_type = function(report) {
      report$dataset_order <- 1L
      report
    },
    datasets_type = function(report) {
      report$datasets <- "dataset-a"
      report
    },
    dataset_unknown_key = function(report) {
      report$datasets[[1L]]$unexpected <- TRUE
      report
    },
    dataset_missing_key = function(report) {
      report$datasets[[1L]]$methods <- NULL
      report
    },
    content_unknown_key = function(report) {
      report$content[[1L]]$unexpected <- "value"
      report
    },
    vector_type = function(report) {
      report$viewer_bundle_assets <- list("asset.png")
      report
    },
    count_range = function(report) {
      report$datasets[[1L]]$cell_count <- -1
      report
    },
    dataset_order_mismatch = function(report) {
      report$datasets[[1L]]$id <- "dataset-b"
      report
    }
  )

  for (name in names(malformed)) {
    value <- malformed[[name]](base)
    value$identity <- .builder_report_identity(value)
    expect_error(
      .builder_report_validate(value, require_class = TRUE),
      "schema",
      info = name
    )
  }

  garbage <- unclass(base)
  garbage$datasets[[1L]]$feature_count <- 1.5
  garbage$identity <- .builder_report_identity(garbage)
  expect_error(
    builder_read_build_report(
      "ignored.json",
      .read_json = function(path) garbage
    ),
    "schema"
  )
})

test_that("report writer rejects redaction, write, parse, and identity failures", {
  fixture <- builder_report_fixture()
  report <- builder_build_report(fixture$plan, fixture$result)

  leaked <- report
  leaked$warnings <- "/Users/private/source/object.rds"
  expect_error(
    builder_write_build_report(fixture$stage, leaked),
    "portable|redact"
  )

  leaked <- report
  leaked$warnings <- paste(
    "pid=12345 source=/Users/alice/raw.rds host=secret-host"
  )
  leaked$identity <- .builder_report_identity(leaked)
  expect_error(
    builder_write_build_report(fixture$stage, leaked),
    "portable|redact"
  )

  for (secret in c(
    "source=/Users/alice/raw.rds",
    "raw_value=secret",
    "private diagnostic: donor=Alice",
    "raw value secret",
    "private diagnostic donor Alice",
    "diagnostic donor Alice",
    "  RAW   VALUE   secret  ",
    "Private\tdiagnostic\tdonor\tAlice",
    "DiAgNoStIc   donor Alice",
    "raw values secret",
    "raw_value secret",
    "private diagnostics donor Alice",
    "private_diagnostic donor Alice",
    "PRIVATE__DIAGNOSTICS   donor Alice",
    "raw-values secret",
    "raw.values secret",
    "raw—values secret",
    "host secret-host",
    "pid 12345",
    "HOST\tsecret-host",
    "PID_12345",
    "prefix[source:C:/Users/alice/raw.rds]suffix",
    "context(raw_token=secret)"
  )) {
    report_path <- file.path(fixture$stage, "build-report.json")
    unlink(report_path, force = TRUE)
    leaked <- report
    leaked$warnings <- secret
    leaked$identity <- .builder_report_identity(leaked)
    expect_error(
      builder_write_build_report(fixture$stage, leaked),
      "portable|redact",
      info = secret
    )
    unlink(report_path, force = TRUE)
  }

  expect_error(
    builder_write_build_report(
      fixture$stage,
      report,
      .move = function(from, to) FALSE
    ),
    "atomic"
  )
  expect_false(file.exists(file.path(fixture$stage, "build-report.json")))

  expect_error(
    builder_write_build_report(
      fixture$stage,
      report,
      .read_json = function(path) stop("injected parse failure")
    ),
    "parse|read"
  )

  expect_error(
    builder_write_build_report(
      fixture$stage,
      report,
      .read_json = function(path) {
        value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
        value$identity <- paste(rep("0", 32L), collapse = "")
        value
      }
    ),
    "identity|round-trip"
  )
})
