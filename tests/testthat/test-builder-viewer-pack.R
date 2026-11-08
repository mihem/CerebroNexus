## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

builder_e2e_source_runtime()

## A dataset over saveCerebro()'s Viewer Pack threshold (100,000 cells) gets a
## `<stem>.viewer` directory next to its CRB, and an H5-backed one over the
## immune sidecar threshold (100,000 repertoire rows) a `<stem>.immune.qs2`
## file. The small fixture takes both paths by mocking the two decisions; the
## build, verification and publication then run unchanged in this process.
test_that("Viewer Packs and immune sidecars publish with their CRB", {
  testthat::local_mocked_bindings(
    .viewerPackEnabled = function(...) TRUE,
    .immuneRepertoireRows = function(...) 1e9,
    .package = "CerebroNexus"
  )
  root <- withr::local_tempdir()
  hermetic_library <- privacy_hermetic_library(root)
  browser_available <- builder_e2e_browser_available()
  object <- builder_example_catalog()$complete_viewer_data$make()$object
  object@misc[c(
    "gene_lists",
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectories",
    "extra_material",
    "hla_typing",
    "hla_typing_source_type",
    "trekker"
  )] <- NULL
  for (section in SeuratObject::Images(object)) {
    object[[section]] <- NULL
  }
  expect_true(length(object@misc$immune_repertoire) > 0L)
  record <- list(
    id = "viewer_pack",
    label = "Viewer pack",
    make = function() list(object = object, format = "Built-in example")
  )
  fixture_entry <- builder_e2e_entry(record)
  snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  fixture_entry$snapshot <- snapshot
  cases <- data.frame(
    backend = c("embedded", "embedded", "h5", "h5"),
    output = c("crb_only", "generated_app", "crb_only", "generated_app"),
    stringsAsFactors = FALSE
  )
  runtime_results <- list()
  for (index in seq_len(nrow(cases))) {
    case <- cases[index, , drop = FALSE]
    label <- paste(case, collapse = "/")
    make_app <- identical(case$output, "generated_app")
    entry <- unserialize(serialize(fixture_entry, NULL))
    entry$id <- paste0("pack-", index)
    entry$settings$name <- paste("Pack", index)
    entry$settings$expression_backend <- case$backend
    release <- file.path(root, paste0("release-", index))
    plan <- builder_freeze_plan(
      list(entry),
      release,
      make_app = make_app,
      app_options = if (make_app) list(show_upload_ui = FALSE) else list()
    )
    expect_null(plan$error, info = label)
    handle <- builder_coordinator_prepare(plan, paste0("pack-build-", index))
    result <- builder_execute_plan(
      plan,
      handle$stage,
      stats::setNames(list(snapshot), entry$id)
    )
    result$build_id <- handle$build_id
    expect_identical(
      result$state,
      "success",
      info = paste(label, result$error %||% "")
    )
    stem <- tools::file_path_sans_ext(plan$items[[1L]]$filename)
    immune_name <- paste0(stem, ".immune.qs2")
    expect_true(
      dir.exists(file.path(handle$stage, paste0(stem, ".viewer"))),
      info = label
    )
    expect_identical(
      file.exists(file.path(handle$stage, immune_name)),
      identical(case$backend, "h5"),
      info = label
    )
    published <- tryCatch(
      builder_coordinator_publish(handle, result),
      error = function(error) {
        try(builder_coordinator_abort(handle), silent = TRUE)
        list(published = FALSE, error = conditionMessage(error))
      }
    )
    expect_true(
      isTRUE(published$published),
      info = paste(label, published$error)
    )
    if (!isTRUE(published$published)) {
      next
    }

    crb <- published$built[[1L]]
    data_dir <- if (make_app) {
      file.path(release, "cerebro_app", "private-data")
    } else {
      release
    }
    expect_identical(
      normalizePath(dirname(crb), winslash = "/"),
      normalizePath(data_dir, winslash = "/"),
      info = label
    )
    pack <- file.path(data_dir, paste0(stem, ".viewer"))
    expect_true(dir.exists(pack), info = label)
    expect_true(
      isTRUE(CerebroNexus::validateViewerPack(crb, pack, full = TRUE)$valid),
      info = label
    )
    expect_identical(
      file.exists(file.path(data_dir, immune_name)),
      identical(case$backend, "h5"),
      info = label
    )
    reopened <- CerebroNexus::readCerebro(crb)
    expect_identical(
      reopened$getExpressionBackend()$type,
      case$backend,
      info = label
    )
    expect_true(
      sum(vapply(reopened$getImmuneRepertoire(), NROW, integer(1))) > 0L,
      info = label
    )
    if (make_app) {
      runtime_results[[label]] <- builder_e2e_run_generated_app(
        published$app_dir,
        hermetic_library,
        root,
        backend = case$backend,
        content = "plain",
        expected_images = list(),
        label = paste0("pack-", index),
        gene = rownames(object)[[1L]],
        browser_available = browser_available
      )
    } else {
      expect_identical(
        sort(list.files(release)),
        sort(c(
          basename(crb),
          paste0(stem, ".viewer"),
          plan$items[[1L]]$sidecars,
          if (identical(case$backend, "h5")) immune_name,
          "build-report.json"
        )),
        info = label
      )
    }
  }
  expect_length(runtime_results, 2L)
  expect_true(all(vapply(
    runtime_results,
    function(result) isTRUE(result$started),
    logical(1)
  )))
  if (browser_available) {
    expect_true(all(vapply(
      runtime_results,
      function(result) isTRUE(result$browser_checked),
      logical(1)
    )))
  }
})
