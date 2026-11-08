## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

## A precomputed Marker import, saved the way the Enhance dialog saves it,
## is frozen, built and written into the CRB.
builder_e2e_source_runtime(environment())

test_that("a saved Marker import builds into the CRB", {
  skip_if_not_installed("SeuratObject")
  object <- .builder_test_with_seed(11L, .builder_fixture_object(24L))
  record <- list(
    id = "marker_import",
    label = "Marker import",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  expect_false(
    "marker_genes" %in%
      builder_dataset_state(entry)$page_expectations$visible_conditional
  )

  root <- withr::local_tempdir()
  csv <- file.path(root, "markers.csv")
  writeLines(
    c(
      "cell_type,gene,p_val",
      "Neuron,Gene1,0.001",
      "Astrocyte,Gene2,0.002",
      "Microglia,Gene3,0.003"
    ),
    csv
  )
  ## Enhance -> Marker genes -> upload -> Confirm -> Save
  levels <- as.character(entry$levels$cell_type)
  draft <- builder_marker_import_new_draft(
    "marker-import-1",
    "Uploaded",
    "cell_type",
    builder_marker_import_inventory(csv),
    levels
  )
  draft <- builder_marker_import_confirm_source(
    draft,
    draft$sources[[1L]]$id,
    "multiple",
    "cell_type"
  )
  expect_true(isTRUE(draft$validation$ready))
  draft$ready <- TRUE
  saved <- builder_freeze_marker_imports(list(draft))[[1L]]
  entry$settings$marker_imports <- stats::setNames(list(saved), saved$id)
  entry$revision <- 1L

  ## Review already lists the Marker genes page.
  expect_true(
    "marker_genes" %in%
      builder_dataset_state(entry)$page_expectations$visible_conditional
  )

  entry$snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  expect_null(plan$error)
  expect_true(
    "marker_genes" %in%
      plan$items[[1L]]$viewer_page_expectations$visible_conditional
  )
  handle <- builder_coordinator_prepare(plan, entry$id)
  result <- builder_execute_plan(
    plan,
    handle$stage,
    stats::setNames(list(entry$snapshot), entry$id)
  )
  expect_identical(result$state, "success", info = format(result$error))
  result$build_id <- handle$build_id
  published <- builder_coordinator_publish(handle, result)
  expect_true(isTRUE(published$published))

  crb <- readCerebro(published$built[[1L]])
  expect_true("Uploaded" %in% crb$getMethodsForMarkerGenes())
  markers <- crb$getMarkerGenes("Uploaded", "cell_type")
  expect_setequal(as.character(markers$gene), c("Gene1", "Gene2", "Gene3"))
})
