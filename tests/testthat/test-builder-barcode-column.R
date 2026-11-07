## A metadata column named cell_barcode does not block the dataset: the CRB
## keeps the cell barcodes as cell_barcode and the source column under a
## free name.
builder_e2e_source_runtime(environment())

test_that("a source cell_barcode column is built as cell_barcode_source", {
  skip_if_not_installed("SeuratObject")
  object <- .builder_test_with_seed(13L, .builder_fixture_object(24L))
  object$cell_barcode <- paste0("BC-", seq_len(ncol(object)))
  record <- list(
    id = "barcode_column",
    label = "Barcode column",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  state <- builder_dataset_state(entry)
  expect_identical(state$readiness, "ready")
  expect_identical(
    state$metadata_policy$columns$cell_barcode$renamed_to,
    "cell_barcode_source"
  )

  root <- withr::local_tempdir()
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

  meta <- readCerebro(published$built[[1L]])$getMetaData()
  expect_identical(as.character(meta$cell_barcode), colnames(object))
  expect_identical(
    as.character(meta$cell_barcode_source),
    object$cell_barcode[as.character(meta$cell_barcode)],
    ignore_attr = TRUE
  )
})
