## A dgCMatrix read from a .crb has no dim(), dimnames() or `[` until Matrix is
## loaded, and the first such call in a session returns NULL or fails even
## though R loads Matrix during it. The Viewer loads Matrix only when a page
## needs it, so each getter must work as the first call of a fresh session.
## Inside the test process Matrix is usually loaded already, hence callr.

first_call_in_fresh_session <- function(getter) {
  callr::r(
    function(lib_paths, crb, getter) {
      .libPaths(lib_paths)
      object <- CerebroNexus::readCerebro(crb)
      list(
        matrix_loaded_by_read = "Matrix" %in% loadedNamespaces(),
        value = getter(object)
      )
    },
    args = list(
      .libPaths(),
      system.file(
        "extdata/examples/demo_spatial.crb",
        package = "CerebroNexus"
      ),
      getter
    )
  )
}

test_that("expression getters work as the first call in a fresh session", {
  skip_if_not_installed("callr")
  skip_on_cran()

  getters <- list(
    gene_names = function(object) length(object$getGeneNames()),
    cell_names = function(object) length(object$getCellNames()),
    expression_row = function(object) {
      ## The slot is readable without Matrix; rownames() is not.
      length(object$getExpressionRow(object$expression@Dimnames[[1]][1]))
    },
    spatial_rows = function(object) {
      spatial <- object$getSpatialData(object$availableSpatial()[1])
      nrow(spatial$expression)
    },
    ## The Viewer's catalog reads the stored object as it is, with the methods
    ## of the version that saved it, before its first session.
    catalog_genes = function(object) {
      path <- system.file(
        "extdata/examples/demo_spatial.crb",
        package = "CerebroNexus"
      )
      payload <- CerebroNexus:::.readCerebroPayload(path)
      info <- CerebroNexus:::.readBundleDatasetInfo(payload, "demo", path)
      info$overview$genes
    }
  )
  for (name in names(getters)) {
    result <- first_call_in_fresh_session(getters[[name]])
    ## readCerebro() leaves Matrix to the first getter that needs it.
    expect_false(result$matrix_loaded_by_read, label = name)
    expect_true(
      is.numeric(result$value) && result$value > 0L,
      label = name
    )
  }
})
