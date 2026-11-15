test_that("a group filter chosen for the previous data set does not empty the next one", {
  metadata <- data.frame(
    cell_barcode = paste0("c", 1:6),
    sample = factor(rep(c("sample_1", "sample_2", "sample_3"), 2)),
    cell_type = factor(rep(c("B cells", "T cells"), 3)),
    stringsAsFactors = FALSE
  )
  server <- function(input, output, session) {
    viewer_runtime_env(environment())
    getMetaData <- function() metadata
    getGroups <- function() c("sample", "cell_type")
    getGroupLevels <- function(group) levels(metadata[[group]])
  }
  shiny::testServer(server, {
    ## What the server reads right after switching back from another data
    ## set: its filters are still bound, with that data set's levels.
    session$setInputs(
      overview_projection_group_filter_sample = c("donor1", "donor2"),
      overview_projection_group_filter_cell_type = "CD8 T"
    )
    filters <- viewerProjectionGroupFilters("overview_projection", metadata)
    expect_length(filters, 0)
    expect_identical(
      viewerProjectionCellIndices(
        "overview_projection",
        metadata = metadata,
        percentage = 100,
        filters = filters
      ),
      1:6
    )

    ## Once the controls are drawn for this data set, they filter again.
    session$setInputs(
      overview_projection_group_filter_sample = levels(metadata$sample),
      overview_projection_group_filter_cell_type = "T cells"
    )
    filters <- viewerProjectionGroupFilters("overview_projection", metadata)
    expect_identical(names(filters), "cell_type")
    expect_identical(
      viewerProjectionCellIndices(
        "overview_projection",
        metadata = metadata,
        percentage = 100,
        filters = filters
      ),
      c(2L, 4L, 6L)
    )
  })
})

test_that("a level unknown to the group marks the filter as stale", {
  getGroupLevels <- function(group) c("a", "b")
  env <- viewer_runtime_env()
  environment(env$viewerGroupFilterStale) <- environment()
  stale <- env$viewerGroupFilterStale
  expect_false(stale(NULL, "g"))
  expect_false(stale(character(), "g"))
  expect_false(stale("a", "g"))
  expect_true(stale(c("a", "z"), "g"))
})
