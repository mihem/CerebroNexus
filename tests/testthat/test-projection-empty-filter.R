test_that("bound empty filters hide cells while unmounted filters keep defaults", {
  shiny::testServer(
    function(input, output, session) {
      sys.source(viewer_test_path("utility_functions.R"), environment())
      metadata <- data.frame(state = factor(c("A", "B", "A")))
      getGroups <- function() "state"
      getGroupLevels <- function(group) levels(metadata[[group]])
      indices <- shiny::reactive(viewerProjectionCellIndices(
        "test",
        metadata,
        100
      ))
    },
    {
      expect_identical(indices(), 1:3)
      session$setInputs(test_group_filter_state = c("A", "B"))
      expect_identical(indices(), 1:3)
      session$setInputs(test_group_filter_state = NULL)
      expect_identical(indices(), integer())
      session$setInputs(test_group_filter_state = "A")
      expect_identical(indices(), c(1L, 3L))
      session$setInputs(test_group_filter_state = c("A", "B"))
      expect_identical(indices(), 1:3)
    }
  )
})
