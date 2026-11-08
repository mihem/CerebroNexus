## One "Group by" control serves every Immune repertoire tab. Compare (and
## Paired Scatter) use the groups as the units they compare, so a grouping chosen
## on Clone Sharing must not become Compare's: its sample choices would be that
## column's levels.

ir_groupby_app <- function() {
  expressions <- viewer_server_statements("immune_repertoire", "settings.R")
  function(input, output, session) {
    reactiveVal <- shiny::reactiveVal
    observeEvent <- shiny::observeEvent
    isolate <- shiny::isolate
    viewer_server_load(
      expressions,
      environment(),
      names = c(
        "ir_groupby_kind",
        "ir_groupby_memory",
        "ir_remembered_groupby"
      ),
      events = "ir_groupBy"
    )
  }
}

test_that("Compare and Paired Scatter are one kind of tab, the others another", {
  shiny::testServer(ir_groupby_app(), {
    expect_identical(ir_groupby_kind("Compare"), "compare")
    expect_identical(ir_groupby_kind("Paired Scatter"), "compare")
    expect_identical(ir_groupby_kind("Clone Sharing"), "results")
    expect_identical(ir_groupby_kind("Diversity"), "results")
    expect_identical(ir_groupby_kind(NULL), "results")
  })
})

test_that("a grouping chosen for Clone Sharing does not carry over to Compare", {
  shiny::testServer(ir_groupby_app(), {
    session$setInputs(ir_tabs = "Clone Sharing", ir_groupBy = "tissue_site")
    expect_identical(ir_remembered_groupby("Clone Sharing"), "tissue_site")
    expect_identical(ir_remembered_groupby("Compare"), "")
    expect_identical(ir_remembered_groupby("Paired Scatter"), "")

    ## The toolbar is rebuilt for Compare with its own (empty) grouping.
    session$setInputs(ir_tabs = "Compare", ir_groupBy = "")
    expect_identical(ir_remembered_groupby("Compare"), "")
    expect_identical(ir_remembered_groupby("Clone Sharing"), "tissue_site")

    session$setInputs(ir_groupBy = "patient")
    expect_identical(ir_remembered_groupby("Compare"), "patient")
    expect_identical(ir_remembered_groupby("Paired Scatter"), "patient")
    expect_identical(ir_remembered_groupby("Clone Sharing"), "tissue_site")

    ## Back on a results tab each kind finds its own choice again.
    session$setInputs(ir_tabs = "Diversity", ir_groupBy = "tissue_site")
    expect_identical(ir_remembered_groupby("Diversity"), "tissue_site")
    expect_identical(ir_remembered_groupby("Compare"), "patient")
  })
})

test_that("a tab without the control leaves the remembered groupings alone", {
  shiny::testServer(ir_groupby_app(), {
    session$setInputs(ir_tabs = "Diversity", ir_groupBy = "patient")
    session$setInputs(ir_tabs = "Clonal UMAP", ir_groupBy = NULL)
    expect_identical(ir_remembered_groupby("Diversity"), "patient")
  })
})

test_that("the toolbar takes the Group by it rebuilds with from the memory of its kind", {
  source <- paste(
    readLines(viewer_test_path("immune_repertoire", "settings.R")),
    collapse = "\n"
  )
  expect_match(source, "prev_gb <- ir_remembered_groupby(tab)", fixed = TRUE)
  ## The control's own current value belongs to the tab it was built for.
  expect_false(grepl(
    "prev_gb <- isolate(input$ir_groupBy)",
    source,
    fixed = TRUE
  ))
})
