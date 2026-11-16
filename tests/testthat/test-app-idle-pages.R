## While the visitor reads the first page, the session prepares the pages the
## data set has (shiny_server.R, viewer_idle_tasks()): their server code is
## loaded before they are opened. Not the HLA & TCR motifs page without a
## Viewer Pack first frame (the demo data sets have no Pack), whose first view
## computes from the repertoire; the Immune repertoire page first of all, which
## has its repertoire encoded.

test_that("a quiet session loads the code of the other pages, not the motifs'", {
  skip_on_cran()
  skip_if_demo_data_missing()
  app_dir <- viewer_app_test_path()
  local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "idle_pages",
    width = 1440,
    height = 1000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 60000)

  loaded <- function() unlist(app$get_value(export = "loaded_pages"))
  expected <- c(
    "immune_repertoire/server.R",
    "overview/server.R",
    "groups/server.R",
    "gene_expression/server.R",
    "color_management/server.R",
    "about/server.R"
  )
  deadline <- Sys.time() + 90
  repeat {
    pages <- loaded()
    if (all(expected %in% pages) || Sys.time() > deadline) {
      break
    }
    Sys.sleep(1)
  }
  expect_true(all(expected %in% pages))
  expect_false("hla_tcr_motifs/server.R" %in% loaded())

  ## A page clicked but not shown (a second click came in the same turn): the
  ## outputs reported shown with its click are reported hidden again, so the
  ## server does not render them while the page stays hidden.
  app$run_js(paste0(
    "document.querySelector('a[href=\"#shiny-tab-groups\"]').click();",
    "document.querySelector('a[href=\"#shiny-tab-about\"]').click();"
  ))
  app$wait_for_idle(timeout = 30000)
  Sys.sleep(1.5)
  app$wait_for_idle(timeout = 30000)
  expect_identical(
    app$get_js(
      "document.querySelector('.tab-pane.active[id^=\"shiny-tab-\"]').id"
    ),
    "shiny-tab-about"
  )
  expect_null(tryCatch(
    app$get_value(output = "groups_by_other_group_plot"),
    error = function(error) NULL
  ))

  ## A page prepared this way opens as one opened the first time: its code ran
  ## in the session (a cache bound to the session, as Groups' charts use,
  ## failed outside it).
  app$set_inputs(sidebar = "groups")
  app$wait_for_idle(timeout = 30000)
  plot <- app$get_value(output = "groups_by_other_group_plot")
  expect_identical(plot$type, "switch")
  app$wait_for_js(
    "document.querySelectorAll('#groups_by_other_group_plot svg rect').length > 0",
    timeout = 10000
  )
  logs <- as.character(app$get_logs()$message)
  expect_false(any(grepl("Error in", logs, fixed = TRUE)))
})
