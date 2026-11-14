## While the visitor reads the first page, the session prepares the pages the
## data set has (shiny_server.R, viewer_idle_tasks()): their server code is
## loaded before they are opened. Not the repertoire pages, which compute as
## soon as they are loaded.

test_that("a quiet session loads the code of the other pages, not the repertoire's", {
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
  expect_false("immune_repertoire/server.R" %in% loaded())
  expect_false("hla_tcr_motifs/server.R" %in% loaded())

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
