## The first view of an address (viewer/core/shared_resources.R) comes only from
## a frame of the data set that address opens.

test_that("the first view is published only for the data set an address opens", {
  skip_on_cran()
  skip_if_demo_data_missing()
  skip_if_not_installed("httr")
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  app_dir <- viewer_app_test_path()
  local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "first_view_address",
    width = 1440,
    height = 1000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)

  kept <- function() {
    list.files(file.path(cache, "R", "CerebroNexus", "first-view"))
  }
  key <- function(path, dataset = "") {
    paste(
      as.character(charToRaw(paste0(path, "\n", dataset))),
      collapse = ""
    )
  }
  published <- function(path, dataset = "") {
    url <- paste0(
      sub("/$", "", app$get_url()),
      "/cerebro-first-view/",
      key(path, dataset),
      ".json"
    )
    httr::status_code(httr::GET(url)) == 200L
  }
  ready <- paste0(
    "CerebroDatasetContext.phase()==='ready' && ",
    "window.cerebroSpecialistViews.get('overview_projection')?.ready()"
  )
  open <- function(query) {
    app$get_chromote_session()$Page$navigate(paste0(
      sub("/$", "", app$get_url()),
      "/",
      query
    ))
    ## a new page has none of the helpers AppDriver injects: wait on the page
    app$wait_for_js(ready, timeout = 60000)
    app$wait_for_js(
      "!jQuery('html').hasClass('shiny-busy')",
      timeout = 30000
    )
    Sys.sleep(2)
  }

  ## Open "/", switch to another data set, then show the Projection: its first
  ## frame is the other data set's, and does not belong to "/".
  app$set_inputs(crb_file_selector = "HLA & TCR", wait_ = FALSE)
  app$wait_for_idle(timeout = 30000)
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(ready, timeout = 60000)
  app$wait_for_idle(timeout = 30000)
  Sys.sleep(2)
  expect_false(published("/"))
  expect_length(kept(), 0L)

  ## a link that names a data set the app does not have
  open("?dataset=zzjunk1&page=projection")
  open("?dataset=zzjunk2&page=projection")
  expect_false(published("/", "zzjunk1"))
  expect_false(published("/", "zzjunk2"))
  expect_length(kept(), 0L)

  ## the address as it is, with its own data set
  open("?page=projection")
  expect_true(published("/"))
  expect_gt(length(kept()), 0L)
})
