## The first view of an address (viewer/core/shared_resources.R) comes only from
## a frame of the data set that address opens.

test_that("the first view is published only for the data set an address opens", {
  skip_on_cran()
  skip_if_demo_data_missing()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
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
  ## the cells of the first view of an address, NA when there is none
  published <- function(path, dataset = "") {
    url <- paste0(
      sub("/$", "", app$get_url()),
      "/cerebro-first-view/",
      key(path, dataset),
      ".json"
    )
    response <- httr::GET(url)
    if (httr::status_code(response) != 200L) {
      return(NA_real_)
    }
    jsonlite::fromJSON(httr::content(
      response,
      "text",
      encoding = "UTF-8"
    ))$cells
  }
  ## a page that is still loading has none of these
  ready <- paste0(
    "typeof CerebroDatasetContext!=='undefined' && ",
    "typeof window.cerebroSpecialistViews!=='undefined' && ",
    "CerebroDatasetContext.phase()==='ready' && ",
    "!!window.cerebroSpecialistViews.get('overview_projection')?.ready()"
  )
  plotted <- function() {
    app$get_js(paste0(
      "Number(document.getElementById('overview_projection_cell_view_host')",
      ".dataset.pointCount)"
    ))
  }
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

  ## Open "/", switch to another data set, then show the Projection: its frame
  ## is the other data set's, and does not belong to "/" (whatever the first
  ## frame of the data set "/" opens has published before the switch).
  app$set_inputs(crb_file_selector = "HLA & TCR", wait_ = FALSE)
  app$wait_for_idle(timeout = 30000)
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(ready, timeout = 60000)
  app$wait_for_idle(timeout = 30000)
  Sys.sleep(2)
  other <- plotted()
  expect_gt(other, 0)
  expect_false(isTRUE(published("/") == other))

  ## a link that names a data set the app does not have leaves nothing
  before <- kept()
  open("?dataset=zzjunk1&page=projection")
  open("?dataset=zzjunk2&page=projection")
  expect_true(is.na(published("/", "zzjunk1")))
  expect_true(is.na(published("/", "zzjunk2")))
  expect_identical(kept(), before)

  ## the address as it is gets the view of its own data set
  open("?page=projection")
  own <- plotted()
  expect_false(own == other)
  expect_equal(published("/"), own)
})
