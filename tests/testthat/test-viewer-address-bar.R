## The address bar follows the page and the data set being read: the server
## sends cerebro_page_url / cerebro_dataset_url and viewer-shell.js writes them.

## The real server functions, with a session that records what it is sent.
address_bar_server <- function() {
  sent <- list()
  scope <- new.env(parent = globalenv())
  scope$session <- list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = c(
      "viewer_initial_page_tabs",
      "viewer_send_page_url",
      "viewer_send_dataset_url"
    )
  )
  list(scope = scope, sent = function() sent)
}

test_that("the page observer sends the page id of the tab being read", {
  server <- address_bar_server()
  send <- server$scope$viewer_send_page_url

  send("geneExpression")
  send("overview")
  send("loadData")
  expect_identical(
    server$sent(),
    list(
      list(type = "cerebro_page_url", message = list(page = "gene_expression")),
      list(type = "cerebro_page_url", message = list(page = "projection")),
      list(type = "cerebro_page_url", message = list(page = "data_info"))
    )
  )

  ## nothing to say for a tab without a page id, or before a tab exists
  before <- length(server$sent())
  for (tab in list("not_a_tab", NULL, NA_character_, character())) {
    send(tab)
  }
  expect_length(server$sent(), before)
})

test_that("every sidebar tab has exactly one page id", {
  tabs <- address_bar_server()$scope$viewer_initial_page_tabs
  expect_false(anyDuplicated(tabs) > 0L)
  expect_false(anyDuplicated(names(tabs)) > 0L)
})

test_that("the data set observer sends the id of a configured data set", {
  server <- address_bar_server()
  send <- server$scope$viewer_send_dataset_url
  ids <- c("TCR", "sample 2", "a&b")

  send("sample 2", ids)
  send("a&b", ids)
  expect_identical(
    server$sent(),
    list(
      list(type = "cerebro_dataset_url", message = list(dataset = "sample 2")),
      list(type = "cerebro_dataset_url", message = list(dataset = "a&b"))
    )
  )

  ## an upload has no id a link could name; nothing selected, or nothing
  ## configured, says nothing
  before <- length(server$sent())
  send("__upload_1__", ids)
  send(NULL, ids)
  send(NA_character_, ids)
  send(character(), ids)
  send(c("TCR", "sample 2"), ids)
  send("TCR", NULL)
  expect_length(server$sent(), before)
})

test_that("the observers are wired to the sidebar and to the selected data set", {
  statements <- viewer_server_statements("shiny_server.R", server = TRUE)
  observers_calling <- function(name) {
    Filter(
      function(x) {
        is.call(x) &&
          identical(as.character(x[[1]])[1], "observeEvent") &&
          name %in% all.names(x[[3]])
      },
      statements
    )
  }
  event_key <- function(x) viewer_server_key(x[[2]])
  option <- function(x, name) isTRUE(x[[name]])

  page <- observers_calling("viewer_send_page_url")
  expect_length(page, 1L)
  expect_identical(event_key(page[[1]]), "input.sidebar")
  ## the first value is the default page, already in the address bar
  expect_true(option(page[[1]], "ignoreInit"))

  dataset <- observers_calling("viewer_send_dataset_url")
  expect_length(dataset, 1L)
  expect_identical(event_key(dataset[[1]]), "available_crb_files.selected")
  expect_true(
    "available_crb_files" %in%
      all.names(dataset[[1]][[3]]) &&
      "names" %in% all.names(dataset[[1]][[3]])
  )
})

test_that("the R and browser halves agree on the address-bar message names", {
  r <- paste(readLines(viewer_test_path("shiny_server.R")), collapse = "\n")
  js <- paste(
    readLines(viewer_test_path("www", "viewer-shell.js")),
    collapse = "\n"
  )
  for (name in c("cerebro_page_url", "cerebro_dataset_url")) {
    expect_match(
      r,
      paste0('sendCustomMessage\\(\\s*"', name, '"'),
      info = name
    )
    expect_match(
      js,
      paste0('addCustomMessageHandler\\("', name, '"'),
      info = name
    )
  }
})

test_that("the address bar handlers write ?page= and ?dataset=", {
  expect_node_fixture(
    "viewer-address-bar.js",
    viewer_test_path("www", "viewer-shell.js"),
    pass = "PASS page and data set write to the address bar"
  )
})
