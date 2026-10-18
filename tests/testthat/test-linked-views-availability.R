linked_catalog <- function(projections = "umap", ...) {
  info <- list(
    projection_names = projections,
    immune_repertoire = FALSE,
    capabilities = list(spatial = FALSE, trajectory = FALSE, trekker = FALSE)
  )
  utils::modifyList(info, list(...))
}

test_that("Linked views depends on available views, not their names or selection", {
  expect_false(viewerLinkedViewsAvailable(linked_catalog()))
  expect_false(viewerLinkedViewsAvailable(linked_catalog("custom_embedding")))
  expect_false(viewerLinkedViewsAvailable(linked_catalog(c(
    "umap",
    "umap",
    NA,
    ""
  ))))
  expect_true(viewerLinkedViewsAvailable(linked_catalog(c("umap", "pca"))))
  for (kind in c("spatial", "trajectory", "trekker")) {
    info <- linked_catalog()
    info$capabilities[[kind]] <- TRUE
    expect_true(viewerLinkedViewsAvailable(info), info = kind)
  }
  expect_true(viewerLinkedViewsAvailable(linked_catalog(
    immune_repertoire = TRUE
  )))
  expect_null(viewerLinkedViewsAvailable(list()))
  expect_null(viewerLinkedViewsAvailable(list(projection_names = "umap")))
  info <- linked_catalog()
  info$projection_names <- NULL
  expect_null(viewerLinkedViewsAvailable(info))
  info$capabilities$spatial <- TRUE
  expect_true(viewerLinkedViewsAvailable(info))
})

test_that("legacy objects use their full registered content", {
  object <- list(availableProjections = function() "umap")
  expect_false(viewerLinkedViewsAvailable(object = object))
  object$availableProjections <- function() c("umap", "tsne")
  expect_true(viewerLinkedViewsAvailable(object = object))
  object$availableProjections <- function() "umap"
  object$availableSpatial <- function() "section"
  expect_true(viewerLinkedViewsAvailable(object = object))
  object$availableSpatial <- function() character()
  object$getMethodsForTrajectories <- function() "custom_method"
  object$getNamesOfTrajectories <- function(method) "trajectory"
  expect_true(viewerLinkedViewsAvailable(object = object))
  object$getNamesOfTrajectories <- function(method) character()
  expect_false(viewerLinkedViewsAvailable(object = object))
  object$getImmuneRepertoire <- function() {
    list(sample = data.frame(clone = "a"))
  }
  expect_true(viewerLinkedViewsAvailable(object = object))
  ## A file-backed repertoire answers from its backend summary.
  object$immune_repertoire_backend <- list(
    type = "bpcells-file",
    samples = "s1"
  )
  object$getImmuneRepertoire <- function() stop("repertoire was hydrated")
  expect_true(viewerLinkedViewsAvailable(object = object))
})

test_that("dataset changes update the gate and leave an unavailable Linked page", {
  statements <- parse(viewer_test_path("shiny_server.R"))
  server_expr <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(x[[2L]], as.name("server"))
    },
    as.list(statements)
  )[[1L]][[3L]]
  gate_expr <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(x[[2L]], as.name("toggleConditionalTab"))
    },
    as.list(server_expr[[3L]])
  )[[1L]][[3L]]
  shiny::testServer(
    function(input, output, session) {
      reactive <- shiny::reactive
      observe <- shiny::observe
      isolate <- shiny::isolate
      req <- shiny::req
      current_dataset_info <- shiny::reactiveVal(linked_catalog(c(
        "umap",
        "tsne"
      )))
      dataset_load_requested <- shiny::reactiveVal(FALSE)
      data_set <- shiny::reactiveVal(list(availableProjections = function() {
        "umap"
      }))
      initial_tab <- "coordinated_views"
      initial_page_applied <- shiny::reactiveVal(FALSE)
      navigation_log <- new.env()
      navigation_log$values <- character()
      updateTabItems <- function(session, id, selected) {
        navigation_log$values <- c(navigation_log$values, selected)
      }
      showNotification <- function(...) NULL
      gate <- eval(gate_expr, environment())
      gate(
        "coordinated_views",
        function() viewerLinkedViewsAvailable(object = data_set()),
        catalog_check = viewerLinkedViewsAvailable,
        fallback_tab = "overview"
      )
    },
    {
      session$setInputs(sidebar = "coordinated_views")
      expect_true(shiny::isolate(initial_page_applied()))
      expect_length(navigation_log$values, 0L)
      current_dataset_info(linked_catalog())
      session$flushReact()
      expect_identical(navigation_log$values, "overview")
      session$setInputs(sidebar = "overview")
      current_dataset_info(linked_catalog(c("umap", "pca")))
      session$flushReact()
      expect_length(navigation_log$values, 1L)
      # Unknown legacy catalog stays reachable, then resolves against loaded data.
      current_dataset_info(list())
      session$setInputs(sidebar = "coordinated_views")
      expect_length(navigation_log$values, 1L)
      dataset_load_requested(TRUE)
      session$flushReact()
      expect_identical(navigation_log$values, c("overview", "overview"))
    }
  )
})
