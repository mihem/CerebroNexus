test_that("hidden Linked palette changes wait until the page returns", {
  expressions <- parse(viewer_test_path("coordinated_views", "server.R"))
  observe_calls <- list()
  visit <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }
    if (
      identical(expr[[1L]], as.name("observe")) &&
        grepl(
          '"coordviews_colors"',
          paste(deparse(expr), collapse = ""),
          fixed = TRUE
        )
    ) {
      observe_calls[[length(observe_calls) + 1L]] <<- expr
    }
    if (length(expr) <= 1L) {
      return(invisible(NULL))
    }
    for (i in seq.int(2L, length(expr))) {
      if (!nzchar(paste(deparse(expr[[i]]), collapse = ""))) {
        next
      }
      visit(expr[[i]])
    }
    invisible(NULL)
  }
  for (expr in expressions) {
    visit(expr)
  }
  expect_gte(length(observe_calls), 1L)
  observer <- observe_calls[[which.min(vapply(
    observe_calls,
    function(x) length(deparse(x)),
    integer(1L)
  ))]]

  shiny::testServer(
    function(input, output, session) {
      observe <- shiny::observe
      req <- shiny::req
      work <- new.env(parent = emptyenv())
      work$sends <- work$builds <- 0L
      observer_env <- new.env(parent = environment())
      observer_env$session <- list(sendCustomMessage = function(type, payload) {
        work$sends <- work$sends + 1L
        work$last <- payload
      })
      coordviews_visible <- shiny::reactiveVal(TRUE)
      color <- shiny::reactiveVal("red")
      dataset <- shiny::reactiveVal("A")
      generation <- shiny::reactiveVal(1L)
      viewer_loaded_dataset_context <- shiny::reactive(list(
        epoch = "test",
        dataset_key = dataset(),
        generation = generation()
      ))
      coordviews_color_patch <- shiny::reactive({
        work$builds <- work$builds + 1L
        list(dataset = dataset(), color = color())
      })
      coordviews_build_log <- new.env(parent = emptyenv())
      coordviews_build_log$sent_palette <- shiny::isolate(list(
        context = viewer_loaded_dataset_context(),
        patch = coordviews_color_patch()
      ))
      eval(observer, observer_env)
    },
    {
      session$flushReact()
      expect_identical(work$sends, 0L)
      coordviews_visible(FALSE)
      session$flushReact()
      before <- work$builds
      color("blue")
      session$flushReact()
      expect_identical(work$builds, before)
      expect_identical(work$sends, 0L)
      coordviews_visible(TRUE)
      session$flushReact()
      expect_identical(work$sends, 1L)
      expect_identical(work$last$color, "blue")
      coordviews_visible(FALSE)
      session$flushReact()
      coordviews_visible(TRUE)
      session$flushReact()
      expect_identical(work$sends, 1L)
    }
  )
})
