test_that("a failed dataset remains recoverable in the same reactive session", {
  # Exercise the actual data accessor without starting unrelated viewer modules.
  statements <- parse(viewer_test_path("shiny_server.R"))
  server <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    identical(x[[2L]], as.name("server")), as.list(statements))[[1L]][[3L]]
  accessor <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
    identical(x[[2L]], as.name("data_set")), as.list(server[[3L]]))[[1L]][[3L]]
  scope <- new.env(parent = asNamespace("shiny"))
  scope$viewer_dataset_snapshot <- shiny::reactiveVal(list(
    ok = FALSE, error = simpleError("unreadable dataset")
  ))
  data <- eval(accessor, scope)
  error <- tryCatch(shiny::isolate(data()), error = identity)
  expect_s3_class(error, "shiny.silent.error")
  expect_s3_class(error, "validation")
  scope$viewer_dataset_snapshot(list(ok = TRUE, data = list(cells = 17L)))
  expect_identical(shiny::isolate(data()), list(cells = 17L))
})
