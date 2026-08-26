test_that("serialized path audit traverses attributes and closure environments", {
  leaked <- local({
    private_path <- "/Users/example/private-data.crb"
    structure(
      function() private_path,
      audit_note = "/private/tmp/export-stage"
    )
  })

  findings <- .findSerializedPathLeaks(leaked)

  expect_true("/private/tmp/export-stage" %in% unname(findings))
  expect_true("/Users/example/private-data.crb" %in% unname(findings))
  expect_true(any(grepl("attributes", names(findings), fixed = TRUE)))
  expect_true(any(grepl("environment", names(findings), fixed = TRUE)))
})

test_that("serialized path audit terminates on cyclic environments", {
  first <- new.env(parent = emptyenv())
  second <- new.env(parent = first)
  first$second <- second
  second$first <- first
  second$safe <- "spatial-assets/dataset/fov/image.png"

  expect_identical(.findSerializedPathLeaks(first), character())
})

test_that("serialized path audit does not fail on unreadable lazy bindings", {
  object <- new.env(parent = emptyenv())
  delayedAssign(
    "unreadable",
    stop("the audit must not fail while inspecting this binding"),
    assign.env = object
  )
  object$safe <- "spatial-assets/dataset/fov/image.png"

  expect_identical(.findSerializedPathLeaks(object), character())
})

test_that("serialized path audit handles classed list vectors", {
  expect_identical(
    .findSerializedPathLeaks(numeric_version("5.0.0")),
    character()
  )
})

test_that("Cerebro serialization removes source references from R6 methods", {
  method <- function() "ok"
  attr(method, "srcref") <- structure(
    integer(),
    srcfile = list(filename = "/Users/example/class-Cerebro.R")
  )
  private <- new.env(parent = emptyenv())
  assign("method", method, envir = private)
  lockBinding("method", private)
  enclos <- new.env(parent = emptyenv())
  enclos$private <- private
  object <- new.env(parent = emptyenv())
  object$.__enclos_env__ <- enclos

  result <- .stripCerebroSourceReferences(object)

  expect_identical(result$.__enclos_env__$private$method(), "ok")
  expect_null(attr(result$.__enclos_env__$private$method, "srcref"))
  expect_true(bindingIsLocked("method", private))
})
