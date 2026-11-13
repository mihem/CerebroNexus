## The lazy("name", package::name) calls of .bindLazyImports().
lazy_import_table <- function() {
  calls <- Filter(
    function(call) is.call(call) && identical(call[[1L]], as.name("lazy")),
    as.list(body(.bindLazyImports))
  )
  data.frame(
    name = vapply(calls, function(call) call[[2L]], character(1)),
    package = vapply(
      calls,
      function(call) as.character(call[[3L]][[2L]]),
      character(1)
    ),
    export = vapply(
      calls,
      function(call) as.character(call[[3L]][[3L]]),
      character(1)
    )
  )
}

test_that("the packages loaded on first use are not NAMESPACE imports", {
  namespace_file <- system.file("NAMESPACE", package = "CerebroNexus")
  skip_if_not(nzchar(namespace_file))
  directives <- parse(namespace_file, keep.source = FALSE)
  imported <- unique(unlist(lapply(directives, function(directive) {
    if (as.character(directive[[1L]]) %in% c("import", "importFrom")) {
      as.character(directive[[2L]])
    }
  })))
  lazy <- c(unique(lazy_import_table()$package), "Matrix")
  expect_setequal(
    lazy,
    c(
      "dplyr",
      "DT",
      "plotly",
      "shinyWidgets",
      "colourpicker",
      "shinycssloaders",
      "mirai",
      "Matrix"
    )
  )
  expect_length(intersect(imported, lazy), 0L)
  ## launchCerebro() and the Viewer need these from their first line
  expect_true(all(c("shiny", "shinydashboard") %in% imported))
})

test_that("a lazy import is bound without evaluating it", {
  ## the imports sit between the namespace and the base namespace
  provided <- new.env(parent = emptyenv())
  assign("sd", "from another import", envir = provided)
  namespace <- new.env(parent = provided)
  assign("median", "defined by the package", envir = namespace)

  evaluated <- character()
  value <- function(name, result) {
    evaluated <<- c(evaluated, name)
    result
  }
  expect_false(.lazyBind(namespace, "median", value("median", 1)))
  expect_false(.lazyBind(namespace, "sd", value("sd", 2)))
  expect_true(.lazyBind(namespace, "var", value("var", stats::var)))
  expect_true(.lazyBind(namespace, "missing", stop("only when used")))
  expect_identical(evaluated, character())

  ## the package's own function and another import's keep their binding
  expect_identical(namespace$median, "defined by the package")
  expect_false(exists("sd", envir = namespace, inherits = FALSE))
  expect_identical(get("sd", envir = namespace), "from another import")
  ## the others are found from the namespace, and are in the imports
  expect_false(exists("var", envir = namespace, inherits = FALSE))
  expect_true(exists("var", envir = provided, inherits = FALSE))
  ## the value is fetched on first use, once
  expect_identical(get("var", envir = namespace), stats::var)
  expect_identical(get("var", envir = namespace), stats::var)
  expect_identical(evaluated, "var")
  expect_error(get("missing", envir = namespace), "only when used")
})

test_that("every lazy import names an export of its package", {
  table <- lazy_import_table()
  expect_identical(table$name, table$export)
  expect_false(anyDuplicated(table$name) > 0L)
  namespace <- asNamespace("CerebroNexus")
  for (index in seq_len(nrow(table))) {
    package <- table$package[[index]]
    name <- table$name[[index]]
    skip_if_not_installed(package)
    expect_true(
      name %in% getNamespaceExports(package),
      info = paste0(package, "::", name)
    )
    ## Found from the package namespace. Not compared with the package's
    ## function: another test may have mocked that one when the name was
    ## first used here.
    expect_true(
      is.function(get(name, envir = namespace)),
      info = paste0(package, "::", name)
    )
    ## not an object of the namespace: R CMD check evaluates those and reads
    ## the functions it gets as the package's own code
    expect_false(
      exists(name, envir = namespace, inherits = FALSE),
      info = paste0(package, "::", name)
    )
  }
})

test_that("every dplyr function the code refers to is bound", {
  skip_if_not_installed("dplyr")
  files <- list.files(
    viewer_test_path(),
    "[.]R$",
    recursive = TRUE,
    full.names = TRUE
  )
  ## the package sources, when the tests run from a source tree
  sources <- testthat::test_path("..", "..", "R")
  if (dir.exists(sources)) {
    files <- c(files, list.files(sources, "[.]R$", full.names = TRUE))
  }
  expect_gt(length(files), 100L)
  referred <- unique(unlist(lapply(files, function(file) {
    all.names(parse(file, keep.source = FALSE), unique = TRUE)
  })))
  referred <- intersect(referred, getNamespaceExports("dplyr"))
  namespace <- asNamespace("CerebroNexus")
  unbound <- referred[
    !vapply(referred, exists, logical(1), envir = namespace)
  ]
  ## A name listed here needs a lazy() line in .bindLazyImports().
  expect_identical(unbound, character())
})

test_that("Matrix generics run for S4 objects and base functions otherwise", {
  dense <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  frame <- data.frame(a = 1:2, b = 3:4)
  expect_identical(t(dense), base::t(dense))
  expect_identical(t(frame), base::t(frame))
  expect_identical(rowSums(dense), base::rowSums(dense))
  expect_identical(colSums(dense, na.rm = TRUE), base::colSums(dense))
  expect_identical(colMeans(frame), base::colMeans(frame))

  skip_if_not_installed("Matrix")
  sparse <- Matrix::Matrix(dense, sparse = TRUE)
  expect_s4_class(t(sparse), "Matrix")
  expect_identical(t(sparse), Matrix::t(sparse))
  expect_identical(rowSums(sparse), Matrix::rowSums(sparse))
  expect_identical(colSums(sparse), Matrix::colSums(sparse))
  expect_identical(colMeans(sparse), Matrix::colMeans(sparse))
})

test_that("launchCerebro loads what every session uses and leaves the rest", {
  launch <- deparse(body(launchCerebro))
  position <- function(pattern) grep(pattern, launch, fixed = TRUE)[[1L]]
  ## before the data sets are in memory
  expect_lt(
    position(".lazy_imports_session"),
    position(".datasetCatalogFromFiles(")
  )
  expect_true(any(grepl(".idle_packages", launch, fixed = TRUE)))
  expect_length(intersect(.lazy_imports_session, .lazy_imports_idle), 0L)
  expect_true(all(
    c(.lazy_imports_session, .lazy_imports_idle) %in%
      c(lazy_import_table()$package, "Matrix")
  ))
  ## the Viewer loads the rest only while a session is quiet
  server_file <- readLines(viewer_test_path("shiny_server.R"))
  expect_true(any(grepl(
    'Cerebro.options[[".idle_packages"]]',
    server_file,
    fixed = TRUE
  )))
})

test_that("reading a CRB loads dplyr, so the tibbles in it keep their methods", {
  ## tibble is loaded by dplyr; none of the NAMESPACE imports loads either
  expect_true(any(grepl(
    "loadNamespace(\"dplyr\")",
    deparse(body(readCerebro)),
    fixed = TRUE
  )))
})
