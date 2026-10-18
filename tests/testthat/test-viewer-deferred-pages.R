## Page servers load when their page is first opened. A page may therefore only
## call functions that the server loads up front or that its own directory
## defines; anything else fails for users who never opened the owning page.

deferred_page_functions <- function(file) {
  exprs <- tryCatch(parse(file, keep.source = FALSE), error = function(e) {
    list()
  })
  names <- character()
  for (e in exprs) {
    if (
      is.call(e) &&
        identical(e[[1]], as.name("<-")) &&
        is.name(e[[2]]) &&
        is.call(e[[3]]) &&
        identical(e[[3]][[1]], as.name("function"))
    ) {
      names <- c(names, as.character(e[[2]]))
    }
  }
  names
}

## Every symbol and string constant in the files. Unlike a "name(" text match
## this also sees do.call("name", ...), lapply(x, name) and match.fun("name").
page_references <- function(files) {
  refs <- character()
  for (file in files) {
    exprs <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) {
      next
    }
    tokens <- utils::getParseData(exprs)
    tokens <- tokens[
      tokens$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST"),
    ]
    refs <- c(refs, gsub("^[\"']|[\"']$", "", tokens$text))
  }
  unique(refs)
}

test_that("page_references sees direct, string and function-reference uses", {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file))
  writeLines(
    c(
      "a <- function() helperA(1)",
      "b <- function() do.call(\"helperB\", list())",
      "c <- function() lapply(1:2, helperC)"
    ),
    file
  )
  expect_true(all(
    c("helperA", "helperB", "helperC") %in% page_references(file)
  ))
})

test_that("deferred pages only call eager or same-page functions", {
  viewer <- viewer_test_path()
  server <- readLines(file.path(viewer, "shiny_server.R"), warn = FALSE)
  pages_line <- regmatches(
    server,
    regexpr("^\\s+[a-zA-Z_]+ = \"[a-z_]+/server\\.R\"", server)
  )
  pages <- unique(sub("/server\\.R\"$", "", sub("^[^\"]*\"", "", pages_line)))
  expect_true(all(
    c("spatial", "gene_expression", "coordinated_views") %in% pages
  ))

  eager_sources <- unlist(regmatches(
    server,
    gregexpr("\"/?viewer/[a-z_]+/[A-Za-z_]+\\.R\"", server)
  ))
  eager_files <- c(
    list.files(viewer, pattern = "\\.R$", full.names = TRUE),
    file.path(viewer, gsub("^\"/?viewer/|\"$", "", eager_sources))
  )
  eager <- unique(unlist(lapply(eager_files, deferred_page_functions)))

  page_files <- lapply(pages, function(page) {
    files <- list.files(
      file.path(viewer, page),
      pattern = "\\.R$",
      full.names = TRUE
    )
    files[!normalizePath(files) %in% normalizePath(eager_files)]
  })
  names(page_files) <- pages
  owners <- list()
  for (page in pages) {
    for (file in page_files[[page]]) {
      for (name in setdiff(deferred_page_functions(file), eager)) {
        owners[[name]] <- unique(c(owners[[name]], page))
      }
    }
  }

  ## Shared helpers in core/ belong to no page. One that shiny_server.R does not
  ## source up front is defined nowhere until something else loads it.
  core_files <- list.files(
    file.path(viewer, "core"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  core_files <- core_files[
    !normalizePath(core_files) %in% normalizePath(eager_files)
  ]
  for (file in core_files) {
    for (name in setdiff(deferred_page_functions(file), eager)) {
      owners[[name]] <- unique(c(owners[[name]], "core (never sourced)"))
    }
  }

  crossings <- character()
  for (page in pages) {
    used <- page_references(page_files[[page]])
    for (name in names(owners)) {
      if (page %in% owners[[name]]) {
        next
      }
      if (name %in% used) {
        crossings <- c(
          crossings,
          paste0(
            page,
            " -> ",
            name,
            " (",
            paste(owners[[name]], collapse = ", "),
            ")"
          )
        )
      }
    }
  }
  expect_identical(crossings, character())
})
