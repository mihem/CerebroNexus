inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "app.R"))][1]
if (is.na(inst_dir)) {
  inst_dir <- system.file(package = "CerebroNexus")
}

viewer_dir <- file.path(inst_dir, "viewer")
linked_dir <- file.path(viewer_dir, "coordinated_views")

test_that("Linked views is additive to the existing specialist pages", {
  expect_true(file.exists(file.path(linked_dir, "UI.R")))
  expect_true(file.exists(file.path(linked_dir, "bundle.R")))
  expect_true(file.exists(file.path(linked_dir, "server.R")))

  expect_true(file.exists(file.path(viewer_dir, "overview", "UI.R")))
  expect_true(file.exists(file.path(viewer_dir, "spatial", "UI.R")))
  expect_true(file.exists(file.path(viewer_dir, "trekker", "UI.R")))

  ui <- paste(
    readLines(file.path(viewer_dir, "shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, '"Projection"', fixed = TRUE)
  expect_match(ui, '"Linked views"', fixed = TRUE)
  expect_match(ui, 'tab_overview', fixed = TRUE)
  expect_match(ui, 'tab_spatial', fixed = TRUE)
  expect_match(ui, 'tab_trekker', fixed = TRUE)
  expect_match(ui, 'tab_coordinated_views', fixed = TRUE)
})

test_that("Linked views chooses a useful categorical default", {
  clone_file <- file.path(viewer_dir, "clone_contract.R")
  bundle_file <- file.path(linked_dir, "bundle.R")
  expect_true(file.exists(clone_file))
  expect_true(file.exists(bundle_file))

  env <- new.env(parent = globalenv())
  sys.source(clone_file, envir = env)
  sys.source(bundle_file, envir = env)

  choose_default <- env$cv_default_group
  expect_true(is.function(choose_default))
  expect_identical(
    choose_default(c("donor", "Cell Tyep", "Sample")),
    "Cell Tyep"
  )
  expect_identical(
    choose_default(c("donor", "cell-type", "sample")),
    "cell-type"
  )
  expect_identical(
    choose_default(c("donor", "SAMPLE")),
    "SAMPLE"
  )

  set.seed(42)
  fallback <- choose_default(c("donor", "batch", "condition"))
  expect_true(fallback %in% c("donor", "batch", "condition"))
  expect_null(choose_default(character()))
})

test_that("Linked views keeps alpha and offers fluid or square plots", {
  ui <- paste(
    readLines(file.path(linked_dir, "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(file.path(viewer_dir, "www", "coordviews.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(ui, '"Point opacity"', fixed = TRUE)
  expect_match(ui, '"cv-opacity"', fixed = TRUE)
  expect_match(ui, 'id = "cv-square-plots"', fixed = TRUE)
  expect_match(ui, '"Keep plots square"', fixed = TRUE)
  expect_no_match(ui, 'id = "cv-square-plots", checked', fixed = TRUE)
  expect_match(js, "function resizePanel(p, width, height)", fixed = TRUE)
  expect_match(js, "var keepPlotsSquare = false", fixed = TRUE)
  expect_match(js, "keepPlotsSquare ?", fixed = TRUE)
  expect_false(grepl("function resizePanelSquare", js, fixed = TRUE))
})

test_that("desktop sidebar starts open and can yield its full width", {
  css <- paste(
    readLines(file.path(viewer_dir, "www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    css,
    "html body.skin-blue .main-header .navbar .sidebar-toggle",
    fixed = TRUE
  )
  expect_match(css, "sidebar-collapse .content-wrapper", fixed = TRUE)
  expect_match(css, "margin-left: 0 !important", fixed = TRUE)
  expect_match(css, "translateX(-230px)", fixed = TRUE)
  expect_match(css, "transition: left .3s ease-in-out", fixed = TRUE)
  expect_match(css, "width: 20px", fixed = TRUE)
  expect_match(css, "height: 42px", fixed = TRUE)
  expect_match(css, "content: \"\"", fixed = TRUE)
  expect_match(css, "border-left: 2px solid currentColor", fixed = TRUE)
  expect_match(css, "3px 0 7px -2px", fixed = TRUE)
  expect_match(css, "5px 0 11px -2px", fixed = TRUE)

  toggle_blocks <- regmatches(
    css,
    gregexpr(
      paste0(
        "html body.skin-blue \\.main-header \\.navbar ",
        "\\.sidebar-toggle \\{[^}]+\\}"
      ),
      css
    )
  )[[1]]
  expect_equal(
    sum(grepl("background: var(--c-surface-3)", toggle_blocks, fixed = TRUE)),
    2
  )
  expect_equal(
    sum(grepl("color: var(--c-text-2)", toggle_blocks, fixed = TRUE)),
    2
  )
  expect_match(
    css,
    paste0(
      "sidebar-toggle:hover {\n",
      "  background: var(--c-amber-100);\n",
      "  color: var(--c-amber-700);"
    ),
    fixed = TRUE
  )
  expect_no_match(css, "sidebar-toggle:hover,\n", fixed = TRUE)
})
