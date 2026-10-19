test_that("About renders configured versions and an explicit missing-version fallback", {
  for (version in list("9.8.7", "custom-build", NULL, "", NA_character_)) {
    shiny::testServer(
      function(input, output, session) {
        Cerebro.options <- list(cerebro_version = version)
        sys.source(viewer_test_path("about", "server.R"), environment())
      },
      {
        expected <- if (length(version) && !is.na(version) && nzchar(version)) {
          version
        } else {
          "unknown"
        }
        expect_match(output$about, paste0("v", expected, "<br>"), fixed = TRUE)
      }
    )
  }
})

test_that("CerebroNexus releases use major.minor.patch versions", {
  description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  if (!nzchar(description_path)) {
    description_path <- testthat::test_path("..", "..", "DESCRIPTION")
  }
  news_path <- system.file("NEWS.md", package = "CerebroNexus")
  if (!nzchar(news_path)) {
    news_path <- testthat::test_path("..", "..", "NEWS.md")
  }
  package_version <- read.dcf(description_path, fields = "Version")[[1]]
  options <- viewer_demo_options()
  news <- readLines(news_path, warn = FALSE)

  expect_match(package_version, "^[0-9]+[.][0-9]+[.][0-9]+$")
  expect_identical(options$cerebro_version, package_version)
  expect_identical(news[[1]], paste0("# CerebroNexus ", package_version))
})
