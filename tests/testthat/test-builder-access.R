## The Builder's access token: launchCerebroBuilder() makes one, opens the
## page with it, and the page and every session need it (inst/builder/access.R).

access_env <- new.env(parent = globalenv())
builder_repo_source("access.R", local = access_env)

test_that("the access token is required once one is set", {
  allowed <- access_env$builder_access_allowed
  token <- strrep("ab", 32L)
  expect_true(allowed(paste0("?token=", token), token))
  expect_true(allowed(paste0("?a=1&token=", token), token))
  expect_false(allowed("", token))
  expect_false(allowed(NULL, token))
  expect_false(allowed(NA_character_, token))
  expect_false(allowed("?token=", token))
  expect_false(allowed(paste0("?token=", strrep("ab", 31L)), token))
  expect_false(allowed(paste0("?token=", strrep("ab", 31L), "ac"), token))
  expect_false(allowed(paste0("?token=", token, "0"), token))
  ## No token set (app.R run directly, as the tests do): nothing is checked.
  expect_true(allowed("", NULL))
})

test_that("only a usable token option counts", {
  token_of <- access_env$builder_access_token
  expect_null(token_of(NULL))
  expect_null(token_of(""))
  expect_null(token_of(NA_character_))
  expect_null(token_of(c("a", "b")))
  expect_identical(token_of("abc"), "abc")
})

test_that("a page request without the token is refused", {
  response <- access_env$builder_access_denied_response()
  expect_s3_class(response, "httpResponse")
  expect_identical(response$status, 403L)
  expect_match(response$content, "launchCerebroBuilder", fixed = TRUE)
})

test_that("the launcher makes a fresh 64-character token", {
  first <- CerebroNexus:::.builderAccessToken()
  expect_match(first, "^[0-9a-f]{64}$")
  expect_false(identical(first, CerebroNexus:::.builderAccessToken()))
})

test_that("the launcher prints the address with the token and opens it", {
  token <- strrep("cd", 32L)
  opened <- character()
  browse <- function(url) opened <<- c(opened, url)
  open <- CerebroNexus:::.builderOpenWithToken(token, TRUE, .browse = browse)
  expect_message(
    url <- open("http://127.0.0.1:4499"),
    paste0("http://127.0.0.1:4499/?token=", token),
    fixed = TRUE
  )
  expect_identical(url, paste0("http://127.0.0.1:4499/?token=", token))
  expect_identical(opened, url)

  opened <- character()
  quiet <- CerebroNexus:::.builderOpenWithToken(token, FALSE, .browse = browse)
  expect_message(quiet("http://127.0.0.1:4499/"), "?token=", fixed = TRUE)
  expect_length(opened, 0L)
})

test_that("the launcher sets the token only while the Builder runs", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("callr")
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  skip_if_not_installed("digest")
  seen <- NULL
  local_mocked_bindings(
    runApp = function(appDir, host, port, launch.browser) {
      seen <<- list(
        token = getOption("CerebroNexus.builder.access_token"),
        launch = launch.browser
      )
    },
    .package = "shiny"
  )
  withr::local_options(CerebroNexus.builder.access_token = "before")
  CerebroNexus::launchCerebroBuilder(launch_browser = FALSE)
  expect_match(seen$token, "^[0-9a-f]{64}$")
  expect_true(is.function(seen$launch))
  expect_identical(getOption("CerebroNexus.builder.access_token"), "before")
})
