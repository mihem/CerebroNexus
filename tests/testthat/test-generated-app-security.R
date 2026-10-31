generated_app_e2e_reset_runtime()

test_that("generated App keeps every data payload outside HTTP resources", {
  bundle <- generated_app_e2e_bundle()
  config <- bundle$config
  private_paths <- unname(config$crb_file_to_load)

  expect_true(all(startsWith(private_paths, "private-data/")))
  expect_true(all(file.exists(file.path(bundle$app_dir, private_paths))))

  private_files <- list.files(
    file.path(bundle$app_dir, "private-data"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  spatial_files <- list.files(
    file.path(bundle$app_dir, "spatial-assets"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  forbidden <- unique(c(
    "/cerebro_config.rds",
    "/private-data",
    paste0("/", private_paths),
    paste0("/private-data/", private_files),
    paste0("/data/", private_files),
    paste0("/data/", basename(private_paths)),
    paste0("/spatial-assets/", spatial_files)
  ))
  forbidden <- forbidden[nzchar(forbidden)]
  for (path in forbidden) {
    response <- generated_app_e2e_request(path)
    expect_identical(httr::status_code(response), 404L, info = path)
  }
})

test_that("generated App source is package-free and contains no remote assets", {
  bundle <- generated_app_e2e_bundle()
  source <- paste(
    readLines(file.path(bundle$app_dir, "app.R"), warn = FALSE),
    collapse = "\n"
  )

  forbidden <- c(
    "CerebroNexus::",
    "library(CerebroNexus",
    "requireNamespace(\"CerebroNexus\"",
    "requireNamespace('CerebroNexus'",
    "packageVersion(\"CerebroNexus\"",
    "packageVersion('CerebroNexus'"
  )
  expect_false(any(vapply(
    forbidden,
    grepl,
    logical(1),
    x = source,
    fixed = TRUE
  )))

  root <- generated_app_e2e_request("/")
  html <- httr::content(root, as = "text", encoding = "UTF-8")
  urls <- generated_app_e2e_document_urls(html)
  expect_true(length(urls) > 0L)
  expect_false(any(grepl("^(?:https?:)?//", urls)))
  expect_identical(generated_app_e2e_external_resource_urls(), character())
  generated_app_e2e_expect_clean_browser()
})

test_that("browser console classification does not exempt dependency failures", {
  logs <- data.frame(
    location = rep("chromote", 2L),
    level = rep("error", 2L),
    message = c(
      "Error: the fixed layout requires the slimscroll plugin!",
      "ReferenceError: generated fixture failure"
    ),
    stringsAsFactors = FALSE
  )

  failures <- generated_app_e2e_browser_failures(logs)
  expect_identical(nrow(failures), 2L)
  expect_true(any(grepl(
    "fixed layout requires the slimscroll plugin",
    failures$message,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "generated fixture failure",
    failures$message,
    fixed = TRUE
  )))
})

test_that("generated App browser emits no unexpected console failures", {
  generated_app_e2e_driver()
  failures <- generated_app_e2e_browser_failures()

  expect_identical(
    nrow(failures),
    0L,
    info = paste(failures$message, collapse = "\n")
  )
})
