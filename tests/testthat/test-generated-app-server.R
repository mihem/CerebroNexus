test_that("generated private App reaches semantic HTTP readiness", {
  bundle <- generated_app_e2e_bundle()
  server <- generated_app_e2e_server()

  expect_true(server$process$is_alive())
  expect_match(server$base_url, "^http://127[.]0[.]0[.]1:[0-9]+$")
  expect_true(dir.exists(server$library))
  expect_false(dir.exists(file.path(server$library, "CerebroNexus")))

  root <- generated_app_e2e_request("/")
  expect_identical(httr::status_code(root), 200L)
  expect_match(
    httr::content(root, as = "text", encoding = "UTF-8"),
    "CerebroNexus",
    fixed = TRUE
  )

  server_source <- paste(
    deparse(body(generated_app_e2e_server)),
    collapse = "\n"
  )
  expect_match(server_source, "httpuv::randomPort", fixed = TRUE)
  expect_false(grepl("Sys.sleep", server_source, fixed = TRUE))

  expect_true(dir.exists(bundle$app_dir))
  expect_true(file.exists(file.path(bundle$app_dir, "app.R")))
})

test_that("generated App serves every versionless Viewer asset locally", {
  root <- generated_app_e2e_request("/")
  html <- httr::content(root, as = "text", encoding = "UTF-8")
  assets <- generated_app_e2e_asset_urls(html)

  expect_true(all(c("custom.css", "fill_height.js") %in% names(assets)))
  expect_true(all(grepl("[?]v=[[:xdigit:]]+", assets)))
  for (asset in assets) {
    response <- generated_app_e2e_request(asset)
    expect_identical(httr::status_code(response), 200L, info = asset)
    expect_gt(length(httr::content(response, as = "raw")), 0L)
  }

  bundle <- generated_app_e2e_bundle()
  relative <- list.files(bundle$app_dir, recursive = TRUE, all.files = TRUE)
  segments <- unlist(strsplit(relative, "[/\\\\]"))
  expect_false(any(grepl("^v[0-9]+(?:[.][0-9]+)+$", segments)))
})
