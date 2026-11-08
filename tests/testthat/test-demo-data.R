## Demo files left out of the package tarball and downloaded on request.

test_that("every registered demo file is a known size and MD5 sum", {
  files <- CerebroNexus:::.demo_data_files
  expect_false(anyDuplicated(files$path) > 0L)
  expect_true(all(files$size > 0))
  expect_true(all(grepl("^[0-9a-f]{32}$", files$md5)))
  expect_true(all(
    CerebroNexus:::.demo_datasets %in% files$path
  ))
  expect_setequal(
    CerebroNexus:::.demoDataFiles("builder/fixtures/spatial_alignment"),
    c(
      "builder/fixtures/spatial_alignment/visium_breast_cancer.rds",
      "builder/fixtures/spatial_alignment/visium_breast_cancer_he.jpg"
    )
  )
  ## A source checkout has the files; they must match the registry.
  local <- vapply(
    files$path,
    function(path) builder_test_inst_path(path),
    character(1)
  )
  here <- file.exists(local)
  expect_identical(unname(file.size(local[here])), files$size[here])
  expect_identical(unname(tools::md5sum(local[here])), files$md5[here])
})

test_that("downloadDemoData() rejects unknown data sets", {
  expect_error(downloadDemoData("nope"), "must name demo data sets")
  expect_error(downloadDemoData(character()), "must name demo data sets")
})

test_that("a demo file is downloaded into the cache and checked", {
  skip_on_cran()
  skip_if_offline("raw.githubusercontent.com")
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  ## Pretend the package does not ship the file, as the CRAN build does not.
  local_mocked_bindings(
    .demoDataPath = function(path) {
      file.path(CerebroNexus:::.demoDataCacheRoot(), path)
    }
  )
  path <- "builder/fixtures/spatial_alignment/visium_breast_cancer_he.jpg"
  expect_message(
    local <- CerebroNexus:::.demoDataFetch(path),
    "Downloading visium_breast_cancer_he.jpg"
  )
  expect_true(startsWith(
    normalizePath(local, winslash = "/"),
    normalizePath(cache, winslash = "/")
  ))
  expect_identical(
    unname(tools::md5sum(local)),
    "106b73c0eb251101e424ea4f585e37b5"
  )
  ## A second call uses the cached copy.
  expect_silent(CerebroNexus:::.demoDataFetch(path))

  ## A file that does not match its MD5 sum is not kept.
  files <- CerebroNexus:::.demo_data_files
  files$md5[files$path == path] <- strrep("0", 32L)
  local_mocked_bindings(.demo_data_files = files)
  unlink(local)
  expect_error(CerebroNexus:::.demoDataFetch(path), "Could not download")
  expect_false(file.exists(local))
  expect_length(list.files(dirname(local)), 0L)
})
