shared_resources <- new.env()
sys.source(viewer_test_path("core", "shared_resources.R"), shared_resources)

test_that("only the Packs of configured data sets are shared, without a login", {
  root <- withr::local_tempdir()
  crb <- file.path(root, "data.crb")
  writeBin(raw(8), crb)
  pack <- file.path(
    normalizePath(root, winslash = "/"),
    "data.viewer"
  )
  roots <- shared_resources$viewerSharedResourceRoots(list(
    crb_file_to_load = c(crb, file.path(root, "missing.crb"))
  ))
  expect_identical(roots, pack)
  expect_identical(
    shared_resources$viewerSharedResourceRoots(list(
      crb_file_to_load = crb,
      .viewer_auth = list(credentials_path = "x")
    )),
    character()
  )
  expect_identical(
    shared_resources$viewerSharedResourceRoots(list(crb_file_to_load = NULL)),
    character()
  )
})

test_that("a shared directory has one prefix for the process", {
  root <- withr::local_tempdir()
  pack <- file.path(normalizePath(root, winslash = "/"), "data.viewer")
  dir.create(file.path(pack, "projections"), recursive = TRUE)
  dir.create(file.path(pack, "metadata"))
  upload <- file.path(root, "upload.viewer", "projections")
  dir.create(upload, recursive = TRUE)

  registered <- list()
  register <- function(prefix, directory) {
    registered[[prefix]] <<- directory
  }
  prefix <- function(kind, directory, roots = pack) {
    shared_resources$viewerSharedResourcePrefix(
      kind,
      directory,
      roots,
      register
    )
  }
  projection <- prefix("projection", file.path(pack, "projections"))
  expect_match(projection, "^cerebro-projection-[a-z0-9]{24}-1$")
  ## the same directory again, as another session would ask: the same URL,
  ## registered once
  expect_identical(
    prefix("projection", file.path(pack, "projections")),
    projection
  )
  expect_length(registered, 1L)
  metadata <- prefix("metadata", file.path(pack, "metadata"))
  expect_match(metadata, "^cerebro-metadata-[a-z0-9]{24}-1$")
  expect_identical(
    sub("-1$", "", sub("^cerebro-metadata-", "", metadata)),
    sub("-1$", "", sub("^cerebro-projection-", "", projection))
  )
  expect_identical(
    normalizePath(registered[[metadata]], winslash = "/"),
    file.path(pack, "metadata")
  )

  ## a directory outside the configured Packs stays with its session, as does
  ## everything when nothing is shared, and a sibling whose name only starts
  ## like a Pack
  expect_null(prefix("projection", upload))
  expect_null(prefix("projection", file.path(pack, "projections"), character()))
  expect_null(prefix("projection", paste0(pack, "-other")))
  expect_length(registered, 2L)

  ## a Pack built again gets new URLs: a URL always names the same bytes
  manifest <- file.path(pack, "manifest.json")
  writeLines("{}", manifest)
  first <- prefix("projection", file.path(pack, "projections"))
  expect_identical(prefix("projection", file.path(pack, "projections")), first)
  Sys.setFileTime(manifest, Sys.time() + 60)
  rebuilt <- prefix("projection", file.path(pack, "projections"))
  expect_false(identical(rebuilt, first))
  expect_match(rebuilt, "^cerebro-projection-[a-z0-9]{24}-[0-9]+$")
})
