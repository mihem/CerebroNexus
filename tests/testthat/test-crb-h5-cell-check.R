test_that("a daemon confirms an H5 file's cell names once per process", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("mirai")
  crb_cache <- new.env()
  sys.source(viewer_test_path("crb_cache.R"), crb_cache)
  sidecar <- withr::local_tempfile(fileext = ".h5")
  cells <- c("AAAC-1", "AAAG-1", "AAAT-1")
  rhdf5::h5createFile(sidecar)
  rhdf5::h5createGroup(sidecar, "expression")
  rhdf5::h5write(cells, sidecar, "expression/genes")
  rhdf5::h5closeAll()
  expected <- rlang::hash(enc2utf8(cells))

  crb_cache$crb_h5_cell_check_start(sidecar, expected)
  ## The session never waits for the daemon; the test does.
  wait <- function(hash) {
    key <- crb_cache$.crbH5CellCheckKey(sidecar, hash)
    mirai::call_mirai(crb_cache$.crb_h5_cell_checks[[key]]$task)
  }
  wait(expected)
  expect_true(crb_cache$crb_h5_cells_verified(sidecar, expected))
  expect_false(crb_cache$crb_h5_cells_pending(sidecar, expected))

  ## Other names fail the check; the session then reads and refuses them.
  crb_cache$crb_h5_cell_check_start(sidecar, "another-hash")
  wait("another-hash")
  expect_false(crb_cache$crb_h5_cells_verified(sidecar, "another-hash"))
  ## No check started: nothing is confirmed until the session records one.
  expect_false(crb_cache$crb_h5_cells_verified(sidecar, "unchecked"))
  crb_cache$crb_h5_cells_confirm(sidecar, "unchecked")
  expect_true(crb_cache$crb_h5_cells_verified(sidecar, "unchecked"))
})

test_that("an unfinished daemon check is not waited for", {
  ## Timing-sensitive; kills a background process.
  skip_on_cran()
  skip_if_not_installed("mirai")
  crb_cache <- new.env()
  sys.source(viewer_test_path("crb_cache.R"), crb_cache)
  sidecar <- withr::local_tempfile(fileext = ".h5")
  entry <- new.env(parent = emptyenv())
  entry$task <- mirai::mirai(Sys.sleep(20))
  on.exit(mirai::stop_mirai(entry$task), add = TRUE)
  key <- crb_cache$.crbH5CellCheckKey(sidecar, "hash")
  assign(key, entry, envir = crb_cache$.crb_h5_cell_checks)

  started <- proc.time()[["elapsed"]]
  expect_false(crb_cache$crb_h5_cells_verified(sidecar, "hash"))
  expect_lt(proc.time()[["elapsed"]] - started, 5)
  ## The task stays for a later session.
  expect_true(crb_cache$crb_h5_cells_pending(sidecar, "hash"))
  expect_null(entry$verified)
})
