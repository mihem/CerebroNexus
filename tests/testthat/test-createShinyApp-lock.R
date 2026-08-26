lock_test_crb <- function(root, backend = NULL) {
  source <- file.path(root, "source")
  dir.create(source, recursive = TRUE, showWarnings = FALSE)
  object <- Cerebro$new()
  if (!is.null(backend)) {
    object$setExpressionBackend(
      type = backend$type,
      location = backend$location
    )
  }
  path <- file.path(source, "dataset.crb")
  saveRDS(object, path)
  path
}

lock_test_build <- function(crb, result_dir, ...) {
  createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = result_dir,
    launch_browser = FALSE,
    verbose = FALSE,
    ...
  )
}

lock_test_wait_for_file <- function(path, process, timeout = 10) {
  deadline <- Sys.time() + timeout
  repeat {
    if (file.exists(path)) {
      return(invisible(TRUE))
    }
    if (!process$is_alive()) {
      stop(
        "Lock worker exited before becoming ready: ",
        paste(process$read_all_error_lines(), collapse = "\n")
      )
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the lock worker.")
    }
    Sys.sleep(0.05)
  }
}

test_that("a bundle lock records ownership and releases cleanly", {
  root <- withr::local_tempdir()
  result <- file.path(root, "app")

  lock <- .acquireBundleLock(result)
  expect_s3_class(lock, "cerebro_bundle_lock")
  expect_identical(lock$path, .bundleLockPath(result))
  expect_true(dir.exists(lock$path))
  expect_setequal(
    list.files(lock$path, all.files = TRUE, no.. = TRUE),
    c("owner-token", "owner.txt")
  )
  owner <- readLines(file.path(lock$path, "owner.txt"), warn = FALSE)
  expect_true(any(grepl(paste0("pid=", Sys.getpid()), owner, fixed = TRUE)))
  expect_true(any(grepl(
    normalizePath(root, winslash = "/"),
    owner,
    fixed = TRUE
  )))

  expect_true(.releaseBundleLock(lock))
  expect_false(dir.exists(lock$path))
  expect_false(.releaseBundleLock(lock))
})

test_that("only one lock holder can build a target", {
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  first <- .acquireBundleLock(result)
  withr::defer(.releaseBundleLock(first))

  expect_error(.acquireBundleLock(result), "already being built")
  expect_true(dir.exists(first$path))

  other <- .acquireBundleLock(file.path(root, "other-app"))
  expect_true(.releaseBundleLock(other))
})

test_that("relative and absolute aliases share one lock", {
  root <- withr::local_tempdir()
  withr::local_dir(root)
  lock <- .acquireBundleLock("app")
  withr::defer(.releaseBundleLock(lock))

  expect_error(
    .acquireBundleLock(file.path(root, "app")),
    "already being built"
  )
  expect_true(.releaseBundleLock(lock))
})

test_that("result names cannot collide with the lock namespace", {
  root <- withr::local_tempdir()
  holder <- .acquireBundleLock(file.path(root, "app"))
  withr::defer(.releaseBundleLock(holder))

  for (reserved in c(".app-build.lock", ".APP-BUILD.LOCK")) {
    expect_error(
      .acquireBundleLock(file.path(root, reserved)),
      "reserved"
    )
  }
  expect_error(
    .acquireBundleLock(file.path(holder$path, "nested-app")),
    "reserved"
  )
  expect_false(dir.exists(file.path(holder$path, ".nested-app-build.lock")))
  expect_true(dir.exists(holder$path))
})

test_that("result names reject Windows aliases on every platform", {
  root <- withr::local_tempdir()
  for (leaf in c("app.", "app ", "CON", "nul.txt", "AUX.log")) {
    expect_error(
      .prepareBundleResultTarget(file.path(root, leaf)),
      "portable relative path"
    )
  }
})

test_that("reserved lock ancestors are rejected before creating directories", {
  root <- withr::local_tempdir()
  reserved_parent <- file.path(root, ".app-build.lock")

  expect_error(
    .prepareBundleResultTarget(
      file.path(reserved_parent, "nested", "child-app")
    ),
    "reserved"
  )
  expect_false(dir.exists(reserved_parent))
})

test_that("raw reserved lock components are rejected before dot folding", {
  root <- withr::local_tempdir()
  reserved_parent <- file.path(root, ".victim-build.lock")
  requested <- file.path(reserved_parent, "..", "app")

  expect_error(.prepareBundleResultTarget(requested), "reserved")
  expect_false(dir.exists(reserved_parent))
})

test_that("Windows trailing aliases cannot hide reserved lock ancestors", {
  root <- withr::local_tempdir()
  reserved_parent <- file.path(root, ".victim-build.lock")

  for (alias_suffix in c(".", " ")) {
    requested <- file.path(
      paste0(reserved_parent, alias_suffix),
      "nested",
      "app"
    )
    expect_error(.prepareBundleResultTarget(requested), "reserved")
  }
  expect_false(dir.exists(reserved_parent))
})

test_that("Windows rooted paths are parsed without the working directory", {
  rooted <- paste0(intToUtf8(92L), "alias", intToUtf8(92L), "app")

  expect_identical(
    .absolutePathStyle(rooted, os_type = "windows"),
    "windows-root"
  )
  expect_true(is.na(.absolutePathStyle(rooted, os_type = "unix")))
  expect_identical(
    .absolutePathComponents(
      rooted,
      style = "windows-root",
      os_type = "windows"
    ),
    list(root = "/", parts = c("alias", "app"))
  )

  resolved <- .resolveNativeSymbolicLinks(
    rooted,
    style = "windows-root",
    os_type = "windows",
    read_link = function(path) "",
    path_exists = function(path) identical(path, "/"),
    normalize_existing = function(path) "C:/",
    entry_exists = function(path) FALSE
  )
  expect_identical(resolved, "C:/alias/app")
})

test_that("Windows native aliases and device namespaces fail closed", {
  for (path in c("C:/app./data", "C:/app /data")) {
    expect_error(
      .resolveNativeSymbolicLinks(
        path,
        style = "drive",
        os_type = "windows",
        read_link = function(path) "",
        path_exists = function(path) FALSE,
        normalize_existing = identity,
        entry_exists = function(path) FALSE
      ),
      "Windows-incompatible"
    )
  }

  backslash <- intToUtf8(92L)
  device_paths <- c(
    paste0(backslash, backslash, "?", backslash, "C:", backslash, "app"),
    paste0(
      backslash,
      backslash,
      ".",
      backslash,
      "C:",
      backslash,
      "app"
    )
  )
  for (path in device_paths) {
    expect_identical(
      .absolutePathStyle(path, os_type = "windows"),
      "windows-device"
    )
    expect_error(
      .canonicalTargetPath(path, os_type = "windows"),
      "device or extended-length"
    )
  }

  malformed_paths <- c(
    "///app/data",
    paste0(backslash, backslash, backslash, "app", backslash, "data"),
    "//server-only",
    "//server///share/app"
  )
  for (path in malformed_paths) {
    expect_identical(
      .absolutePathStyle(path, os_type = "windows"),
      "windows-malformed"
    )
    expect_error(
      .canonicalTargetPath(path, os_type = "windows"),
      "malformed Windows"
    )
  }
})

test_that("Windows path probes use link-aware physical resolution", {
  expect_identical(
    .readNativeLink(
      "C:/work/link",
      os_type = "windows",
      is_link = function(path) TRUE,
      link_path = function(path) "//server/share/target"
    ),
    "//server/share/target"
  )
  expect_identical(
    .readNativeLink(
      "C:/work/regular",
      os_type = "windows",
      is_link = function(path) FALSE,
      link_path = function(path) stop("must not be called")
    ),
    ""
  )
  expect_identical(
    .normalizeExistingPath(
      "Z:/app",
      os_type = "windows",
      path_real = function(path) "//server/share/app"
    ),
    "//server/share/app"
  )

  canonicalize <- function(path) {
    switch(
      path,
      "Z:/app/data/future.h5" = "//server/share/app/data/future.h5",
      "//server/share/app" = "//server/share/app",
      path
    )
  }
  expect_true(.pathWithin(
    "Z:/app/data/future.h5",
    "//server/share/app",
    os_type = "windows",
    canonicalize = canonicalize
  ))
})

test_that("repeated-slash paths cannot hide a reserved symlink ancestor", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  reserved_parent <- file.path(root, ".victim-build.lock")
  dir.create(reserved_parent)
  alias <- file.path(root, "alias")
  linked <- file.symlink(reserved_parent, alias)
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  for (extra_slashes in 1:3) {
    requested <- file.path(
      paste0(strrep("/", extra_slashes), root),
      "alias",
      "nested",
      "app"
    )

    expect_error(.prepareBundleResultTarget(requested), "reserved")
    expect_false(dir.exists(file.path(reserved_parent, "nested")))
  }
})

test_that("broken reparse entries are link-like without readlink support", {
  expect_true(.pathIsSymbolicLink(
    "C:/work/app",
    read_link = function(path) "",
    path_exists = function(path) FALSE,
    entry_exists = function(path) TRUE
  ))
  expect_false(.pathIsSymbolicLink(
    "C:/work/future-app",
    read_link = function(path) "",
    path_exists = function(path) FALSE,
    entry_exists = function(path) FALSE
  ))

  normalize_reparse <- function(path) {
    switch(
      path,
      "C:/work" = "C:/work",
      "C:/work/app" = "D:/foreign/app",
      path
    )
  }
  expect_true(.pathIsSymbolicLink(
    "C:/work/app",
    os_type = "windows",
    read_link = function(path) "",
    path_exists = function(path) TRUE,
    normalize_existing = normalize_reparse,
    entry_exists = function(path) TRUE
  ))

  normalize_case_only <- function(path) {
    switch(
      path,
      "C:/work" = "C:/Work",
      "C:/work/app" = "C:/Work/App",
      path
    )
  }
  expect_false(.pathIsSymbolicLink(
    "C:/work/app",
    os_type = "windows",
    read_link = function(path) "",
    path_exists = function(path) TRUE,
    normalize_existing = normalize_case_only,
    entry_exists = function(path) TRUE
  ))
})

test_that("a non-owner cannot remove a bundle lock", {
  root <- withr::local_tempdir()
  lock <- .acquireBundleLock(file.path(root, "app"))
  withr::defer(.releaseBundleLock(lock))
  intruder <- lock
  intruder$token <- "not-the-owner"

  expect_warning(.releaseBundleLock(intruder), "ownership")
  expect_true(dir.exists(lock$path))
  expect_true(.releaseBundleLock(lock))
})

test_that("unexpected lock contents are preserved for diagnosis", {
  root <- withr::local_tempdir()
  lock <- .acquireBundleLock(file.path(root, "app"))
  extra <- file.path(lock$path, "foreign-file")
  writeLines("FOREIGN", extra)

  expect_warning(.releaseBundleLock(lock), "unexpected")
  expect_true(dir.exists(lock$path))
  expect_identical(readLines(extra), "FOREIGN")
  unlink(extra)
  expect_true(.releaseBundleLock(lock))
})

test_that("metadata directories are never deleted as owned lock files", {
  root <- withr::local_tempdir()
  lock <- .acquireBundleLock(file.path(root, "app"))
  owner_path <- file.path(lock$path, "owner.txt")
  owner <- readLines(owner_path, warn = FALSE)
  unlink(owner_path)
  dir.create(owner_path)
  foreign <- file.path(owner_path, "do-not-delete")
  writeLines("FOREIGN", foreign)

  expect_warning(.releaseBundleLock(lock), "unexpected")
  expect_true(dir.exists(lock$path))
  expect_identical(readLines(foreign), "FOREIGN")

  unlink(foreign)
  expect_true(file.remove(owner_path))
  writeLines(owner, owner_path)
  expect_true(.releaseBundleLock(lock))
})

test_that("special metadata files are rejected before they can block", {
  skip_on_os("windows")
  mkfifo <- Sys.which("mkfifo")
  skip_if(!nzchar(mkfifo), "mkfifo is not available")
  root <- withr::local_tempdir()
  lock <- .acquireBundleLock(file.path(root, "app"))
  token_path <- file.path(lock$path, "owner-token")
  unlink(token_path)
  expect_identical(system2(mkfifo, token_path), 0L)

  inspection <- .inspectBundleLock(lock$path, lock$token)
  expect_false(inspection$valid)
  expect_identical(inspection$kind, "contents")

  unlink(token_path)
  writeLines(lock$token, token_path)
  expect_true(.releaseBundleLock(lock))
})

test_that("lock ownership is revalidated after atomic isolation", {
  root <- withr::local_tempdir()
  lock <- .acquireBundleLock(file.path(root, "app"))
  inspect_lock <- .inspectBundleLock
  inspection_calls <- 0L
  testthat::local_mocked_bindings(
    .inspectBundleLock = function(path, token) {
      inspection_calls <<- inspection_calls + 1L
      result <- inspect_lock(path, token)
      if (identical(inspection_calls, 1L) && isTRUE(result$valid)) {
        writeLines("FOREIGN", file.path(path, "owner-token"))
      }
      result
    },
    .package = "CerebroNexus"
  )

  expect_warning(.releaseBundleLock(lock), "ownership changed")
  expect_identical(inspection_calls, 2L)
  expect_true(dir.exists(lock$path))
  expect_identical(
    readLines(file.path(lock$path, "owner-token")),
    "FOREIGN"
  )
})

test_that("releasing an old lock never removes a newly acquired lock", {
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  old_lock <- .acquireBundleLock(result)
  new_lock <- NULL
  testthat::local_mocked_bindings(
    .isolateBundleLock = function(from, to) {
      moved <- file.rename(from, to)
      if (moved) {
        new_lock <<- .acquireBundleLock(result)
      }
      moved
    },
    .package = "CerebroNexus"
  )

  expect_true(.releaseBundleLock(old_lock))
  expect_s3_class(new_lock, "cerebro_bundle_lock")
  expect_true(dir.exists(new_lock$path))
  expect_identical(
    readLines(file.path(new_lock$path, "owner-token")),
    new_lock$token
  )
  expect_true(.releaseBundleLock(new_lock))
})

test_that("an existing unowned lock is never removed automatically", {
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  lock_path <- .bundleLockPath(result)
  dir.create(lock_path)
  writeLines("UNKNOWN", file.path(lock_path, "foreign-file"))

  expect_error(.acquireBundleLock(result), "already being built")
  expect_true(dir.exists(lock_path))
  expect_identical(
    readLines(file.path(lock_path, "foreign-file")),
    "UNKNOWN"
  )
})

test_that("a failed build releases its own lock", {
  root <- withr::local_tempdir()
  crb <- lock_test_crb(
    root,
    backend = list(type = "h5", location = "missing.h5")
  )
  result <- file.path(root, "app")

  expect_error(lock_test_build(crb, result), "not found")
  expect_false(dir.exists(.bundleLockPath(result)))
  expect_false(file.exists(result))
})

test_that("an existing lock blocks createShinyApp without mutation", {
  root <- withr::local_tempdir()
  crb <- lock_test_crb(root)
  result <- file.path(root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  lock <- .acquireBundleLock(result)
  withr::defer(.releaseBundleLock(lock))

  expect_error(
    lock_test_build(crb, result, overwrite = TRUE),
    "already being built"
  )
  expect_identical(readLines(file.path(result, "marker.txt")), "OLD")
  expect_true(dir.exists(lock$path))
})

test_that("a parent symlink cannot retarget a locked publication", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  first_parent <- file.path(root, "first")
  second_parent <- file.path(root, "second")
  linked_parent <- file.path(root, "current")
  dir.create(first_parent)
  dir.create(second_parent)
  if (!file.symlink(first_parent, linked_parent)) {
    skip("Directory symbolic links are not available on this platform")
  }
  crb <- lock_test_crb(root)
  requested_result <- file.path(linked_parent, "app")
  build_ops <- .bundleBuildOps()
  original_copy <- build_ops$copy
  retargeted <- FALSE
  build_ops$copy <- function(from, to, ...) {
    if (!retargeted) {
      retargeted <<- TRUE
      unlink(linked_parent)
      if (!file.symlink(second_parent, linked_parent)) {
        stop("Failed to retarget the test parent link.")
      }
    }
    original_copy(from, to, ...)
  }
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() build_ops,
    .package = "CerebroNexus"
  )

  expect_warning(
    returned <- lock_test_build(crb, requested_result),
    "changed while the app was being built"
  )
  expect_identical(
    returned,
    normalizePath(
      file.path(first_parent, "app"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_true(file.exists(file.path(first_parent, "app", "app.R")))
  expect_false(file.exists(file.path(second_parent, "app", "app.R")))
  expect_false(dir.exists(.bundleLockPath(file.path(first_parent, "app"))))
  expect_false(dir.exists(.bundleLockPath(file.path(second_parent, "app"))))
})

test_that("stage cleanup completes before the lock is released", {
  root <- withr::local_tempdir()
  crb <- lock_test_crb(root)
  result <- file.path(root, "app")
  build_ops <- .bundleBuildOps()
  original_copy <- build_ops$copy
  build_ops$copy <- function(from, to, ...) {
    if (identical(basename(from), "_bundle_app.R")) {
      stop("injected stage failure")
    }
    original_copy(from, to, ...)
  }
  release_lock <- .releaseBundleLock
  stages_seen_at_release <- NA_integer_
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() build_ops,
    .releaseBundleLock = function(lock) {
      stages_seen_at_release <<- length(list.files(
        root,
        pattern = "^\\.app-stage-",
        all.files = TRUE
      ))
      release_lock(lock)
    },
    .package = "CerebroNexus"
  )

  expect_error(lock_test_build(crb, result), "injected stage failure")
  expect_identical(stages_seen_at_release, 0L)
  expect_false(dir.exists(.bundleLockPath(result)))
})

test_that("stage cleanup errors cannot strand the build lock", {
  root <- withr::local_tempdir()
  crb <- lock_test_crb(root)
  result <- file.path(root, "app")
  build_ops <- .bundleBuildOps()
  original_copy <- build_ops$copy
  build_ops$copy <- function(from, to, ...) {
    if (identical(basename(from), "_bundle_app.R")) {
      stop("injected build failure")
    }
    original_copy(from, to, ...)
  }
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() build_ops,
    .removeBundleStage = function(stage) 1L,
    .package = "CerebroNexus"
  )
  withr::local_options(warn = 2L)

  expect_error(
    lock_test_build(crb, result),
    "Failed to remove the private app staging directory"
  )
  expect_false(dir.exists(.bundleLockPath(result)))
  stages <- list.files(
    root,
    pattern = "^\\.app-stage-",
    full.names = TRUE,
    all.files = TRUE
  )
  expect_length(stages, 1L)
  unlink(stages, recursive = TRUE, force = TRUE)
})

test_that("the bundle lock excludes a second R process", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  result <- file.path(root, "app")
  ready <- file.path(root, "ready")
  release <- file.path(root, "release")
  implementation_source <- test_path(
    "..",
    "..",
    "R",
    "createShinyApp.R"
  )
  path_contract_source <- test_path(
    "..",
    "..",
    "R",
    "bundle_path_contract.R"
  )
  worker <- callr::r_bg(
    function(
      implementation_source,
      path_contract_source,
      result,
      ready,
      release
    ) {
      implementation <- if (file.exists(implementation_source)) {
        environment <- new.env(parent = globalenv())
        sys.source(path_contract_source, envir = environment)
        sys.source(implementation_source, envir = environment)
        environment
      } else {
        loadNamespace("CerebroNexus")
      }
      acquire_lock <- get(".acquireBundleLock", envir = implementation)
      release_lock <- get(".releaseBundleLock", envir = implementation)
      lock <- acquire_lock(result)
      on.exit(release_lock(lock), add = TRUE)
      writeLines("READY", ready)
      deadline <- Sys.time() + 20
      while (!file.exists(release) && Sys.time() < deadline) {
        Sys.sleep(0.05)
      }
      if (!file.exists(release)) {
        stop("Timed out waiting for release signal.")
      }
      invisible(TRUE)
    },
    args = list(
      implementation_source,
      path_contract_source,
      result,
      ready,
      release
    ),
    libpath = .libPaths(),
    supervise = TRUE
  )
  withr::defer({
    if (!file.exists(release)) {
      writeLines("RELEASE", release)
    }
    worker$wait(timeout = 5000)
    if (worker$is_alive()) {
      worker$kill_tree()
    }
  })
  lock_test_wait_for_file(ready, worker)

  expect_error(.acquireBundleLock(result), "already being built")
  expect_true(dir.exists(.bundleLockPath(result)))
  writeLines("RELEASE", release)
  worker$wait(timeout = 5000)
  expect_false(worker$is_alive())
  expect_identical(worker$get_exit_status(), 0L)
  expect_false(dir.exists(.bundleLockPath(result)))
})
