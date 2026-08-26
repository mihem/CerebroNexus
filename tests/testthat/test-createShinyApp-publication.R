publication_test_ops <- function(...) {
  overrides <- list(...)
  defaults <- list(
    access = function(path, mode) file.access(path, mode = mode),
    list_dir = function(path) {
      list.files(path, all.files = TRUE, no.. = TRUE)
    },
    chmod = function(path, mode) Sys.chmod(path, mode = mode),
    rename = function(from, to) file.rename(from, to),
    unlink = function(path, recursive = TRUE, force = TRUE) {
      unlink(path, recursive = recursive, force = force)
    }
  )
  utils::modifyList(defaults, overrides)
}

publication_build_ops <- function(...) {
  overrides <- list(...)
  defaults <- list(
    copy = function(from, to, ...) file.copy(from, to, ...),
    save_rds = function(object, file) saveRDS(object, file),
    write_lines = function(text, connection) writeLines(text, connection)
  )
  utils::modifyList(defaults, overrides)
}

publication_test_tree <- function(root) {
  result <- file.path(root, "app")
  stage <- file.path(root, ".app-stage")
  dir.create(result)
  dir.create(stage)
  writeLines("OLD", file.path(result, "marker.txt"))
  writeLines("NEW", file.path(stage, "marker.txt"))
  list(result = result, stage = stage)
}

publication_test_backups <- function(root) {
  list.files(
    root,
    pattern = "^\\.app-backup-",
    full.names = TRUE,
    all.files = TRUE
  )
}

test_that("bundle staging removes macOS filesystem metadata", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "viewer", "www")
  dir.create(nested, recursive = TRUE)
  writeLines("finder", file.path(root, ".DS_Store"))
  writeLines("finder", file.path(nested, ".DS_Store"))
  writeLines("appledouble", file.path(nested, "._asset.js"))
  writeLines("keep", file.path(nested, "asset.js"))

  expect_true(.removeBundleSystemMetadata(root))
  expect_false(file.exists(file.path(root, ".DS_Store")))
  expect_false(file.exists(file.path(nested, ".DS_Store")))
  expect_false(file.exists(file.path(nested, "._asset.js")))
  expect_true(file.exists(file.path(nested, "asset.js")))
})

publication_expect_stage_failure <- function(failure) {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  dir.create(source)
  crb <- file.path(source, "dataset.crb")
  saveRDS(Cerebro$new(), crb)
  result <- file.path(root, "app")
  dir.create(result)
  writeLines("OLD", file.path(result, "marker.txt"))
  old_mode <- file.info(result)$mode[[1L]]
  normalized_crb <- normalizePath(crb, winslash = "/", mustWork = TRUE)
  ops <- publication_build_ops()
  expected <- switch(
    failure,
    copy = {
      original_copy <- ops$copy
      ops$copy <- function(from, to, ...) {
        normalized_source <- normalizePath(
          unname(from),
          winslash = "/",
          mustWork = TRUE
        )
        if (identical(normalized_source, normalized_crb)) {
          return(FALSE)
        }
        original_copy(from, to, ...)
      }
      "Failed to copy Cerebro data file"
    },
    save = {
      ops$save_rds <- function(object, file) {
        stop("injected config write error")
      }
      "injected config write error"
    },
    write = {
      original_copy <- ops$copy
      ops$copy <- function(from, to, ...) {
        if (identical(basename(from), "_bundle_app.R")) {
          stop("injected app write error")
        }
        original_copy(from, to, ...)
      }
      "injected app write error"
    },
    parse = {
      original_copy <- ops$copy
      ops$copy <- function(from, to, ...) {
        if (identical(basename(from), "_bundle_app.R")) {
          writeLines("shiny::shinyApp(", to)
          return(TRUE)
        }
        original_copy(from, to, ...)
      }
      "Generated app.R is invalid"
    }
  )
  testthat::local_mocked_bindings(
    .bundleBuildOps = function() ops,
    .package = "CerebroNexus",
    .env = parent.frame()
  )

  expect_error(
    createShinyApp(
      cerebro_data = c("Dataset" = crb),
      result_dir = result,
      overwrite = TRUE,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    expected
  )
  expect_identical(readLines(file.path(result, "marker.txt")), "OLD")
  expect_equal(
    as.integer(file.info(result)$mode[[1L]]),
    as.integer(old_mode)
  )
  expect_length(
    list.files(
      root,
      pattern = "^\\.app-(stage|backup)-",
      all.files = TRUE
    ),
    0L
  )
}

test_that("stage copy failure never moves the old deployment", {
  publication_expect_stage_failure("copy")
})

test_that("stage config write failure never moves the old deployment", {
  publication_expect_stage_failure("save")
})

test_that("stage app write failure never moves the old deployment", {
  publication_expect_stage_failure("write")
})

test_that("invalid generated app source never moves the old deployment", {
  publication_expect_stage_failure("parse")
})

test_that("destination inspection rejects inaccessible directories", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  access_calls <- 0L
  list_calls <- 0L
  ops <- publication_test_ops(
    access = function(path, mode) {
      access_calls <<- access_calls + 1L
      -1L
    },
    list_dir = function(path) {
      list_calls <<- list_calls + 1L
      character()
    }
  )

  expect_error(
    .bundleDestinationState(tree$result, overwrite = FALSE, ops = ops),
    "inspect"
  )
  expect_identical(access_calls, 1L)
  expect_identical(list_calls, 0L)
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
})

test_that("destination inspection fails closed on real POSIX permissions", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  old_mode <- file.info(tree$result)$mode[[1L]]
  Sys.chmod(tree$result, mode = "0000")
  withr::defer(Sys.chmod(tree$result, mode = old_mode))
  skip_if(
    file.access(tree$result, mode = 5L) == 0L,
    "The test user bypasses directory permission checks"
  )

  expect_error(
    .bundleDestinationState(tree$result, overwrite = FALSE),
    "not readable and searchable"
  )
})

test_that("destination inspection fails closed on unreadable results", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)

  for (failure in c("warning", "error", "non-character")) {
    calls <- new.env(parent = emptyenv())
    calls$chmod <- 0L
    calls$rename <- 0L
    calls$unlink <- 0L
    list_dir <- switch(
      failure,
      warning = function(path) {
        warning("cannot enumerate")
        character()
      },
      error = function(path) stop("cannot enumerate"),
      `non-character` = function(path) NULL
    )
    ops <- publication_test_ops(
      list_dir = list_dir,
      chmod = function(path, mode) {
        calls$chmod <- calls$chmod + 1L
        TRUE
      },
      rename = function(from, to) {
        calls$rename <- calls$rename + 1L
        TRUE
      },
      unlink = function(path, recursive = TRUE, force = TRUE) {
        calls$unlink <- calls$unlink + 1L
        0L
      }
    )

    expect_error(
      .bundleDestinationState(tree$result, overwrite = FALSE, ops = ops),
      "inspect"
    )
    expect_identical(calls$chmod, 0L)
    expect_identical(calls$rename, 0L)
    expect_identical(calls$unlink, 0L)
    expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
  }
})

test_that("chmod failure leaves the old deployment in place", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  rename_calls <- 0L
  ops <- publication_test_ops(
    chmod = function(path, mode) FALSE,
    rename = function(from, to) {
      rename_calls <<- rename_calls + 1L
      file.rename(from, to)
    }
  )

  expect_error(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "permissions"
  )
  expect_identical(rename_calls, 0L)
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
  expect_true(dir.exists(tree$stage))
  expect_length(publication_test_backups(root), 0L)
})

test_that("a failed backup rename leaves the old deployment in place", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  ops <- publication_test_ops(rename = function(from, to) FALSE)

  expect_error(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "stage the existing app"
  )
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
  expect_true(dir.exists(tree$stage))
  expect_length(publication_test_backups(root), 0L)
})

test_that("backup staging never deletes a foreign destination", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  ops <- publication_test_ops(rename = function(from, to) {
    if (!file.rename(from, to)) {
      stop("Failed to arrange the backup-collision test fixture.")
    }
    dir.create(from)
    writeLines("FOREIGN", file.path(from, "marker.txt"))
    FALSE
  })

  error <- tryCatch(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    error = identity
  )
  backups <- publication_test_backups(root)
  expect_s3_class(error, "error")
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "FOREIGN")
  expect_length(backups, 1L)
  expect_identical(readLines(file.path(backups, "marker.txt")), "OLD")
  expect_match(
    conditionMessage(error),
    normalizePath(tree$result, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
  expect_match(
    conditionMessage(error),
    normalizePath(backups, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
  expect_true(dir.exists(tree$stage))
})

test_that("a failed publish restores the previous deployment", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  Sys.chmod(tree$result, mode = "0711")
  old_mode <- file.info(tree$result)$mode[[1L]]
  rename_calls <- 0L
  ops <- publication_test_ops(rename = function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      return(FALSE)
    }
    file.rename(from, to)
  })

  expect_error(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "previous bundle was restored"
  )
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
  expect_equal(
    as.integer(file.info(tree$result)$mode[[1L]]),
    as.integer(old_mode)
  )
  expect_true(dir.exists(tree$stage))
  expect_length(publication_test_backups(root), 0L)
})

test_that("a publish rename error still restores the previous deployment", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  rename_calls <- 0L
  ops <- publication_test_ops(rename = function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      stop("injected publish rename error")
    }
    file.rename(from, to)
  })

  expect_error(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "previous bundle was restored"
  )
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "OLD")
  expect_true(dir.exists(tree$stage))
  expect_length(publication_test_backups(root), 0L)
})

test_that("a failed restore keeps the previous bundle recoverable", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  rename_calls <- 0L
  ops <- publication_test_ops(rename = function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      return(FALSE)
    }
    if (identical(rename_calls, 3L)) {
      stop("injected restore rename error")
    }
    file.rename(from, to)
  })

  error <- tryCatch(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    error = identity
  )
  backups <- publication_test_backups(root)
  expect_s3_class(error, "error")
  expect_false(file.exists(tree$result))
  expect_length(backups, 1L)
  expect_identical(readLines(file.path(backups, "marker.txt")), "OLD")
  expect_match(
    conditionMessage(error),
    normalizePath(backups, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
})

test_that("rollback never deletes a foreign destination", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  rename_calls <- 0L
  ops <- publication_test_ops(rename = function(from, to) {
    rename_calls <<- rename_calls + 1L
    if (identical(rename_calls, 2L)) {
      dir.create(to)
      writeLines("FOREIGN", file.path(to, "marker.txt"))
      return(FALSE)
    }
    file.rename(from, to)
  })

  error <- tryCatch(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    error = identity
  )
  backups <- publication_test_backups(root)
  expect_s3_class(error, "error")
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "FOREIGN")
  expect_length(backups, 1L)
  expect_identical(readLines(file.path(backups, "marker.txt")), "OLD")
  expect_match(
    conditionMessage(error),
    normalizePath(tree$result, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
  expect_match(
    conditionMessage(error),
    normalizePath(backups, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
})

test_that("successful publication removes the previous backup", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)

  expect_no_warning(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750")
    )
  )
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "NEW")
  expect_length(publication_test_backups(root), 0L)
})

test_that("backup cleanup failure keeps the newly published bundle", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  ops <- publication_test_ops(
    unlink = function(path, recursive = TRUE, force = TRUE) 1L
  )

  expect_warning(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "old backup remains"
  )
  backups <- publication_test_backups(root)
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "NEW")
  expect_length(backups, 1L)
  expect_identical(readLines(file.path(backups, "marker.txt")), "OLD")
})

test_that("backup cleanup errors keep the newly published bundle", {
  root <- withr::local_tempdir()
  tree <- publication_test_tree(root)
  ops <- publication_test_ops(
    unlink = function(path, recursive = TRUE, force = TRUE) {
      stop("injected backup cleanup error")
    }
  )

  expect_warning(
    .publishBundleStage(
      tree$stage,
      tree$result,
      overwrite = TRUE,
      publish_mode = as.octmode("0750"),
      ops = ops
    ),
    "old backup remains"
  )
  backups <- publication_test_backups(root)
  expect_identical(readLines(file.path(tree$result, "marker.txt")), "NEW")
  expect_length(backups, 1L)
  expect_identical(readLines(file.path(backups, "marker.txt")), "OLD")
})
