builder_publish_source <- function(local = parent.frame()) {
  root <- builder_test_inst_path("builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
}

## Crash tests start a fresh R process that sources the Builder and then
## exits mid-publish. On a busy machine that can take well over ten seconds;
## wait for the exit itself (bounded), and never leave the process behind.
builder_publish_await_exit <- function(
  process,
  timeout = 120,
  envir = parent.frame()
) {
  withr::defer(
    if (process$is_alive()) try(process$kill(), silent = TRUE),
    envir = envir
  )
  deadline <- Sys.time() + timeout
  while (process$is_alive() && Sys.time() < deadline) {
    process$wait(timeout = 1000)
  }
  invisible(process)
}

write_builder_release_record_fixture <- function(root, members) {
  lines <- c(
    "CEREBRO_BUILDER_RELEASE_V1",
    vapply(
      members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
  )
  writeBin(
    charToRaw(paste0(paste(lines, collapse = "\n"), "\n")),
    file.path(root, ".cerebro-builder-release-v1")
  )
}

test_that("native file operation diagnostics retain action paths and failures", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    source <- file.path(root, "source")
    destination <- file.path(root, "destination")
    warning_attempt <- .nativeFileOperationAttempt(
      action = "install prepared release",
      source = source,
      destination = destination,
      operation = function() {
        warning("Access is denied (Win32 error 5).", call. = FALSE)
        FALSE
      }
    )
    warning_message <- .nativeFileOperationMessage(warning_attempt)

    expect_false(warning_attempt$ok)
    expect_match(warning_message, "install prepared release", fixed = TRUE)
    expect_match(
      warning_message,
      normalizePath(
        source,
        winslash = "/",
        mustWork = FALSE
      ),
      fixed = TRUE
    )
    expect_match(
      warning_message,
      normalizePath(
        destination,
        winslash = "/",
        mustWork = FALSE
      ),
      fixed = TRUE
    )
    expect_match(warning_message, "Win32 error 5", fixed = TRUE)

    error_attempt <- .nativeFileOperationAttempt(
      action = "protect prior release",
      source = source,
      destination = destination,
      operation = function() {
        stop("sharing violation (Win32 error 32)", call. = FALSE)
      }
    )
    error_message <- .nativeFileOperationMessage(error_attempt)
    expect_false(error_attempt$ok)
    expect_match(error_message, "sharing violation", fixed = TRUE)
    expect_match(error_message, "Win32 error 32", fixed = TRUE)
  })
})

test_that("release moves retry only while the atomic state is unambiguous", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    source <- file.path(root, "source")
    destination <- file.path(root, "destination")
    writeLines("payload", source)
    calls <- 0L
    sleeps <- numeric()
    moved <- .builder_release_move_attempt(
      source,
      destination,
      action = "install prepared release",
      .move = function(from, to) {
        calls <<- calls + 1L
        if (calls < 3L) {
          return(FALSE)
        }
        file.rename(from, to)
      },
      .sleep = function(delay) sleeps <<- c(sleeps, delay),
      .retry_delays = c(0, 0.01, 0.02)
    )

    expect_true(moved$ok)
    expect_identical(moved$attempt_count, 3L)
    expect_identical(sleeps, c(0.01, 0.02))
    expect_false(file.exists(source))
    expect_true(file.exists(destination))
    expect_match(.nativeFileOperationMessage(moved), "attempts=3", fixed = TRUE)

    writeLines("new", source)
    unlink(destination)
    calls <- 0L
    collision <- .builder_release_move_attempt(
      source,
      destination,
      action = "install prepared release",
      .move = function(from, to) {
        calls <<- calls + 1L
        writeLines("other owner", to)
        FALSE
      },
      .sleep = function(delay) stop("unsafe retry"),
      .retry_delays = c(0, 0.01, 0.02)
    )
    expect_false(collision$ok)
    expect_identical(calls, 1L)
    expect_identical(collision$attempt_count, 1L)
    expect_identical(readLines(source), "new")
    expect_identical(readLines(destination), "other owner")
  })
})

test_that("release move retry policy rejects invalid delays", {
  local({
    builder_publish_source()
    expect_error(
      .builder_release_move_attempt(
        "source",
        "destination",
        "move release",
        .move = function(...) FALSE,
        .retry_delays = c(0, NA_real_)
      ),
      "retry delays are invalid",
      fixed = TRUE
    )
  })
})

test_that("release existence includes dangling lexical entries", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "dangling")
    outside <- file.path(root, "missing")
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    expect_true(.builder_release_exists(target))
    expect_identical(Sys.readlink(target), outside)
    expect_false(file.exists(outside))
  })
})

test_that("release relative paths share one canonical root", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    nested <- file.path(root, "nested")
    dir.create(nested)
    paths <- c(file.path(root, "one"), file.path(nested, "two"))

    expect_identical(
      unname(.builder_release_relative(paths, root)),
      c("one", file.path("nested", "two"))
    )
    expect_error(
      .builder_release_relative(c(paths, dirname(root)), root),
      "escaped its owned root"
    )
  })
})

test_that("release identity ignores Finder metadata but not other dotfiles", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    nested <- file.path(release, "nested")
    dir.create(nested, recursive = TRUE)
    writeLines("payload", file.path(release, "dataset.crb"))
    writeLines("finder", file.path(release, ".DS_Store"))
    writeLines("finder", file.path(nested, ".DS_Store"))
    writeLines("foreign", file.path(release, ".unknown"))

    identity <- builder_release_identity(release)
    paths <- vapply(identity$entries, `[[`, character(1), "path")

    expect_setequal(paths, c(".unknown", "dataset.crb", "nested"))
    expect_false(any(basename(paths) == ".DS_Store"))
  })
})

test_that("release identity reports each file without changing the identity", {
  local({
    builder_publish_source()
    release <- file.path(withr::local_tempdir(), "release")
    dir.create(file.path(release, "nested"), recursive = TRUE)
    writeLines("a", file.path(release, "a.crb"))
    writeLines("b", file.path(release, "nested", "b.rds"))
    writeLines("c", file.path(release, "nested", "c.rds"))

    seen <- character()
    identity <- builder_release_identity(
      release,
      .on_file = function(index, total) {
        seen <<- c(seen, paste0(index, "/", total))
      }
    )

    expect_identical(seen, c("0/3", "1/3", "2/3", "3/3"))
    expect_identical(identity, builder_release_identity(release))
  })
})

test_that("output preflight reports only foreign top-level members", {
  local({
    builder_publish_source()
    release <- withr::local_tempdir()
    writeLines("crb", file.path(release, "dataset.crb"))
    writeLines("finder", file.path(release, ".DS_Store"))
    writeLines("foreign", file.path(release, "notes.txt"))

    checked <- builder_release_output_preflight(
      release,
      expected_roots = "dataset.crb"
    )

    expect_false(checked$ok)
    expect_identical(checked$foreign, "notes.txt")
    expect_identical(
      checked$prior_state$identity,
      builder_release_state(
        release,
        exact_record = FALSE,
        allow_abandoned = TRUE
      )$identity
    )
    unlink(file.path(release, "notes.txt"))
    expect_true(
      builder_release_output_preflight(
        release,
        expected_roots = "dataset.crb"
      )$ok
    )
  })
})

test_that("release ownership records require canonical recursive members", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(file.path(release, "cerebro_app", "www"), recursive = TRUE)
    writeLines("crb", file.path(release, "01-a.crb"))
    writeLines("app", file.path(release, "cerebro_app", "app.R"))
    writeLines("asset", file.path(release, "cerebro_app", "www", "asset.txt"))
    members <- list(
      list(type = "D", path = "cerebro_app"),
      list(type = "D", path = "cerebro_app/www"),
      list(type = "F", path = "01-a.crb"),
      list(type = "F", path = "cerebro_app/app.R"),
      list(type = "F", path = "cerebro_app/www/asset.txt")
    )
    write_builder_release_record_fixture(release, members)

    record <- .builder_release_read_record(release)

    expect_identical(record$members, members)
    expect_type(record$bytes, "raw")
    expect_identical(record$identity, builder_release_identity(release))
  })
})

test_that("ownership records round-trip sorted UTF-8 paths", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    paths <- enc2utf8(c("a.crb", "é.crb", "中文.crb"))
    for (path in paths) {
      writeLines(path, file.path(release, path), useBytes = TRUE)
    }
    identity <- builder_release_identity(release)

    record <- .builder_release_write_record(
      release,
      identity,
      token = "utf8-round-trip"
    )

    expected_lines <- sort(enc2utf8(paste0("F\t", paths)), method = "radix")
    expected_paths <- sub("^F\t", "", expected_lines)
    record_paths <- vapply(record$members, `[[`, character(1), "path")
    expect_identical(record_paths, expected_paths)
    expect_true(all(validUTF8(record_paths)))
    non_ascii <- grepl("[^ -~]", record_paths)
    expect_true(all(Encoding(record_paths[non_ascii]) == "UTF-8"))
    expect_identical(
      vapply(.builder_release_read_record(release)$members, `[[`, "", "path"),
      expected_paths
    )
  })
})

test_that("malformed ownership records fail closed", {
  local({
    builder_publish_source()
    cases <- list(
      header = c("CEREBRO_BUILDER_RELEASE_V2", "F\tdataset.crb"),
      blank = c("CEREBRO_BUILDER_RELEASE_V1", ""),
      tag = c("CEREBRO_BUILDER_RELEASE_V1", "X\tdataset.crb"),
      delimiter = c("CEREBRO_BUILDER_RELEASE_V1", "F dataset.crb"),
      dot = c("CEREBRO_BUILDER_RELEASE_V1", "F\t./dataset.crb"),
      parent = c("CEREBRO_BUILDER_RELEASE_V1", "F\ta/../dataset.crb"),
      duplicate = c(
        "CEREBRO_BUILDER_RELEASE_V1",
        "F\tdataset.crb",
        "F\tdataset.crb"
      ),
      absolute = c("CEREBRO_BUILDER_RELEASE_V1", "F\t/tmp/dataset.crb"),
      device = c("CEREBRO_BUILDER_RELEASE_V1", "F\tCON"),
      control = c("CEREBRO_BUILDER_RELEASE_V1", "F\tbad\001name")
    )
    for (case in cases) {
      root <- withr::local_tempdir()
      release <- file.path(root, "release")
      dir.create(release)
      writeLines("crb", file.path(release, "dataset.crb"))
      writeLines(
        case,
        file.path(release, ".cerebro-builder-release-v1"),
        useBytes = TRUE
      )
      expect_error(
        .builder_release_read_record(release),
        "ownership record",
        info = paste(case, collapse = " | ")
      )
    }
  })
})

test_that("ownership records reject extra blank lines", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeBin(
      charToRaw("CEREBRO_BUILDER_RELEASE_V1\n\n"),
      file.path(release, ".cerebro-builder-release-v1")
    )

    expect_error(
      .builder_release_read_record(release),
      "blank member"
    )
  })
})

test_that("ownership records reject trailing member delimiters", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    writeBin(
      charToRaw(
        "CEREBRO_BUILDER_RELEASE_V1\nF\tdataset.crb\t\n"
      ),
      file.path(release, ".cerebro-builder-release-v1")
    )

    expect_error(
      .builder_release_read_record(release),
      "malformed member"
    )
  })
})

test_that("ownership records reject identity mismatches and links", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()

    missing <- file.path(root, "missing")
    dir.create(missing)
    write_builder_release_record_fixture(
      missing,
      list(list(type = "F", path = "missing.crb"))
    )
    expect_error(.builder_release_read_record(missing), "ownership record")

    wrong_type <- file.path(root, "wrong-type")
    dir.create(file.path(wrong_type, "dataset.crb"), recursive = TRUE)
    write_builder_release_record_fixture(
      wrong_type,
      list(list(type = "F", path = "dataset.crb"))
    )
    expect_error(.builder_release_read_record(wrong_type), "ownership record")

    hidden <- file.path(root, "hidden")
    dir.create(hidden)
    writeLines("crb", file.path(hidden, "dataset.crb"))
    writeLines("foreign", file.path(hidden, ".unknown"))
    write_builder_release_record_fixture(
      hidden,
      list(list(type = "F", path = "dataset.crb"))
    )
    expect_error(.builder_release_read_record(hidden), "ownership record")

    linked <- file.path(root, "linked")
    dir.create(linked)
    writeLines("crb", file.path(linked, "dataset.crb"))
    link_ok <- tryCatch(
      file.symlink("dataset.crb", file.path(linked, "alias.crb")),
      error = function(error) FALSE
    )
    if (isTRUE(link_ok)) {
      write_builder_release_record_fixture(
        linked,
        list(
          list(type = "F", path = "alias.crb"),
          list(type = "F", path = "dataset.crb")
        )
      )
      expect_error(.builder_release_read_record(linked), "symbolic link")
    }
  })
})

test_that("deleted releases are recognized as reusable empty shells", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    write_builder_release_record_fixture(
      release,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "F", path = "cerebro_app/app.R"),
        list(type = "F", path = "dataset.crb")
      )
    )
    writeLines("finder", file.path(release, ".DS_Store"))

    state <- builder_release_state(
      release,
      exact_record = FALSE,
      allow_abandoned = TRUE
    )

    expect_true(state$record$abandoned)
    expect_length(state$record$foreign, 0L)
    expect_identical(
      vapply(state$record$members, `[[`, character(1), "path"),
      c("cerebro_app", "cerebro_app/app.R", "dataset.crb")
    )
  })
})

test_that("partially deleted releases are never treated as empty shells", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("still here", file.path(release, "dataset.crb"))
    write_builder_release_record_fixture(
      release,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "F", path = "cerebro_app/app.R"),
        list(type = "F", path = "dataset.crb")
      )
    )

    expect_error(
      builder_release_state(
        release,
        exact_record = FALSE,
        allow_abandoned = TRUE
      ),
      "does not match its recorded members"
    )
  })
})

test_that("ownership record replacement is atomic", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "atomic-test",
        .move = function(from, to) FALSE
      ),
      "atomically"
    )
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_identical(readLines(file.path(release, "dataset.crb")), "crb")
    expect_length(list.files(release, all.files = TRUE, no.. = TRUE), 1L)
  })
})

test_that("record temp creation never follows a dangling symlink", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    token <- "dangling-test"
    temporary <- file.path(
      release,
      paste0(".o-", token, ".tmp")
    )
    outside <- file.path(root, "outside.txt")
    link_ok <- tryCatch(
      file.symlink(outside, temporary),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(link_ok))

    expect_error(
      .builder_release_write_record(release, identity, token),
      "temporary"
    )
    expect_false(file.exists(outside))
    expect_true(.builder_release_link(temporary))
  })
})

test_that("record temp replacement cannot write through an outside link", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    probe_source <- file.path(root, "symlink-source")
    probe_link <- file.path(root, "symlink-probe")
    writeLines("probe", probe_source)
    link_available <- suppressWarnings(file.symlink(probe_source, probe_link))
    if (isTRUE(link_available)) {
      unlink(probe_link, force = TRUE)
    }
    skip_if_not(isTRUE(link_available), "Symbolic links are unavailable")
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside.txt")
    writeLines("keep me", outside)
    replaced <- FALSE

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "replacement-test",
        .after_create = function(temporary) {
          unlink(temporary, force = TRUE)
          replaced <<- isTRUE(file.symlink(outside, temporary))
        }
      ),
      "temporary.*unsafe"
    )
    expect_true(replaced)
    expect_identical(readLines(outside), "keep me")
  })
})

test_that("record publication rejects a hard-link swap during rename", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside-record")

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "hard-link-swap",
        .move = function(from, to) {
          file.copy(from, outside)
          unlink(from, force = TRUE)
          linked <- file.link(outside, from)
          isTRUE(linked) && file.rename(from, to)
        }
      ),
      "changed during publication"
    )
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_true(file.exists(outside))
  })
})

test_that("record publication tolerates metadata changes caused by rename", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)

    record <- .builder_release_write_record(
      release,
      identity,
      token = "metadata-changing-rename",
      .move = function(from, to) {
        moved <- file.rename(from, to)
        if (isTRUE(moved)) {
          Sys.setFileTime(to, Sys.time() + 120)
        }
        moved
      }
    )

    expect_identical(
      record$bytes,
      readBin(
        file.path(release, ".cerebro-builder-release-v1"),
        what = "raw",
        n = file.info(file.path(
          release,
          ".cerebro-builder-release-v1"
        ))$size
      )
    )
    state <- builder_release_state(release)
    expect_true(is.list(state$record))
    expect_identical(state$record$bytes, record$bytes)
  })
})

test_that("record publication still rejects changed bytes after rename", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "content-changing-rename",
        .move = function(from, to) {
          moved <- file.rename(from, to)
          if (isTRUE(moved)) {
            writeBin(charToRaw("changed"), to)
          }
          moved
        }
      ),
      "changed during publication",
      fixed = TRUE
    )
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("record publication rejects a hard-link swap during final parsing", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    outside <- file.path(root, "outside-record")
    original_identity <- builder_release_identity
    swapped <- FALSE
    builder_release_identity <- function(target, ...) {
      record <- file.path(target, ".cerebro-builder-release-v1")
      if (!swapped && file.exists(record)) {
        file.copy(record, outside)
        unlink(record, force = TRUE)
        swapped <<- isTRUE(file.link(outside, record))
      }
      original_identity(target, ...)
    }

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "hard-link-final-parse"
      ),
      "changed during publication"
    )
    expect_true(swapped)
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
    expect_true(file.exists(outside))
  })
})

test_that("failed final record parsing removes the unpublished record", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    original_identity <- builder_release_identity
    corrupted <- FALSE
    builder_release_identity <- function(target, ...) {
      record <- file.path(target, ".cerebro-builder-release-v1")
      if (!corrupted && file.exists(record)) {
        writeLines("bad", record)
        corrupted <<- TRUE
      }
      original_identity(target, ...)
    }

    expect_error(
      .builder_release_write_record(
        release,
        identity,
        token = "failed-final-parse"
      ),
      "ownership record|changed during publication"
    )
    expect_true(corrupted)
    expect_false(file.exists(file.path(
      release,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("ownership records remain owner-only under a permissive umask", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("crb", file.path(release, "dataset.crb"))
    identity <- builder_release_identity(release)
    old_umask <- Sys.umask("0000")
    withr::defer(Sys.umask(old_umask))

    .builder_release_write_record(
      release,
      identity,
      token = "owner-only"
    )

    expect_identical(
      as.octmode(
        file.info(file.path(
          release,
          ".cerebro-builder-release-v1"
        ))$mode
      ),
      as.octmode("600")
    )
  })
})

test_that("release identity is complete, stable, and rejects links", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "matrix"), recursive = TRUE)
    writeLines("crb", file.path(target, "dataset.crb"))
    writeLines("matrix", file.path(target, "matrix", "part.bin"))

    first <- builder_release_identity(target)
    second <- builder_release_identity(target)
    expect_identical(first, second)
    expect_identical(
      vapply(first$entries, `[[`, character(1), "path"),
      c("dataset.crb", "matrix", "matrix/part.bin")
    )
    expect_identical(
      vapply(first$entries, `[[`, character(1), "type"),
      c("file", "directory", "file")
    )

    linked <- file.path(target, "linked")
    linked_ok <- tryCatch(
      file.symlink("dataset.crb", linked),
      error = function(e) FALSE
    )
    if (isTRUE(linked_ok)) {
      expect_error(builder_release_identity(target), "symbolic link")
    }
  })
})

test_that("release state reuses content digests for its confirmation scan", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("crb", file.path(target, "dataset.crb"))
    original_md5 <- .builder_release_payload_md5
    calls <- 0L
    .builder_release_payload_md5 <- function(...) {
      calls <<- calls + 1L
      original_md5(...)
    }

    state <- builder_release_state(target, exact_record = FALSE)

    expect_true(state$identity$exists)
    expect_identical(calls, 1L)
  })
})

test_that("release identity rejects a file replaced by a directory", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    slot <- file.path(target, "slot")
    hidden <- file.path(slot, "hidden.txt")
    dir.create(target)
    writeLines("ordinary file", slot)
    calls <- 0L
    swap_after_first_list <- function(path) {
      calls <<- calls + 1L
      if (identical(calls, 2L)) {
        unlink(slot)
        dir.create(slot)
        writeLines("hidden", hidden)
      }
      .builder_release_list_directory(path)
    }

    attempt <- tryCatch(
      builder_release_identity(
        target,
        .list_directory = swap_after_first_list
      ),
      error = function(error) error
    )
    if (!inherits(attempt, "error")) {
      paths <- vapply(attempt$entries, `[[`, character(1), "path")
      expect_identical(paths, "slot")
      expect_false("slot/hidden.txt" %in% paths)
    }
    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "changed after enumeration")
    }
    expect_true(file.exists(hidden))
  })
})

test_that("release identity rejects a same-name directory replacement", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    slot <- file.path(target, "slot")
    hidden <- file.path(slot, "hidden.txt")
    replacement <- file.path(root, "replacement")
    replacement_hidden <- file.path(replacement, "hidden.txt")
    dir.create(slot, recursive = TRUE)
    writeLines("old", hidden)
    original <- fs::file_info(slot, follow = FALSE)
    dir.create(replacement)
    writeLines("new", replacement_hidden)
    replacement_info <- fs::file_info(replacement, follow = FALSE)
    expect_false(identical(
      as.double(original$inode),
      as.double(replacement_info$inode)
    ))
    calls <- 0L
    replace_before_confirmation <- function(path) {
      calls <<- calls + 1L
      if (identical(calls, 3L)) {
        unlink(slot, recursive = TRUE)
        if (!file.rename(replacement, slot)) {
          stop("The replacement fixture could not be moved.")
        }
      }
      .builder_release_list_directory(path)
    }

    attempt <- tryCatch(
      builder_release_identity(
        target,
        .list_directory = replace_before_confirmation
      ),
      error = function(error) error
    )
    if (!inherits(attempt, "error")) {
      paths <- vapply(attempt$entries, `[[`, character(1), "path")
      expect_identical(paths, c("slot", "slot/hidden.txt"))
    }
    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "changed after enumeration")
    }
    expect_identical(readLines(hidden), "new")
  })
})

test_that("release identity rejects special filesystem nodes without blocking", {
  skip_on_os("windows")
  skip_if_not_installed("callr")
  skip_if(Sys.which("mkfifo") == "")
  local({
    builder_root <- builder_test_inst_path("builder")
    if (!dir.exists(builder_root)) {
      builder_root <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    expect_identical(
      system2("mkfifo", file.path(target, ".cerebro-builder-release-v1")),
      0L
    )
    process <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        tryCatch(
          builder_release_state(target),
          error = function(error) conditionMessage(error)
        )
      },
      list(builder_root, target)
    )
    on.exit(if (process$is_alive()) process$kill(), add = TRUE)
    process$wait(timeout = 1500)
    finished <- !process$is_alive()
    if (!finished) {
      process$kill()
    }

    expect_true(finished, info = "release identity blocked on a FIFO")
    if (finished) {
      expect_match(process$get_result(), "unsupported filesystem entry")
    }
  })
})

test_that("release identity rejects an unreadable release root", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("foreign", file.path(release, "foreign.txt"))
    Sys.chmod(release, mode = "0000")
    withr::defer(Sys.chmod(release, mode = "0700"))
    skip_if(file.access(release, mode = 5L) == 0L)

    expect_error(
      builder_release_identity(release),
      "enumerate"
    )
  })
})

test_that("release identity rejects unreadable nested directories", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    secret <- file.path(release, "secret")
    dir.create(secret, recursive = TRUE)
    writeLines("foreign", file.path(secret, "foreign.txt"))
    Sys.chmod(secret, mode = "0000")
    withr::defer(Sys.chmod(secret, mode = "0700"))
    skip_if(file.access(secret, mode = 5L) == 0L)

    expect_error(
      builder_release_identity(release),
      "enumerate"
    )
  })
})

test_that("release identity rejects an invalid payload hash", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    dir.create(release)
    writeLines("payload", file.path(release, "dataset.crb"))

    expect_error(
      builder_release_identity(
        release,
        .hash_file = function(path) NA_character_
      ),
      "read safely"
    )
  })
})

test_that("release identity rejects payload changes during hashing", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    release <- file.path(root, "release")
    payload <- file.path(release, "dataset.crb")
    dir.create(release)
    writeLines("before", payload)
    mutate_while_hashing <- function(path) {
      md5 <- .builder_release_payload_md5(path)
      writeLines("changed after hashing", path)
      md5
    }

    expect_error(
      builder_release_identity(release, .hash_file = mutate_while_hashing),
      "changed while its identity was read"
    )
  })
})

test_that("clean release preparation initializes control once", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    original_ensure <- .builder_release_ensure_control
    calls <- 0L
    .builder_release_ensure_control <- function(target) {
      calls <<- calls + 1L
      original_ensure(target)
    }

    handle <- builder_prepare_release(target, "build-clean-control")

    expect_identical(calls, 1L)
    expect_true(builder_abort_release(handle)$aborted)
  })
})

test_that("compare-and-swap rejects a release changed after Review", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("reviewed", file.path(target, "dataset.crb"))
    expected <- builder_release_identity(target)
    handle <- builder_prepare_release(target, "build-cas", expected)
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    writeLines("changed", file.path(target, "dataset.crb"))

    expect_error(
      builder_publish_release(handle),
      "changed after Review"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "changed")
    expect_true(dir.exists(handle$stage))
    builder_abort_release(handle)
  })
})

test_that("release stages use bounded names and guard before payload creation", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    handle <- builder_prepare_release(
      target,
      paste(rep("long-build-identity", 20L), collapse = "-")
    )

    expect_match(basename(handle$stage), "^s-[[:xdigit:]]+$")
    expect_lte(nchar(basename(handle$stage), type = "bytes"), 32L)
    expect_false(grepl("\\\\", handle$stage))
    expect_true(builder_abort_release(handle)$aborted)

    guarded_target <- file.path(root, "guarded")
    control <- builder_release_control_path(guarded_target)
    expect_error(
      builder_prepare_release(
        guarded_target,
        "guarded-build",
        .stage_guard = function(stage) {
          stop("path-budget-sentinel", call. = FALSE)
        }
      ),
      "path-budget-sentinel"
    )
    expect_false(dir.exists(file.path(control, "lock")))
    expect_length(
      list.files(file.path(control, "stages"), all.files = TRUE, no.. = TRUE),
      0L
    )
  })
})

test_that("release control paths do not repeat long output names", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, paste0("release-", strrep("x", 180L)))
    control <- builder_release_control_path(target)

    expect_match(basename(control), "^[.]cbr-[0-9a-f]{16}$")
    expect_identical(nchar(basename(control), type = "bytes"), 21L)
    expect_false(grepl(basename(target), control, fixed = TRUE))

    handle <- builder_prepare_release(target, strrep("build-name", 40L))
    expect_identical(handle$control, control)
    expect_lte(nchar(basename(handle$stage), type = "bytes"), 16L)
    expect_true(builder_abort_release(handle)$aborted)
  })
})

test_that("legacy long control directories remain recoverable", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "legacy-release")
    handle <- builder_prepare_release(target, "legacy-build")
    legacy <- .builder_release_legacy_control_path(target)
    expect_true(file.rename(handle$control, legacy))

    journal_path <- file.path(legacy, "journal.rds")
    journal <- readRDS(journal_path)
    journal$control <- legacy
    journal$stage <- file.path(legacy, "stages", basename(handle$stage))
    journal$backup <- file.path(legacy, "backup")
    journal$journal <- journal_path
    journal$lock <- file.path(legacy, "lock")
    saveRDS(journal, journal_path, version = 3L)
    .builder_release_pid_alive <- function(pid) FALSE

    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$state, "recovery_required")
    expect_identical(recovery$control, legacy)
    expect_true(builder_recover_release(target, action = "abort")$recovered)

    replacement <- builder_prepare_release(target, "new-build")
    expect_identical(
      replacement$control,
      builder_release_control_path(target)
    )
    expect_true(builder_abort_release(replacement)$aborted)
  })
})

test_that("stage removal is verified and uses extended Windows paths", {
  local({
    builder_publish_source()
    expect_identical(
      .builder_release_io_path("C:/stage/payload", os_type = "windows"),
      "\\\\?\\C:\\stage\\payload"
    )
    expect_error(
      {
        stage <- file.path(withr::local_tempdir(), "stage")
        dir.create(stage)
        writeLines("payload", file.path(stage, "payload.txt"))
        .builder_release_remove_stage(
          stage,
          .unlink = function(...) 1L,
          os_type = "windows"
        )
      },
      "could not be removed safely"
    )
  })
})

test_that("prepare removes a journal-owned aborted stage", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    abandoned <- builder_prepare_release(target, "abandoned-build")
    writeLines("partial", file.path(abandoned$stage, "partial.crb"))
    abandoned <- .builder_release_write_phase(abandoned, "aborted")
    .builder_release_release_lock(
      abandoned$control,
      abandoned$lock,
      abandoned$token
    )

    replacement <- builder_prepare_release(target, "replacement-build")
    expect_false(dir.exists(abandoned$stage))
    expect_true(dir.exists(replacement$stage))
    expect_true(builder_abort_release(replacement)$aborted)
  })
})

test_that("a locked Windows release root publishes in place and can be rebuilt", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)

    publish_with_locked_root <- function(value, build_id) {
      handle <- builder_prepare_release(
        target,
        build_id,
        builder_release_identity(target)
      )
      writeLines(value, file.path(handle$stage, "dataset.crb"))
      locked_root <- function(from, to) {
        if (
          identical(.canonicalTargetPath(from), .canonicalTargetPath(target)) &&
            identical(
              .canonicalTargetPath(to),
              .canonicalTargetPath(handle$backup)
            )
        ) {
          return(FALSE)
        }
        file.rename(from, to)
      }
      phases <- character()
      published <- builder_publish_release(
        handle,
        .move = locked_root,
        .allow_in_place = TRUE,
        .verify_payload = function(root, phase) {
          phases <<- c(phases, phase)
          TRUE
        }
      )
      published$verification_phases <- phases
      published
    }

    first <- publish_with_locked_root("first", "build-locked-first")
    expect_true(first$published)
    expect_identical(readLines(file.path(target, "dataset.crb")), "first")
    expect_identical(first$publication_mode, "in_place")
    expect_true(first$control_cleaned)
    expect_false(dir.exists(builder_release_control_path(target)))
    expect_identical(
      first$verification_phases,
      c("before_rename", "after_in_place")
    )

    second <- publish_with_locked_root("second", "build-locked-second")
    expect_true(second$published)
    expect_identical(readLines(file.path(target, "dataset.crb")), "second")
    expect_identical(
      second$verification_phases,
      c("before_rename", "after_in_place")
    )
    expect_false(dir.exists(file.path(
      builder_release_control_path(target),
      "backup"
    )))
  })
})

test_that("failed in-place publication restores the prior release", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-locked-restore",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    locked_and_fail_install <- function(from, to) {
      if (
        identical(.canonicalTargetPath(from), .canonicalTargetPath(target)) &&
          identical(
            .canonicalTargetPath(to),
            .canonicalTargetPath(handle$backup)
          )
      ) {
        return(FALSE)
      }
      if (
        identical(dirname(.canonicalTargetPath(from)), handle$stage) &&
          identical(dirname(.canonicalTargetPath(to)), handle$target)
      ) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    expect_error(
      builder_publish_release(
        handle,
        .move = locked_and_fail_install,
        .allow_in_place = TRUE
      ),
      "prior release was restored"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
    expect_false(dir.exists(handle$backup))
  })
})

test_that("publication failures retain the native install diagnostic", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-native-diagnostic",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    move <- function(from, to) {
      if (
        identical(.canonicalTargetPath(from), handle$stage) &&
          identical(.canonicalTargetPath(to), handle$target)
      ) {
        warning("sharing violation (Win32 error 32)", call. = FALSE)
        return(FALSE)
      }
      file.rename(from, to)
    }

    failure <- tryCatch(
      builder_publish_release(
        handle,
        .move = move,
        .allow_in_place = FALSE
      ),
      error = identity
    )

    expect_s3_class(failure, "error")
    message <- conditionMessage(failure)
    expect_match(message, "prior release was restored", fixed = TRUE)
    expect_match(message, "install prepared release", fixed = TRUE)
    expect_match(message, handle$stage, fixed = TRUE)
    expect_match(message, handle$target, fixed = TRUE)
    expect_match(message, "Win32 error 32", fixed = TRUE)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("failed post-install verification restores an in-place release", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-in-place-guard",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    locked_root <- function(from, to) {
      if (
        identical(.canonicalTargetPath(from), .canonicalTargetPath(target)) &&
          identical(
            .canonicalTargetPath(to),
            .canonicalTargetPath(handle$backup)
          )
      ) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    expect_error(
      builder_publish_release(
        handle,
        .move = locked_root,
        .allow_in_place = TRUE,
        .verify_payload = function(root, phase) {
          !identical(phase, "after_in_place")
        }
      ),
      "verification failed"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
    expect_identical(readLines(file.path(handle$stage, "dataset.crb")), "new")
  })
})

test_that("process death between renames leaves exact recoverable evidence", {
  skip_if_not_installed("callr")
  local({
    root_path <- builder_test_inst_path("builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        expected <- builder_release_identity(target)
        handle <- builder_prepare_release(target, "build-crash", expected)
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "old_moved")) {
              quit(save = "no", status = 17L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    builder_publish_await_exit(crash)
    expect_identical(crash$get_exit_status(), 17L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$state, "recovery_required")
    expect_true(recovery$backup_exists)
    expect_match(recovery$message, "backup was preserved", fixed = TRUE)
    expect_true(dir.exists(recovery$backup))
    expect_false(dir.exists(target))

    restored <- builder_recover_release(target, action = "restore")
    expect_true(restored$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("a dead release that never moved output is cleared automatically", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    abandoned <- builder_prepare_release(
      target,
      "build-abandoned-before-publish",
      builder_release_identity(target)
    )
    owner <- readRDS(file.path(abandoned$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(abandoned$lock, "owner.rds"))

    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "prepared")
    expect_false(recovery$backup_exists)
    expect_true(recovery$safe_to_abort)
    expect_match(recovery$message, "No previous-release backup", fixed = TRUE)

    replacement <- builder_prepare_release(
      target,
      "build-after-abandoned",
      builder_release_identity(target)
    )
    expect_s3_class(replacement, "builder_release_handle")
    expect_false(dir.exists(abandoned$stage))
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
    expect_true(builder_abort_release(replacement)$aborted)
  })
})

test_that("interrupted abort cleanup stays retryable and verifies stage removal", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    abandoned <- builder_prepare_release(
      target,
      "build-abort-cleanup-retry",
      builder_release_identity(target)
    )
    writeLines("partial", file.path(abandoned$stage, "partial.crb"))
    owner <- readRDS(file.path(abandoned$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(abandoned$lock, "owner.rds"))

    remove_stage <- .builder_release_remove_stage
    .builder_release_remove_stage <- function(stage, ...) {
      stop("simulated stage lock", call. = FALSE)
    }
    expect_error(
      builder_recover_release(target, action = "abort"),
      "simulated stage lock"
    )
    retry <- builder_discover_recovery(target)
    expect_identical(retry$phase, "recovering_abort")
    expect_identical(retry$journal$recovery_from, "prepared")
    expect_true(retry$safe_to_abort)

    .builder_release_remove_stage <- remove_stage
    recovered <- builder_recover_release(target, action = "abort")
    expect_true(recovered$recovered)
    expect_true(recovered$control_cleaned)
    expect_false(dir.exists(abandoned$stage))
    expect_false(dir.exists(abandoned$control))
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("clearing an interrupted release refuses a changed target", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    abandoned <- builder_prepare_release(
      target,
      "build-changed-before-recovery",
      builder_release_identity(target)
    )
    owner <- readRDS(file.path(abandoned$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(abandoned$lock, "owner.rds"))
    writeLines("changed", file.path(target, "dataset.crb"))

    expect_error(
      builder_recover_release(target, action = "abort"),
      "release target changed"
    )
    expect_true(dir.exists(abandoned$stage))
  })
})

test_that("clearing treats an absent and empty target as content-equivalent", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    abandoned <- builder_prepare_release(
      target,
      "build-empty-target",
      builder_release_identity(target)
    )
    writeLines("partial", file.path(abandoned$stage, "partial.crb"))
    owner <- readRDS(file.path(abandoned$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(abandoned$lock, "owner.rds"))
    unlink(target, recursive = TRUE, force = TRUE)

    recovered <- builder_recover_release(target, action = "abort")

    expect_true(recovered$recovered)
    expect_true(recovered$control_cleaned)
    expect_false(dir.exists(abandoned$control))
    expect_false(dir.exists(target))
  })
})

test_that("process death after protecting the prior release keeps recovery intent", {
  skip_if_not_installed("callr")
  local({
    root_path <- builder_test_inst_path("builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        handle <- builder_prepare_release(
          target,
          "build-moving-crash",
          builder_release_identity(target)
        )
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_move = function(move) {
            if (identical(move, "old_to_backup")) {
              quit(save = "no", status = 21L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    builder_publish_await_exit(crash)
    expect_identical(crash$get_exit_status(), 21L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "old_moving")
    expect_false(dir.exists(target))
    expect_true(dir.exists(recovery$backup))
    expect_true(builder_recover_release(target, "restore")$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("process death after the new release can still restore the prior one", {
  skip_if_not_installed("callr")
  local({
    root_path <- builder_test_inst_path("builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        expected <- builder_release_identity(target)
        handle <- builder_prepare_release(target, "build-new-crash", expected)
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "new_published")) {
              quit(save = "no", status = 18L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    builder_publish_await_exit(crash)
    expect_identical(crash$get_exit_status(), 18L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "new_published")
    expect_identical(readLines(file.path(target, "dataset.crb")), "new")
    restored <- builder_recover_release(target, action = "restore")
    expect_true(restored$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("final target identity is captured before the release becomes terminal", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-final-identity",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    original_identity <- builder_release_identity
    builder_release_identity <- function(path, ...) {
      payload <- file.path(path, "dataset.crb")
      if (
        identical(normalizePath(path), normalizePath(target)) &&
          file.exists(payload) &&
          identical(readLines(payload), "new")
      ) {
        stop("final identity failed")
      }
      original_identity(path, ...)
    }

    expect_error(builder_publish_release(handle), "final identity failed")
    builder_release_identity <- original_identity
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "old_moved")
    expect_true(dir.exists(recovery$backup))
  })
})

test_that("restoration failure preserves the exact backup for recovery", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "build-restore-failure",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    moves <- 0L
    fail_publish_and_restore <- function(from, to) {
      moves <<- moves + 1L
      if (moves %in% c(2L, 3L)) {
        return(FALSE)
      }
      file.rename(from, to)
    }

    failure <- tryCatch(
      builder_publish_release(handle, .move = fail_publish_and_restore),
      error = function(error) error
    )
    expect_s3_class(failure, "error")
    expect_true(grepl(handle$backup, conditionMessage(failure), fixed = TRUE))
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$state, "recovery_required")
    expect_identical(recovery$backup, handle$backup)
    expect_true(dir.exists(handle$backup))
    expect_identical(readLines(file.path(handle$backup, "dataset.crb")), "old")
  })
})

test_that("a dead completed owner cannot strand the release lock", {
  skip_if_not_installed("callr")
  local({
    root_path <- builder_test_inst_path("builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        handle <- builder_prepare_release(target, "build-complete-crash")
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_phase = function(phase) {
            if (identical(phase, "complete")) {
              quit(save = "no", status = 19L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    builder_publish_await_exit(crash)
    expect_identical(crash$get_exit_status(), 19L)

    builder_publish_source()
    expect_true(dir.exists(file.path(
      builder_release_control_path(target),
      "lock"
    )))
    next_handle <- builder_prepare_release(
      target,
      "build-after-complete",
      builder_release_identity(target)
    )
    expect_s3_class(next_handle, "builder_release_handle")
    expect_true(builder_abort_release(next_handle)$aborted)
  })
})

test_that("completed temporary releases can remove their verified control data", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "checkpoint")
    handle <- builder_prepare_release(target, "temporary-checkpoint")
    writeLines("temporary", file.path(handle$stage, "dataset.crb"))
    published <- builder_publish_release(handle)
    expect_true(isTRUE(published$published))
    control <- builder_release_control_path(target)
    legacy <- .builder_release_legacy_control_path(target)
    expect_true(published$control_cleaned)
    expect_false(dir.exists(control))
    expect_true(dir.create(legacy, mode = "0700"))
    unlink(target, recursive = TRUE, force = TRUE)

    expect_true(builder_release_cleanup_control(target))
    expect_false(dir.exists(control))
    expect_false(dir.exists(legacy))
  })
})

test_that("terminal release controls are removed after success and abort", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()

    published_target <- file.path(root, "published")
    published_handle <- builder_prepare_release(
      published_target,
      "build-success"
    )
    writeLines("complete", file.path(published_handle$stage, "dataset.crb"))
    published <- builder_publish_release(published_handle)
    expect_true(published$published)
    expect_true(published$control_cleaned)
    expect_null(published$journal)
    expect_false(dir.exists(published_handle$control))
    expect_identical(
      readLines(file.path(published_target, "dataset.crb")),
      "complete"
    )

    aborted_target <- file.path(root, "aborted")
    aborted_handle <- builder_prepare_release(aborted_target, "build-abort")
    writeLines("partial", file.path(aborted_handle$stage, "dataset.crb"))
    aborted <- builder_abort_release(aborted_handle)
    expect_true(aborted$aborted)
    expect_true(aborted$control_cleaned)
    expect_false(dir.exists(aborted_handle$control))
    expect_false(dir.exists(aborted_handle$stage))
    expect_false(dir.exists(aborted_target))
  })
})

test_that("unsafe terminal controls are retained without failing publication", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    handle <- builder_prepare_release(target, "build-retained-control")
    writeLines("complete", file.path(handle$stage, "dataset.crb"))
    diagnostic <- file.path(handle$control, "diagnostics", "keep.txt")
    writeLines("keep", diagnostic)

    published <- builder_publish_release(handle)

    expect_true(published$published)
    expect_false(published$control_cleaned)
    expect_match(published$warning, "control was retained", fixed = TRUE)
    expect_true(file.exists(diagnostic))
    expect_false(dir.exists(handle$lock))
    expect_identical(readLines(file.path(target, "dataset.crb")), "complete")
  })
})

test_that("a crash after the stage rename but before journaling is recoverable", {
  skip_if_not_installed("callr")
  local({
    root_path <- builder_test_inst_path("builder")
    if (!dir.exists(root_path)) {
      root_path <- system.file("builder", package = "CerebroNexus")
    }
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    crash <- callr::r_bg(
      function(builder_root, target) {
        source(file.path(builder_root, "core", "bundle_path_contract.R"))
        source(file.path(builder_root, "publish.R"))
        handle <- builder_prepare_release(
          target,
          "build-rename-crash",
          builder_release_identity(target)
        )
        writeLines("new", file.path(handle$stage, "dataset.crb"))
        builder_publish_release(
          handle,
          .after_move = function(move) {
            if (identical(move, "new_to_target")) {
              quit(save = "no", status = 20L, runLast = FALSE)
            }
          }
        )
      },
      list(root_path, target)
    )
    builder_publish_await_exit(crash)
    expect_identical(crash$get_exit_status(), 20L)

    builder_publish_source()
    recovery <- builder_discover_recovery(target)
    expect_identical(recovery$phase, "old_moved")
    expect_identical(readLines(file.path(target, "dataset.crb")), "new")
    expect_true(builder_recover_release(target, "restore")$recovered)
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("foreign or unverifiable locks remain fail closed", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    control <- builder_release_control_path(target)
    dir.create(control, mode = "0700")
    lock <- file.path(control, "lock")
    dir.create(lock, mode = "0700")
    owner <- list(
      schema_version = 1L,
      token = "foreign-token",
      host = "another-host",
      pid = 999999L,
      acquired_at = "unknown"
    )
    saveRDS(owner, file.path(lock, "owner.rds"))
    journal <- list(
      schema_version = 1L,
      target = .builder_release_path(target),
      control = control,
      stage = file.path(control, "stages", "foreign"),
      backup = file.path(control, "backup"),
      journal = file.path(control, "journal.rds"),
      lock = lock,
      build_id = "foreign-build",
      token = "foreign-token",
      phase = "old_moved"
    )
    saveRDS(journal, file.path(control, "journal.rds"))

    expect_error(
      builder_recover_release(target, "restore"),
      "manual inspection"
    )
    expect_true(dir.exists(lock))
    expect_identical(readRDS(file.path(lock, "owner.rds")), owner)
  })
})

test_that("release lock ownership can move to a trusted publication process", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    handle <- builder_prepare_release(target, "build-owner-transfer")
    original <- .builder_release_lock_owner(handle$lock)
    child_pid <- original$pid + 1L

    transferred <- .builder_release_transfer_lock(
      handle$lock,
      handle$token,
      from_pid = original$pid,
      to_pid = child_pid
    )

    expect_identical(transferred$pid, child_pid)
    expect_identical(
      .builder_release_lock_owner(handle$lock)$pid,
      child_pid
    )
    expect_error(
      .builder_release_transfer_lock(
        handle$lock,
        handle$token,
        from_pid = original$pid,
        to_pid = original$pid + 2L
      ),
      "ownership changed"
    )
    expect_true(builder_abort_release(handle)$aborted)
  })
})

test_that("interrupted atomic writes stay outside control and lock roots", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    handle <- builder_prepare_release(target, "build-atomic-orphan")
    diagnostics <- file.path(handle$control, "diagnostics")
    orphan <- file.path(diagnostics, ".journal.rds.interrupted.tmp")
    writeLines("interrupted", orphan)

    expect_silent(.builder_release_assert_control(handle$control))
    expect_true(.builder_release_lock_known(handle$lock))
    expect_true(builder_abort_release(handle)$aborted)
    expect_true(file.exists(orphan))

    next_handle <- builder_prepare_release(target, "build-after-orphan")
    expect_s3_class(next_handle, "builder_release_handle")
    expect_true(builder_abort_release(next_handle)$aborted)
  })
})

test_that("completed releases cannot be rewritten as aborted", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    handle <- builder_prepare_release(target, "build-complete-abort")
    handle <- .builder_release_write_phase(handle, "complete")

    expect_error(builder_abort_release(handle), "cannot be aborted")
    expect_identical(
      .builder_release_journal(target, required = TRUE)$phase,
      "complete"
    )
  })
})

test_that("terminal journals with a surviving lock require stale-lock cleanup", {
  local({
    builder_publish_source()
    for (phase in c("complete", "aborted", "recovered")) {
      root <- withr::local_tempdir()
      target <- file.path(root, "release")
      handle <- builder_prepare_release(target, paste0("build-", phase))
      handle <- .builder_release_write_phase(handle, phase)

      recovery <- builder_discover_recovery(target)
      expect_identical(recovery$state, "stale_lock")
      expect_identical(recovery$journal$phase, phase)
    }
  })
})

test_that("foreign control occupants are preserved and fail closed", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    control <- builder_release_control_path(target)
    dir.create(control, mode = "0700")
    foreign <- file.path(control, "do-not-delete.txt")
    writeLines("foreign", foreign)

    expect_error(
      builder_prepare_release(target, "build-foreign"),
      "unknown control"
    )
    expect_identical(readLines(foreign), "foreign")
  })
})

test_that("payload verification requires exact TRUE and restores the prior release", {
  local({
    builder_publish_source()
    for (result in list(FALSE, NULL, structure("boom", class = "error"))) {
      root <- withr::local_tempdir()
      target <- file.path(root, "release")
      dir.create(target)
      writeLines("old", file.path(target, "dataset.crb"))
      handle <- builder_prepare_release(
        target,
        "guard",
        builder_release_identity(target)
      )
      writeLines("new", file.path(handle$stage, "dataset.crb"))
      callback <- if (inherits(result, "error")) {
        function(...) stop("private callback detail")
      } else {
        force(result)
        function(...) result
      }
      expect_error(
        builder_publish_release(handle, .verify_payload = callback),
        "verification failed"
      )
      expect_identical(readLines(file.path(target, "dataset.crb")), "old")
      expect_true(dir.exists(handle$stage))
    }
  })
})

test_that("post-rename payload verification restores the old release", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    handle <- builder_prepare_release(
      target,
      "post-rename-guard",
      builder_release_identity(target)
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))
    expect_error(
      builder_publish_release(
        handle,
        .verify_payload = function(root, phase) {
          if (identical(phase, "after_rename")) FALSE else TRUE
        }
      ),
      "verification failed"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
    expect_true(dir.exists(handle$stage))
  })
})
