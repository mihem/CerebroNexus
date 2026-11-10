## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

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

test_that("a published release folder follows the umask", {
  skip_on_os("windows")
  local({
    builder_publish_source()
    root <- withr::local_tempdir()
    old_umask <- Sys.umask("022")
    withr::defer(Sys.umask(old_umask))
    publish <- function(name, build_id) {
      target <- file.path(root, name)
      handle <- builder_prepare_release(
        target,
        build_id,
        builder_release_identity(target)
      )
      # Owner-only while it is built.
      expect_identical(as.character(file.info(handle$stage)$mode), "700")
      writeLines("new", file.path(handle$stage, "dataset.crb"))
      builder_publish_release(handle)
      as.character(file.info(target)$mode)
    }

    expect_identical(publish("shared", "build-umask"), "755")
    Sys.umask("077")
    expect_identical(publish("private", "build-umask-private"), "700")
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

test_that("abandoned new-release controls are swept from their parent", {
  local({
    builder_publish_source()
    root <- withr::local_tempdir()

    ## killed: the lock owner is gone and the staged data is still there
    killed <- builder_prepare_release(file.path(root, "killed"), "build-killed")
    writeLines("staged", file.path(killed$stage, "dataset.crb"))
    owner <- readRDS(file.path(killed$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(killed$lock, "owner.rds"))

    ## failed: aborted, but diagnostics kept the control
    failed <- builder_prepare_release(file.path(root, "failed"), "build-failed")
    writeLines("log", file.path(failed$control, "diagnostics", "trace.txt"))
    expect_false(builder_abort_release(failed)$control_cleaned)
    expect_true(dir.exists(failed$control))

    ## running: this process still owns the lock
    running <- builder_prepare_release(file.path(root, "running"), "build-live")
    writeLines("staged", file.path(running$stage, "dataset.crb"))

    ## published (a Retry after Stop): leftover files kept the finished
    ## control beside the published release
    published_target <- file.path(root, "published")
    published <- builder_prepare_release(published_target, "build-done")
    writeLines("log", file.path(published$control, "diagnostics", "keep.txt"))
    writeLines("done", file.path(published$stage, "dataset.crb"))
    published_result <- builder_publish_release(published)
    expect_true(published_result$published)
    expect_false(published_result$control_cleaned)

    ## occupied: an interrupted release whose target exists needs recovery
    occupied_target <- file.path(root, "occupied")
    occupied <- builder_prepare_release(occupied_target, "build-occupied")
    owner <- readRDS(file.path(occupied$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(occupied$lock, "owner.rds"))
    dir.create(occupied_target)

    removed <- builder_release_sweep_parent(root)
    expect_setequal(
      basename(removed),
      basename(c(killed$control, failed$control, published$control))
    )
    expect_false(dir.exists(killed$control))
    expect_false(dir.exists(failed$control))
    expect_false(dir.exists(published$control))
    expect_identical(
      readLines(file.path(published_target, "dataset.crb")),
      "done"
    )
    expect_true(file.exists(file.path(running$stage, "dataset.crb")))
    expect_true(dir.exists(occupied$control))
    expect_true(dir.exists(occupied$lock))
    expect_true(builder_abort_release(running)$aborted)
    expect_identical(
      builder_release_sweep_parent(file.path(root, "none")),
      character()
    )
  })
})

test_that("a new Builder sweeps the output folders a killed Builder used", {
  local({
    builder_publish_source()
    withr::local_envvar(CEREBRO_BUILDER_STATE_DIR = withr::local_tempdir())
    killed_parent <- withr::local_tempdir()
    live_parent <- withr::local_tempdir()
    expect_true(builder_release_remember_parent(killed_parent))
    expect_true(builder_release_remember_parent(killed_parent))
    expect_true(builder_release_remember_parent(live_parent))
    expect_length(readLines(builder_release_registry_path()), 2L)

    ## kill -9: no session end, the lock owner is gone
    killed <- builder_prepare_release(
      file.path(killed_parent, "CerebroNexus_viewer_000001"),
      "build-killed"
    )
    writeLines("staged", file.path(killed$stage, "dataset.crb"))
    owner <- readRDS(file.path(killed$lock, "owner.rds"))
    owner$pid <- .Machine$integer.max
    saveRDS(owner, file.path(killed$lock, "owner.rds"))
    ## another Builder is still building here
    running <- builder_prepare_release(
      file.path(live_parent, "CerebroNexus_viewer_000002"),
      "build-live"
    )

    kept <- builder_release_sweep_remembered()
    expect_false(dir.exists(killed$control))
    expect_true(dir.exists(running$lock))
    expect_identical(
      kept,
      normalizePath(live_parent, winslash = "/", mustWork = TRUE)
    )
    expect_identical(readLines(builder_release_registry_path()), kept)

    expect_true(builder_abort_release(running)$aborted)
    expect_identical(builder_release_sweep_remembered(), character())
    expect_false(file.exists(builder_release_registry_path()))
  })
})
