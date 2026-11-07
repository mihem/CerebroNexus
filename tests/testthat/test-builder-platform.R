builder_repo_source("io.R", local = globalenv())

test_that("native pickers cover datasets, projects, and output folders", {
  expect_identical(
    builder_native_picker_kinds(),
    c(
      "output_directory",
      "project_directory",
      "project_manifest",
      "dataset_files"
    )
  )
  expect_identical(
    eval(formals(builder_native_picker_spec)$kind),
    builder_native_picker_kinds()
  )
  expect_identical(
    eval(formals(builder_start_native_picker)$kind),
    builder_native_picker_kinds()
  )
})

test_that("Windows native pickers use modern STA shell dialogs", {
  powershell <- "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
  folder <- builder_native_picker_spec(
    "project_directory",
    .system = "Windows",
    .powershell = powershell
  )

  expect_identical(folder$command, powershell)
  expect_false("select" %in% names(folder))
  expect_true("-STA" %in% folder$args)
  expect_identical(folder$encoding, "UTF-8")
  expect_match(
    paste(folder$args, collapse = "\n"),
    "IFileOpenDialog",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    "OpenFileDialog",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    "$dialog.AutoUpgradeEnabled = $true",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    "Builder project (*.json;*.json.bak)|*.json;*.json.bak|",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("dataset_files"),
    "$dialog.Multiselect = $true",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("dataset_files"),
    "$dialog.FileNames",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("output_directory"),
    "[BuilderModernFolderPicker]::Pick",
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_directory"),
    "FileOpenOptions.PickFolders",
    fixed = TRUE
  )
  expect_false(grepl(
    "FolderBrowserDialog",
    builder_windows_picker_script("project_directory"),
    fixed = TRUE
  ))
  pwsh <- builder_native_picker_spec(
    "output_directory",
    .system = "Windows",
    .which = function(command) {
      if (identical(command, "pwsh.exe")) {
        "C:/Program Files/PowerShell/7/pwsh.exe"
      } else {
        ""
      }
    }
  )
  expect_identical(
    pwsh$command,
    "C:/Program Files/PowerShell/7/pwsh.exe"
  )
})

test_that("every native picker has one typed selection contract", {
  contracts <- lapply(
    builder_native_picker_kinds(),
    builder_native_picker_contract
  )

  expect_identical(
    vapply(contracts, `[[`, character(1), "kind"),
    builder_native_picker_kinds()
  )
  expect_identical(
    vapply(contracts, `[[`, character(1), "type"),
    c("directory", "directory", "file", "file")
  )
  expect_true(all(vapply(
    contracts,
    function(contract) nzchar(contract$prompt),
    logical(1)
  )))
})

test_that("browser upload accept lists share backend format contracts", {
  expect_identical(
    builder_file_accept(builder_dataset_extensions()),
    ".rds,.qs2,.qs,.h5ad,.h5mu,.zip"
  )
  expect_identical(
    builder_file_accept(builder_table_extensions()),
    ".csv,.tsv,.txt,.xls,.xlsx,.xlsm"
  )
  expect_identical(
    builder_file_accept(builder_image_extensions()),
    ".png,.jpg,.jpeg"
  )
})

test_that("unavailable optional readers suggest usable formats", {
  capabilities <- builder_format_capabilities(
    .require_namespace = function(package) !identical(package, "qs")
  )
  available <- builder_available_formats(capabilities)
  unavailable <- Filter(
    function(format) !isTRUE(format$available),
    capabilities
  )

  expect_identical(
    vapply(available, `[[`, character(1), "id"),
    c("rds", "qs2", "h5ad", "h5mu", "spatialdata")
  )
  expect_identical(
    vapply(unavailable, `[[`, character(1), "id"),
    "qs"
  )
  expect_match(
    unavailable[[1L]]$unavailable_reason,
    'QS unavailable',
    fixed = TRUE
  )
  expect_false(
    builder_path_format_capability(
      "dataset.qs",
      capabilities
    )$available
  )
  rds_only <- builder_format_capabilities(
    .require_namespace = function(package) FALSE
  )
  expect_identical(
    vapply(builder_available_formats(rds_only), `[[`, character(1), "id"),
    "rds"
  )
  expect_identical(
    builder_path_format_capability("dataset.qs2", rds_only)$unavailable_reason,
    "QS2 unavailable. Use RDS."
  )
  expect_identical(
    builder_path_format_capability("dataset.qs", rds_only)$unavailable_reason,
    "QS unavailable. Use RDS."
  )
  unlabelled <- builder_format_capabilities(
    formats = list(
      list(id = "rds", extensions = "rds", package = NULL),
      list(id = "qs", extensions = "qs", package = "qs")
    ),
    .require_namespace = function(package) FALSE
  )
  expect_identical(
    builder_path_format_capability("dataset.qs", unlabelled)$unavailable_reason,
    "QS unavailable. Use RDS."
  )
})

test_that("per-file format checks reuse one reader probe", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_profile_inst_path("builder", "io.R"), envir = runtime)
  probes <- 0L
  probe <- function(formats) {
    probes <<- probes + 1L
    runtime$builder_format_capabilities(
      formats,
      .require_namespace = function(package) !identical(package, "qs")
    )
  }
  first <- runtime$builder_cached_format_capabilities(.probe = probe)
  second <- runtime$builder_cached_format_capabilities(.probe = probe)
  expect_identical(probes, 1L)
  expect_identical(first, second)
  expect_false(runtime$builder_path_format_capability("a.qs")$available)
  expect_true(runtime$builder_path_format_capability("a.qs2")$available)
  expect_identical(
    vapply(runtime$builder_available_formats(), `[[`, character(1), "id"),
    c("rds", "qs2", "h5ad", "h5mu", "spatialdata")
  )
  expect_identical(probes, 1L)

  # A changed format table is probed again rather than served stale.
  changed <- runtime$builder_formats
  changed[[1L]]$label <- "RDS changed"
  runtime$builder_cached_format_capabilities(changed, .probe = probe)
  expect_identical(probes, 2L)
})

test_that("on-disk backend flags are probed once without loading packages", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_profile_inst_path("builder", "loading.R"), envir = runtime)
  probed <- character()
  installed <- function(package) {
    probed <<- c(probed, package)
    identical(package, "HDF5Array")
  }
  first <- runtime$builder_backend_capabilities(.installed = installed)
  second <- runtime$builder_backend_capabilities(.installed = installed)
  expect_identical(first, list(h5 = TRUE))
  expect_identical(second, first)
  expect_identical(probed, "HDF5Array")

  session <- paste(
    readLines(builder_profile_inst_path("builder", "session.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_false(grepl('requireNamespace("BPCells"', session, fixed = TRUE))
  expect_false(grepl('requireNamespace("HDF5Array"', session, fixed = TRUE))
  expect_identical(
    lengths(regmatches(
      session,
      gregexpr(
        "backends <- builder_backend_capabilities()",
        session,
        fixed = TRUE
      )
    )),
    1L
  )
})

test_that("macOS specs cover every directory and file picker", {
  specs <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(kind, .system = "Darwin")
  })

  expect_true(all(vapply(
    specs,
    function(spec) {
      identical(spec$command, "osascript") &&
        identical(spec$cancel_status, integer()) &&
        identical(spec$encoding, "UTF-8")
    },
    logical(1)
  )))
  expect_true(all(vapply(
    specs,
    function(spec) {
      grepl(
        "on error number -128",
        paste(spec$args, collapse = "\n"),
        fixed = TRUE
      )
    },
    logical(1)
  )))
  expect_true(all(vapply(
    specs[1:2],
    function(spec) {
      grepl("choose folder", paste(spec$args, collapse = "\n"), fixed = TRUE)
    },
    logical(1)
  )))
  expect_match(
    paste(specs[[3L]]$args, collapse = "\n"),
    'choose file with prompt "Open a Builder project." of type {"public.json", "bak"}',
    fixed = TRUE
  )
  expect_match(
    paste(specs[[4L]]$args, collapse = "\n"),
    "with multiple selections allowed",
    fixed = TRUE
  )
})

test_that("Linux Zenity and KDialog specs cover every picker", {
  which_zenity <- function(command) {
    if (identical(command, "zenity")) "/usr/bin/zenity" else ""
  }
  which_kdialog <- function(command) {
    if (identical(command, "kdialog")) "/usr/bin/kdialog" else ""
  }
  zenity <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(
      kind,
      .system = "Linux",
      .which = which_zenity
    )
  })
  kdialog <- lapply(builder_native_picker_kinds(), function(kind) {
    builder_native_picker_spec(
      kind,
      .system = "Linux",
      .which = which_kdialog
    )
  })

  expect_true(all(vapply(
    c(zenity, kdialog),
    function(spec) {
      identical(spec$cancel_status, 1L) &&
        identical(spec$encoding, "UTF-8")
    },
    logical(1)
  )))
  expect_true(all(vapply(
    zenity[1:2],
    function(spec) {
      "--directory" %in% spec$args
    },
    logical(1)
  )))
  ## The project backup can be chosen to recover a project.
  expect_true(
    "--file-filter=Builder project | *.json *.json.bak" %in% zenity[[3L]]$args
  )
  expect_true("--file-filter=All files | *" %in% zenity[[3L]]$args)
  expect_true("--multiple" %in% zenity[[4L]]$args)
  ## Dataset files without an extension stay selectable.
  expect_true("--file-filter=All files | *" %in% zenity[[4L]]$args)
  expect_true(any(startsWith(zenity[[4L]]$args, "--separator=")))
  expect_true(all(vapply(
    kdialog[1:2],
    function(spec) {
      "--getexistingdirectory" %in% spec$args
    },
    logical(1)
  )))
  expect_true("--getopenfilename" %in% kdialog[[3L]]$args)
  expect_true("Builder project (*.json *.json.bak)" %in% kdialog[[3L]]$args)
  expect_true("--multiple" %in% kdialog[[4L]]$args)
  expect_true("--separate-output" %in% kdialog[[4L]]$args)
  expect_true(all(vapply(
    c(zenity, kdialog),
    function(spec) {
      "--title" %in% spec$args || any(startsWith(spec$args, "--title="))
    },
    logical(1)
  )))
})

test_that("Linux without a desktop picker fails into the server-path fallback", {
  expect_error(
    builder_native_picker_spec(
      "output_directory",
      .system = "Linux",
      .which = function(command) ""
    ),
    "No system picker is available",
    fixed = TRUE
  )
})

builder_platform_picker_process <- function(
  status,
  output = character(),
  errors = character()
) {
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  process$get_exit_status <- function() status
  process$read_all_output_lines <- function() output
  process$read_all_error_lines <- function() errors
  process
}

test_that("native picker failures cannot masquerade as a selection or cancellation", {
  root <- withr::local_tempdir()
  failed_with_output <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(2L, root, "dialog failed"),
    result = NULL,
    cancel_status = 1L
  ))
  failed_without_detail <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(2L),
    result = NULL,
    cancel_status = 1L
  ))
  cancelled <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(1L),
    result = NULL,
    cancel_status = 1L
  ))

  expect_identical(failed_with_output$status, "error")
  expect_match(failed_with_output$error, "dialog failed", fixed = TRUE)
  expect_identical(failed_without_detail$status, "error")
  expect_match(failed_without_detail$error, "status 2", fixed = TRUE)
  expect_identical(cancelled, list(status = "cancelled", path = NULL))
})

test_that("native picker cancellation is explicit and empty success is an error", {
  marker <- builder_native_picker_cancel_marker()
  cancelled <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(0L, marker),
    result = NULL,
    cancel_status = integer()
  ))
  empty <- builder_collect_native_picker(list(
    kind = "output_directory",
    process = builder_platform_picker_process(0L),
    result = NULL,
    cancel_status = integer()
  ))

  expect_identical(cancelled, list(status = "cancelled", path = NULL))
  expect_identical(empty$status, "error")
  expect_true(empty$fallback)
  expect_match(empty$error, "without returning a selection", fixed = TRUE)
  expect_match(
    builder_macos_picker_script("output_directory"),
    marker,
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("output_directory"),
    marker,
    fixed = TRUE
  )
  expect_match(
    builder_windows_picker_script("project_manifest"),
    marker,
    fixed = TRUE
  )
})

test_that("dataset picker returns every selected local path", {
  root <- withr::local_tempdir()
  first <- file.path(root, "one.qs")
  second <- file.path(root, "two.qs2")
  file.create(first, second)

  selected <- builder_native_picker_result(
    "dataset_files",
    function() c(first, second)
  )
  expected <- normalizePath(
    c(first, second),
    winslash = "/",
    mustWork = TRUE
  )

  expect_identical(selected$status, "selected")
  expect_identical(selected$path, expected[[1L]])
  expect_identical(selected$paths, expected)
})

test_that("output folder picker keeps a trailing space in the name", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  spaced <- file.path(root, "out ")
  dir.create(spaced)
  dir.create(file.path(root, "out"))

  selected <- builder_native_picker_result(
    "output_directory",
    function() spaced
  )

  expect_identical(selected$status, "selected")
  expect_identical(basename(selected$path), "out ")
})

test_that("an upload above max_file_size is refused with the limit", {
  message <- builder_upload_limit_message(
    "big.rds",
    12 * 1024^2,
    limit = 5 * 1024^2
  )
  expect_match(message, "big.rds is 12 MB", fixed = TRUE)
  expect_match(message, "larger than the 5 MB upload limit", fixed = TRUE)
  expect_match(message, "max_file_size", fixed = TRUE)
  expect_null(builder_upload_limit_message("ok.rds", 1024, limit = 5 * 1024^2))
  expect_null(builder_upload_limit_message("big.rds", 1e12, limit = Inf))
  expect_null(builder_upload_limit_message("big.rds", 1e12, limit = NULL))
})

test_that("native picker subprocess hides the Windows console host", {
  arguments <- NULL
  process <- builder_platform_picker_process(1L)
  started <- builder_start_native_picker(
    "output_directory",
    .spec = list(command = "picker", args = character()),
    .process_new = function(...) {
      arguments <<- list(...)
      process
    }
  )

  expect_identical(started$process, process)
  expect_true(arguments$windows_hide_window)
})
