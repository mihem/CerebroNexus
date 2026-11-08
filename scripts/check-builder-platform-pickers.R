## Lightweight cross-platform Builder picker contract check.
##
## This deliberately sources only the pure IO helpers. It runs on Windows,
## macOS, and Linux CI without installing CerebroNexus or its analysis stack.

sys.source(file.path("inst", "builder", "io.R"), envir = globalenv())
sys.source(
  file.path("inst", "builder", "app_bundle", "contract.R"),
  envir = globalenv()
)
sys.source(file.path("R", "createShinyApp.R"), envir = globalenv())
sys.source(
  file.path("inst", "builder", "app_bundle", "topology.R"),
  envir = globalenv()
)
sys.source(
  file.path("inst", "builder", "core", "bundle_path_contract.R"),
  envir = globalenv()
)
sys.source(file.path("inst", "builder", "publish.R"), envir = globalenv())
sys.source(file.path("inst", "builder", "coordinator.R"), envir = globalenv())

permissions <- if (identical(.Platform$OS.type, "windows")) {
  "rw-"
} else {
  "rw-r--r--"
}
stopifnot(.builder_app_permissions_valid(permissions))
stopifnot(identical(
  .bundleWindowsExtendedPath("C:/stage/image.jpg", os_type = "windows"),
  "\\\\?\\C:\\stage\\image.jpg"
))
stopifnot(identical(
  .builder_release_io_path("C:/stage/image.jpg", os_type = "windows"),
  "\\\\?\\C:\\stage\\image.jpg"
))
budget_plan <- list(
  items = list(list(
    filename = paste0(strrep("d", 48L), ".crb"),
    sidecars = character()
  ))
)
stopifnot(.builder_coordinator_assert_windows_path_budget(
  budget_plan,
  "C:/CerebroBuild/control/stages/stage-123456789abc",
  app_expected = TRUE,
  os_type = "windows"
))
deep_budget_stage <- paste0(
  "C:/",
  paste(rep("deep-output-folder", 14L), collapse = "/"),
  "/stage-123456789abc"
)
budget_error <- tryCatch(
  .builder_coordinator_assert_windows_path_budget(
    budget_plan,
    deep_budget_stage,
    app_expected = TRUE,
    os_type = "windows"
  ),
  error = identity
)
stopifnot(
  inherits(budget_error, "error"),
  grepl(
    "selected output folder is too deep",
    conditionMessage(budget_error),
    fixed = TRUE
  ),
  .builder_coordinator_assert_windows_path_budget(
    budget_plan,
    deep_budget_stage,
    app_expected = TRUE,
    os_type = "unix"
  )
)
spatial_budget_plan <- budget_plan
spatial_budget_plan$items[[1L]]$id <- "dataset-a"
spatial_budget_plan$items[[1L]]$spatial_image_storage <- "external"
spatial_source_name <- paste0(strrep("histology", 22L), ".jpg")
spatial_budget_plan$items[[1L]]$images <- list(
  section = list(
    image = list(
      source = list(name = spatial_source_name)
    )
  )
)
spatial_budget_stage <- "C:/CerebroBuild/control/stages/stage-123456789abc"
spatial_budget_candidates <- .builder_coordinator_windows_path_candidates(
  spatial_budget_plan,
  spatial_budget_stage,
  app_expected = TRUE,
  .viewer_relative = character()
)
stopifnot(
  .builder_coordinator_assert_windows_path_budget(
    spatial_budget_plan,
    spatial_budget_stage,
    app_expected = TRUE,
    os_type = "windows",
    .viewer_relative = character()
  ),
  any(grepl(
    ".spatial/d-ffffffffffff/s-ffffffffffff/i-ffffffffffff.jpg",
    spatial_budget_candidates,
    fixed = TRUE
  )),
  !any(grepl(
    ".builder-spatial-assets",
    spatial_budget_candidates,
    fixed = TRUE
  )),
  !any(grepl(spatial_source_name, spatial_budget_candidates, fixed = TRUE))
)
spatial_target <- .spatialImageBundleTarget(
  "17. a dataset identifier long enough to require hashing",
  "fov",
  "2019-042.jpg",
  "2019-042.jpg"
)
stopifnot(
  grepl("^spatial-assets/u[0-9a-f]{32}\\.jpg$", spatial_target),
  nchar(spatial_target, type = "bytes") <= 64L,
  .builder_app_spatial_target_valid(
    spatial_target,
    "17. a dataset identifier long enough to require hashing",
    "fov",
    "2019-042.jpg"
  ),
  identical(
    spatial_target,
    .builder_app_spatial_target(
      "17. a dataset identifier long enough to require hashing",
      "fov",
      "2019-042.jpg",
      file.path("C:/uploaded-images", "2019-042.jpg")
    )
  )
)
topology_root_length <- 130L
topology_temp <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
topology_segment_length <- topology_root_length -
  nchar(topology_temp, type = "bytes") -
  1L
topology_root <- file.path(
  topology_temp,
  paste(rep("s", topology_segment_length), collapse = "")
)
if (!dir.create(topology_root)) {
  stop("Could not create the staged-App topology test root.")
}
topology_root <- normalizePath(topology_root, winslash = "/", mustWork = TRUE)
on.exit(unlink(topology_root, recursive = TRUE, force = TRUE), add = TRUE)
topology_asset <- file.path(topology_root, spatial_target)
if (!dir.create(dirname(topology_asset), recursive = TRUE)) {
  stop("Could not create the staged-App spatial asset directory.")
}
writeBin(as.raw(1:32), topology_asset)
topology <- .builder_app_enumerate_tree(topology_root)
stopifnot(
  nchar(topology_root, type = "bytes") == topology_root_length,
  normalizePath(topology_asset, winslash = "/", mustWork = TRUE) %in%
    normalizePath(topology$paths, winslash = "/", mustWork = TRUE)
)

viewer_copy_root <- tempfile("builder-viewer-copy-")
if (!dir.create(viewer_copy_root)) {
  stop("Could not create the Viewer copy test root.")
}
on.exit(unlink(viewer_copy_root, recursive = TRUE, force = TRUE), add = TRUE)
viewer_source <- normalizePath(
  file.path("inst", "viewer"),
  winslash = "/",
  mustWork = TRUE
)
if (!.bundleCopyPath(viewer_source, viewer_copy_root, recursive = TRUE)) {
  stop("Could not copy the trusted Viewer tree.")
}
viewer_copy <- file.path(viewer_copy_root, basename(viewer_source))
viewer_source_identity <- .builder_app_portable_tree_entries(
  .builder_app_without_system_metadata(
    .builder_app_tree_identity(viewer_source)
  )
)
viewer_copy_identity <- .builder_app_portable_tree_entries(
  .builder_app_without_system_metadata(
    .builder_app_tree_identity(viewer_copy)
  )
)
stopifnot(
  length(viewer_source_identity) > 0L,
  identical(viewer_copy_identity, viewer_source_identity)
)

if (identical(.Platform$OS.type, "windows")) {
  long_root <- tempfile("builder-long-path-")
  if (!dir.create(long_root)) {
    stop("Could not create the Windows long-path test root.")
  }
  on.exit(
    try(.builder_release_remove_stage(long_root), silent = TRUE),
    add = TRUE
  )
  source_file <- file.path(long_root, "source.jpg")
  writeBin(as.raw(1:32), source_file)
  target_dir <- long_root
  segment <- paste(rep("a", 40L), collapse = "")
  while (nchar(file.path(target_dir, segment), type = "bytes") <= 210L) {
    target_dir <- file.path(target_dir, segment)
  }
  remainder <- 253L - nchar(target_dir, type = "bytes") - 1L
  target_dir <- file.path(
    target_dir,
    paste(rep("d", remainder), collapse = "")
  )
  if (!.bundleCreateDirectory(target_dir, recursive = TRUE)) {
    stop("Could not create the Windows long-path test directory.")
  }
  target_file <- file.path(
    target_dir,
    "bbbbbbbbbbbbbbbbbbbbbbbbbb.jpg"
  )
  stopifnot(
    nchar(target_dir, type = "bytes") > 248L,
    nchar(target_file, type = "bytes") > 260L,
    .bundleCopyPath(source_file, target_file),
    .bundleCopiedTargetExists(target_file)
  )
  source_tree <- file.path(long_root, "source-tree")
  source_tree_file <- file.path(source_tree, "nested", "payload.bin")
  dir.create(dirname(source_tree_file), recursive = TRUE)
  writeBin(as.raw(33:64), source_tree_file)
  stopifnot(.bundleCopyPath(
    source_tree,
    target_dir,
    recursive = TRUE
  ))
  copied_tree_file <- file.path(
    target_dir,
    basename(source_tree),
    "nested",
    "payload.bin"
  )
  stopifnot(
    nchar(copied_tree_file, type = "bytes") > 260L,
    .bundleCopiedTargetExists(copied_tree_file),
    identical(
      readBin(.bundleWindowsExtendedPath(copied_tree_file), "raw", n = 32L),
      as.raw(33:64)
    )
  )
  .builder_release_remove_stage(long_root)
  stopifnot(!dir.exists(long_root))
}

kinds <- builder_native_picker_kinds()
stopifnot(identical(
  kinds,
  c(
    "output_directory",
    "project_directory",
    "project_manifest",
    "dataset_files"
  )
))

contracts <- lapply(kinds, builder_native_picker_contract)
stopifnot(identical(
  vapply(contracts, `[[`, character(1), "type"),
  c("directory", "directory", "file", "file")
))

windows_which <- function(command) {
  if (command %in% c("powershell.exe", "powershell", "pwsh.exe", "pwsh")) {
    "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
  } else {
    ""
  }
}
windows <- lapply(kinds, function(kind) {
  builder_native_picker_spec(
    kind,
    .system = "Windows",
    .which = windows_which
  )
})
stopifnot(
  all(vapply(windows, function(spec) "-STA" %in% spec$args, logical(1))),
  all(vapply(
    windows,
    function(spec) {
      identical(spec$encoding, "UTF-8")
    },
    logical(1)
  )),
  all(vapply(
    windows[1:2],
    function(spec) {
      grepl("IFileOpenDialog", paste(spec$args, collapse = "\n"), fixed = TRUE)
    },
    logical(1)
  )),
  all(vapply(
    windows[1:2],
    function(spec) {
      !grepl(
        "FolderBrowserDialog",
        paste(spec$args, collapse = "\n"),
        fixed = TRUE
      )
    },
    logical(1)
  )),
  grepl(
    "$dialog.AutoUpgradeEnabled = $true",
    paste(windows[[3L]]$args, collapse = "\n"),
    fixed = TRUE
  ),
  grepl(
    "$dialog.Multiselect = $true",
    paste(windows[[4L]]$args, collapse = "\n"),
    fixed = TRUE
  ),
  grepl(
    "$dialog.FileNames",
    paste(windows[[4L]]$args, collapse = "\n"),
    fixed = TRUE
  )
)

macos <- lapply(kinds, function(kind) {
  builder_native_picker_spec(kind, .system = "Darwin")
})
stopifnot(
  all(vapply(
    macos,
    function(spec) {
      identical(spec$command, "osascript")
    },
    logical(1)
  )),
  all(vapply(
    macos,
    function(spec) {
      grepl("on error number -128", spec$args[[2L]], fixed = TRUE)
    },
    logical(1)
  )),
  grepl(
    "with multiple selections allowed",
    macos[[4L]]$args[[2L]],
    fixed = TRUE
  )
)

picker_which <- function(name) {
  force(name)
  function(command) {
    if (identical(command, name)) file.path("/usr/bin", name) else ""
  }
}
zenity <- lapply(kinds, function(kind) {
  builder_native_picker_spec(
    kind,
    .system = "Linux",
    .which = picker_which("zenity")
  )
})
kdialog <- lapply(kinds, function(kind) {
  builder_native_picker_spec(
    kind,
    .system = "Linux",
    .which = picker_which("kdialog")
  )
})
stopifnot(
  all(vapply(
    zenity[1:2],
    function(spec) {
      "--directory" %in% spec$args
    },
    logical(1)
  )),
  "--file-filter=Builder project | *.json" %in% zenity[[3L]]$args,
  "--multiple" %in% zenity[[4L]]$args,
  any(startsWith(zenity[[4L]]$args, "--separator=")),
  all(vapply(
    kdialog[1:2],
    function(spec) {
      "--getexistingdirectory" %in% spec$args
    },
    logical(1)
  )),
  "--getopenfilename" %in% kdialog[[3L]]$args,
  "Builder project (*.json)" %in% kdialog[[3L]]$args,
  "--multiple" %in% kdialog[[4L]]$args,
  "--separate-output" %in% kdialog[[4L]]$args
)

## On macOS, compile the generated AppleScript without opening its dialogs.
if (identical(Sys.info()[["sysname"]], "Darwin")) {
  for (index in seq_along(kinds)) {
    script_path <- tempfile(fileext = ".applescript")
    compiled <- tempfile(fileext = ".scpt")
    writeLines(macos[[index]]$args[[2L]], script_path, useBytes = TRUE)
    status <- system2("osacompile", c("-o", compiled, script_path))
    if (!identical(status, 0L)) {
      stop("AppleScript picker did not compile: ", kinds[[index]])
    }
  }
}

message("Builder picker contracts passed on ", Sys.info()[["sysname"]])
