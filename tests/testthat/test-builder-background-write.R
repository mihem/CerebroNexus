test_that("import callbacks do not serialize the live Builder environment", {
  runtime <- new.env(parent = globalenv())
  for (file in c("project.R", "session.R")) {
    builder_repo_source(file, local = runtime)
  }
  runtime$large_session_value <- raw(2e6)
  callbacks <- lapply(
    list(
      importer = runtime$builder_project_load_retained_source,
      copy = runtime$builder_project_copy_file,
      marker = runtime$builder_project_transport_retained_write
    ),
    runtime$builder_session_worker_callback
  )
  callbacks$collect <- runtime$.builder_session_collect_failed_import
  expect_lt(length(serialize(callbacks, NULL)), 50000L)
  root <- withr::local_tempdir()
  source <- file.path(root, "source.rds")
  saveRDS(list(value = 42L), source)
  result <- callr::r(
    function(callbacks, root, source) {
      builder_seurat_file_adapter <<- function(path) readRDS(path)
      .builder_register_adapter <<- function(adapter, id, progress) adapter
      target <- file.path(root, "retained", "source.rds")
      marker <- file.path(root, "retained.rds")
      value <- callbacks$importer(
        "ds1",
        list(
          source = source,
          retained_path = target,
          transport_retained_path = marker
        ),
        NULL,
        .copy = callbacks$copy,
        .transport_writer = callbacks$marker
      )
      collected <- FALSE
      callbacks$collect(function(full) {
        collected <<- full
      })
      list(value = value$value, marker = readRDS(marker), collected = collected)
    },
    args = list(callbacks = callbacks, root = root, source = source)
  )
  expect_identical(result$value, 42L)
  expect_identical(result$marker$id, "ds1")
  expect_true(file.exists(result$marker$retained_path))
  expect_true(result$collected)
})

test_that("WSL picker paths preserve spaces and reject failed translation", {
  builder_repo_source("core/bundle_path_contract.R", local = environment())
  builder_repo_source("io.R", local = environment())
  selected <- c("C:\\Data space\\sample.rds", "D:\\研究\\builder-project.json")
  calls <- 0L
  converted <- builder_wsl_picker_paths(
    selected,
    .run = function(command, args, ...) {
      calls <<- calls + 1L
      # One shell converts every path, each as its own argument.
      expect_identical(command, "sh")
      expect_identical(args[[1]], "-c")
      expect_match(args[[2]], 'wslpath -u "$path"', fixed = TRUE)
      paths <- args[-(1:3)]
      expect_identical(paths, selected)
      list(
        status = 0L,
        stdout = paste0(
          "/mnt/",
          tolower(substr(paths, 1, 1)),
          gsub("\\", "/", substring(paths, 3), fixed = TRUE),
          "\n",
          collapse = ""
        )
      )
    }
  )
  expect_identical(calls, 1L)
  expect_identical(
    converted,
    c("/mnt/c/Data space/sample.rds", "/mnt/d/研究/builder-project.json")
  )
  expect_identical(builder_wsl_picker_paths(character()), character())
  expect_error(
    builder_wsl_picker_paths(selected, .run = function(...) {
      list(status = 1L, stdout = "")
    }),
    "Cannot access"
  )
  expect_error(
    builder_wsl_picker_paths(selected, .run = function(...) {
      list(status = 0L, stdout = "/mnt/c/only-one\n")
    }),
    "Cannot access"
  )
  spec <- builder_native_picker_spec(
    "dataset_files",
    .system = "Linux",
    .wsl = TRUE,
    .which = function(command) paste0("/bin/", command)
  )
  expect_identical(spec$path_style, "windows")
  expect_true("-EncodedCommand" %in% spec$args)
  expect_true(length(base64enc::base64decode(tail(spec$args, 1))) > 0L)
  root <- withr::local_tempdir()
  manifest <- file.path(root, "builder-project.json")
  writeLines("{}", manifest)
  expect_identical(
    builder_server_path_resolve(
      "C:\\Project files\\builder-project.json",
      "file",
      roots = root,
      .wsl = TRUE,
      .translate = function(value) {
        expect_identical(value, "C:\\Project files\\builder-project.json")
        manifest
      }
    ),
    as.character(fs::path_real(manifest))
  )
  dir.create(file.path(root, "restricted"))
  expect_error(
    builder_server_path_resolve(
      "C:\\Project files\\builder-project.json",
      "file",
      roots = file.path(root, "restricted"),
      .wsl = TRUE,
      .translate = function(value) manifest
    ),
    "allowed server folders"
  )
})
