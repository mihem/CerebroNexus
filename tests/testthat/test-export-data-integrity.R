test_that("external modes require a distinct .crb output path", {
  object <- make_ir_seurat(n_cells = 12L)
  output <- file.path(withr::local_tempdir(), "dataset.h5")

  expect_error(
    export_ir(object, output, expression_matrix_mode = "h5"),
    regexp = "same path|\\.crb|sidecar",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
})

test_that("external export basenames are portable across filesystems", {
  skip_if_not_installed("HDF5Array")

  object <- make_ir_seurat(n_cells = 12L)
  root <- withr::local_tempdir()
  for (name in c(
    "CON.crb",
    "CON .crb",
    "COM1 .crb",
    "bad:name.crb",
    ".crb",
    "bad?.crb"
  )) {
    output <- file.path(root, name)
    expect_error(
      export_ir(object, output, expression_matrix_mode = "h5"),
      regexp = "portable|reserved|file name",
      ignore.case = TRUE
    )
    ## Windows treats names such as CON and COM1 as devices, so file.exists()
    ## can be TRUE for them (win-builder, Windows Server 2022); look in the
    ## folder instead: no file of that name may have been created.
    expect_false(name %in% list.files(root, all.files = TRUE))
  }
})

artifact_mode <- function(path) {
  sprintf("%03o", bitwAnd(as.integer(file.info(path)$mode), 511L))
}

minimal_export <- function(type = "embedded", location = NULL) {
  export <- new.env(parent = emptyenv())
  backend <- list(type = type, location = location)
  export$expression_backend <- backend
  export$getExpressionBackend <- local({
    function() backend
  })
  class(export) <- c("Cerebro", "R6")
  export
}

test_that("the export stage and new artifacts are owner-only", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  stage <- .createPrivateExportStage(output)
  withr::defer(unlink(stage, recursive = TRUE, force = TRUE))
  expect_match(basename(stage), "^\\.crb-stage-")
  expect_identical(artifact_mode(stage), "700")

  .publishCerebroExport(
    export = minimal_export(),
    final_file = output,
    stage_dir = stage,
    expression_matrix_mode = "embedded"
  )
  expect_identical(artifact_mode(output), "600")

  sidecar_name <- "external.h5"
  sidecar_output <- file.path(root, "external.crb")
  sidecar_stage <- .createPrivateExportStage(sidecar_output)
  withr::defer(unlink(sidecar_stage, recursive = TRUE, force = TRUE))
  writeLines("private matrix", file.path(sidecar_stage, sidecar_name))
  .publishCerebroExport(
    export = minimal_export("h5", sidecar_name),
    final_file = sidecar_output,
    stage_dir = sidecar_stage,
    expression_matrix_mode = "h5"
  )
  expect_identical(
    artifact_mode(file.path(root, sidecar_name)),
    "600"
  )
})

test_that("CRB publication uses the fast gzip profile", {
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  stage <- file.path(root, "stage")
  dir.create(stage)
  opened <- NULL
  open_gz <- function(
    description,
    open = "",
    encoding = getOption("encoding"),
    compression = 6
  ) {
    opened <<- list(open = open, compression = compression)
    base::gzfile(description, open, encoding, compression)
  }

  .publishCerebroExport(
    export = minimal_export(),
    final_file = output,
    stage_dir = stage,
    expression_matrix_mode = "embedded",
    .open_gz = open_gz
  )

  expect_identical(opened, list(open = "wb", compression = 1L))
  expect_s3_class(readRDS(output), "Cerebro")
})

test_that("export stage failures are actionable without exposing local paths", {
  root <- file.path("private", "lab", "project")
  stage <- file.path(root, ".crb-stage-example")

  unwritable <- .exportStageFailureMessage(
    root,
    stage,
    writable = FALSE,
    os_type = "unix"
  )
  expect_match(unwritable, "not writable", fixed = TRUE)
  expect_match(unwritable, "writable local folder", fixed = TRUE)

  long_windows_path <- paste0("C:/", strrep("deep/", 55L), ".crb-stage-example")
  too_long <- .exportStageFailureMessage(
    dirname(long_windows_path),
    long_windows_path,
    writable = TRUE,
    os_type = "windows"
  )
  expect_match(too_long, "path is too long", fixed = TRUE)
  expect_match(too_long, "shorter folder", fixed = TRUE)

  rejected <- .exportStageFailureMessage(
    root,
    stage,
    writable = TRUE,
    os_type = "unix"
  )
  expect_match(rejected, "synced or network folder", fixed = TRUE)
  expect_false(grepl(root, rejected, fixed = TRUE))
})

test_that("replacing a CRB preserves its existing mode", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export(), output)
  Sys.chmod(output, mode = "0600")
  stage <- file.path(root, "stage")
  dir.create(stage, mode = "0700")

  .publishCerebroExport(
    export = minimal_export(),
    final_file = output,
    stage_dir = stage,
    expression_matrix_mode = "embedded"
  )

  expect_identical(artifact_mode(output), "600")
})

test_that("publication refuses to replace an unowned sidecar", {
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export(), output)

  sidecar_name <- "dataset.h5"
  sidecar <- file.path(root, sidecar_name)
  writeLines("unrelated user file", sidecar)
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeLines("new matrix", file.path(stage, sidecar_name))

  before_crb <- unname(tools::md5sum(output))
  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", sidecar_name),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "already exists|collision|refus",
    ignore.case = TRUE
  )
  expect_identical(readLines(sidecar), "unrelated user file")
  expect_identical(unname(tools::md5sum(output)), before_crb)
})

test_that("backend ownership checks do not execute serialized bindings", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "binding-ran")

  active <- new.env(parent = emptyenv())
  class(active) <- c("Cerebro", "R6")
  makeActiveBinding(
    "expression_backend",
    function(value) {
      writeLines("active", sentinel)
      list(type = "h5", location = "dataset.h5")
    },
    active
  )
  active_path <- file.path(root, "active.crb")
  saveRDS(active, active_path)
  expect_null(.readPublishedExportBackend(active_path))
  expect_false(file.exists(sentinel))

  lazy <- new.env(parent = emptyenv())
  class(lazy) <- c("Cerebro", "R6")
  delayedAssign(
    "expression_backend",
    {
      writeLines("lazy", sentinel)
      list(type = "h5", location = "dataset.h5")
    },
    assign.env = lazy
  )
  lazy_path <- file.path(root, "lazy.crb")
  saveRDS(lazy, lazy_path)
  expect_null(.readPublishedExportBackend(lazy_path))
  expect_false(file.exists(sentinel))

  plain <- minimal_export("h5", "dataset.h5")
  plain$getExpressionBackend <- function() {
    writeLines("getter", sentinel)
    list(type = "h5", location = "wrong.h5")
  }
  plain_path <- file.path(root, "plain.crb")
  saveRDS(plain, plain_path)
  expect_identical(
    .readPublishedExportBackend(plain_path),
    list(type = "h5", location = "dataset.h5")
  )
  expect_false(file.exists(sentinel))

  factor_backend <- minimal_export("h5", "dataset.h5")
  factor_backend$expression_backend$type <- factor("h5")
  factor_path <- file.path(root, "factor.crb")
  saveRDS(factor_backend, factor_path)
  expect_null(.readPublishedExportBackend(factor_path))
})

test_that("publisher accepts only its fixed sidecar name", {
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeLines("matrix", file.path(stage, "other.h5"))

  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "other.h5"),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "fixed|expected|dataset\\.h5",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
  expect_false(file.exists(file.path(root, "other.h5")))
})

test_that("publisher rejects a symbolic-link staged sidecar", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  target <- file.path(root, "private-source.h5")
  writeLines("do not publish", target)
  stage <- file.path(root, "stage")
  dir.create(stage)
  expect_true(file.symlink(target, file.path(stage, "dataset.h5")))

  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "dataset.h5"),
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "symbolic.link|symlink",
    ignore.case = TRUE
  )
  expect_false(file.exists(output))
  expect_false(file.exists(file.path(root, "dataset.h5")))
  expect_identical(readLines(target), "do not publish")
})

test_that("publisher rejects a sidecar with the wrong artifact type", {
  root <- withr::local_tempdir()

  h5_output <- file.path(root, "h5.crb")
  h5_stage <- file.path(root, "h5-stage")
  dir.create(h5_stage)
  dir.create(file.path(h5_stage, "h5.h5"))
  expect_error(
    .publishCerebroExport(
      export = minimal_export("h5", "h5.h5"),
      final_file = h5_output,
      stage_dir = h5_stage,
      expression_matrix_mode = "h5"
    ),
    regexp = "h5.*file|regular file",
    ignore.case = TRUE
  )
})

test_that("H5 replacement updates only its owned portable sibling", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)

  export_ir(object, output, expression_matrix_mode = "h5")
  first_location <- readRDS(output)$getExpressionBackend()$location
  first_sidecar <- file.path(root, first_location)
  first_md5 <- unname(tools::md5sum(first_sidecar))

  changed <- SeuratObject::LayerData(object[["RNA"]], layer = "data")
  changed@x <- changed@x + 100
  SeuratObject::LayerData(object[["RNA"]], layer = "data") <- changed
  export_ir(object, output, expression_matrix_mode = "h5")
  second_location <- readRDS(output)$getExpressionBackend()$location
  second_sidecar <- file.path(root, second_location)

  expect_identical(first_location, "dataset.h5")
  expect_identical(second_location, first_location)
  expect_false(identical(unname(tools::md5sum(second_sidecar)), first_md5))
  expect_true(file.exists(second_sidecar))
})

test_that("createShinyApp bundles the sidecar produced by the exporter", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)
  export_ir(object, output, expression_matrix_mode = "h5")

  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c(dataset = output),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(app_dir, "private-data", "dataset.crb")))
  expect_true(file.exists(file.path(app_dir, "private-data", "dataset.h5")))
})

test_that("switching to embedded removes the old owned sidecar", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)

  export_ir(object, output, expression_matrix_mode = "h5")
  old_location <- readRDS(output)$getExpressionBackend()$location
  old_sidecar <- file.path(root, old_location)
  expect_true(file.exists(old_sidecar))

  export_ir(object, output, expression_matrix_mode = "embedded")

  expect_identical(
    readRDS(output)$getExpressionBackend()$type,
    "embedded"
  )
  expect_false(file.exists(old_sidecar))
})

test_that("a publication error restores the previous H5 export", {
  skip_if_not_installed("rhdf5")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  saveRDS(minimal_export("h5", "dataset.h5"), output)
  old_sidecar <- file.path(root, "dataset.h5")
  writeLines("old matrix", old_sidecar)
  before_crb <- unname(tools::md5sum(output))
  before_sidecar <- unname(tools::md5sum(old_sidecar))

  ## The staged file is not a readable H5 matrix, so thinning the CRB fails
  ## after the sidecar has already been moved into place.
  stage <- file.path(root, "stage")
  dir.create(stage)
  writeLines("new matrix", file.path(stage, "dataset.h5"))
  export <- minimal_export("h5", "dataset.h5")
  lockEnvironment(export)

  expect_error(
    suppressWarnings(.publishCerebroExport(
      export = export,
      final_file = output,
      stage_dir = stage,
      expression_matrix_mode = "h5"
    ))
  )
  expect_identical(unname(tools::md5sum(output)), before_crb)
  expect_identical(unname(tools::md5sum(old_sidecar)), before_sidecar)
  expect_length(
    list.files(
      root,
      all.files = TRUE,
      pattern = "^\\.(crb|side|spatial|immune)-b-"
    ),
    0L
  )
})


test_that("late validation cannot replace an existing H5 export", {
  skip_if_not_installed("HDF5Array")

  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  object <- make_ir_seurat(n_cells = 12L)
  expect_no_error(export_ir(object, output, expression_matrix_mode = "h5"))
  location <- readRDS(output)$getExpressionBackend()$location
  sidecar <- file.path(root, location)
  before_crb <- unname(tools::md5sum(output))
  before_sidecar <- unname(tools::md5sum(sidecar))

  changed <- SeuratObject::LayerData(object[["RNA"]], layer = "data")
  changed@x <- changed@x + 100
  SeuratObject::LayerData(object[["RNA"]], layer = "data") <- changed
  object@misc$immune_repertoire <- list(
    s1 = data.frame(CTgene = "TRAV1_TRAJ1")
  )

  expect_error(
    export_ir(object, output, expression_matrix_mode = "h5"),
    regexp = "barcode"
  )
  expect_identical(unname(tools::md5sum(output)), before_crb)
  expect_identical(unname(tools::md5sum(sidecar)), before_sidecar)
})


## ---------------------------------------------------------------------------
## Immune repertoire sidecar
## ---------------------------------------------------------------------------

export_ir_sidecar <- function(object, file, ...) {
  exportFromSeurat(
    object = object,
    file = file,
    experiment_name = "immune sidecar",
    organism = "hg",
    groups = c("sample", "cluster"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    codec = "qs2",
    verbose = FALSE,
    ...
  )
  invisible(file)
}

immune_seurat <- function(n_cells = 12L) {
  object <- make_ir_seurat(n_cells = n_cells)
  object@misc$immune_repertoire <- make_ir_repertoire(object)
  object
}

test_that("an always-sidecar export publishes the repertoire next to the CRB", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")

  export_ir_sidecar(object, output, immune_sidecar = "always")

  sidecar <- file.path(root, "dataset.immune.qs2")
  expect_true(file.exists(sidecar))
  expect_identical(
    .readPublishedImmuneBackend(output),
    list(type = "file", file = "dataset.immune.qs2")
  )
  payload <- .readCerebroPayload(output)
  expect_length(payload$immune_repertoire, 0L)
  descriptor <- payload$immune_repertoire_backend
  expect_identical(descriptor$md5, unname(tools::md5sum(sidecar)))
  expect_setequal(descriptor$samples, c("donorA", "donorB"))
  expect_setequal(descriptor$chains, c("TRA", "TRB"))
  expect_false("root" %in% names(descriptor))

  inline <- file.path(root, "inline.crb")
  export_ir_sidecar(object, inline, immune_sidecar = "never")
  expect_false(file.exists(file.path(root, "inline.immune.qs2")))
  expect_length(.readCerebroPayload(inline)$immune_repertoire, 2L)
  expect_identical(
    readCerebro(output)$getImmuneRepertoire(),
    readCerebro(inline)$getImmuneRepertoire()
  )
})

test_that("the sidecar follows the codec and the auto threshold", {
  skip_if_not_installed("HDF5Array")
  object <- immune_seurat()
  root <- withr::local_tempdir()

  rds <- file.path(root, "rds.crb")
  export_ir(object, rds, immune_sidecar = "always")
  expect_true(file.exists(file.path(root, "rds.immune.rds")))
  expect_false(file.exists(file.path(root, "rds.immune.qs2")))

  ## auto externalises only an H5-backed CRB above the threshold.
  embedded <- file.path(root, "embedded.crb")
  export_ir_sidecar(object, embedded, immune_sidecar_threshold = 1L)
  expect_false(file.exists(file.path(root, "embedded.immune.qs2")))

  small <- file.path(root, "small.crb")
  export_ir_sidecar(
    object,
    small,
    expression_matrix_mode = "h5",
    immune_sidecar_threshold = 1000L
  )
  expect_false(file.exists(file.path(root, "small.immune.qs2")))

  large <- file.path(root, "large.crb")
  export_ir_sidecar(
    object,
    large,
    expression_matrix_mode = "h5",
    immune_sidecar_threshold = 1L
  )
  expect_true(file.exists(file.path(root, "large.immune.qs2")))
  expect_true(file.exists(file.path(root, "large.h5")))
})

test_that("re-exporting replaces the owned immune sidecar", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  sidecar <- file.path(root, "dataset.immune.qs2")

  export_ir_sidecar(object, output, immune_sidecar = "always")
  first_md5 <- unname(tools::md5sum(sidecar))

  changed <- object@misc$immune_repertoire
  changed$donorA$CTaa[[1L]] <- "CHANGED"
  object@misc$immune_repertoire <- changed
  export_ir_sidecar(object, output, immune_sidecar = "always")

  expect_false(identical(unname(tools::md5sum(sidecar)), first_md5))
  expect_identical(
    .readCerebroPayload(output)$immune_repertoire_backend$md5,
    unname(tools::md5sum(sidecar))
  )
  expect_identical(
    readCerebro(output)$getImmuneRepertoire()$donorA$CTaa[[1L]],
    "CHANGED"
  )
  expect_length(
    list.files(
      root,
      all.files = TRUE,
      pattern = "^\\.(crb|side|spatial|immune)-b-"
    ),
    0L
  )
})

test_that("an export refuses to overwrite a foreign immune sidecar name", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  foreign <- file.path(root, "dataset.immune.qs2")
  writeLines("unrelated user file", foreign)

  expect_error(
    export_ir_sidecar(object, output, immune_sidecar = "always"),
    "not owned by the published CRB"
  )
  expect_identical(readLines(foreign), "unrelated user file")
  expect_false(file.exists(output))

  ## A CRB that does not name the file as its own sidecar does not own it.
  export_ir_sidecar(object, output, immune_sidecar = "never")
  before_crb <- unname(tools::md5sum(output))
  expect_error(
    export_ir_sidecar(object, output, immune_sidecar = "always"),
    "not owned by the published CRB"
  )
  expect_identical(readLines(foreign), "unrelated user file")
  expect_identical(unname(tools::md5sum(output)), before_crb)
})

test_that("a failure after the immune sidecar is installed removes it again", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")

  testthat::local_mocked_bindings(
    .stageSpatialMolecules = function(...) {
      stop("injected publication failure", call. = FALSE)
    },
    .package = "CerebroNexus"
  )
  expect_error(
    export_ir_sidecar(object, output, immune_sidecar = "always"),
    "injected publication failure"
  )
  expect_false(file.exists(file.path(root, "dataset.immune.qs2")))
  expect_false(file.exists(output))
})

test_that("a failure after the immune sidecar is installed restores the old one", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")
  sidecar <- file.path(root, "dataset.immune.qs2")
  export_ir_sidecar(object, output, immune_sidecar = "always")
  before_crb <- unname(tools::md5sum(output))
  before_sidecar <- unname(tools::md5sum(sidecar))

  changed <- object@misc$immune_repertoire
  changed$donorB$CTaa[[1L]] <- "CHANGED"
  object@misc$immune_repertoire <- changed
  testthat::local_mocked_bindings(
    .stageSpatialMolecules = function(...) {
      stop("injected publication failure", call. = FALSE)
    },
    .package = "CerebroNexus"
  )
  expect_error(
    export_ir_sidecar(object, output, immune_sidecar = "always"),
    "injected publication failure"
  )
  expect_identical(unname(tools::md5sum(output)), before_crb)
  expect_identical(unname(tools::md5sum(sidecar)), before_sidecar)
  expect_length(
    list.files(
      root,
      all.files = TRUE,
      pattern = "^\\.(crb|side|spatial|immune)-b-"
    ),
    0L
  )
})

test_that(".readPublishedImmuneBackend reads only a stored file descriptor", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "binding-ran")
  immune_export <- function(descriptor) {
    export <- new.env(parent = emptyenv())
    class(export) <- c("Cerebro", "R6")
    export$immune_repertoire_backend <- descriptor
    export
  }
  published <- function(export, name = "export.crb") {
    path <- file.path(root, name)
    saveRDS(export, path)
    .readPublishedImmuneBackend(path)
  }

  expect_identical(
    published(immune_export(list(
      type = "file",
      file = "dataset.immune.qs2",
      md5 = strrep("a", 32L),
      samples = "s1",
      chains = "TRB"
    ))),
    list(type = "file", file = "dataset.immune.qs2")
  )
  expect_null(published(immune_export(NULL)))
  expect_null(published(immune_export(list(
    type = "retired-file",
    file = "immune_repertoire.qs2"
  ))))
  expect_null(published(immune_export(list(
    type = "file",
    file = "nested/dataset.immune.qs2"
  ))))
  expect_null(published(immune_export(list(type = "file", file = ".."))))
  expect_null(.readPublishedImmuneBackend(file.path(root, "missing.crb")))

  active <- new.env(parent = emptyenv())
  class(active) <- c("Cerebro", "R6")
  makeActiveBinding(
    "immune_repertoire_backend",
    function(value) {
      writeLines("active", sentinel)
      list(type = "file", file = "dataset.immune.qs2")
    },
    active
  )
  expect_null(published(active, "active.crb"))

  lazy <- new.env(parent = emptyenv())
  class(lazy) <- c("Cerebro", "R6")
  delayedAssign(
    "immune_repertoire_backend",
    {
      writeLines("lazy", sentinel)
      list(type = "file", file = "dataset.immune.qs2")
    },
    assign.env = lazy
  )
  expect_null(published(lazy, "lazy.crb"))
  expect_false(file.exists(sentinel))
})


test_that("a re-export removes an immune sidecar it no longer uses", {
  object <- immune_seurat()
  root <- withr::local_tempdir()
  output <- file.path(root, "dataset.crb")

  export_ir_sidecar(object, output, immune_sidecar = "always")
  expect_true(file.exists(file.path(root, "dataset.immune.qs2")))

  export_ir_sidecar(object, output, immune_sidecar = "never")
  expect_setequal(list.files(root), "dataset.crb")
  expect_length(.readCerebroPayload(output)$immune_repertoire, 2L)

  export_ir_sidecar(object, output, immune_sidecar = "always")
  export_ir(object, output, immune_sidecar = "always")
  expect_setequal(
    list.files(root),
    c("dataset.crb", "dataset.immune.rds")
  )
})

test_that("convertSeuratToCerebro forwards the H5 level and the sidecar options", {
  formals_of <- names(formals(convertSeuratToCerebro))
  expect_true(all(
    c("h5_compression_level", "immune_sidecar", "immune_sidecar_threshold") %in%
      formals_of
  ))
})
