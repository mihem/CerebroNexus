ir_cache_functions <- function() {
  scope <- new.env(parent = globalenv())
  scope$IR_ENCODING_VERSION <- 1L
  sys.source(viewer_test_path("immune_repertoire", "encoding_cache.R"), scope)
  for (expr in parse(viewer_test_path("immune_repertoire", "data.R"))) {
    if (
      is.call(expr) &&
        as.character(expr[[1]])[1] %in% c("<-", "=") &&
        is.call(expr[[3]]) &&
        identical(expr[[3]][[1]], as.name("function"))
    ) {
      eval(expr, scope)
    }
  }
  scope
}

test_that("one reader opens a repertoire sidecar, also without the namespace", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  thin <- list(
    s1 = data.frame(barcode = c(2L, 1L, 3L), CTaa = c("A", "A", "B"))
  )
  attr(thin$s1, "cerebro_cell_index_column") <- "barcode"
  file <- file.path(root, "x.immune.qs2")
  qs2::qs_save(thin, file)
  md5 <- unname(tools::md5sum(file))
  expect_identical(.readRepertoireSidecar(file, md5, 4L), thin)
  ## the copy another R process gets needs nothing of the namespace
  reader <- .readRepertoireSidecarFunction()
  expect_identical(environmentName(parent.env(environment(reader))), "base")
  expect_identical(reader(file, md5, 4L), thin)
  ## checks: the checksum, the range of the index, one index per row
  expect_error(reader(file, "0", 4L), "checksum does not match")
  expect_error(reader(file, md5, 2L), "invalid repertoire cell index")
  short <- thin
  short$s1 <- structure(
    list(barcode = c(2L, 1L), CTaa = c("A", "A", "B")),
    class = "data.frame",
    row.names = 1:3,
    cerebro_cell_index_column = "barcode"
  )
  qs2::qs_save(short, file)
  expect_error(
    reader(file, unname(tools::md5sum(file)), 4L),
    "invalid repertoire cell index"
  )
  ## an RDS sidecar too
  rds <- file.path(root, "x.immune.rds")
  saveRDS(thin, rds)
  expect_identical(reader(rds, unname(tools::md5sum(rds)), 4L), thin)
})

test_that("an encoding made in memory is kept for the next session", {
  skip_if_not_installed("qs2")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(version = 1L, sample = 1:3)
  }
  key <- list(path = "a.crb", size = 1, mtime = 2, repertoire = "")
  first <- scope$ir_process_memo(
    "encoded",
    key,
    compute,
    new.env(),
    disk = TRUE
  )
  expect_identical(calls, 1L)
  ## A new process (an empty memo) reads the file instead of encoding again.
  second <- scope$ir_process_memo(
    "encoded",
    key,
    compute,
    new.env(),
    disk = TRUE
  )
  expect_identical(calls, 1L)
  expect_identical(second, first)
  ## Another data set, or a changed file, is another entry.
  other <- modifyList(key, list(path = "b.crb"))
  scope$ir_process_memo("encoded", other, compute, new.env(), disk = TRUE)
  expect_identical(calls, 2L)
  ## The same file in another version replaces the old entry.
  changed <- modifyList(key, list(size = 9))
  scope$ir_process_memo("encoded", changed, compute, new.env(), disk = TRUE)
  expect_identical(calls, 3L)
  ## Without `disk` nothing is written or read.
  scope$ir_process_memo("plain", key, compute, new.env())
  expect_identical(calls, 4L)
  expect_length(list.files(Sys.getenv("CEREBRO_IR_CACHE")), 2L)
})

test_that("a kept encoding reads its clone names only when they are asked for", {
  skip_if_not_installed("qs2")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), scope)
  scope$IR_ENCODING_VERSION <- 1L
  scope$ir_process_cache <- new.env()
  names_of <- list(CTaa = list(both = c("CASSZ", "CASSA", "CASSM")))
  compute <- function() {
    list(
      version = 1L,
      clones = list(CTaa = list(both = 1:3)),
      names = names_of
    )
  }
  key <- list(path = "a.crb", size = 1, mtime = 2, repertoire = "")
  first <- scope$ir_process_memo(
    "encoded",
    key,
    compute,
    new.env(),
    disk = TRUE
  )
  ## the process that encoded keeps the names in memory
  expect_identical(first$names, names_of)
  file <- scope$ir_disk_cache_file("encoded", key)
  names_file <- scope$ir_disk_cache_names_file(file)
  expect_true(file.exists(names_file))
  ## a later process reads the columns and the ranks, not the names
  later <- scope$ir_process_memo(
    "encoded",
    key,
    function() stop("encoded again"),
    new.env(),
    disk = TRUE
  )
  expect_null(later$names)
  expect_identical(later$rank$CTaa$both, c(3L, 1L, 2L))
  expect_identical(
    scope$ir_clone_names(later, "CTaa", "both", c(2L, 3L)),
    c("CASSA", "CASSM")
  )
  ## without its names file the kept encoding is not used
  unlink(names_file)
  expect_null(scope$ir_disk_cache_read(file))
  ## one kept in one file by an earlier release is kept again in two
  qs2::qs_save(compute(), file)
  expect_identical(scope$ir_disk_cache_read(file)$names, names_of)
  expect_true(file.exists(names_file))
  expect_true(isTRUE(qs2::qs_read(file)$names_apart))
})

test_that("a damaged or outdated cache file is encoded again", {
  skip_if_not_installed("qs2")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  key <- list(path = "b.crb")
  file <- scope$ir_disk_cache_file("encoded", key)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines("not a qs2 file", file)
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(version = 1L)
  }
  scope$ir_process_memo("encoded", key, compute, new.env(), disk = TRUE)
  expect_identical(calls, 1L)
  ## Written again, readable now; an older encoding version is not used.
  expect_identical(scope$ir_disk_cache_read(file)$version, 1L)
  qs2::qs_save(list(version = 0L), file)
  expect_null(scope$ir_disk_cache_read(file))
  ## A cache directory that cannot be written is not an error.
  withr::local_envvar(CEREBRO_IR_CACHE = file.path(file, "inside-a-file"))
  expect_no_error(
    scope$ir_process_memo("encoded", key, compute, new.env(), disk = TRUE)
  )
})

test_that("encoding in memory says why and how to avoid it", {
  scope <- ir_cache_functions()
  expect_message(
    scope$ir_notify_slow_encoding(list(manifest = list())),
    "built before the fast Immune repertoire format"
  )
  expect_message(
    scope$ir_notify_slow_encoding(NULL),
    "has no Viewer Pack"
  )
  expect_message(
    scope$ir_notify_slow_encoding(NULL),
    "buildViewerPack(<crb file>, overwrite = TRUE)",
    fixed = TRUE
  )
})

test_that("another R process encodes the repertoire, and keeps it for the next session", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("mirai")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), scope)
  scope$ir_process_cache <- new.env()
  scope$ir_encode_jobs <- new.env()
  scope$Cerebro.options <- list(
    cerebro_root = dirname(dirname(viewer_test_path("immune_repertoire")))
  )
  ## as .viewerMirai() in viewer/crb_cache.R starts it
  scope$.viewerMirai <- function(.expr, ...) {
    expr <- substitute(.expr)
    mirai::mirai(expr, ...)
  }
  repertoire <- list(
    s1 = data.frame(
      barcode = c("a1", "a2", "a3"),
      CTgene = c("TRAV1.TRAJ1_TRBV2.TRBJ1", "TRAV1.TRAJ1_TRBV2.TRBJ1", NA),
      CTnt = c("x_y", "x_y", ""),
      CTaa = c("CAA_CBB", "CAA_CBB", ""),
      CTstrict = c(
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        ""
      )
    )
  )
  cells <- c("a2", "a1", "a3", "a4")
  ## large enough for another process (50,000 rows)
  repertoire$s1 <- repertoire$s1[rep(1:3, length.out = 50001L), ]
  scope$data_set <- function() list(immune_repertoire = repertoire)
  scope$getImmuneRepertoire <- function() repertoire
  scope$getMetaData <- function() data.frame(cell_barcode = cells)
  scope$ir_notify_slow_encoding <- function(pack) invisible(NULL)
  key <- list(path = "a.crb", size = 1, mtime = 2, repertoire = "")
  here <- function() stop("encoded in this process")

  first <- scope$ir_encoded_background(key, NULL, here)
  expect_false(first$done)
  ## a small repertoire is encoded at once, in this process
  large <- scope$getImmuneRepertoire
  scope$getImmuneRepertoire <- function() lapply(repertoire, utils::head, 3L)
  small <- scope$ir_encoded_background(
    modifyList(key, list(path = "small.crb")),
    NULL,
    function() list(version = 2L, encoded = "here")
  )
  scope$getImmuneRepertoire <- large
  expect_true(small$done)
  expect_identical(small$value$encoded, "here")
  deadline <- Sys.time() + 60
  repeat {
    state <- scope$ir_encoded_background(key, NULL, here)
    if (isTRUE(state$done) || Sys.time() > deadline) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(state$done)
  expected <- scope$ir_encode_repertoire(repertoire, cells, rank = TRUE)
  ## the same encoding, its names kept apart and read on demand
  expect_identical(state$value$clones, expected$clones)
  expect_identical(state$value$cell, expected$cell)
  expect_identical(state$value$rank, expected$rank)
  expect_null(state$value$names)
  expect_identical(
    scope$ir_clone_names(state$value, "CTaa", "both", 1L),
    expected$names$CTaa$both[[1L]]
  )
  ## the next process reads it from the cache, without a job
  scope$ir_process_cache <- new.env()
  again <- scope$ir_encoded_background(key, NULL, here)
  expect_true(again$done)
  expect_identical(again$value$clones, expected$clones)
})

test_that("the job reads a thin sidecar's cell rows, not the cell names", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  skip_if_not_installed("mirai")
  withr::local_envvar(CEREBRO_IR_CACHE = withr::local_tempdir())
  scope <- ir_cache_functions()
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), scope)
  scope$ir_process_cache <- new.env()
  scope$ir_encode_jobs <- new.env()
  scope$Cerebro.options <- list(
    cerebro_root = dirname(dirname(viewer_test_path("immune_repertoire")))
  )
  scope$.viewerMirai <- function(.expr, ..., .env = character()) {
    expr <- substitute(.expr)
    mirai::mirai(expr, ...)
  }
  cells <- c("a2", "a1", "a3", "a4")
  named <- list(
    s1 = data.frame(
      barcode = c("a1", "a2", "a3"),
      CTgene = c("TRAV1.TRAJ1_TRBV2.TRBJ1", "TRAV1.TRAJ1_TRBV2.TRBJ1", NA),
      CTnt = c("x_y", "x_y", ""),
      CTaa = c("CAA_CBB", "CAA_CBB", ""),
      CTstrict = c(
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        "TRAV1.TRAJ1;x_TRBV2.TRBJ1;y",
        ""
      )
    )
  )
  thin <- named
  thin$s1$barcode <- match(thin$s1$barcode, cells)
  attr(thin$s1, "cerebro_cell_index_column") <- "barcode"
  root <- withr::local_tempdir()
  qs2::qs_save(thin, file.path(root, "x.immune.qs2"))
  scope$data_set <- function() {
    list(
      immune_repertoire = NULL,
      immune_repertoire_backend = list(
        type = "file",
        root = root,
        file = "x.immune.qs2",
        md5 = unname(tools::md5sum(file.path(root, "x.immune.qs2")))
      )
    )
  }
  ## a Pack whose cell names cannot be read: the job must not need them
  pack <- list(
    path = file.path(root, "missing.viewer"),
    cell_count = length(cells),
    manifest = list(
      assets = data.frame(path = "common/cell_order.qs2"),
      n_cells = length(cells)
    )
  )
  here <- function() stop("encoded in this process")
  key <- list(path = "thin.crb", size = 1, mtime = 2, repertoire = "")
  deadline <- Sys.time() + 60
  repeat {
    state <- scope$ir_encoded_background(key, pack, here, here_bytes = 0)
    if (isTRUE(state$done) || Sys.time() > deadline) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(state$done)
  expected <- scope$ir_encode_repertoire(named, cells, rank = TRUE)
  expect_identical(state$value$cell, expected$cell)
  expect_identical(state$value$clones, expected$clones)
})
