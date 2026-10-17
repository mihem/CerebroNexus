boundary_utils <- function() {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("crb_cache.R"), envir = env)
  sys.source(viewer_test_path("msigdb.R"), envir = env)
  source(viewer_test_path("utility_functions.R"), local = env)
  env
}

test_that("CRB eviction bounds prototypes and preserves active clones and identities", {
  env <- boundary_utils()
  paths <- vapply(1:3, function(i) tempfile(fileext = ".crb"), character(1))
  objects <- lapply(1:3, function(i) {
    obj <- Cerebro$new()
    obj$setMetaData(data.frame(cell_barcode = paste0("cell", i)))
    obj
  })
  for (i in 1:2) {
    env$.cacheCrbPrototype(paths[i], objects[[i]], list(type = "embedded"))
  }
  active <- env$.cloneCachedCrb(objects[[2]])
  keys <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  # Explicit ordering avoids wall-clock resolution assumptions in the test.
  env$.crb_process_cache[[keys[1]]]$last_used <- 1
  env$.crb_process_cache[[keys[2]]]$last_used <- 2
  env$.touchCrbPrototype(keys[1])
  env$.cacheCrbPrototype(paths[3], objects[[3]], list(type = "embedded"))
  expect_setequal(ls(env$.crb_process_cache), keys[c(1, 3)])
  expect_identical(active$getMetaData()$cell_barcode, "cell2")
  expect_error(
    env$.cacheCrbPrototype(paths[2], objects[[2]], list(type = "h5")),
    "backend configuration changed"
  )
  env$.cacheCrbPrototype(paths[2], objects[[2]], list(type = "embedded"))
  expect_length(ls(env$.crb_process_cache), 2)
  # A live prototype must remain protected even without its registry entry.
  rm(list = keys[2], envir = env$.crb_backend_identities)
  expect_error(
    env$.checkCrbBackendIdentity(keys[2], list(type = "h5")),
    "backend configuration changed"
  )
})

test_that("catalogue construction retains only two launch prototypes", {
  paths <- vapply(1:4, function(i) tempfile(fileext = ".crb"), character(1))
  on.exit(unlink(paths))
  for (i in seq_along(paths)) {
    saveRDS(Cerebro$new(), paths[i])
  }
  catalog <- .datasetCatalogFromFiles(paths, retain_prototypes = TRUE)
  expect_length(catalog, 4)
  expect_named(attr(catalog, "prototypes"), paths[1:2])
})

test_that("prefetch is shared across sessions and releases completed task results", {
  env <- boundary_utils()
  # The directly loaded process module must not capture either session below.
  expect_identical(environment(env$start_crb_prefetch), env)
  start <- function(path) env$start_crb_prefetch(path, list(type = "embedded"))
  files <- vapply(1:3, function(i) tempfile(fileext = ".crb"), character(1))
  for (i in 1:3) {
    saveRDS(list(marker = i), files[i])
  }
  on.exit(unlink(files), add = TRUE)
  first <- shiny::MockShinySession$new()
  second <- shiny::MockShinySession$new()
  on.exit(
    {
      first$close()
      second$close()
    },
    add = TRUE
  )
  mirai::daemons(1L)
  on.exit(mirai::daemons(0L), add = TRUE)
  identity <- list(type = "embedded")
  shiny::withReactiveDomain(first, env$start_crb_prefetch(files[1], identity))
  identity$type <- "changed-after-start"
  key <- normalizePath(files[1], winslash = "/")
  task <- env$crb_prefetch_tasks[[key]]
  shiny::withReactiveDomain(second, start(files[1]))
  expect_identical(env$crb_prefetch_tasks[[key]], task)
  shiny::withReactiveDomain(second, start(files[2]))
  shiny::withReactiveDomain(second, start(files[3]))
  expect_length(ls(env$crb_prefetch_tasks), 2)
  first$close() # the initiating session must not own process completion
  ## Shared CI runners can take far longer to start the background worker.
  deadline <- Sys.time() + 60
  while (length(env$crb_prefetch_tasks) && Sys.time() < deadline) {
    later::run_now(0.05)
    shiny:::flushReact()
    Sys.sleep(0.01)
  }
  expect_length(ls(env$crb_prefetch_tasks), 0)
  expect_identical(env$.crb_process_cache[[key]]$object$marker, 1L)
  expect_identical(
    env$.crb_process_cache[[key]]$backend_identity,
    list(type = "embedded")
  )
  expect_length(ls(env$.crb_process_cache), 2)
  shiny::withReactiveDomain(second, start(files[1]))
  expect_length(ls(env$crb_prefetch_tasks), 0) # completed cache prevents another read
  writeLines("invalid CRB", files[3])
  start(files[3])
  warnings <- character()
  deadline <- Sys.time() + 60
  withCallingHandlers(
    {
      while (length(env$crb_prefetch_tasks) && Sys.time() < deadline) {
        later::run_now(0.05)
        shiny:::flushReact()
        Sys.sleep(0.01)
      }
    },
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(ls(env$crb_prefetch_tasks), 0)
  expect_true(any(grepl("CRB background prefetch failed", warnings)))
  expect_null(env$.crb_process_cache[[normalizePath(files[3], winslash = "/")]])
  saveRDS(list(marker = 3L), files[3])
  start(files[3])
  deadline <- Sys.time() + 60
  while (length(env$crb_prefetch_tasks) && Sys.time() < deadline) {
    later::run_now(0.05)
    shiny:::flushReact()
    Sys.sleep(0.01)
  }
  expect_identical(
    env$.crb_process_cache[[normalizePath(
      files[3],
      winslash = "/"
    )]]$object$marker,
    3L
  )
})

test_that("MSigDB catalogue queries collections separately and shares compact gene results", {
  env <- boundary_utils()
  env$.msigdbCollectionsFunction <- function() {
    data.frame(gs_collection = c("H", "C2"))
  }
  calls <- character()
  env$.msigdbFunction <- function() {
    function(species, collection) {
      calls <<- c(calls, paste(species, collection))
      data.frame(
        gs_name = paste0(collection, c("_A", "_A", "_B")),
        gs_collection = collection,
        gene_symbol = c("g2", "g1", "g3"),
        irrelevant = rep("wide unused field", 3)
      )
    }
  }
  expect_identical(env$getGeneSetNames(), c("C2_A", "C2_B", "H_A", "H_B"))
  expect_length(calls, 2)
  expect_identical(env$getMsigdbGenes("Mus musculus", "H_A"), c("g1", "g2"))
  expect_identical(env$getMsigdbGenes("Mus musculus", "H_B"), "g3")
  expect_length(calls, 3) # second set in the collection reuses compact mapping
  expect_identical(env$getMsigdbGenes("Homo sapiens", "H_B"), "g3")
  expect_length(calls, 4) # species is part of identity
  compact <- env$.msigdbCache()$compact_collection
  expect_named(compact, c("species", "collection", "genes"))
  expect_true(all(vapply(compact$genes, is.character, logical(1))))
  for (i in 1:140) {
    env$.cacheMsigdbGenes(env$.msigdbCache(), paste0("genes\r", i), "g")
  }
  expect_lte(length(grep("^genes\r", ls(env$.msigdbCache()))), 128)
  expect_lte(
    sum(vapply(
      mget(
        grep("^genes\r", ls(env$.msigdbCache()), value = TRUE),
        env$.msigdbCache()
      ),
      function(x) as.numeric(object.size(x)),
      numeric(1)
    )),
    4 * 1024^2
  )
})

test_that("Enrichr applies timeout and rejects HTTP errors before parsing", {
  requests <- list()
  fail_at <- Inf
  request <- function(url, ...) {
    dots <- list(...)
    configs <- Filter(function(x) inherits(x, "request"), dots)
    requests[[length(requests) + 1L]] <<- list(url = url, configs = configs)
    structure(
      list(
        status_code = if (length(requests) == fail_at) 503L else 200L,
        url = url,
        content = charToRaw("Term\tP.value\nA\t0.1\n")
      ),
      class = "response"
    )
  }
  local_mocked_bindings(POST = request, GET = request, .package = "httr")
  # Keep the fixture in process regardless of the caller's configured future plan.
  local_mocked_bindings(future_sapply = base::sapply, .package = "future.apply")
  result <- .send_enrichr_query("GENE", "DB", "https://example.invalid")
  expect_identical(result$DB$Term, "A")
  expect_length(requests, 3)
  expect_true(all(vapply(
    requests,
    function(r) {
      any(vapply(
        r$configs,
        function(config) identical(config$options$timeout_ms, 60000),
        logical(1)
      ))
    },
    logical(1)
  )))
  for (index in 1:3) {
    fail_at <- index
    requests <- list()
    expect_error(
      .send_enrichr_query("GENE", "DB", "https://example.invalid"),
      "503"
    )
    expect_length(requests, index)
  }
  requests <- list()
  expect_warning(
    expect_null(.send_enrichr_query(
      character(),
      "DB",
      "https://example.invalid"
    )),
    "non-empty"
  )
  expect_length(requests, 0)
})

test_that("delayed exports round-trip without whole-matrix as.matrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Seurat")
  suppressPackageStartupMessages(requireNamespace("DelayedArray"))
  class_name <- "CerebroNoDenseDelayedMatrix"
  if (!methods::isClass(class_name)) {
    methods::setClass(class_name, contains = "DelayedMatrix")
  }
  methods::setMethod("as.matrix", class_name, function(x, ...) {
    stop("whole-matrix realization forbidden")
  })
  on.exit(
    {
      methods::removeMethod("as.matrix", class_name)
      methods::removeClass(class_name)
    },
    add = TRUE
  )
  counts <- matrix(
    c(0, 1, 2, 0, 3, 4),
    2,
    dimnames = list(c("g1", "g2"), c("c1", "c2", "c3"))
  )
  obj <- SeuratObject::CreateSeuratObject(methods::as(counts, "CsparseMatrix"))
  obj$sample <- "s1"
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      1:6,
      3,
      dimnames = list(colnames(counts), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  exact <- counts / 7
  exact[2, 1] <- 1.123456789012345
  delayed <- methods::as(DelayedArray::DelayedArray(exact), class_name)
  cases <- list(
    fractional = list(data = delayed, expected = exact),
    integer_rle = list(
      data = methods::as(counts, "RleArray"),
      expected = counts
    )
  )
  for (case in cases) {
    resolution <- list(
      data = case$data,
      assay = "RNA",
      requested = "data",
      resolved = "data",
      joined = FALSE,
      fallback = FALSE
    )
    for (backend in c("h5", "bpcells")) {
      directory <- withr::local_tempdir()
      file <- file.path(directory, "test.crb")
      expect_no_error(exportFromSeurat(
        obj,
        file = file,
        experiment_name = "test",
        organism = "hg",
        groups = "sample",
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        codec = "rds",
        expression_matrix_mode = backend,
        .expression_resolution = resolution,
        verbose = FALSE
      ))
      result <- if (backend == "h5") {
        t(HDF5Array::TENxMatrix(
          file.path(directory, "test.h5"),
          group = "expression"
        ))
      } else {
        BPCells::open_matrix_dir(file.path(directory, "test.bpcells"))
      }
      expect_identical(dimnames(result), dimnames(case$expected))
      expect_equal(as.matrix(result), case$expected, tolerance = 0)
      if (backend == "bpcells") {
        expect_identical(BPCells::storage_order(result), "row")
        expect_identical(
          BPCells::matrix_type(result),
          if (identical(case$expected, counts)) "uint32_t" else "double"
        )
      }
      expect_length(
        list.files(directory, pattern = "delayed-expression", recursive = TRUE),
        0
      )
    }
  }
})
