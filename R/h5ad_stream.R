## Matrices with more than 2^31 - 1 non-zero values ---------------------------
## A dgCMatrix cannot hold them (its column pointers are 32-bit), so a matrix
## that is stored cell by cell in an HDF5 file (the CSR `X` of an h5ad file, or a
## 10x file, whose columns are cells) is not read into R when it is that large.
## It is streamed: read in blocks of cells, the sums the group summaries need are
## accumulated block by block, and the gene-major expression file the Viewer
## reads (TENx CSC of cells x genes) is written in two steps. The source is read
## once: each block of cells is sorted by gene and left in a scratch file, cut
## into buckets of genes that are compressed one by one. Then the genes are cut
## into ranges, and each range is collected from the buckets that hold its genes.
## Both steps run in several R processes (`cerebro.h5ad_stream_workers`); the
## ranges are left in scratch files for the main process to append, in gene
## order, to the H5 file.
##
## Reading the source is the costly part: a 10x file of 1.3 million cells and
## 2.6 billion values takes about two minutes to read and decompress. The ranges
## used to be collected by a pass over the whole source each: about ten passes
## for that file, 9 of the writer's 12 minutes.
##
## The functions that run in worker processes (`.h5adStreamReadBlock`,
## `.h5adStreamBuild`, `.h5adStreamSplitTask`, `.h5adStreamRangeTask`,
## `.h5adStreamSumsTask`) use only base R, Matrix, methods and rhdf5, and are
## shipped with `globalenv()` as their environment, so a worker does not need
## this package installed.

#' Number of non-zero values from which an h5ad X is streamed, not read.
#' @keywords internal
#' @noRd
.h5adStreamThreshold <- function() {
  getOption("cerebro.h5ad_stream_nnz", .Machine$integer.max)
}

#' A matrix stored cell by cell as three arrays (data, indices, indptr) in a group
#' of an HDF5 file. `n_obs` cells, `n_vars` genes.
#' @keywords internal
#' @noRd
.h5adStreamSource <- function(file, path, n_obs, n_vars, prefix = "") {
  indptr <- rhdf5::h5read(
    file,
    paste0(prefix, path, "/indptr"),
    bit64conversion = "double"
  )
  dim(indptr) <- NULL
  if (length(indptr) != n_obs + 1) {
    stop(
      "`",
      path,
      "` has an indptr that does not match its cells.",
      call. = FALSE
    )
  }
  list(
    file = file,
    prefix = prefix,
    path = path,
    n_obs = n_obs,
    n_vars = n_vars,
    nnz = indptr[[length(indptr)]],
    indptr = indptr
  )
}

#' The cell-major matrix of a 10x HDF5 file (CSC of genes x cells, group such as
#' "mm10" or "matrix"), as a source for `.h5adStreamWriteTENx()`.
#' @keywords internal
#' @noRd
.h5adStreamFrom10x <- function(file, group) {
  shape <- as.numeric(rhdf5::h5read(file, paste0(group, "/shape")))
  .h5adStreamSource(file, group, n_obs = shape[[2]], n_vars = shape[[1]])
}

#' Describe the X of an h5ad file when it has to be streamed: an HDF5 CSR matrix
#' with more non-zero values than `.h5adStreamThreshold()`. `NULL` otherwise.
#' @keywords internal
#' @noRd
.h5adStreamPlan <- function(ctx, path, n_obs, n_vars) {
  if (identical(ctx$kind, "zarr") || !.h5adHas(ctx, path)) {
    return(NULL)
  }
  if (!identical(.h5adEncoding(ctx, path), "csr_matrix")) {
    return(NULL)
  }
  indptr_end <- .h5adRead(
    ctx,
    paste0(path, "/indptr"),
    start = n_obs + 1,
    count = 1,
    bit64conversion = "double"
  )
  if (!(as.numeric(indptr_end) > .h5adStreamThreshold())) {
    return(NULL)
  }
  .h5adStreamSource(ctx$file, path, n_obs, n_vars, prefix = ctx$prefix)
}

#' Ranges of cells (0-based start, end) of at most `max_nnz` non-zero values each.
#' @keywords internal
#' @noRd
.h5adStreamBlocks <- function(
  source,
  max_nnz = getOption("cerebro.h5ad_stream_block", 2^26)
) {
  p <- source$indptr
  n <- source$n_obs
  from <- numeric(0)
  to <- numeric(0)
  c0 <- 0
  while (c0 < n) {
    c1 <- findInterval(p[[c0 + 1]] + max_nnz, p) - 1
    c1 <- min(n, max(c1, c0 + 1))
    from <- c(from, c0)
    to <- c(to, c1)
    c0 <- c1
  }
  list(from = from, to = to)
}

#' One block of cells: gene index (0-based), values (unless `values = FALSE`) and
#' the column pointers of the block. Runs in worker processes: base and rhdf5 only.
#' @keywords internal
#' @noRd
.h5adStreamReadBlock <- function(source, c0, c1, values = TRUE) {
  s <- source$indptr[[c0 + 1]]
  n <- source$indptr[[c1 + 1]] - s
  read <- function(name) {
    x <- rhdf5::h5read(
      source$file,
      paste0(source$prefix, source$path, "/", name),
      start = s + 1,
      count = n
    )
    dim(x) <- NULL
    x
  }
  list(
    gene = as.integer(read("indices")),
    value = if (values) as.numeric(read("data")),
    p = as.integer(source$indptr[(c0 + 1):(c1 + 1)] - s),
    first = s
  )
}

#' A block as a genes x cells dgCMatrix. The CSR rows of the file are the columns
#' of it; when the indices of a row are not sorted the matrix is built from
#' triplets instead. Runs in worker processes: base, Matrix and methods only.
#' @keywords internal
#' @noRd
.h5adStreamBuild <- function(x, n_vars) {
  nc <- length(x$p) - 1L
  built <- tryCatch(
    methods::new(
      "dgCMatrix",
      i = x$gene,
      p = x$p,
      x = x$value,
      Dim = c(n_vars, nc)
    ),
    error = function(e) NULL
  )
  if (is.null(built)) {
    built <- Matrix::sparseMatrix(
      i = x$gene + 1L,
      j = rep.int(seq_len(nc), diff(x$p)),
      x = x$value,
      dims = c(n_vars, nc)
    )
  }
  built
}

#' Sums over some blocks of cells: per level of `members` (cells x levels, all
#' groups side by side), the summed values and the number of positive values of
#' every gene. Runs in worker processes.
#' @keywords internal
#' @noRd
.h5adStreamSumsTask <- function(blocks, source, members, helpers) {
  loadNamespace("Matrix")
  k <- ncol(members)
  sums <- matrix(0, source$n_vars, k)
  positive <- matrix(0, source$n_vars, k)
  for (b in seq_along(blocks$from)) {
    c0 <- blocks$from[[b]]
    c1 <- blocks$to[[b]]
    x <- helpers$read(source, c0, c1)
    expr <- helpers$build(x, source$n_vars)
    m <- members[(c0 + 1):c1, , drop = FALSE]
    sums <- sums + as.matrix(expr %*% m)
    expr@x <- as.numeric(expr@x > 0)
    positive <- positive + as.matrix(expr %*% m)
  }
  list(sums = sums, positive = positive)
}

#' Edges of the buckets of genes a block file is cut into: 0-based, from 0 to
#' `n_vars`, at most `buckets` buckets of consecutive genes.
#' @keywords internal
#' @noRd
.h5adStreamBucketEdges <- function(
  n_vars,
  buckets = getOption("cerebro.h5ad_stream_buckets", 256L)
) {
  k <- max(1L, min(as.integer(buckets), as.integer(n_vars)))
  unique(round(seq(0, n_vars, length.out = k + 1L)))
}

#' How the buckets of a block file are compressed: zstd where R has it (4.5 and
#' later), else not at all (`cerebro.h5ad_stream_codec` chooses).
#' @keywords internal
#' @noRd
.h5adStreamCodec <- function() {
  getOption(
    "cerebro.h5ad_stream_codec",
    if ("zstd" %in% eval(formals(memCompress)$type)) "zstd" else "none"
  )
}

#' One block of cells (`item`: 0-based `c0`, `c1`, and `id`), sorted by gene,
#' with the cells ascending within a gene, left in `<dir>/block-<id>.bin`. Each
#' bucket of genes (`edges`) is there as its values (float64) and then its
#' 0-based cells (int32), each compressed on its own with `codec`. Returns the
#' file, the values per gene and, per bucket, the byte offset and length of the
#' values and of the cells. Runs in worker processes.
#' @keywords internal
#' @noRd
.h5adStreamSplitTask <- function(item, source, edges, dir, codec, helpers) {
  x <- helpers$read(source, item$c0, item$c1)
  n_vars <- edges[[length(edges)]]
  cell <- rep.int(
    seq.int(as.integer(item$c0), length.out = length(x$p) - 1L),
    diff(x$p)
  )
  ## a stable sort keeps the cells of a gene in order
  o <- order(x$gene, method = "radix")
  counts <- tabulate(x$gene + 1L, nbins = n_vars)
  value <- x$value[o]
  cell <- cell[o]
  rm(x, o)
  at <- c(0, cumsum(counts))[edges + 1]
  pack <- function(v) {
    bytes <- writeBin(v, raw())
    if (identical(codec, "none")) bytes else memCompress(bytes, codec)
  }
  path <- file.path(dir, sprintf("block-%05d.bin", item$id))
  connection <- file(path, open = "wb")
  on.exit(close(connection))
  index <- matrix(0, length(edges) - 1L, 4L)
  offset <- 0
  for (k in seq_len(length(edges) - 1L)) {
    if (at[[k + 1L]] > at[[k]]) {
      i <- (at[[k]] + 1):at[[k + 1L]]
      values <- pack(value[i])
      cells <- pack(cell[i])
      writeBin(values, connection)
      writeBin(cells, connection)
    } else {
      values <- cells <- raw()
    }
    index[k, ] <- c(
      offset,
      length(values),
      offset + length(values),
      length(cells)
    )
    offset <- offset + length(values) + length(cells)
  }
  list(path = path, counts = counts, index = index)
}

#' One range of genes `g0`..`g1` (0-based, end excluded): the values of those
#' genes in gene order, cells ascending within a gene, collected from the block
#' files (`blocks`, in the order of their cells) of `.h5adStreamSplitTask()`.
#' Returned, or, with `dir`, left in `<dir>/range-<id>-x.bin` (float64) and
#' `-i.bin` (int32, 0-based cell). Runs in worker processes.
#' @keywords internal
#' @noRd
.h5adStreamRangeTask <- function(
  range,
  blocks,
  edges,
  gene_ptr,
  codec,
  dir,
  helpers
) {
  g0 <- range$g0
  g1 <- range$g1
  ng <- g1 - g0
  first <- gene_ptr[[g0 + 1]]
  size <- gene_ptr[[g1 + 1]] - first
  value <- numeric(size)
  cell <- integer(size)
  fill <- numeric(ng)
  offset <- gene_ptr[(g0 + 1):g1] - first
  ## the buckets that hold genes of the range
  from <- findInterval(g0, edges)
  to <- findInterval(g1 - 1, edges)
  unpack <- function(connection, at, bytes, what, n) {
    seek(connection, at)
    raw_bytes <- readBin(connection, "raw", n = bytes)
    if (!identical(codec, "none")) {
      raw_bytes <- memDecompress(raw_bytes, codec)
    }
    readBin(raw_bytes, what, n = n)
  }
  for (block in blocks) {
    connection <- file(block$path, open = "rb")
    for (k in from:to) {
      genes <- edges[[k]]:(edges[[k + 1L]] - 1)
      here_all <- block$counts[genes + 1]
      n <- sum(here_all)
      if (!n) {
        next
      }
      values <- unpack(
        connection,
        block$index[k, 1L],
        block$index[k, 2L],
        "double",
        n
      )
      cells <- unpack(
        connection,
        block$index[k, 3L],
        block$index[k, 4L],
        "integer",
        n
      )
      ## the bucket's values are sorted by gene: those of the range are one run
      inside <- genes >= g0 & genes < g1
      skip <- sum(here_all[genes < g0])
      take <- sum(here_all[inside])
      if (!take) {
        next
      }
      keep <- (skip + 1):(skip + take)
      here <- numeric(ng)
      here[genes[inside] - g0 + 1] <- here_all[inside]
      run_start <- cumsum(c(0, here))[seq_len(ng)]
      position <- rep.int(offset + fill, here) +
        (seq_len(take) - rep.int(run_start, here))
      value[position] <- values[keep]
      cell[position] <- cells[keep]
      fill <- fill + here
    }
    close(connection)
  }
  if (is.null(dir)) {
    return(list(value = value, cell = cell))
  }
  path <- file.path(dir, sprintf("range-%05d", range$id))
  connection <- file(paste0(path, "-x.bin"), open = "wb")
  writeBin(value, connection)
  close(connection)
  connection <- file(paste0(path, "-i.bin"), open = "wb")
  writeBin(cell, connection)
  close(connection)
  list(path = path, size = size)
}

#' Number of R processes for `n_tasks` independent pieces of work: the option
#' `cerebro.h5ad_stream_workers`, else all physical cores but one, at most 4.
#' @keywords internal
#' @noRd
.h5adStreamWorkers <- function(n_tasks) {
  workers <- getOption("cerebro.h5ad_stream_workers")
  if (is.null(workers)) {
    cores <- parallel::detectCores(logical = FALSE)
    workers <- min(4L, max(1L, if (is.na(cores)) 1L else cores - 1L))
  }
  max(1L, min(as.integer(workers), as.integer(n_tasks)))
}

#' The functions that run in workers, with `globalenv()` as their environment.
#' @keywords internal
#' @noRd
.h5adStreamShipped <- function() {
  ship <- function(f) {
    environment(f) <- globalenv()
    f
  }
  list(
    read = ship(.h5adStreamReadBlock),
    build = ship(.h5adStreamBuild),
    split = ship(.h5adStreamSplitTask),
    range = ship(.h5adStreamRangeTask),
    sums = ship(.h5adStreamSumsTask)
  )
}

#' Run `fun(item)` for every item, `workers` at a time in R processes (in this
#' process for one worker). `after(result, item)` is called in item order, in
#' this process, as soon as the result of an item and of those before it are
#' in: the workers go on with the next items meanwhile. Waves of `workers` items
#' each, with the appending after a wave, left the workers idle while this
#' process wrote, and the whole wave waiting for its slowest item.
#'
#' The workers are mirai daemons of a compute profile of their own, so that
#' they neither use nor end daemons that the caller (or the Viewer) runs.
#' `fun` is sent to each of them once.
#' @keywords internal
#' @noRd
.h5adStreamRun <- function(items, fun, workers, after = NULL) {
  results <- vector("list", length(items))
  finish <- function(k, value) {
    results[[k]] <<- if (is.null(after)) value else after(value, items[[k]])
  }
  if (workers <= 1L || length(items) <= 1L) {
    for (k in seq_along(items)) {
      finish(k, fun(items[[k]]))
    }
    return(results)
  }
  profile <- paste0(
    "cerebro-h5-stream-",
    Sys.getpid(),
    "-",
    sample.int(1e9, 1L)
  )
  mirai::daemons(workers, dispatcher = TRUE, .compute = profile)
  on.exit(mirai::daemons(0, .compute = profile), add = TRUE)
  mirai::everywhere(
    {
      .libPaths(paths)
      NULL
    },
    paths = .libPaths(),
    task = fun,
    .compute = profile
  )
  tasks <- lapply(items, function(item) {
    mirai::mirai(task(item), .args = list(item = item), .compute = profile)
  })
  for (k in seq_along(items)) {
    value <- mirai::call_mirai(tasks[[k]])$data
    tasks[k] <- list(NULL)
    if (mirai::is_error_value(value)) {
      stop(
        "A worker of the H5 writer failed: ",
        paste(as.character(value), collapse = " "),
        call. = FALSE
      )
    }
    finish(k, value)
  }
  results
}

#' The function a worker runs for its share of the blocks.
#' @keywords internal
#' @noRd
.h5adStreamSumsCall <- function(shipped, source, members) {
  function(item) {
    shipped$sums(item, source, members, shipped)
  }
}

#' Per group level: the summed values and the number of positive values of every
#' gene, accumulated over the blocks. `members` is a cells x levels indicator
#' matrix (all groups side by side). Returns two genes x levels matrices.
#' @keywords internal
#' @noRd
.h5adStreamGroupSums <- function(source, members) {
  blocks <- .h5adStreamBlocks(source)
  workers <- .h5adStreamWorkers(length(blocks$from))
  shipped <- .h5adStreamShipped()
  ## each worker gets a share of the blocks
  share <- split(
    seq_along(blocks$from),
    rep_len(seq_len(workers), length(blocks$from))
  )
  items <- lapply(share, function(i) {
    list(from = blocks$from[i], to = blocks$to[i])
  })
  parts <- .h5adStreamRun(
    items,
    .h5adStreamSumsCall(shipped, source, members),
    workers
  )
  list(
    sums = Reduce(`+`, lapply(parts, `[[`, "sums")),
    positive = Reduce(`+`, lapply(parts, `[[`, "positive"))
  )
}

#' The functions a worker runs for one block of cells and for one range of
#' genes. Made here, so that what a function carries to the worker is these
#' arguments and nothing else.
#' @keywords internal
#' @noRd
.h5adStreamSplitCall <- function(shipped, source, edges, scratch, codec) {
  function(item) {
    shipped$split(item, source, edges, scratch, codec, shipped)
  }
}

#' @keywords internal
#' @noRd
.h5adStreamRangeCall <- function(
  shipped,
  blocks,
  edges,
  gene_ptr,
  codec,
  out
) {
  function(range) {
    shipped$range(range, blocks, edges, gene_ptr, codec, out, shipped)
  }
}

#' Write the matrix of `source` as a TENxMatrix-format group of cells x genes,
#' which is gene-major: the entries of one gene are contiguous. The source is
#' read once, block by block, into block files sorted by gene; the genes are
#' then cut into ranges of at most `budget` values, and a worker collects one
#' range in memory from the block files.
#' @keywords internal
#' @noRd
.h5adStreamWriteTENx <- function(
  source,
  path,
  group,
  dimnames,
  level = 1L,
  budget = getOption("cerebro.h5ad_stream_budget", 2^28),
  slice = 2^24,
  scratch_dir = dirname(path)
) {
  n_vars <- source$n_vars
  blocks <- .h5adStreamBlocks(source)
  shipped <- .h5adStreamShipped()
  scratch <- tempfile("h5-stream-", scratch_dir)
  dir.create(scratch, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  ## The source, read once: one block file per block of cells
  edges <- .h5adStreamBucketEdges(n_vars)
  codec <- .h5adStreamCodec()
  items <- lapply(seq_along(blocks$from), function(b) {
    list(c0 = blocks$from[[b]], c1 = blocks$to[[b]], id = b)
  })
  split <- .h5adStreamRun(
    items,
    .h5adStreamSplitCall(shipped, source, edges, scratch, codec),
    .h5adStreamWorkers(length(items))
  )

  ## values per gene
  counts <- numeric(n_vars)
  for (block in split) {
    counts <- counts + block$counts
  }
  gene_ptr <- c(0, cumsum(counts))
  nnz <- gene_ptr[[n_vars + 1]]
  if (nnz != source$nnz) {
    stop(
      "The indices of the matrix do not add up to its values.",
      call. = FALSE
    )
  }

  HDF5Array::TENxRealizationSink(
    c(as.integer(source$n_obs), as.integer(n_vars)),
    dimnames,
    "double",
    filepath = path,
    group = group,
    level = level
  )
  write <- function(name, values, start, n) {
    dataset <- paste0(group, "/", name)
    for (from in seq(1, n, by = slice)) {
      to <- min(from + slice - 1, n)
      rhdf5::h5write(
        values(from, to),
        path,
        dataset,
        start = start + from - 1,
        count = to - from + 1
      )
    }
  }
  if (nnz > 0) {
    rhdf5::h5set_extent(path, paste0(group, "/data"), nnz)
    rhdf5::h5set_extent(path, paste0(group, "/indices"), nnz)
  }

  ## the ranges of genes
  ranges <- list()
  g0 <- 0
  while (g0 < n_vars) {
    g1 <- findInterval(gene_ptr[[g0 + 1]] + budget, gene_ptr) - 1
    g1 <- min(n_vars, max(g1, g0 + 1))
    ranges[[length(ranges) + 1L]] <- list(
      g0 = g0,
      g1 = g1,
      id = length(ranges) + 1L
    )
    g0 <- g1
  }
  workers <- .h5adStreamWorkers(length(ranges))
  ## one worker returns its range, several leave theirs in files
  out <- if (workers > 1L) scratch

  ## The main process appends the ranges in order, a wave at a time
  append <- function(done, range) {
    first <- gene_ptr[[range$g0 + 1]]
    size <- gene_ptr[[range$g1 + 1]] - first
    if (size > 0) {
      if (is.null(out)) {
        value <- done$value
        cell <- done$cell
        write("data", function(from, to) value[from:to], first + 1, size)
        write("indices", function(from, to) cell[from:to], first + 1, size)
      } else {
        for (kind in c("x", "i")) {
          file_path <- paste0(done$path, "-", kind, ".bin")
          connection <- file(file_path, open = "rb")
          write(
            if (kind == "x") "data" else "indices",
            function(from, to) {
              readBin(
                connection,
                if (kind == "x") "double" else "integer",
                n = to - from + 1
              )
            },
            first + 1,
            size
          )
          close(connection)
          unlink(file_path)
        }
      }
    }
    NULL
  }
  .h5adStreamRun(
    ranges,
    .h5adStreamRangeCall(
      shipped,
      lapply(split, function(block) block[c("path", "counts", "index")]),
      edges,
      gene_ptr,
      codec,
      out
    ),
    workers,
    after = append
  )
  rhdf5::h5set_extent(path, paste0(group, "/indptr"), n_vars + 1)
  write("indptr", function(from, to) gene_ptr[from:to], 1, n_vars + 1)
  invisible(path)
}
