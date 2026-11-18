## h5ad matrices with more than 2^31 - 1 non-zero values ----------------------
## A dgCMatrix cannot hold them (its column pointers are 32-bit), so the h5 route
## of `convertH5adToCerebro()` does not read such an X into R. It streams it:
## the CSR matrix (cells x genes) is read in blocks of cells, the sums the
## summaries need are accumulated block by block, and the gene-major file the
## Viewer reads (TENx CSC of cells x genes) is written by a few passes over the
## blocks, each pass collecting a range of genes in memory.

#' Number of non-zero values from which an h5ad X is streamed, not read.
#' @keywords internal
#' @noRd
.h5adStreamThreshold <- function() {
  getOption("cerebro.h5ad_stream_nnz", .Machine$integer.max)
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
  indptr <- .h5adRead(
    ctx,
    paste0(path, "/indptr"),
    bit64conversion = "double"
  )
  dim(indptr) <- NULL
  nnz <- indptr[[length(indptr)]]
  if (!(nnz > .h5adStreamThreshold())) {
    return(NULL)
  }
  if (length(indptr) != n_obs + 1) {
    stop("`", path, "` has an indptr that does not match its cells.", call. = FALSE)
  }
  list(
    file = ctx$file,
    prefix = ctx$prefix,
    path = path,
    n_obs = n_obs,
    n_vars = n_vars,
    nnz = nnz,
    indptr = indptr
  )
}

#' Ranges of cells (0-based start, end) of at most `max_nnz` non-zero values each.
#' @keywords internal
#' @noRd
.h5adStreamBlocks <- function(
  stream,
  max_nnz = getOption("cerebro.h5ad_stream_block", 2^26)
) {
  p <- stream$indptr
  n <- stream$n_obs
  from <- integer(0)
  to <- integer(0)
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

#' One block of cells: gene index (0-based), values, and the cell of every value.
#' @keywords internal
#' @noRd
.h5adStreamRead <- function(stream, c0, c1, with_cell = TRUE) {
  s <- stream$indptr[[c0 + 1]]
  n <- stream$indptr[[c1 + 1]] - s
  read <- function(name, ...) {
    x <- rhdf5::h5read(
      stream$file,
      paste0(stream$prefix, stream$path, "/", name),
      start = s + 1,
      count = n,
      ...
    )
    dim(x) <- NULL
    x
  }
  list(
    gene = as.integer(read("indices")),
    value = as.numeric(read("data")),
    p = as.integer(stream$indptr[(c0 + 1):(c1 + 1)] - s),
    ## 1-based position of the cell inside the whole matrix
    cell = if (with_cell) {
      rep.int(
        as.integer(seq.int(c0 + 1, length.out = c1 - c0)),
        diff(stream$indptr[(c0 + 1):(c1 + 1)])
      )
    }
  )
}

#' A block as a genes x cells dgCMatrix. The CSR rows of an h5ad are the columns
#' of it; when the indices of a row are not sorted the matrix is built from
#' triplets instead.
#' @keywords internal
#' @noRd
.h5adStreamMatrix <- function(x, n_vars) {
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

#' Per group level: the summed values and the number of positive values of every
#' gene, accumulated over the blocks. `members` is a cells x levels indicator
#' matrix (all groups side by side). Returns two genes x levels matrices.
#' @keywords internal
#' @noRd
.h5adStreamGroupSums <- function(stream, members) {
  k <- ncol(members)
  sums <- matrix(0, stream$n_vars, k)
  positive <- matrix(0, stream$n_vars, k)
  blocks <- .h5adStreamBlocks(stream)
  for (b in seq_along(blocks$from)) {
    c0 <- blocks$from[[b]]
    c1 <- blocks$to[[b]]
    x <- .h5adStreamRead(stream, c0, c1, with_cell = FALSE)
    m <- members[(c0 + 1):c1, , drop = FALSE]
    expr <- .h5adStreamMatrix(x, stream$n_vars)
    sums <- sums + as.matrix(expr %*% m)
    expr@x <- as.numeric(expr@x > 0)
    positive <- positive + as.matrix(expr %*% m)
  }
  list(sums = sums, positive = positive)
}

#' Write the matrix of `stream` as a TENxMatrix-format group of cells x genes,
#' which is gene-major: the entries of one gene are contiguous. One pass over the
#' blocks collects the genes of one range (at most `budget` values) in memory.
#' @keywords internal
#' @noRd
.h5adStreamWriteTENx <- function(
  stream,
  path,
  group,
  dimnames,
  level = NULL,
  budget = getOption("cerebro.h5ad_stream_budget", 2^29),
  slice = 2^24
) {
  n_vars <- stream$n_vars
  blocks <- .h5adStreamBlocks(stream)

  ## values per gene
  counts <- numeric(n_vars)
  for (b in seq_along(blocks$from)) {
    x <- .h5adStreamRead(stream, blocks$from[[b]], blocks$to[[b]])
    counts <- counts + tabulate(x$gene + 1L, nbins = n_vars)
  }
  gene_ptr <- c(0, cumsum(counts))
  nnz <- gene_ptr[[n_vars + 1]]
  if (nnz != stream$nnz) {
    stop("The indices of the matrix do not add up to its values.", call. = FALSE)
  }

  HDF5Array::TENxRealizationSink(
    c(stream$n_obs, n_vars),
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

  g0 <- 0
  while (g0 < n_vars) {
    g1 <- findInterval(gene_ptr[[g0 + 1]] + budget, gene_ptr) - 1
    g1 <- min(n_vars, max(g1, g0 + 1))
    ng <- g1 - g0
    first <- gene_ptr[[g0 + 1]]
    size <- gene_ptr[[g1 + 1]] - first
    if (size > 0) {
      value <- numeric(size)
      cell <- integer(size)
      fill <- numeric(ng)
      offset <- gene_ptr[(g0 + 1):g1] - first
      for (b in seq_along(blocks$from)) {
        x <- .h5adStreamRead(stream, blocks$from[[b]], blocks$to[[b]])
        keep <- which(x$gene >= g0 & x$gene < g1)
        if (!length(keep)) {
          next
        }
        gene <- x$gene[keep] - g0 + 1L
        ## a stable sort keeps the cells of a gene in order
        o <- order(gene, method = "radix")
        here <- tabulate(gene, nbins = ng)
        run_start <- cumsum(c(0, here))[seq_len(ng)]
        position <- rep.int(offset + fill, here) +
          (seq_along(o) - rep.int(run_start, here))
        value[position] <- x$value[keep][o]
        cell[position] <- x$cell[keep][o] - 1L   # 0-based
        fill <- fill + here
      }
      write("data", function(from, to) value[from:to], first + 1, size)
      write("indices", function(from, to) cell[from:to], first + 1, size)
      rm(value, cell)
    }
    g0 <- g1
  }
  rhdf5::h5set_extent(path, paste0(group, "/indptr"), n_vars + 1)
  write("indptr", function(from, to) gene_ptr[from:to], 1, n_vars + 1)
  invisible(path)
}
