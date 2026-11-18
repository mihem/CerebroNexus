## Immune receptors and pseudotime on the way out to AnnData --------------------
##
## scirpy keeps receptor chains in obsm["airr"], an awkward array with one list
## of chain records per cell. Cerebro keeps them as scRepertoire clone tables, so
## the chains are read back out of those tables (one record per chain, with the
## gene calls and CDR3 sequences the tables hold).

.airrLociPattern <- "^(TR[ABGD]|IG[HKL])"

#' Split a scRepertoire gene string ("TRAV1.TRAJ1.TRAC", several chains joined by
#' ";") into one record per chain with v/d/j/c calls and the locus.
#' @keywords internal
#' @noRd
.airrSplitGenes <- function(genes) {
  parts <- strsplit(genes, ";", fixed = TRUE)[[1L]]
  lapply(parts, function(chain) {
    calls <- strsplit(chain, ".", fixed = TRUE)[[1L]]
    calls <- calls[!calls %in% c("None", "NA", "")]
    locus <- regmatches(calls, regexpr(.airrLociPattern, calls))
    is_v <- grepl(paste0(.airrLociPattern, "V"), calls)
    is_j <- grepl(paste0(.airrLociPattern, "J"), calls)
    ## a D segment carries a number (IGHD3-3); the heavy constant gene IGHD does not
    is_d <- grepl(paste0(.airrLociPattern, "D[0-9]"), calls)
    rest <- calls[!(is_v | is_j | is_d)]
    first <- function(x) if (length(x)) x[[1L]] else NA_character_
    list(
      locus = first(locus),
      v_call = first(calls[is_v]),
      d_call = first(calls[is_d]),
      j_call = first(calls[is_j]),
      c_call = first(rest)
    )
  })
}

#' The chains of a repertoire as a data frame: cell, locus, calls and CDR3s.
#' @keywords internal
#' @noRd
.crbRepertoireChains <- function(repertoire, cells) {
  if (!is.list(repertoire) || !length(repertoire)) {
    return(NULL)
  }
  pieces <- list()
  split_values <- function(x) {
    if (is.null(x) || is.na(x)) {
      character()
    } else {
      strsplit(x, ";", fixed = TRUE)[[1L]]
    }
  }
  ## T cells: TCR1 / TCR2; B cells: heavy chain (IGH) and light chain columns
  slots <- list(
    list(genes = "TCR1", aa = "cdr3_aa1", nt = "cdr3_nt1"),
    list(genes = "TCR2", aa = "cdr3_aa2", nt = "cdr3_nt2"),
    list(genes = "IGH", aa = "cdr3_aa1", nt = "cdr3_nt1"),
    list(genes = c("IGLC", "IGKC"), aa = "cdr3_aa2", nt = "cdr3_nt2")
  )
  for (sample in names(repertoire)) {
    table <- repertoire[[sample]]
    if (!is.data.frame(table) || !"barcode" %in% names(table)) {
      next
    }
    for (slot in slots) {
      if (!all(c(slot$aa, slot$nt) %in% names(table))) {
        next
      }
      for (column in intersect(slot$genes, names(table))) {
        for (i in which(!is.na(table[[column]]))) {
          chains <- .airrSplitGenes(table[[column]][[i]])
          aa <- split_values(table[[slot$aa]][[i]])
          nt <- split_values(table[[slot$nt]][[i]])
          for (k in seq_along(chains)) {
            chain <- chains[[k]]
            if (is.na(chain$locus)) {
              next
            }
            pieces[[length(pieces) + 1L]] <- list(
              cell = as.character(table$barcode[[i]]),
              locus = chain$locus,
              v_call = chain$v_call,
              d_call = chain$d_call,
              j_call = chain$j_call,
              c_call = chain$c_call,
              junction_aa = if (k <= length(aa)) aa[[k]] else NA_character_,
              junction = if (k <= length(nt)) nt[[k]] else NA_character_
            )
          }
        }
      }
    }
  }
  if (!length(pieces)) {
    return(NULL)
  }
  ## one data frame for all chains: a data.frame() per chain took 0.5 ms each
  column <- function(name) vapply(pieces, `[[`, "", name)
  chains <- data.frame(
    cell = column("cell"),
    locus = column("locus"),
    v_call = column("v_call"),
    d_call = column("d_call"),
    j_call = column("j_call"),
    c_call = column("c_call"),
    junction_aa = column("junction_aa"),
    junction = column("junction"),
    stringsAsFactors = FALSE
  )
  chains <- chains[chains$cell %in% cells, , drop = FALSE]
  if (!nrow(chains)) {
    return(NULL)
  }
  chains$index <- match(chains$cell, cells)
  chains[order(chains$index), , drop = FALSE]
}

#' A uint8 dataset (the bytes of awkward string content).
#' @keywords internal
#' @noRd
.h5adPutUint8 <- function(file, path, x) {
  rhdf5::h5createDataset(
    file,
    path,
    dims = length(x),
    H5type = "H5T_NATIVE_UINT8",
    chunk = max(1L, min(length(x), 65536L)),
    level = 1L
  )
  rhdf5::h5write(as.integer(x), file, path)
}

#' Write obsm["airr"] as an awkward array (a list of chain records per cell).
#'
#' The layout is the one awkward-array / anndata write: a ListOffsetArray over a
#' RecordArray, one buffer per node named "<form_key>-<part>". Offsets and
#' indexes are 32-bit, which the form says ("i32").
#' @return TRUE when written.
#' @keywords internal
#' @noRd
.h5adPutAirr <- function(stage, chains, n_cells) {
  fields <- c(
    "locus",
    "v_call",
    "d_call",
    "j_call",
    "c_call",
    "junction_aa",
    "junction"
  )
  counts <- tabulate(chains$index, nbins = n_cells)
  datasets <- list()
  node <- 0L
  next_key <- function() {
    key <- paste0("node", node)
    node <<- node + 1L
    key
  }
  string_part <- function(values) {
    offsets_key <- next_key()
    data_key <- next_key()
    bytes <- as.integer(charToRaw(enc2utf8(paste(values, collapse = ""))))
    list(
      form = sprintf(
        paste0(
          '{"class":"ListOffsetArray","offsets":"i32","content":',
          '{"class":"NumpyArray","primitive":"uint8","inner_shape":[],',
          '"parameters":{"__array__":"char"},"form_key":"%s"},',
          '"parameters":{"__array__":"string"},"form_key":"%s"}'
        ),
        data_key,
        offsets_key
      ),
      offsets_key = offsets_key,
      offsets = c(0L, cumsum(nchar(values, type = "bytes"))),
      data_key = data_key,
      data = bytes
    )
  }
  top_key <- next_key()
  record_key <- next_key()
  contents <- character()
  for (field in fields) {
    values <- chains[[field]]
    missing <- is.na(values) | !nzchar(values)
    if (!any(missing)) {
      part <- string_part(values)
      contents <- c(contents, part$form)
      datasets[[paste0(part$offsets_key, "-offsets")]] <- part$offsets
      datasets[[paste0(part$data_key, "-data")]] <- part$data
    } else if (all(missing)) {
      index_key <- next_key()
      empty_key <- next_key()
      contents <- c(
        contents,
        sprintf(
          paste0(
            '{"class":"IndexedOptionArray","index":"i32","content":',
            '{"class":"EmptyArray","parameters":{},"form_key":"%s"},',
            '"parameters":{},"form_key":"%s"}'
          ),
          empty_key,
          index_key
        )
      )
      datasets[[paste0(index_key, "-index")]] <- rep(-1L, length(values))
    } else {
      index_key <- next_key()
      part <- string_part(values[!missing])
      index <- rep(-1L, length(values))
      index[!missing] <- seq_len(sum(!missing)) - 1L
      contents <- c(
        contents,
        sprintf(
          paste0(
            '{"class":"IndexedOptionArray","index":"i32","content":%s,',
            '"parameters":{},"form_key":"%s"}'
          ),
          part$form,
          index_key
        )
      )
      datasets[[paste0(index_key, "-index")]] <- index
      datasets[[paste0(part$offsets_key, "-offsets")]] <- part$offsets
      datasets[[paste0(part$data_key, "-data")]] <- part$data
    }
  }
  productive_key <- next_key()
  contents <- c(
    contents,
    sprintf(
      paste0(
        '{"class":"NumpyArray","primitive":"bool","inner_shape":[],',
        '"parameters":{},"form_key":"%s"}'
      ),
      productive_key
    )
  )
  form <- sprintf(
    paste0(
      '{"class":"ListOffsetArray","offsets":"i32","content":',
      '{"class":"RecordArray","fields":[%s],"contents":[%s],"parameters":{},',
      '"form_key":"%s"},"parameters":{},"form_key":"%s"}'
    ),
    paste0('"', c(fields, "productive"), '"', collapse = ","),
    paste(contents, collapse = ","),
    record_key,
    top_key
  )

  .h5adPutGroup(
    stage,
    "obsm/airr",
    "awkward-array",
    "0.1.0",
    form = form,
    length = n_cells
  )
  rhdf5::h5write(
    c(0L, cumsum(counts)),
    stage,
    paste0("obsm/airr/", top_key, "-offsets")
  )
  for (name in names(datasets)) {
    path <- paste0("obsm/airr/", name)
    if (endsWith(name, "-data")) {
      .h5adPutUint8(stage, path, datasets[[name]])
    } else {
      rhdf5::h5write(as.integer(datasets[[name]]), stage, path)
    }
  }
  .h5adPutBool(
    stage,
    paste0("obsm/airr/", productive_key, "-data"),
    rep(TRUE, nrow(chains)),
    encode = FALSE
  )
  TRUE
}

#' Pseudotime of each trajectory as an obs column (NaN for cells without one).
#' The name is what scanpy and scVelo call it: the part in brackets of
#' "cell_type (dpt_pseudotime)", the entry name of a pseudotime trajectory, or
#' "<method>_<name>_pseudotime" for others.
#' @keywords internal
#' @noRd
.crbPseudotimeColumns <- function(object, cells, taken) {
  methods <- tryCatch(object$getMethodsForTrajectories(), error = function(e) {
    character()
  })
  result <- list()
  for (method in methods) {
    for (name in object$getNamesOfTrajectories(method)) {
      meta <- tryCatch(
        object$getTrajectory(method, name)[["meta"]],
        error = function(e) NULL
      )
      if (
        !is.data.frame(meta) ||
          !"pseudotime" %in% names(meta) ||
          is.null(rownames(meta))
      ) {
        next
      }
      column <- if (grepl("\\(.+\\)$", name)) {
        sub("^.*\\((.+)\\)$", "\\1", name)
      } else if (identical(method, "pseudotime")) {
        name
      } else {
        paste0(method, "_", name, "_pseudotime")
      }
      if (column %in% c(taken, names(result))) {
        next
      }
      values <- rep(NA_real_, length(cells))
      index <- match(rownames(meta), cells)
      values[index[!is.na(index)]] <- as.numeric(meta$pseudotime[!is.na(index)])
      result[[column]] <- values
    }
  }
  result
}
