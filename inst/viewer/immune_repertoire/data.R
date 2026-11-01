## The default Clonal projection is backed by the compact Viewer Pack. Keep its
## availability gate on the backend summary so opening the page does not hydrate
## the full repertoire sidecar merely to decide whether controls may render.
ir_repertoire_available <- reactive({
  isTRUE(getImmuneRepertoireSummary()$available)
})

## Said once, when the repertoire really is encoded in memory (not read from
## the process memo or the cache): the page takes about a minute on a million
## cells, and the way out is to rebuild the Pack.
ir_notify_slow_encoding <- function(pack) {
  text <- paste0(
    if (is.list(pack)) {
      "This Viewer Pack was built before the fast Immune repertoire format"
    } else {
      "This data set has no Viewer Pack"
    },
    ", so its repertoire is encoded now: about a minute on a million cells, ",
    "less when the next session reuses it. ",
    "buildViewerPack(<crb file>, overwrite = TRUE) makes this page open in ",
    "seconds."
  )
  message(text)
  tryCatch(
    shiny::showNotification(
      text,
      type = "warning",
      duration = 30,
      id = "ir-pack-outdated"
    ),
    error = function(error) NULL
  )
  invisible(NULL)
}

## ---- The encoded repertoire (encode.R) -------------------------------- ##
## The tabs read the repertoire's integer columns: the Viewer Pack's, or for a
## data set without a Pack, or with a Pack built before the encoding, the same
## encoding made in memory, once per process for the data set's files.
ir_encoded <- reactive({
  req(!is.null(data_set()))
  pack <- viewerPackCurrent()
  encoded <- if (is.list(pack)) {
    viewerPackImmuneEncoded(pack, IR_ENCODING_VERSION)
  }
  if (!is.null(encoded)) {
    encoded
  } else {
    ir_process_memo(
      "encoded",
      ir_dataset_process_key(data_set()),
      function() {
        repertoire <- getImmuneRepertoire()
        if (!length(repertoire)) {
          return(NULL)
        }
        ir_notify_slow_encoding(pack)
        ## The ranks are sorted when a plot needs them (ir_clone_rank()).
        ir_encode_repertoire(
          repertoire,
          as.character(getMetaData()$cell_barcode),
          rank = FALSE
        )
      },
      disk = TRUE
    )
  }
})

## The clones `ids` of `column` on `chain`: in memory, or read from the
## Pack's blocks of names that hold them.
ir_clone_names <- function(encoded, column, chain, ids) {
  names <- encoded$names[[column]][[chain]]
  if (!is.null(names)) {
    return(names[ids])
  }
  viewerPackImmuneCloneNames(
    viewerPackCurrent(),
    column,
    chain,
    ids,
    encoded$name_blocks
  )
}

## ---- Grouping values of the repertoire rows --------------------------- ##
## The data set's cell metadata in canonical order, the order of the encoded
## `cell` rows: the Pack's first-frame table (every column but the cell
## names), else the full table.
ir_cell_metadata <- function() {
  pack <- tryCatch(viewerPackCurrent(), error = function(e) NULL)
  metadata <- if (is.list(pack)) {
    tryCatch(viewerProjectionFirstFrameCache()$meta_data, error = function(e) {
      NULL
    })
  }
  if (is.data.frame(metadata)) {
    return(metadata)
  }
  tryCatch(getMetaData(), error = function(e) NULL)
}

## The value of column `name` for every repertoire row: the tables' own column,
## else (when `metadata` is joined) the metadata's value at the row's cell, NA
## for a row without one; "sample" falls back to the sample names. NULL when
## the column is nowhere.
ir_row_values <- function(
  encoded,
  name,
  metadata = ir_cell_metadata(),
  joined = TRUE
) {
  if (is.null(name) || !length(name) || is.na(name) || !nzchar(name)) {
    return(NULL)
  }
  in_metadata <- isTRUE(joined) &&
    is.data.frame(metadata) &&
    name %in% names(metadata)
  own <- encoded$columns[[name]]
  if (!is.null(own)) {
    carried <- attr(own, "samples")
    character <- isTRUE(attr(own, "character"))
    attributes(own) <- attributes(own)[c("levels", "class")]
    if (is.null(carried) || !in_metadata) {
      return(if (character) as.character(own) else own)
    }
    ## Tables without the column take the metadata's.
    values <- as.character(own)
    fill <- !encoded$sample %in% carried
    values[fill] <- as.character(metadata[[name]][encoded$cell[fill]])
    return(values)
  }
  if (in_metadata) {
    return(metadata[[name]][encoded$cell])
  }
  if (identical(name, "sample")) {
    return(encoded$samples[encoded$sample])
  }
  NULL
}

## Per sample, whether its clone column has any clone: the tabs leave out a
## sample whose clones are all NA, as scRepertoire does.
ir_samples_with_clones <- function(encoded, clone_col) {
  both <- encoded$clones[[clone_col]]$both
  tabulate(encoded$sample[!is.na(both)], length(encoded$samples)) > 0L
}

## ---- Helper: read a dynamic function-specific parameter --------------- ##
## The function-specific controls (IR_PARAM_SPEC) are rendered into a dynamic
## panel, so an input may be absent on tabs where the parameter doesn't apply.
## Returns `default` when the input is missing/empty.
ir_param <- function(id, default = NULL) {
  v <- input[[id]]
  if (
    is.null(v) ||
      length(v) == 0 ||
      (is.character(v) && length(v) == 1 && !nzchar(v))
  ) {
    default
  } else {
    v
  }
}

## ---- Resolve the generic "Order groups" (order.by) value -------------- ##
## Maps the ir_p_order_by control to scRepertoire's order.by argument: the
## empty default becomes NULL (scRepertoire's own ordering), otherwise the
## chosen value (e.g. "alphanumeric") is passed through.
ir_order_by <- function() {
  v <- input[["ir_p_order_by"]]
  if (is.null(v) || !nzchar(v)) NULL else v
}

## ---- Parse the Homeostasis clone-size thresholds ---------------------- ##
## clonalHomeostasis' cloneSize is a *named* numeric vector of upper bounds
## (Rare < Small < ... < Hyperexpanded). The UI takes them as a comma-separated
## list of 5 increasing numbers; this builds the named vector. Returns NULL on
## anything malformed so scRepertoire falls back to its own default.
IR_CLONE_SIZE_NAMES <- c(
  "Rare",
  "Small",
  "Medium",
  "Large",
  "Hyperexpanded"
)
IR_CLONE_SIZE_DEFAULT <- setNames(
  c(1e-04, 0.001, 0.01, 0.1, 1),
  IR_CLONE_SIZE_NAMES
)
ir_clone_size <- function() {
  v <- input[["ir_p_clone_size"]]
  if (is.null(v) || !nzchar(v)) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  nums <- suppressWarnings(as.numeric(trimws(strsplit(v, ",")[[1]])))
  if (
    length(nums) != length(IR_CLONE_SIZE_NAMES) ||
      any(is.na(nums)) ||
      is.unsorted(nums, strictly = TRUE)
  ) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  setNames(nums, IR_CLONE_SIZE_NAMES)
}

## ---- Comparable groups for Paired Scatter / Compare ------------------ ##
## clonalScatter (x.axis/y.axis) and clonalCompare (samples) operate on the
## *names of the groups that group.by produces*. With group.by = None the
## groups are the samples; with group.by = <column> they are that column's
## levels among the repertoire's rows. This reactive returns those group names
## so the Paired Scatter X/Y and Compare selectors stay in sync.
ir_compare_groups <- reactive({
  encoded <- ir_encoded()
  if (is.null(encoded)) {
    return(character(0))
  }
  ir_group_names(encoded, ir_row_values(encoded, input$ir_groupBy))
})

ir_group_names <- function(encoded, values) {
  if (is.null(values)) {
    return(encoded$samples)
  }
  vals <- if (is.factor(values)) {
    levels(values)[unique(as.integer(values))]
  } else {
    unique(as.character(values))
  }
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) encoded$samples else sort(vals)
}


## ---- Candidate columns for the Sharing "unit" selector ---------------- ##
## The Sharing tab needs a "smallest sharing unit" (default: sample). Offer any
## column of the repertoire's tables or the cell metadata that is categorical
## (character/factor with > 1 distinct non-empty value among the repertoire's
## rows) and is not a scRepertoire column. `sample` is placed first so it
## becomes the default selection.
ir_discover_sharing_units <- function(encoded, metadata = NULL) {
  if (is.null(encoded) || !length(encoded$samples)) {
    return(character())
  }
  shared <- names(Filter(
    function(v) is.null(attr(v, "samples")),
    encoded$columns
  ))
  candidates <- setdiff(
    union(shared, colnames(metadata)),
    c(IR_SCR_COLUMNS, "cell_barcode")
  )
  keep <- vapply(
    candidates,
    function(column) {
      values <- ir_row_values(encoded, column, metadata)
      if (!is.character(values) && !is.factor(values)) {
        return(FALSE)
      }
      # Distinct values first: a factor's unique() works on its codes.
      values <- as.character(unique(values))
      length(unique(values[!is.na(values) & nzchar(values)])) > 1L
    },
    logical(1)
  )
  choices <- candidates[keep]
  if (length(encoded$samples) > 1L) {
    choices <- union("sample", choices)
  }
  if ("sample" %in% choices) {
    c("sample", setdiff(choices, "sample"))
  } else {
    choices
  }
}


## ---- Results shared by the sessions of a process ----------------------- ##
## Every session gets its own copy of a cached data set, so a value computed
## from the data set's files alone is keyed by those files instead: the CRB
## (path, size, modification time) and the repertoire sidecar's checksum. A
## data set without a file on disk has no key and is not shared.
ir_dataset_process_key <- function(ds) {
  path <- attr(ds, "cerebro_dataset_path", exact = TRUE)
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !file.exists(path)
  ) {
    return(NULL)
  }
  info <- file.info(path)
  backend <- tryCatch(ds$immune_repertoire_backend, error = function(e) NULL)
  list(
    path = normalizePath(path, winslash = "/"),
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    repertoire = if (is.list(backend)) as.character(backend$md5 %||% "") else ""
  )
}

ir_process_cache <- if (exists(".ir_process_cache", inherits = TRUE)) {
  get(".ir_process_cache", inherits = TRUE)
} else {
  new.env(parent = emptyenv())
}

## Where an encoding made in memory is kept between sessions: the user cache
## directory (CEREBRO_IR_CACHE overrides it). The Pack's encoding is the real
## store; this one spares a data set without it (or with a Pack built before
## the encoding) the minute of encoding at every start.
ir_disk_cache_file <- function(name, key) {
  dir <- Sys.getenv("CEREBRO_IR_CACHE", unset = "")
  if (!nzchar(dir)) {
    dir <- file.path(tools::R_user_dir("CerebroNexus", "cache"), "immune")
  }
  file.path(
    dir,
    paste0(
      name,
      "-v",
      IR_ENCODING_VERSION,
      "-",
      rlang::hash(as.character(key$path %||% "")),
      "-",
      rlang::hash(key),
      ".qs2"
    )
  )
}

ir_disk_cache_read <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  value <- tryCatch(qs2::qs_read(file), error = function(e) NULL)
  if (is.list(value) && identical(value$version, IR_ENCODING_VERSION)) {
    value
  }
}

## Written through a temporary name, so a session that stops half way or reads
## at the same time never sees a partial file. A failure (a read-only cache) is
## not an error: the next start encodes again.
ir_disk_cache_write <- function(file, value) {
  tryCatch(
    {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      partial <- paste0(file, ".", Sys.getpid(), ".tmp")
      on.exit(unlink(partial), add = TRUE)
      qs2::qs_save(value, partial)
      file.rename(partial, file)
      ## The data set's earlier entries (another file version, another encoding
      ## version) are out of date for good.
      same_path <- sub("-[0-9a-f]+[.]qs2$", "", basename(file))
      stale <- list.files(
        dirname(file),
        pattern = paste0("^", gsub("-v[0-9]+-", "-v[0-9]+-", same_path), "-")
      )
      unlink(file.path(dirname(file), setdiff(stale, basename(file))))
    },
    error = function(e) NULL
  )
  invisible(NULL)
}

## compute() once per (name, key) in the process; without a key, every call.
## `disk`: also keep the result in the user cache between sessions.
ir_process_memo <- function(
  name,
  key,
  compute,
  cache = ir_process_cache,
  disk = FALSE
) {
  if (is.null(key)) {
    return(compute())
  }
  id <- paste0(name, ":", rlang::hash(key))
  if (!exists(id, envir = cache, inherits = FALSE)) {
    value <- NULL
    file <- if (isTRUE(disk)) ir_disk_cache_file(name, key)
    if (!is.null(file)) {
      value <- ir_disk_cache_read(file)
    }
    if (is.null(value)) {
      value <- compute()
      if (!is.null(file) && !is.null(value)) {
        ir_disk_cache_write(file, value)
      }
    }
    assign(id, value, envir = cache)
  }
  get(id, envir = cache, inherits = FALSE)
}

## Discovery scans the whole repertoire and metadata (seconds for large data)
## and depends only on the loaded dataset, not on the visit to this tab.
ir_sharing_unit_memo <- viewerMemoOne()

ir_sharing_unit_choices <- reactive({
  req(identical(input[["ir_tabs"]], "Clone Sharing"))
  ds <- data_set()
  discover <- function() {
    ir_discover_sharing_units(ir_encoded(), ir_cell_metadata())
  }
  key <- ir_dataset_process_key(ds)
  if (is.null(key)) {
    ir_sharing_unit_memo(ds, discover)
  } else {
    ir_process_memo("sharing_units", key, discover)
  }
})


## ---- Reactive: parameters --------------------------------------------- ##
ir_params <- reactive({
  gb <- input$ir_groupBy
  if (is.null(gb) || gb == "") {
    gb <- NULL
  }
  list(
    cloneCall = input$ir_cloneCall,
    chain = input$ir_chain,
    groupBy = gb
  )
})

## ---- Resolve chain: for functions that don't accept "both" ------------ ##
specific_chain <- reactive({
  ch <- input$ir_chain
  if (is.null(ch) || ch == "both") {
    chains <- ir_encoded()$chains
    if ("TRB" %in% chains) {
      return("TRB")
    }
    if (length(chains) > 0) {
      return(chains[1])
    }
    return("TRB")
  }
  ch
})


##----------------------------------------------------------------------------##
## Clonal UMAP data layer
##----------------------------------------------------------------------------##

## ---- Chains that define each receptor class --------------------------- ##
IR_TCR_CHAINS <- CEREBRO_TCR_CHAINS
IR_BCR_CHAINS <- CEREBRO_BCR_CHAINS

## ---- Which receptor classes are present in the data ------------------- ##
## Returns a named vector ("TCR" / "BCR") of the receptor types actually
## detected, so the Clonal UMAP selector only offers what exists. The names
## are the labels shown to the user. The sidecar descriptor names the chains
## without reading the repertoire.
ir_receptor_types <- reactive({
  chains <- as.character(getImmuneRepertoireSummary()$chains)
  present <- c(
    if (any(chains %in% IR_TCR_CHAINS)) "TCR" else character(0),
    if (any(chains %in% IR_BCR_CHAINS)) "BCR" else character(0)
  )
  if (!length(present)) {
    present <- tryCatch(ir_encoded()$receptors, error = function(e) NULL) %||%
      character(0)
  }
  stats::setNames(present, present)
})

## ---- Clone-size bin breaks / labels (scRepertoire cloneSize defaults) -- ##
## A clone's size = number of cells carrying that clonotype (within the
## selected receptor). Cells are binned into the standard expansion levels.
IR_CLONE_BINS <- CEREBRO_CLONE_BINS
IR_CLONE_LABELS <- CEREBRO_CLONE_LABELS

ir_bind_clone_histograms <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      sample = character(),
      abundance = integer(),
      n_clones = integer()
    ))
  }
  do.call(rbind, rows)
}

## Clone-size histogram as clonalAbundance() plots it, without scRepertoire:
## one row per (unit, abundance) with the number of clones of that size. The
## units are the samples, or with `groups` (the grouping value of every row)
## that column's levels: a clone keeps its size in its sample and counts once
## for every group its cells fall in. Samples without a clone are left out;
## NA and blank clones are not counted.
ir_abundance_histogram <- function(
  encoded,
  clone_col,
  chain = "both",
  groups = NULL
) {
  ids <- encoded$clones[[clone_col]][[chain]]
  keep <- !is.na(ids)
  blank <- if (identical(chain, "both")) encoded$blank[[clone_col]] else NA
  if (!is.na(blank)) {
    keep <- keep & ids != blank
  }
  if (!any(keep)) {
    return(ir_bind_clone_histograms(list()))
  }
  sample <- encoded$sample[keep]
  clones <- ir_encode_pairs(sample, ids[keep])
  cells <- tabulate(clones$index, length(clones$first))
  if (is.null(groups)) {
    ## Clones per (sample, size), samples in order, sizes ascending.
    clone_sample <- sample[clones$first]
    o <- order(clone_sample, cells)
    clone_sample <- clone_sample[o]
    cells <- cells[o]
    n <- length(cells)
    start <- which(c(
      TRUE,
      clone_sample[-1L] != clone_sample[-n] | cells[-1L] != cells[-n]
    ))
    return(data.frame(
      sample = encoded$samples[clone_sample[start]],
      abundance = cells[start],
      n_clones = diff(c(start, n + 1L)),
      stringsAsFactors = FALSE
    ))
  }
  groups <- as.character(groups[keep])
  pair <- ir_encode_pairs(clones$index, groups)$first
  pair <- pair[!is.na(groups[pair])]
  if (!length(pair)) {
    return(ir_bind_clone_histograms(list()))
  }
  counts <- as.data.frame(
    table(sample = groups[pair], abundance = cells[clones$index[pair]]),
    stringsAsFactors = FALSE
  )
  counts <- counts[counts$Freq > 0L, , drop = FALSE]
  data.frame(
    sample = counts$sample,
    abundance = as.integer(counts$abundance),
    n_clones = as.integer(counts$Freq),
    stringsAsFactors = FALSE
  )
}


## The Abundance histogram for the controls `pars`.
ir_abundance_counts_for <- function(pars, encoded) {
  req(!is.null(encoded))
  ir_abundance_histogram(
    encoded,
    ir_clonecall_col(pars$cloneCall),
    pars$chain,
    ir_row_values(encoded, pars$groupBy)
  )
}


## clonalCompare(exportTable = TRUE) without scRepertoire, for the compared
## units only (samples, or with `groups`, the grouping value of every row,
## that column's levels): the cells of every clone in a unit, or their share
## of the unit's clonal cells, kept for the clones among any unit's
## `top_clones` largest (ties by clone name: `clone_rank` is each id's place
## among the sorted names). Samples whose clones are all NA are left out
## first, as clonalCompare does. Rows come unit by unit, in no particular
## clone order. `clone_names(ids)` names the clones kept; without it they are
## their ids.
ir_compare_table <- function(
  encoded,
  clone_col,
  chain,
  groups,
  units,
  top_clones = NULL,
  proportion = TRUE,
  clone_rank = NULL,
  clone_names = NULL
) {
  ids <- encoded$clones[[clone_col]][[chain]]
  rows <- ir_samples_with_clones(encoded, clone_col)[encoded$sample]
  if (is.null(groups)) {
    position <- match(encoded$samples, units)[encoded$sample]
  } else if (is.factor(groups)) {
    ## A factor's levels are matched once, not every cell's label.
    position <- match(levels(groups), units)[as.integer(groups)]
  } else {
    position <- match(as.character(groups), units)
  }
  keep <- rows & !is.na(ids) & !is.na(position)
  ids <- ids[keep]
  unit <- position[keep]
  ## Clones of the compared units numbered from 1.
  present_ids <- unique(ids)
  local <- match(ids, present_ids)
  rank_of <- if (is.null(clone_rank)) present_ids else clone_rank[present_ids]
  rows <- lapply(seq_along(units), function(k) {
    n <- tabulate(local[unit == k], nbins = length(present_ids))
    clone <- which(n > 0L)
    value <- n[clone]
    if (isTRUE(proportion)) {
      value <- value / sum(value)
    }
    list(clone = clone, value = value)
  })
  if (length(top_clones) == 1L && !is.na(top_clones)) {
    ## Each unit's largest clones; only the ties at the cut are put in name
    ## order instead of every clone.
    top <- unique(unlist(lapply(rows, function(r) {
      if (!length(r$value)) {
        return(integer())
      }
      cut <- sort(r$value, decreasing = TRUE)[min(top_clones, length(r$value))]
      candidates <- which(r$value >= cut)
      candidates <- candidates[order(
        -r$value[candidates],
        rank_of[r$clone[candidates]]
      )]
      r$clone[head(candidates, top_clones)]
    })))
    rows <- lapply(rows, function(r) {
      keep <- r$clone %in% top
      list(clone = r$clone[keep], value = r$value[keep])
    })
  }
  sizes <- vapply(rows, function(r) length(r$clone), integer(1))
  if (!sum(sizes)) {
    stop(
      "Please reasses the filtering strategies here, there are not\n",
      "            enough clones to examine."
    )
  }
  kept <- present_ids[unlist(lapply(rows, `[[`, "clone"), use.names = FALSE)]
  if (!is.null(clone_names)) {
    labels <- unique(kept)
    kept <- clone_names(labels)[match(kept, labels)]
  }
  table <- data.frame(
    clones = kept,
    value = unlist(lapply(rows, `[[`, "value"), use.names = FALSE),
    Sample = rep(units, sizes),
    stringsAsFactors = FALSE
  )
  names(table)[[2L]] <- if (isTRUE(proportion)) "Proportion" else "Count"
  table
}


## ---- Homeostasis histograms -------------------------------------------- ##
## Homeostasis histograms as clonalHomeostasis() counts them: per sample, or
## with `groups` (the grouping value of every row) per level of that column
## pooled across the samples (NA groups left out), the clones' cells over every
## clonal cell of the unit. Blank clones count, cells without a clone on the
## chain do not, and samples whose clones are all NA are left out first. A
## unit without a clone keeps an empty row; on all chains a grouping factor's
## unused levels do too, as split() leaves them.
ir_homeostasis_counts <- function(
  encoded,
  clone_col,
  chain = "both",
  groups = NULL
) {
  histogram <- function(ids, unit) {
    ids <- ids[!is.na(ids)]
    if (!length(ids)) {
      return(data.frame(sample = unit, abundance = 0L, n_clones = 0L))
    }
    bins <- tabulate(tabulate(match(ids, unique(ids))))
    abundance <- which(bins > 0L)
    data.frame(
      sample = unit,
      abundance = abundance,
      n_clones = bins[abundance],
      stringsAsFactors = FALSE
    )
  }
  has <- ir_samples_with_clones(encoded, clone_col)
  rows <- has[encoded$sample]
  ids <- encoded$clones[[clone_col]][[chain]][rows]
  if (is.null(groups)) {
    by_sample <- split(ids, factor(encoded$sample[rows], which(has)))
    return(ir_bind_clone_histograms(Map(
      histogram,
      by_sample,
      encoded$samples[has]
    )))
  }
  groups <- groups[rows]
  by_group <- split(
    ids,
    if (identical(chain, "both")) groups else as.character(groups)
  )
  ir_bind_clone_histograms(Map(histogram, by_group, names(by_group)))
}

## The indices clonalDiversity() offers, with immApex's formulas, of a unit's
## clone sizes (positive counts).
ir_diversity_metrics <- local({
  shannon <- function(n) {
    p <- n / sum(n)
    -sum(p * log(p))
  }
  inv_simpson <- function(n) {
    p <- n / sum(n)
    1 / sum(p * p)
  }
  norm_entropy <- function(n) {
    if (length(n) == 1L) 0 else shannon(n) / log(length(n))
  }
  hill <- function(q) {
    function(n) {
      p <- n / sum(n)
      if (q == 1) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q))
    }
  }
  list(
    shannon = shannon,
    inv.simpson = inv_simpson,
    gini.simpson = function(n) 1 - 1 / inv_simpson(n),
    norm.entropy = norm_entropy,
    pielou = norm_entropy,
    ace = function(n) {
      s_obs <- length(n)
      if (s_obs == 0) {
        return(0)
      }
      rare <- n[n <= 10]
      s_rare <- length(rare)
      s_abund <- s_obs - s_rare
      if (s_rare == 0) {
        return(s_obs)
      }
      n_rare <- sum(rare)
      f1 <- sum(rare == 1)
      coverage <- 1 - f1 / n_rare
      if (coverage == 0) {
        return(s_obs)
      }
      den <- coverage * n_rare * (n_rare - 1)
      gamma_sq <- if (den == 0) {
        0
      } else {
        max(0, s_rare * sum(rare * (rare - 1)) / den - 1)
      }
      max(s_obs, s_abund + s_rare / coverage + f1 / coverage * gamma_sq)
    },
    chao1 = function(n) {
      f1 <- sum(n == 1)
      f2 <- sum(n == 2)
      if (f1 > 1 && f2 > 0) {
        length(n) + f1 * (f1 - 1) / (2 * (f2 + 1))
      } else {
        length(n)
      }
    },
    gini = function(n) {
      s <- length(n)
      if (s <= 1) {
        return(0)
      }
      sum((2 * seq_len(s) - s - 1) * sort(n)) / (s * sum(n))
    },
    d50 = function(n) {
      if (!length(n)) {
        return(0)
      }
      which.max(cumsum(sort(n, decreasing = TRUE)) >= sum(n) * 50 / 100)
    },
    hill0 = hill(0),
    hill1 = hill(1),
    hill2 = hill(2)
  )
})

## clonalDiversity(return.boots = TRUE) without scRepertoire: `n_boots` values
## of `metric` per unit, each of as many cells as the smallest unit has, drawn
## with replacement, with the same draws from the R session's generator. Units
## are the samples, or with `groups` the levels of `group_by` (with `x_values`
## too, of both) pooled over the samples; `groups` and `x_values` are the
## columns' values of every row. Samples whose clones are all NA are left out
## first, rows on another chain are dropped, and cells without a clone are
## drawn but not counted. Units left without a row are left out:
## clonalDiversity() keeps the empty combinations of `group_by` and `x_axis`,
## which makes every value one of no cells (a group nested in an x level), and
## stops at a sample without a cell on the chain (a BCR sample on TRB). With
## `x_axis`, each unit takes the x value of its first row, merged and renamed
## as clonalDiversity() does.
ir_diversity_table <- function(
  encoded,
  clone_col,
  chain,
  group_by = NULL,
  x_axis = NULL,
  metric = "shannon",
  n_boots = 100,
  groups = NULL,
  x_values = NULL
) {
  diversity <- ir_diversity_metrics[[metric]]
  if (is.null(diversity)) {
    stop("Invalid `metric`.")
  }
  samples <- which(ir_samples_with_clones(encoded, clone_col))
  keep <- encoded$sample %in% samples
  if (!identical(chain, "both")) {
    keep[encoded$other[[chain]]] <- FALSE
  }
  ids <- encoded$clones[[clone_col]][[chain]][keep]
  sample <- factor(encoded$sample[keep], samples)
  column <- function(values) as.character(values)[keep]
  if (is.null(group_by)) {
    units <- stats::setNames(split(ids, sample), encoded$samples[samples])
    x_values <- if (!is.null(x_axis)) {
      stats::setNames(split(column(x_values), sample), encoded$samples[samples])
    }
  } else {
    by <- list(column(groups))
    if (!is.null(x_axis)) {
      by[[2L]] <- column(x_values)
    }
    if (length(by) == 1L) {
      by <- by[[1L]]
    }
    units <- split(ids, by)
    x_values <- if (!is.null(x_axis)) split(by[[2L]], by)
  }
  keep <- lengths(units) > 0L
  units <- units[keep]
  x_values <- x_values[keep]
  size <- min(lengths(units))
  values <- lapply(units, function(unit) {
    clones <- match(unit, unique(unit[!is.na(unit)]))
    bins <- max(clones, 0L, na.rm = TRUE)
    vapply(
      seq_len(n_boots),
      function(boot) {
        counts <- tabulate(
          clones[sample.int(length(clones), size, replace = TRUE)],
          bins
        )
        diversity(counts[counts > 0L])
      },
      numeric(1)
    )
  })
  output <- data.frame(
    group = rep(names(units), lengths(values)),
    value = unlist(values, use.names = FALSE),
    metric = metric
  )
  names(output)[1L] <- group_by %||% "Group"
  if (is.null(x_axis)) {
    output$x.axis <- 1
    return(output)
  }
  x_variable <- as.data.frame(do.call(
    rbind,
    lapply(x_values, function(x) unique(x)[1L])
  ))
  colnames(x_variable) <- x_axis
  output <- merge(
    output,
    x_variable,
    by.x = group_by %||% "Group",
    by.y = "row.names"
  )
  output[, 1L] <- gsub("\\..*", "", output[, 1L])
  output
}


ir_homeostasis_matrix <- function(counts, clone_size) {
  samples <- unique(counts$sample)
  bounds <- c(0, unname(clone_size))
  labels <- paste0(
    names(clone_size),
    " (",
    head(bounds, -1),
    " < X <= ",
    tail(bounds, -1),
    ")"
  )
  result <- matrix(
    0,
    length(samples),
    length(clone_size),
    dimnames = list(samples, labels)
  )
  for (sample_index in seq_along(samples)) {
    sample <- samples[[sample_index]]
    rows <- counts[counts$sample == sample, , drop = FALSE]
    total <- sum(as.double(rows$abundance) * rows$n_clones)
    if (total == 0) {
      next
    }
    fraction <- rows$abundance / total
    for (i in seq_along(clone_size)) {
      keep <- fraction > bounds[[i]] & fraction <= bounds[[i + 1L]]
      result[sample_index, i] <- sum(fraction[keep] * rows$n_clones[keep])
    }
  }
  result
}

ir_homeostasis_plot <- function(
  counts,
  clone_size,
  palette,
  order_by = NULL
) {
  mat <- ir_homeostasis_matrix(counts, clone_size)
  if (!nrow(mat)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  ## Bars in sorted order, or with order.by = "alphanumeric" by their letters,
  ## then their number, as clonalHomeostasis() orders them.
  units <- sort(unique(rownames(mat)))
  if (identical(order_by, "alphanumeric")) {
    units <- units[order(
      gsub("[0-9]", "", units),
      suppressWarnings(as.numeric(gsub("[^0-9]", "", units)))
    )]
  }
  frame <- data.frame(
    sample = factor(rep(rownames(mat), ncol(mat)), levels = units),
    category = factor(
      rep(colnames(mat), each = nrow(mat)),
      levels = rev(colnames(mat))
    ),
    value = as.vector(mat)
  )
  ggplot2::ggplot(
    frame,
    ggplot2::aes(x = as.factor(sample), y = value, fill = category)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      colour = "black",
      linewidth = .25
    ) +
    ggplot2::scale_fill_manual(
      values = rev(grDevices::hcl.colors(ncol(mat), palette))
    ) +
    ggplot2::labs(
      x = "Samples",
      y = "Relative Abundance",
      fill = "Clonal Group"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

## The exact clone-size profile of every unit: the strict clones of `chain`
## (blank clones left out) counted per sample, or per level of `groups` (the
## grouping value of every row), in log2 bins.
ir_clone_size_profiles <- function(
  encoded,
  chain = "both",
  groups = NULL,
  threshold = 1
) {
  if (is.null(encoded) || !length(encoded$samples)) {
    return(NULL)
  }
  threshold <- suppressWarnings(as.integer(threshold))
  if (is.na(threshold) || threshold < 1L) {
    threshold <- 1L
  }
  clones <- encoded$clones$CTstrict[[chain]]
  if (identical(chain, "both")) {
    clones[clones %in% encoded$blank[["CTstrict"]]] <- NA
  }
  groups <- if (is.null(groups)) {
    encoded$samples[encoded$sample]
  } else {
    as.character(groups)
  }
  keep <- !is.na(clones) & !is.na(groups) & nzchar(groups)
  if (!any(keep)) {
    return(NULL)
  }
  ids <- clones[keep]
  groups <- groups[keep]
  ## Groups in the order a per-sample split() first meets them.
  group_names <- character()
  for (values in split(groups, encoded$sample[keep])) {
    group_names <- c(group_names, setdiff(sort(unique(values)), group_names))
  }
  ## The cells of every clone per group, counted over integer clone ids.
  group_index <- match(groups, group_names)
  n_ids <- max(ids)
  counts <- lapply(
    split(ids, factor(group_index, levels = seq_along(group_names))),
    function(group_ids) {
      n <- tabulate(group_ids, nbins = n_ids)
      n[n > 0L]
    }
  )
  names(counts) <- group_names
  counts <- lapply(counts, function(x) x[x >= threshold])
  counts <- counts[lengths(counts) > 0L]
  if (!length(counts)) {
    return(NULL)
  }
  max_size <- max(unlist(counts, use.names = FALSE))
  max_bin <- floor(log2(max_size))
  profile <- matrix(
    0,
    nrow = length(counts),
    ncol = max_bin + 1L,
    dimnames = list(names(counts), as.character(0:max_bin))
  )
  for (group in names(counts)) {
    bins <- floor(log2(counts[[group]]))
    profile[group, ] <- tabulate(bins + 1L, nbins = ncol(profile))
  }
  totals <- rowSums(profile)
  profile <- profile / pmax(totals, 1)
  list(profile = profile, clone_counts = counts, threshold = threshold)
}


ir_size_distribution_profile <- reactive({
  encoded <- ir_encoded()
  chain <- req(input$ir_chain)
  profiles <- ir_clone_size_profiles(
    encoded,
    chain,
    ir_row_values(encoded, ir_param("ir_groupBy", NULL)),
    ir_param("ir_p_sd_threshold", 1)
  )
  # Keep just the current compact matrix, not the large named clone counts.
  if (!is.null(profiles)) {
    profiles$clone_counts <- NULL
  }
  profiles
})


ir_size_profile_plot <- function(profiles, method = "ward.D2") {
  if (is.null(profiles)) {
    return(NULL)
  }
  profile <- profiles$profile
  if (nrow(profile) == 1L) {
    frame <- data.frame(
      size = 2^(seq_len(ncol(profile)) - 1L),
      proportion = as.numeric(profile[1L, ]),
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(
        frame,
        ggplot2::aes(x = size, y = proportion)
      ) +
        ggplot2::geom_col(fill = "#4c72a6") +
        ggplot2::scale_x_log10() +
        ggplot2::labs(
          x = "Clone size (log scale)",
          y = "Proportion of clonotypes",
          title = rownames(profile)[[1L]]
        ) +
        ggplot2::theme_bw(base_size = 11)
    )
  }
  allowed <- c(
    "ward.D2",
    "ward.D",
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid"
  )
  if (!(method %in% allowed)) {
    method <- "ward.D2"
  }
  shiny::validate(shiny::need(
    requireNamespace("ggdendro", quietly = TRUE),
    "Install the ggdendro package to cluster clone-size profiles."
  ))
  tree <- stats::hclust(stats::dist(profile), method = method)
  dendro <- ggdendro::dendro_data(tree, type = "rectangle")
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = ggdendro::segment(dendro),
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    ggplot2::geom_text(
      data = ggdendro::label(dendro),
      ggplot2::aes(x = x, y = -0.02, label = label, hjust = 0),
      size = 3
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse(expand = c(0.2, 0)) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      subtitle = paste0(
        "Exact empirical clone-size profiles; minimum clone size = ",
        profiles$threshold,
        "."
      )
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}

## ---- Which CT* column a cloneCall maps to ----------------------------- ##
ir_clonecall_col <- function(cloneCall) {
  cerebro_clonecall_col(cloneCall)
}

## ---- Clonal UMAP data: coords + per-cell expansion level --------------- ##
## The chosen projection's canonical coordinates with each cell's
## clone-expansion level, restricted to the selected receptor (TCR/BCR): a
## cell's first repertoire row of the receptor with a clone, the clone's size
## counted over those cells. Returns a data.frame (x, y, expansion,
## cell_index) or NULL when it cannot be built (no projection, no data for the
## receptor).
##
##   projection : a name from availableProjections()
##   receptor   : "TCR" | "BCR"
##   cloneCall  : "gene" | "nt" | "aa" | "strict" (clone identity column)
##   show_all   : when TRUE, also include every other cell in the projection
##                with expansion = NA (drawn as a grey background by the
##                renderer), so the receptor cells are shown in context.
##   cells      : optional canonical (metadata) rows to restrict to (e.g.
##                from the Group filters); NULL = all cells in the projection.
##   split_output: return renderer-ready grouped coordinate arrays plus compact
##                deferred key sources. This avoids intermediate data.frames on
##                the interactive path. The default preserves the static-plot
##                contract.
ir_clonal_umap_data <- function(
  projection,
  receptor,
  cloneCall = "gene",
  show_all = TRUE,
  cells = NULL,
  percentage = 100,
  max_background = 100000L,
  split_output = FALSE
) {
  if (is.null(projection) || !nzchar(projection)) {
    return(NULL)
  }
  if (
    !(projection %in%
      tryCatch(availableProjections(), error = function(e) character(0)))
  ) {
    return(NULL)
  }
  encoded <- ir_encoded()
  if (is.null(encoded)) {
    return(NULL)
  }
  coords <- tryCatch(
    viewerProjectionFirstFrameCoordinates(projection),
    error = function(e) NULL
  )
  if (is.null(coords) || nrow(coords) == 0) {
    return(NULL)
  }
  percentage <- suppressWarnings(as.numeric(percentage))
  if (length(percentage) != 1L || !is.finite(percentage)) {
    percentage <- 100
  }
  percentage <- min(100, max(0, percentage))
  max_background <- suppressWarnings(as.integer(max_background))
  if (
    length(max_background) != 1L || is.na(max_background) || max_background < 0L
  ) {
    max_background <- 100000L
  }
  visible_indices <- seq_len(nrow(coords))
  ## `cells` are canonical (metadata) rows (ir_umap_cells_to_show()), the
  ## rows of the canonical coordinates.
  if (!is.null(cells)) {
    visible_indices <- visible_indices[visible_indices %in% cells]
    if (!length(visible_indices)) {
      return(NULL)
    }
  }

  clone_col <- ir_clonecall_col(cloneCall)
  clone <- encoded$clones[[clone_col]]$both
  blank <- encoded$blank[[clone_col]]
  bit <- if (identical(receptor, "BCR")) 2L else 1L
  rows <- which(
    bitwAnd(encoded$receptor, bit) > 0L &
      !is.na(clone) &
      (is.na(blank) | clone != blank) &
      !is.na(encoded$cell)
  )
  rows <- rows[!duplicated(encoded$cell[rows])]
  coord_index <- encoded$cell[rows]
  clones <- clone[rows]
  # Display filters and sampling must not redefine the biological clone size.
  size <- if (length(clones) && min(clones) >= 1L) {
    ## Clone ids are 1..n_clones: count them in place.
    tabulate(clones, nbins = max(clones))[clones]
  } else {
    tabulate(match(clones, unique(clones)))[match(clones, unique(clones))]
  }
  expansion_code <- findInterval(size, IR_CLONE_BINS, left.open = TRUE)
  ## Every row is visible unless `cells` narrows them (a lookup of each
  ## receptor cell in a million-long index otherwise).
  valid <- coord_index <= nrow(coords)
  if (!is.null(cells)) {
    valid <- valid & coord_index %in% visible_indices
  }
  coord_index <- coord_index[valid]
  has_receptor <- length(coord_index) > 0L
  if (!has_receptor) {
    # No receptor cells. With show_all we can still draw the grey background;
    # otherwise there is nothing to plot.
    if (!isTRUE(show_all)) {
      return(NULL)
    }
    expansion <- factor(levels = IR_CLONE_LABELS)
  } else {
    expansion <- factor(
      expansion_code[valid],
      levels = seq_along(IR_CLONE_LABELS),
      labels = IR_CLONE_LABELS
    )
  }
  receptor_indices <- unique(coord_index)

  # Coloured layer: receptor cells with an expansion level, joined to coords.
  if (has_receptor) {
    idx <- coord_index
    keep_n <- min(length(idx), ceiling(length(idx) * percentage / 100))
    if (keep_n < length(idx)) {
      keep <- if (keep_n > 0L) {
        unique(as.integer(round(seq(1, length(idx), length.out = keep_n))))
      } else {
        integer()
      }
      coord_index <- coord_index[keep]
      expansion <- expansion[keep]
      idx <- idx[keep]
    }
  } else {
    idx <- integer(0)
  }
  if (length(idx) == 0 && !isTRUE(show_all)) {
    return(NULL)
  }
  coord_x <- if (is.data.frame(coords)) coords[[1L]] else coords[, 1L]
  coord_y <- if (is.data.frame(coords)) coords[[2L]] else coords[, 2L]
  # Background layer: every other cell in the projection, expansion = NA, so the
  # renderer can draw them in grey. Only when show_all is requested.
  bg_idx <- integer()
  if (isTRUE(show_all)) {
    bg_mask <- if (is.null(cells)) {
      rep(TRUE, nrow(coords))
    } else {
      mask <- rep(FALSE, nrow(coords))
      mask[visible_indices] <- TRUE
      mask
    }
    bg_mask[receptor_indices] <- FALSE
    if (any(bg_mask)) {
      bg_idx <- which(bg_mask)
      target <- min(
        length(bg_idx),
        ceiling(length(bg_idx) * percentage / 100),
        as.integer(max_background)
      )
      if (target < length(bg_idx)) {
        keep <- if (target > 0L) {
          unique(as.integer(round(seq(1, length(bg_idx), length.out = target))))
        } else {
          integer()
        }
        bg_idx <- bg_idx[keep]
      }
    }
  }
  if (length(bg_idx) + length(idx) == 0L) {
    return(NULL)
  }
  if (isTRUE(split_output)) {
    expansion_codes <- as.integer(expansion)
    visible_expansions <- sort(unique(expansion_codes))
    visible_expansions <- visible_expansions[!is.na(visible_expansions)]
    coloured_rows <- lapply(
      visible_expansions,
      function(code) which(expansion_codes == code)
    )
    return(list(
      x = c(
        if (length(bg_idx)) list(as.numeric(coord_x[bg_idx])) else list(),
        lapply(coloured_rows, function(rows) as.numeric(coord_x[idx[rows]]))
      ),
      y = c(
        if (length(bg_idx)) list(as.numeric(coord_y[bg_idx])) else list(),
        lapply(coloured_rows, function(rows) as.numeric(coord_y[idx[rows]]))
      ),
      trace_codes = c(
        if (length(bg_idx)) NA_integer_ else integer(),
        visible_expansions
      ),
      coloured_rows = coloured_rows,
      background_key = bg_idx,
      coloured_key = coord_index,
      key_type = "cell_index"
    ))
  }

  # One frame: grey background rows (expansion = NA) first, then the coloured
  # receptor rows.
  rows <- c(bg_idx, idx)
  data.frame(
    x = as.numeric(coord_x[rows]),
    y = as.numeric(coord_y[rows]),
    expansion = factor(
      c(rep(NA_character_, length(bg_idx)), as.character(expansion)),
      levels = IR_CLONE_LABELS
    ),
    cell_index = c(bg_idx, coord_index),
    stringsAsFactors = FALSE
  )
}


## The cell names of a Clonal UMAP frame's canonical rows.
ir_clonal_umap_barcodes <- function(data) {
  if (!("cell_index" %in% colnames(data))) {
    return(NULL)
  }
  cells <- tryCatch(
    viewerPackCellBarcodes(viewerPackCurrent(), data$cell_index),
    error = function(e) NULL
  )
  if (!is.null(cells)) {
    return(cells)
  }
  metadata <- tryCatch(getMetaData(), error = function(e) NULL)
  if (
    !is.data.frame(metadata) ||
      !("cell_barcode" %in% colnames(metadata)) ||
      any(data$cell_index < 1L | data$cell_index > nrow(metadata))
  ) {
    return(NULL)
  }
  as.character(metadata$cell_barcode[data$cell_index])
}

## The sharing class of every clone (integer ids `clone`, 1..n_clones) from the
## distinct sharing units and groups its rows span:
##   Private               : found in exactly 1 unit
##   Public (within-group) : >= 2 units, all in the same group
##   Public (cross-group)  : spans >= 2 groups
## Without groups, Private / Public (>= 2 units). NA units and groups do not
## count.
ir_sharing_classes <- function(clone, n_clones, units, groups = NULL) {
  n_distinct <- function(values) {
    ok <- !is.na(values)
    levels <- unique(values[ok])
    codes <- (as.double(clone[ok]) - 1) *
      (length(levels) + 1) +
      match(values[ok], levels)
    tabulate(clone[ok][!duplicated(codes)], nbins = n_clones)
  }
  n_units <- n_distinct(units)
  if (is.null(groups)) {
    return(factor(
      ifelse(n_units <= 1, "Private", "Public"),
      levels = IR_SHARING_LEVELS_2
    ))
  }
  n_groups <- n_distinct(groups)
  factor(
    ifelse(
      n_units <= 1,
      "Private",
      ifelse(n_groups >= 2, "Public (cross-group)", "Public (within-group)")
    ),
    levels = IR_SHARING_LEVELS_3
  )
}

## ---- Sharing-class factor levels --------------------------------------- ##
IR_SHARING_LEVELS_3 <- c(
  "Private",
  "Public (within-group)",
  "Public (cross-group)"
)
IR_SHARING_LEVELS_2 <- c("Private", "Public")

## ---- Is the chain a BCR chain? ----------------------------------------- ##
## IGH/IGK/IGL undergo somatic hypermutation, so identical-CDR3 clone calling
## is over-strict for them (one clone can split into near-neighbour variants).
## The Clone Sharing plot surfaces this as a subtitle caveat.
ir_is_bcr_chain <- function(chain) {
  is.character(chain) &&
    length(chain) == 1 &&
    !is.na(chain) &&
    any(startsWith(chain, IR_BCR_CHAINS))
}

## ---- BCR caveat line appended to plot subtitles ------------------------ ##
IR_BCR_SHM_CAVEAT <- "BCR: CDR3 not collapsed by SHM; clones may be split."

## ---- Friendly display labels for the sharing classes ------------------- ##
## The data layer (ir_sharing_classes) keeps immunology-standard labels
## (Private / Public (within-group) / Public (cross-group)); the plot maps
## them to self-explanatory axis labels. Two-class mode reuses the first two.
## The count in the Private and Shared labels is of the sharing unit's values,
## so it names the unit the user chose: "sample" -> "1 sample" / "≥ 2 samples",
## "tissue_site" -> "1 tissue site" / "≥ 2 tissue sites".
ir_sharing_unit_noun <- function(unit_col) {
  noun <- if (
    is.character(unit_col) &&
      length(unit_col) == 1L &&
      !is.na(unit_col) &&
      nzchar(trimws(unit_col))
  ) {
    trimws(gsub("[._]+", " ", unit_col))
  } else {
    "sample"
  }
  list(
    one = noun,
    many = if (grepl("s$", noun)) noun else paste0(noun, "s")
  )
}

ir_sharing_display_labels <- function(unit_col = "sample") {
  noun <- ir_sharing_unit_noun(unit_col)
  c(
    "Private" = paste0("Private (1 ", noun$one, ")"),
    "Public (within-group)" = "Shared within group",
    "Public (cross-group)" = "Shared across groups",
    "Public" = paste0("Shared (≥ 2 ", noun$many, ")")
  )
}

## ---- Build the Clone Sharing ggplot ------------------------------------ ##
## Classifies each V;J;CDR3 clonotype of `chain` by the sharing units and the
## groups it spans and bars the class counts, using friendly x-axis labels.
## The units are `unit_col`'s values of the repertoire rows: the tables' own
## column, else the cell metadata's (except for "sample", which is the sample
## names unless the tables or the grouping carry it); the groups are
## `group_by`'s. Returns NULL on empty data or when the unit column is absent.
## Shared by the live renderer and the demo.
ir_build_sharing_plot <- function(
  encoded,
  chain,
  unit_col,
  group_by = NULL,
  metadata = NULL
) {
  clone <- encoded$sharing[[chain]]
  kept <- which(!is.na(clone))
  if (!length(kept)) {
    return(NULL)
  }
  grouped <- !is.null(group_by) && nzchar(group_by)
  units <- ir_row_values(
    encoded,
    unit_col,
    metadata,
    joined = !identical(unit_col, "sample") ||
      (grouped && identical(group_by, "sample"))
  )
  if (is.null(units)) {
    return(NULL)
  }
  groups <- if (grouped) ir_row_values(encoded, group_by, metadata)
  grouped <- !is.null(groups)
  sharing <- ir_sharing_classes(
    clone[kept],
    max(clone[kept]),
    as.character(units[kept]),
    if (grouped) as.character(groups[kept])
  )
  counts <- as.data.frame(table(sharing = sharing))
  counts$pct <- counts$Freq / sum(counts$Freq) * 100
  # Map the raw class labels to friendly display labels, preserving order.
  display_labels <- ir_sharing_display_labels(unit_col)
  counts$display <- factor(
    display_labels[as.character(counts$sharing)],
    levels = display_labels[levels(counts$sharing)]
  )
  # Clean, human-readable hover text. ggplotly otherwise derives the tooltip
  # from the raw aes names (display, Freq) and repeats `display` because both
  # the bar fill and the geom_text label map it — so we supply an explicit
  # `text` aes and pass tooltip = "text" to ggplotly (see visualizations.R).
  counts$tooltip <- sprintf(
    "%s\n%d clonotypes (%.1f%%)",
    as.character(counts$display),
    counts$Freq,
    counts$pct
  )
  same_col <- !is.null(group_by) &&
    nzchar(group_by) &&
    identical(group_by, unit_col)
  subtitle <- paste(
    "Each clonotype = one receptor.",
    "Private = in a single unit; Shared = in ≥ 2 units."
  )
  if (same_col) {
    subtitle <- paste(
      subtitle,
      "Group and sharing unit are the same column; within/cross is undefined.",
      sep = "\n"
    )
  } else if (is.null(group_by) || !nzchar(group_by)) {
    subtitle <- paste(
      subtitle,
      "No group selected — showing Private / Shared only.",
      sep = "\n"
    )
  }
  if (ir_is_bcr_chain(chain)) {
    subtitle <- paste(subtitle, IR_BCR_SHM_CAVEAT, sep = "\n")
  }
  counts$label <- sprintf("%d (%.1f%%)", counts$Freq, counts$pct)
  # Lift the labels a fixed fraction of the tallest bar above each bar top, so
  # they clear the bar edge and a zero-height (Freq == 0) bar's label is not
  # clamped onto the axis. vjust = 0 anchors the label bottom at that y.
  label_lift <- 0.04 * max(counts$Freq, 1)
  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = display, y = Freq, fill = display)
  ) +
    # `text` is not a ggplot aesthetic (ggplot warns and ignores it on static
    # render); it exists purely to feed ggplotly's hover tooltip. Suppress the
    # known, harmless "unknown aesthetics: text" warning so it doesn't noise up
    # test output.
    suppressWarnings(ggplot2::geom_col(
      ggplot2::aes(text = tooltip),
      width = 0.6
    )) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, y = Freq + label_lift),
      vjust = 0,
      size = 3.2
    ) +
    ggplot2::scale_fill_manual(
      values = cerebro_group_colors(length(unique(counts$display)))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Number of clonotypes",
      # Clear the fill legend title only (keep the legend itself): ggplotly
      # ignores the static legend.position = "none" and shows the legend, and
      # without this its title falls back to the raw fill column name ("display").
      fill = NULL,
      title = "Clonotype sharing",
      subtitle = subtitle
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}
