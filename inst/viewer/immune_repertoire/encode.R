##----------------------------------------------------------------------------##
## The immune repertoire as integer columns
##----------------------------------------------------------------------------##
## scRepertoire packs a cell's chains into strings (CTgene "TRAV1.TRAJ2.TRAC_
## TRBV3.TRBJ1.TRBC1", CTaa "CAVR_CASSL", ...). Every repertoire tab used to
## parse those strings again on each visit, after reading the whole table.
## ir_encode_repertoire() parses them once, into one integer per receptor row
## and question:
##
##   sample        the row's sample (a position in `samples`)
##   cell          the row's canonical cell row (NA: not a cell of the data set)
##   receptor      1 for a TCR row, 2 for a BCR row (CTstrict names the chains)
##   clones        per clone column (CTgene, CTnt, CTaa, CTstrict), the clone of
##                 the row for all chains ("both") and on each chain present, as
##                 scRepertoire's .offTheChain() takes it: TRA/TRG/IGH are the
##                 first "_" part, TRB/TRD/IGK/IGL the second
##   blank         per clone column, the id of the empty clone "" (NA if none)
##   other         per chain, the rows whose part at that position belongs to
##                 another chain (scRepertoire drops them)
##   sharing       per chain, the V;J;CDR3 clonotype of the Clone Sharing tab
##   isotype       the constant gene of the first IGH part (IGHA/D/E/G/M), as a
##                 position in `isotype_levels`
##   columns       the repertoire's own categorical columns (factors)
##   names         per clone column and chain, the clone of each id
##   rank          per clone column and chain, each id's place in the sorted
##                 names: Compare orders ties without reading the names
##
## A Viewer Pack stores this (immune/encoded.qs2, the names apart); a data set
## without one encodes its repertoire in memory when the page first needs it.
## Pure base R: the Pack builder sources this file too.

## 2: the clone ranks are in byte order (1: the locale's collation), so a Pack or
## a cached encoding of version 1 sorts equally large clones differently.
IR_ENCODING_VERSION <- 2L
IR_ENCODED_COLUMNS <- c("CTgene", "CTnt", "CTaa", "CTstrict")
IR_ENCODED_CHAINS <- c("TRA", "TRB", "TRG", "TRD", "IGH", "IGK", "IGL")
## scRepertoire's .offTheChain(): these chains are the first "_" part of a
## clone, the others the second.
IR_FIRST_CHAINS <- c("TRA", "TRG", "IGH")
IR_SECOND_CHAINS <- c("TRB", "TRD", "IGK", "IGL")

## Columns scRepertoire computes: not metadata, so not encoded as columns.
IR_SCR_COLUMNS <- c(
  "barcode",
  IR_ENCODED_COLUMNS,
  "clonalProportion",
  "clonalFrequency",
  "cloneSize",
  "Frequency",
  "frequency",
  "cloneType"
)

## The distinct (a, b) pairs of two equally long vectors: `first` holds the
## row of each pair's first occurrence, `index` each row's pair.
ir_encode_pairs <- function(a, b) {
  a <- match(a, unique(a))
  b <- match(b, unique(b))
  key <- a + (b - 1) * max(a, 0L)
  first <- which(!duplicated(key))
  list(first = first, index = match(key, key[first]))
}

## Integer ids of `values` in order of first appearance (NA stays NA).
ir_encode_ids <- function(values) {
  levels <- unique(values[!is.na(values)])
  list(ids = match(values, levels), levels = levels)
}

## The first or the second "_" part of each value (NA when it has none).
ir_encode_part <- function(x, first) {
  out <- rep(NA_character_, length(x))
  rows <- which(!is.na(x) & nzchar(x))
  x <- x[rows]
  if (!first) {
    cut <- regexpr("_", x, fixed = TRUE)
    keep <- cut > 0L
    rows <- rows[keep]
    x <- substr(x[keep], cut[keep] + 1L, .Machine$integer.max)
  }
  stop <- regexpr("_", x, fixed = TRUE) - 1L
  stop[stop < 0L] <- .Machine$integer.max
  out[rows] <- substr(x, 1L, stop)
  out
}

## What ir_encode_sharing_parts() reads of the parts whatever the chain: each
## pair's parts flattened, and the first allele of every gene part. Made once
## for all chains (the regular expression over every part was most of a
## chain's time).
ir_encode_sharing_prep <- function(gene_parts, aa_parts) {
  gene_flat <- unlist(gene_parts, use.names = FALSE)
  sizes <- lengths(gene_parts)
  aa_sizes <- lengths(aa_parts)
  list(
    gene_flat = gene_flat,
    aa_flat = unlist(aa_parts, use.names = FALSE),
    sizes = sizes,
    aa_sizes = aa_sizes,
    offsets = cumsum(sizes) - sizes,
    aa_offsets = cumsum(aa_sizes) - aa_sizes,
    owners = rep.int(seq_along(gene_parts), sizes),
    first_allele = sub(";.*$", "", gene_flat)
  )
}

## The V;J;CDR3 clonotype of `chain` for each (CTgene, CTaa) pair: the first
## "_" part whose first allele names the chain, its first V and J genes and
## the CDR3 at the same position. NA when any of them is missing.
ir_encode_sharing_parts <- function(
  gene_parts,
  aa_parts,
  chain,
  prep = ir_encode_sharing_prep(gene_parts, aa_parts)
) {
  pick <- function(flat, sizes, offsets, slot) {
    valid <- !is.na(slot) & slot <= sizes
    out <- rep(NA_character_, length(sizes))
    out[valid] <- flat[offsets[valid] + slot[valid]]
    out[is.na(out) | out == "NA" | !nzchar(out)] <- NA_character_
    sub(";.*$", "", out)
  }
  owners <- prep$owners
  first_allele <- prep$first_allele
  hits <- which(grepl(chain, first_allele, fixed = TRUE) & first_allele != "NA")
  hits <- hits[!duplicated(owners[hits])]
  slot <- rep(NA_integer_, length(gene_parts))
  slot[owners[hits]] <- hits - prep$offsets[owners[hits]]
  gene <- pick(prep$gene_flat, prep$sizes, prep$offsets, slot)
  cdr3 <- pick(prep$aa_flat, prep$aa_sizes, prep$aa_offsets, slot)
  tokens <- strsplit(gene, ".", fixed = TRUE)
  token_owner <- rep.int(seq_along(tokens), lengths(tokens))
  flat <- as.character(unlist(tokens, use.names = FALSE))
  first_token <- function(prefix) {
    hit <- which(startsWith(flat, paste0(chain, prefix)))
    hit <- hit[!duplicated(token_owner[hit])]
    out <- rep(NA_character_, length(tokens))
    out[token_owner[hit]] <- flat[hit]
    out
  }
  v <- first_token("V")
  j <- first_token("J")
  ok <- !is.na(v) & !is.na(j) & !is.na(cdr3) & nzchar(cdr3)
  out <- rep(NA_character_, length(ok))
  out[ok] <- paste(v[ok], j[ok], cdr3[ok], sep = ";")
  out
}

## The isotype of each CTgene: the constant gene after the "." of the first
## IGH part (IGHA/D/E/G/M only), else NA.
ir_encode_isotypes <- function(genes) {
  parts <- strsplit(genes, "_", fixed = TRUE)
  values <- as.character(unlist(parts, use.names = FALSE))
  owners <- rep.int(seq_along(parts), lengths(parts))
  hits <- which(startsWith(values, "IGH"))
  hits <- hits[!duplicated(owners[hits])]
  igh <- rep(NA_character_, length(genes))
  igh[owners[hits]] <- values[hits]
  isotype <- ifelse(
    !is.na(igh) & grepl(".", igh, fixed = TRUE),
    sub("^.*\\.", "", igh),
    NA_character_
  )
  ifelse(grepl("^IGH[ADEGM]", isotype), isotype, NA_character_)
}

## `repertoire`: the list of sample tables. `cells`: the data set's cell names
## in canonical order (the Pack's, or the metadata's).
ir_encode_repertoire <- function(repertoire, cells = NULL, rank = TRUE) {
  frames <- lapply(repertoire, function(frame) {
    if (is.data.frame(frame)) frame else data.frame()
  })
  samples <- names(frames) %||% as.character(seq_along(frames))
  sizes <- vapply(frames, nrow, integer(1))
  n <- sum(sizes)
  column <- function(name) {
    values <- lapply(frames, function(frame) {
      if (name %in% names(frame)) {
        as.character(frame[[name]])
      } else {
        rep(NA_character_, nrow(frame))
      }
    })
    unlist(values, use.names = FALSE) %||% character()
  }
  ## Each row's canonical cell row. A thin CRB's sidecar table holds it already
  ## (attr "cerebro_cell_index_column"): its cells need no names to match.
  indexed <- vapply(
    frames,
    function(frame) {
      identical(
        attr(frame, "cerebro_cell_index_column", exact = TRUE),
        "barcode"
      ) &&
        is.integer(frame$barcode)
    },
    logical(1)
  )
  indexed_rows <- rep.int(indexed, sizes)
  cell <- rep(NA_integer_, n)
  if (any(indexed)) {
    cell[indexed_rows] <- unlist(
      lapply(frames[indexed], `[[`, "barcode"),
      use.names = FALSE
    )
  }
  if (!is.null(cells) && !all(indexed)) {
    ## one match() for all tables: each call hashes the million cell names
    named <- frames[!indexed]
    cell[!indexed_rows] <- match(
      unlist(
        lapply(named, function(frame) {
          if ("barcode" %in% names(frame)) {
            as.character(frame$barcode)
          } else {
            rep(NA_character_, nrow(frame))
          }
        }),
        use.names = FALSE
      ),
      cells
    )
  }
  genes <- column("CTgene")
  gene_ids <- ir_encode_ids(genes)
  ## The chains named by any gene: looked for in the distinct genes, not in
  ## every row.
  chains <- IR_ENCODED_CHAINS[vapply(
    IR_ENCODED_CHAINS,
    function(chain) any(grepl(chain, gene_ids$levels, fixed = TRUE)),
    logical(1)
  )]

  ## Receptor of each row from CTstrict, or CTgene in a table without it.
  ## When every table with rows has CTstrict, this is the CTstrict column,
  ## and its ids are that column's clones below (one string hash less).
  strict_everywhere <- all(vapply(
    frames,
    function(frame) !nrow(frame) || "CTstrict" %in% names(frame),
    logical(1)
  ))
  reference <- unlist(
    lapply(frames, function(frame) {
      name <- if ("CTstrict" %in% names(frame)) "CTstrict" else "CTgene"
      if (name %in% names(frame)) {
        as.character(frame[[name]])
      } else {
        rep(NA_character_, nrow(frame))
      }
    }),
    use.names = FALSE
  ) %||%
    character()
  reference_ids <- ir_encode_ids(reference)
  has <- function(chains) {
    Reduce(
      `|`,
      lapply(chains, function(chain) {
        grepl(chain, reference_ids$levels, fixed = TRUE)
      }),
      logical(length(reference_ids$levels))
    )
  }
  receptor <- (has(c("TRA", "TRB", "TRG", "TRD")) *
    1L +
    has(c("IGH", "IGK", "IGL")) * 2L)[reference_ids$ids]
  receptor[is.na(receptor)] <- 0L

  ## Per chain, the genes whose part at the chain's position names another
  ## chain (its first three letters; "NA" and "Non" name none).
  other_genes <- list()
  other <- list()
  for (first in c(TRUE, FALSE)) {
    check <- substr(ir_encode_part(gene_ids$levels, first), 1L, 3L)
    check[check %in% c("NA", "Non")] <- NA
    for (chain in intersect(
      chains,
      if (first) IR_FIRST_CHAINS else IR_SECOND_CHAINS
    )) {
      other_genes[[chain]] <- !is.na(check) &
        !grepl(chain, check, fixed = TRUE)
      other[[chain]] <- which(other_genes[[chain]][gene_ids$ids])
    }
  }
  other <- other[intersect(chains, names(other))]
  clones <- list()
  blank <- integer()
  names_of <- list()
  aa <- NULL
  aa_ids <- NULL
  for (name in IR_ENCODED_COLUMNS) {
    ## The columns hashed already (CTgene, and CTstrict as the receptor's
    ## reference) are not hashed again.
    if (identical(name, "CTgene")) {
      values <- genes
      both <- gene_ids
    } else if (identical(name, "CTstrict") && strict_everywhere) {
      values <- reference
      both <- reference_ids
    } else {
      values <- column(name)
      both <- ir_encode_ids(values)
    }
    if (identical(name, "CTaa")) {
      aa <- values
      aa_ids <- both$ids
    }
    clones[[name]] <- list(both = both$ids)
    names_of[[name]] <- list(both = both$levels)
    blank[[name]] <- match("", both$levels)
    pairs <- ir_encode_pairs(gene_ids$ids, both$ids)
    pair_gene <- gene_ids$ids[pairs$first]
    for (first in c(TRUE, FALSE)) {
      on_part <- intersect(
        chains,
        if (first) IR_FIRST_CHAINS else IR_SECOND_CHAINS
      )
      if (!length(on_part)) {
        next
      }
      part <- ir_encode_part(values[pairs$first], first)
      part[part %in% c("NA", "")] <- NA_character_
      for (chain in on_part) {
        ## A part whose gene belongs to another chain is no clone of this one.
        other_gene <- other_genes[[chain]][pair_gene]
        chain_part <- part
        chain_part[!is.na(other_gene) & other_gene] <- NA_character_
        ids <- ir_encode_ids(chain_part)
        clones[[name]][[chain]] <- ids$ids[pairs$index]
        names_of[[name]][[chain]] <- ids$levels
      }
    }
  }

  ## The CTaa column and its clones from the loop above: ir_encode_pairs()
  ## numbers the ids by first appearance as it numbered match(aa, unique(aa)),
  ## an NA string being a class of its own either way.
  pairs <- ir_encode_pairs(gene_ids$ids, aa_ids)
  gene_parts <- strsplit(genes[pairs$first], "_", fixed = TRUE)
  aa_parts <- strsplit(aa[pairs$first], "_", fixed = TRUE)
  prep <- ir_encode_sharing_prep(gene_parts, aa_parts)
  sharing <- list()
  for (chain in chains) {
    ids <- ir_encode_ids(
      ir_encode_sharing_parts(gene_parts, aa_parts, chain, prep)
    )
    sharing[[chain]] <- ids$ids[pairs$index]
  }

  isotypes <- ir_encode_isotypes(gene_ids$levels)
  isotype_levels <- unique(isotypes[!is.na(isotypes)])
  isotype <- match(isotypes, isotype_levels)[gene_ids$ids]

  ## The tables' own categorical columns: a grouping column a table carries
  ## is read from it rather than from the cell metadata. attr "samples" lists
  ## the tables that carry a column the others lack.
  carried <- Filter(function(frame) nrow(frame) > 0L, frames)
  columns <- list()
  for (name in setdiff(
    unique(unlist(lapply(carried, names))),
    IR_SCR_COLUMNS
  )) {
    has <- vapply(frames, function(frame) name %in% names(frame), logical(1))
    values <- lapply(frames[has], `[[`, name)
    if (
      !all(vapply(values, function(v) is.character(v) || is.factor(v), TRUE))
    ) {
      next
    }
    character <- !all(vapply(values, is.factor, logical(1)))
    levels <- if (character) NULL else unique(unlist(lapply(values, levels)))
    values <- unlist(
      lapply(seq_along(frames), function(i) {
        if (has[[i]]) {
          as.character(frames[[i]][[name]])
        } else {
          rep(NA_character_, sizes[[i]])
        }
      }),
      use.names = FALSE
    )
    levels <- levels %||% unique(values[!is.na(values)])
    columns[[name]] <- structure(
      factor(values, levels = levels),
      character = character,
      samples = if (!all(has[sizes > 0L])) which(has)
    )
  }

  list(
    version = IR_ENCODING_VERSION,
    samples = samples,
    sample = rep.int(seq_along(frames), sizes),
    cell = cell,
    receptor = receptor,
    receptors = c("TCR", "BCR")[c(
      any(bitwAnd(receptor, 1L) > 0L),
      any(bitwAnd(receptor, 2L) > 0L)
    )],
    chains = chains,
    clones = clones,
    blank = blank,
    other = other,
    sharing = sharing,
    isotype = isotype,
    isotype_levels = isotype_levels,
    columns = columns,
    rank = if (isTRUE(rank)) ir_clone_ranks(names_of),
    names = names_of
  )
}

## The place of each clone id when the clone names are sorted (the tie order of
## the largest clones). Byte order, not the locale's collation: sorting hundreds
## of thousands of names with strcoll takes about a minute on the Ren data, and
## the order should not depend on the machine that built the Pack.
ir_clone_ranks <- function(names_of) {
  lapply(names_of, function(chains) {
    lapply(chains, function(levels) {
      rank <- integer(length(levels))
      rank[order(levels, method = "radix")] <- seq_along(levels)
      rank
    })
  })
}

## The ranks of the clones of `column` on `chain`: stored in a Pack's encoding,
## else sorted from the names the encoding holds in memory.
ir_clone_rank <- function(encoded, column, chain) {
  stored <- encoded$rank[[column]][[chain]]
  if (!is.null(stored)) {
    return(stored)
  }
  names <- encoded$names[[column]][[chain]]
  if (is.null(names)) {
    return(NULL)
  }
  ir_clone_ranks(stats::setNames(
    list(stats::setNames(list(names), chain)),
    column
  ))[[column]][[chain]]
}

## ---- The cells of a Clonal projection frame ----------------------------- ##
## The Viewer draws the frame (immune_repertoire/data.R, ir_clonal_umap_data())
## and the Viewer Pack builder stores the default one (R/viewer_pack.R) with
## this function, so that both hold the same cells. 1: the cells and codes
## below, the Pack's frame in the order the Viewer paints it.
IR_CLONAL_FRAME_VERSION <- 1L

## On the `n` canonical rows of a projection: the cells of `receptor` (a cell's
## first row of the receptor with a clone of `clone_col`) as `coloured`, with
## their clone-expansion `code` (the clone's size, counted over those cells,
## cut by `bins` as findInterval(left.open = TRUE) cuts it), and with
## `show_all` every other cell as `background`, at most `max_background` of
## them. `cells` (canonical rows, NULL for all) and `percentage` narrow what is
## shown, never the clone sizes. NULL when there is nothing to draw.
ir_clonal_frame_cells <- function(
  encoded,
  receptor,
  clone_col,
  n,
  bins,
  cells = NULL,
  show_all = TRUE,
  percentage = 100,
  max_background = 100000L
) {
  visible_indices <- seq_len(n)
  if (!is.null(cells)) {
    visible_indices <- visible_indices[visible_indices %in% cells]
    if (!length(visible_indices)) {
      return(NULL)
    }
  }
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
  code <- findInterval(size, bins, left.open = TRUE)
  ## Every row is visible unless `cells` narrows them (a lookup of each
  ## receptor cell in a million-long index otherwise).
  valid <- coord_index <= n
  if (!is.null(cells)) {
    valid <- valid & coord_index %in% visible_indices
  }
  coord_index <- coord_index[valid]
  code <- code[valid]
  # No receptor cells: with show_all the grey background is still drawn.
  if (!length(coord_index) && !isTRUE(show_all)) {
    return(NULL)
  }
  receptor_indices <- unique(coord_index)
  keep_n <- min(
    length(coord_index),
    ceiling(length(coord_index) * percentage / 100)
  )
  if (keep_n < length(coord_index)) {
    keep <- if (keep_n > 0L) {
      unique(as.integer(round(seq(
        1,
        length(coord_index),
        length.out = keep_n
      ))))
    } else {
      integer()
    }
    coord_index <- coord_index[keep]
    code <- code[keep]
  }
  if (!length(coord_index) && !isTRUE(show_all)) {
    return(NULL)
  }
  # Background: every other cell in the projection, drawn in grey.
  bg_idx <- integer()
  if (isTRUE(show_all)) {
    bg_mask <- if (is.null(cells)) {
      rep(TRUE, n)
    } else {
      mask <- rep(FALSE, n)
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
  if (length(bg_idx) + length(coord_index) == 0L) {
    return(NULL)
  }
  list(coloured = coord_index, code = code, background = bg_idx)
}
