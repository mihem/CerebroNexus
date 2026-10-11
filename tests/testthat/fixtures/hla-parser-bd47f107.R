# Frozen bd47f107 parser: correctness oracle, not production code.
legacy_hla_parse_ir_segments <- function(data, chain) {
  if (is.null(data) || length(data) == 0 || is.null(chain) || !nzchar(chain)) {
    return(NULL)
  }
  chain_slot_index <- function(ct_gene_vec) {
    ct_gene_vec <- as.character(ct_gene_vec)
    vapply(
      strsplit(ct_gene_vec, "_", fixed = TRUE),
      function(parts) {
        first <- sub(";.*$", "", parts)
        idx <- which(grepl(chain, first, fixed = TRUE) & first != "NA")
        if (length(idx) == 0) {
          NA_integer_
        } else {
          idx[1]
        }
      },
      integer(1)
    )
  }
  pick_slot <- function(ct_vec, slot_idx) {
    if (length(ct_vec) == 0) {
      return(character(0))
    }
    split_all <- strsplit(as.character(ct_vec), "_", fixed = TRUE)
    val <- mapply(
      function(parts, i) {
        if (is.na(i) || i > length(parts)) {
          NA_character_
        } else {
          parts[i]
        }
      },
      split_all,
      slot_idx,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    val[is.na(val) | val == "NA" | !nzchar(val)] <- NA_character_
    val
  }
  first_allele <- function(x) {
    ifelse(is.na(x), NA_character_, sub(";.*$", "", x))
  }
  rows <- lapply(data, function(df) {
    if (is.null(df) || !all(c("barcode", "CTgene", "CTaa") %in% colnames(df))) {
      return(NULL)
    }
    slot_idx <- chain_slot_index(df$CTgene)
    gene_seg <- first_allele(pick_slot(df$CTgene, slot_idx))
    cdr3 <- first_allele(pick_slot(df$CTaa, slot_idx))
    pull_token <- function(prefix) {
      vapply(
        strsplit(gene_seg, ".", fixed = TRUE),
        function(toks) {
          hit <- toks[grepl(paste0("^", chain, prefix), toks)]
          if (length(hit) == 0) {
            NA_character_
          } else {
            hit[1]
          }
        },
        character(1)
      )
    }
    v_gene <- pull_token("V")
    j_gene <- pull_token("J")
    keep <- !is.na(v_gene) & !is.na(cdr3) & nzchar(cdr3)
    if (!any(keep)) {
      return(NULL)
    }
    out <- df[keep, , drop = FALSE]
    out$v_gene <- v_gene[keep]
    out$j_gene <- j_gene[keep]
    out$cdr3 <- cdr3[keep]
    out$clone_vjc <- paste(out$v_gene, out$j_gene, out$cdr3, sep = ";")
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(NULL)
  }
  all_cols <- unique(unlist(lapply(rows, colnames)))
  rows <- lapply(rows, function(df) {
    miss <- setdiff(all_cols, colnames(df))
    for (col in miss) {
      df[[col]] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
