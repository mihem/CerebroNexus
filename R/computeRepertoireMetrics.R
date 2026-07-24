#' Precompute immune-repertoire metric tables for storage in a .crb
#'
#' Computes, with \pkg{scRepertoire}, the clonal-metric tables the Immune
#' Repertoire tab draws — across the clone definitions and chains the tab lets
#' the user pick — so the viewer can render scRepertoire-identical figures
#' straight from the stored tables, without recomputing (or depending on)
#' scRepertoire at runtime. The tables are keyed \code{"metric|cloneCall|chain"}
#' (clonal overlap also by method) and cover the by-sample view; grouped views
#' and clone definitions/chains outside the grid fall back to the app's native
#' metrics at runtime.
#'
#' Returns an empty list when there is nothing to compute or scRepertoire is not
#' installed (the app then computes everything natively, as before).
#'
#' @param immune_repertoire Named \code{list} of per-sample data.frames in
#'   scRepertoire "combined" format (\code{CTgene}, \code{CTnt}, \code{CTaa},
#'   \code{CTstrict}).
#' @param verbose Emit progress messages. Default \code{TRUE}.
#'
#' @return Named \code{list}: a \code{meta} entry plus one entry per
#'   \code{"metric|cloneCall|chain"} key holding that metric's scRepertoire
#'   export table. Empty list when nothing could be computed.
#'
#' @noRd
computeRepertoireMetrics <- function(immune_repertoire, verbose = TRUE) {
  if (
    is.null(immune_repertoire) ||
      !is.list(immune_repertoire) ||
      length(immune_repertoire) == 0
  ) {
    return(list())
  }
  if (!requireNamespace("scRepertoire", quietly = TRUE)) {
    if (verbose) {
      message(
        "[computeRepertoireMetrics] scRepertoire not installed; skipping ",
        "precompute (viewer will compute metrics natively)."
      )
    }
    return(list())
  }

  ir <- immune_repertoire
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)

  ## Chains actually present in the data (plus "both" = combined clonotype).
  ct <- unlist(lapply(ir, function(df) {
    if ("CTgene" %in% names(df)) as.character(df$CTgene) else character(0)
  }))
  families <- c("TRA", "TRB", "TRG", "TRD", "IGH", "IGK", "IGL")
  present <- families[vapply(
    families,
    function(f) any(grepl(f, ct)),
    logical(1)
  )]
  chains <- c("both", present)
  clone_calls <- c("gene", "nt", "aa", "strict")

  tables <- list(
    meta = list(
      engine = "scRepertoire",
      engine_version = as.character(utils::packageVersion("scRepertoire")),
      samples = names(ir),
      cloneCalls = clone_calls,
      chains = chains
    )
  )
  key <- function(...) paste(..., sep = "|")

  for (cc in clone_calls) {
    for (ch in chains) {
      tables[[key("clonalHomeostasis", cc, ch)]] <- safe(
        scRepertoire::clonalHomeostasis(
          ir,
          cloneCall = cc,
          chain = ch,
          exportTable = TRUE
        )
      )
      tables[[key("clonalProportion", cc, ch)]] <- safe(
        scRepertoire::clonalProportion(
          ir,
          cloneCall = cc,
          chain = ch,
          exportTable = TRUE
        )
      )
      tables[[key("clonalQuant", cc, ch)]] <- safe(
        scRepertoire::clonalQuant(
          ir,
          cloneCall = cc,
          chain = ch,
          scale = FALSE,
          exportTable = TRUE
        )
      )
      for (m in c("overlap", "morisita", "jaccard")) {
        tables[[key("clonalOverlap", cc, ch, m)]] <- safe(
          scRepertoire::clonalOverlap(
            ir,
            cloneCall = cc,
            chain = ch,
            method = m,
            exportTable = TRUE
          )
        )
      }
    }
  }

  ## clonalLength is defined on the CDR3 sequence, so only nt / aa.
  for (cc in c("nt", "aa")) {
    for (ch in chains) {
      tables[[key("clonalLength", cc, ch)]] <- safe(
        scRepertoire::clonalLength(
          ir,
          cloneCall = cc,
          chain = ch,
          exportTable = TRUE
        )
      )
    }
  }

  ## percentAA is keyed by chain only (no clone definition).
  for (ch in chains) {
    tables[[key("percentAA", ch)]] <- safe(
      scRepertoire::percentAA(ir, chain = ch, exportTable = TRUE)
    )
  }

  ## Drop failed cells so the store only advertises what is actually there.
  tables <- tables[!vapply(tables, is.null, logical(1))]

  if (verbose) {
    n <- length(setdiff(names(tables), "meta"))
    message(
      "[computeRepertoireMetrics] precomputed ",
      n,
      " tables via scRepertoire ",
      tables$meta$engine_version,
      " (",
      length(clone_calls),
      " clone defs x ",
      length(chains),
      " chains)."
    )
  }

  tables
}
