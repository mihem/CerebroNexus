## ---- Help-example routing --------------------------------------------- ##
## Keep data provenance in one table so the button, provenance note, data
## loader, and namespace boundary cannot disagree. Tabs absent from this map do
## not have a standalone example and must not show an Example button.
IR_HELP_DEMO_KINDS <- c(
  "Abundance" = "backed",
  "Diversity" = "backed",
  "Homeostasis" = "backed",
  "Compare" = "backed",
  "Paired Scatter" = "backed",
  "SizeDist" = "backed",
  "Clone Sharing" = "local_tcr",
  "Isotype" = "local_bcr"
)

ir_help_demo_kind <- function(tab) {
  if (
    !is.character(tab) ||
      length(tab) != 1L ||
      is.na(tab) ||
      !nzchar(tab)
  ) {
    return("none")
  }
  kind <- unname(IR_HELP_DEMO_KINDS[tab])
  if (length(kind) != 1L || is.na(kind)) "none" else kind
}

ir_help_has_example <- function(tab) {
  !identical(ir_help_demo_kind(tab), "none")
}

## ---- Local RNG boundary ------------------------------------------------ ##
## set.seed() changes the RNG stream for the entire Shiny R process, including
## other sessions. Generate reproducible help data without changing the
## caller's stream. This is base R because bundled runtime code cannot rely on
## withr, which is only a Suggests dependency.
ir_with_preserved_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )
  set.seed(seed)
  force(expr)
}

## ---- Synthetic BCR demo ----------------------------------------------- ##
ir_make_bcr_demo_data <- function(seed = 42L) {
  ir_with_preserved_seed(seed, {
    n <- 200L

    make_ctgene <- function(iso) {
      v <- sample(
        c("IGHV1-2", "IGHV1-18", "IGHV3-23", "IGHV3-30", "IGHV4-34"),
        1L
      )
      d <- sample(c("IGHD2-2", "IGHD3-10", "IGHD6-13"), 1L)
      j <- sample(c("IGHJ4", "IGHJ5", "IGHJ6"), 1L)
      paste(v, d, j, iso, sep = ".")
    }

    make_nt <- function() {
      paste(
        sample(c("A", "T", "G", "C"), 300L, replace = TRUE),
        collapse = ""
      )
    }

    make_bc <- function(i, prefix) {
      paste0(prefix, "_BCR_", sprintf("%04d", i))
    }

    make_df <- function(prefix, iso_probs) {
      isos <- sample(names(iso_probs), n, replace = TRUE, prob = iso_probs)
      clones <- sample(1L:40L, n, replace = TRUE)
      data.frame(
        barcode = vapply(seq_len(n), function(i) make_bc(i, prefix), ""),
        CTgene = vapply(isos, make_ctgene, ""),
        CTnt = vapply(seq_len(n), function(i) make_nt(), ""),
        CTaa = paste0(
          "C",
          vapply(
            seq_len(n),
            function(i) {
              paste(
                sample(
                  strsplit("ARNDCQEGHILKMFPSTWYV", "")[[1]],
                  15L,
                  replace = TRUE
                ),
                collapse = ""
              )
            },
            ""
          )
        ),
        CTstrict = vapply(
          clones,
          function(clone) sprintf("IGH_clone_%03d", clone),
          ""
        ),
        sample = prefix,
        cloneSize = sample(1L:8L, n, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }

    list(
      "Pre-vaccination" = make_df(
        "Pre-vaccination",
        c(
          IGHM = 0.60,
          IGHD = 0.20,
          IGHG1 = 0.10,
          IGHG2 = 0.05,
          IGHA1 = 0.05
        )
      ),
      "Post-vaccination" = make_df(
        "Post-vaccination",
        c(
          IGHM = 0.20,
          IGHD = 0.05,
          IGHG1 = 0.30,
          IGHG2 = 0.15,
          IGHG3 = 0.10,
          IGHA1 = 0.15,
          IGHE = 0.05
        )
      )
    )
  })
}

## ---- Synthetic TCR demo ----------------------------------------------- ##
ir_make_tcr_demo_data <- function(seed = 7L) {
  ir_with_preserved_seed(seed, {
    aa <- strsplit("ARNDCQEGHILKMFPSTWYV", "")[[1]]
    vs <- c("TRBV5-1", "TRBV7-9", "TRBV19", "TRBV20-1", "TRBV28")
    js <- c("TRBJ1-1", "TRBJ2-1", "TRBJ2-7")
    pool <- lapply(seq_len(60L), function(k) {
      list(
        CTgene = paste0(sample(vs, 1L), ".None.", sample(js, 1L), ".TRBC2"),
        CTaa = paste0(
          "CASS",
          paste(sample(aa, 9L, replace = TRUE), collapse = ""),
          "F"
        ),
        CTnt = paste(
          sample(c("A", "T", "G", "C"), 42L, replace = TRUE),
          collapse = ""
        )
      )
    })
    make_df <- function(prefix, idx) {
      n <- length(idx)
      data.frame(
        barcode = sprintf("%s_TCR_%04d", prefix, seq_len(n)),
        CTgene = vapply(idx, function(k) pool[[k]]$CTgene, ""),
        CTnt = vapply(idx, function(k) pool[[k]]$CTnt, ""),
        CTaa = vapply(idx, function(k) pool[[k]]$CTaa, ""),
        CTstrict = vapply(
          idx,
          function(k) sprintf("TRB_clone_%03d", k),
          ""
        ),
        sample = prefix,
        cloneSize = sample(1L:8L, n, replace = TRUE),
        stringsAsFactors = FALSE
      )
    }

    list(
      "Healthy" = make_df("Healthy", sample(1L:40L, 200L, replace = TRUE)),
      "Disease" = make_df("Disease", sample(20L:60L, 200L, replace = TRUE))
    )
  })
}
