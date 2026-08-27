##----------------------------------------------------------------------------##
## What a clone IS, shared by every page that shows one.
##----------------------------------------------------------------------------##
## Two pages colour cells by clonal expansion: the Immune repertoire tab's Clonal
## UMAP and the Linked views workspace. They each used to answer the three
## questions below for themselves, and they answered two of them differently --
## so the same cell in the same data set belonged to a different clone on each
## page, and a 500-cell clone was "Hyperexpanded" on one and "Large" on the
## other. That is not a styling inconsistency; it is two different scientific
## results under one name, which is the kind of thing a reader would have to
## discover by noticing the numbers do not add up.
##
##   1. which CT* column names a clone   (gene / nt / aa / strict)
##   2. which receptor's chains count    (TCR or BCR, never mixed)
##   3. where the expansion bins fall    (scRepertoire's cloneSize defaults)
##
## Sourced before any module, so both pages read the same answers. Anything that
## renders a clone-expansion level -- including www/coordviews.js, which carries
## its own copy of the labels for the client-side clone layout -- has to agree
## with this file.

## ---- 1. chains that define each receptor class ------------------------- ##
CEREBRO_TCR_CHAINS <- c("TRA", "TRB", "TRG", "TRD")
CEREBRO_BCR_CHAINS <- c("IGH", "IGK", "IGL")

cerebro_receptor_chains <- function(receptor) {
  if (identical(receptor, "BCR")) CEREBRO_BCR_CHAINS else CEREBRO_TCR_CHAINS
}

## ---- 2. which CT* column a cloneCall maps to --------------------------- ##
## `gene` is the default because it is scRepertoire's, and because the Clonal
## UMAP has always used it. CTstrict is stricter (gene + nucleotide), so reading
## it instead SPLITS clones: a page on CTstrict reports more, smaller clones than
## a page on CTgene, from identical data.
CEREBRO_CLONE_CALL <- "gene"

## Spelled out rather than using %||%, which is base R only from 4.4 and would
## otherwise be inherited from whichever package happened to be attached.
cerebro_clonecall_col <- function(clone_call = CEREBRO_CLONE_CALL) {
  if (is.null(clone_call) || is.na(clone_call[1]) || !nzchar(clone_call[1])) {
    clone_call <- CEREBRO_CLONE_CALL
  }
  switch(
    clone_call[1],
    "gene" = "CTgene",
    "nt" = "CTnt",
    "aa" = "CTaa",
    "strict" = "CTstrict",
    "CTgene"
  )
}

## ---- 3. clone-size bins (scRepertoire cloneSize defaults) -------------- ##
## A clone's size = number of cells carrying that clonotype within the selected
## receptor. Five bins, not four: dropping the top one folds every clone above
## 100 cells into "Large", which is exactly where an expanded repertoire is
## interesting.
CEREBRO_CLONE_BINS <- c(0, 1, 5, 20, 100, Inf)
CEREBRO_CLONE_LABELS <- c(
  "Single (0 < X <= 1)",
  "Small (1 < X <= 5)",
  "Medium (5 < X <= 20)",
  "Large (20 < X <= 100)",
  "Hyperexpanded (100 < X)"
)

cerebro_clone_expansion <- function(size) {
  cut(
    size,
    breaks = CEREBRO_CLONE_BINS,
    labels = CEREBRO_CLONE_LABELS,
    right = TRUE,
    include.lowest = TRUE
  )
}

## ---- receptor detection ------------------------------------------------ ##
## Which receptor classes an IR list actually carries, in the order the Clonal
## UMAP offers them -- so "the default receptor" means the same thing on both
## pages. Reads the chain names out of CTstrict, which spells them out even when
## the clone is being called on another column.
cerebro_receptors_present <- function(ir) {
  if (is.null(ir) || !length(ir)) {
    return(character(0))
  }
  refs <- unlist(
    lapply(ir, function(df) {
      if (is.null(df) || !("CTstrict" %in% colnames(df))) {
        return(character(0))
      }
      as.character(df$CTstrict)
    }),
    use.names = FALSE
  )
  refs <- refs[!is.na(refs)]
  present <- character(0)
  if (
    any(vapply(
      CEREBRO_TCR_CHAINS,
      function(ch) any(grepl(ch, refs, fixed = TRUE)),
      logical(1)
    ))
  ) {
    present <- c(present, "TCR")
  }
  if (
    any(vapply(
      CEREBRO_BCR_CHAINS,
      function(ch) any(grepl(ch, refs, fixed = TRUE)),
      logical(1)
    ))
  ) {
    present <- c(present, "BCR")
  }
  present
}

## Rows of an IR table belonging to `receptor`. Mirrors the Clonal UMAP: match on
## CTstrict when present (it names the chains whatever the clone call is), and
## fall back to the clone column only when it is absent.
cerebro_rows_in_receptor <- function(df, receptor, clone_col) {
  keep <- cerebro_receptor_chains(receptor)
  ref <- if ("CTstrict" %in% colnames(df)) {
    as.character(df$CTstrict)
  } else {
    as.character(df[[clone_col]])
  }
  vapply(
    ref,
    function(s) {
      any(vapply(keep, function(ch) grepl(ch, s, fixed = TRUE), logical(1)))
    },
    logical(1),
    USE.NAMES = FALSE
  )
}
