## ---- Reactive: raw repertoire data (as stored in crb) ------------------ ##
ir_data_raw <- reactive({
  req(!is.null(data_set()))
  data <- getImmuneRepertoire()
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(NULL)
  }
  data
})

## ---- Candidate columns for sample splitting --------------------------- ##
ir_sample_col_choices <- reactive({
  data <- ir_data_raw()
  if (is.null(data)) {
    return(character(0))
  }
  shared <- Reduce(intersect, lapply(data, colnames))
  scr_cols <- c(
    "barcode",
    "CTgene",
    "CTnt",
    "CTaa",
    "CTstrict",
    "clonalProportion",
    "clonalFrequency",
    "cloneSize",
    "Frequency",
    "frequency",
    "cloneType"
  )
  candidates <- setdiff(shared, scr_cols)
  ok <- vapply(
    candidates,
    function(col) {
      vals <- unique(unlist(lapply(data, function(df) unique(df[[col]]))))
      n <- length(vals)
      n >= 2L && n <= 200L
    },
    logical(1)
  )
  candidates[ok]
})

## ---- Reactive: repertoire data (re-split by user-chosen column) ------- ##
ir_data <- reactive({
  data <- ir_data_raw()
  if (is.null(data)) {
    return(NULL)
  }
  col <- input$ir_sampleCol
  if (is.null(col) || col == "" || col == "(original)") {
    return(data)
  }
  merged <- dplyr::bind_rows(lapply(names(data), function(nm) {
    df <- data[[nm]]
    df$.orig_sample <- nm
    df
  }))
  if (is.null(merged) || nrow(merged) == 0) {
    return(data)
  }
  if (!col %in% colnames(merged)) {
    return(data)
  }
  split_list <- split(merged, merged[[col]])
  lapply(split_list, function(df) {
    df$.orig_sample <- NULL
    df
  })
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
}) %>%
  debounce(500)

## ---- Debounced slider values (avoid recompute on every drag tick) ----- ##
ir_diversity_boots_d <- debounce(reactive(input$ir_diversity_boots), 400)
ir_rarefaction_boots_d <- debounce(reactive(input$ir_rarefaction_boots), 400)
ir_kmer_top_motifs_d <- debounce(reactive(input$ir_kmer_top_motifs), 400)

## ---- Reactive: number of groups for faceted plots --------------------- ##
n_groups <- reactive({
  gb <- ir_params()$groupBy
  if (is.null(gb)) {
    return(1L)
  }
  data <- ir_data()
  if (is.null(data)) {
    return(1L)
  }
  lvls <- unique(unlist(lapply(data, function(df) {
    if (gb %in% names(df)) unique(as.character(df[[gb]])) else character(0)
  })))
  max(1L, length(lvls))
})

## ---- Dynamic gene parameter for vizGenes/percentGeneUsage ------------- ##
default_gene_family <- reactive({
  chains <- detect_chains(ir_data())
  tcr_chains <- intersect(chains, c("TRA", "TRB", "TRG", "TRD"))
  bcr_chains <- intersect(chains, c("IGH", "IGK", "IGL"))
  if (length(tcr_chains) > 0 && "TRB" %in% tcr_chains) {
    return("TRBV")
  }
  if (length(tcr_chains) > 0) {
    return(paste0(tcr_chains[1], "V"))
  }
  if (length(bcr_chains) > 0 && "IGH" %in% bcr_chains) {
    return("IGHV")
  }
  if (length(bcr_chains) > 0) {
    return(paste0(bcr_chains[1], "V"))
  }
  "TRBV"
})

## ---- Resolve chain: for functions that don't accept "both" ------------ ##
specific_chain <- reactive({
  ch <- input$ir_chain
  if (is.null(ch) || ch == "both") {
    chains <- detect_chains(ir_data())
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

## ---- Count unique genes for dynamic plot height ----------------------- ##
n_genes <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(0L)
  }
  gene_family <- default_gene_family()
  # Gather all gene values across samples
  all_genes <- unique(unlist(lapply(data, function(df) {
    # CTgene has format like "TRBV1.TRBJ2" — extract the gene family portion
    ct <- as.character(df$CTgene)
    ct <- ct[!is.na(ct)]
    # Split by "." and keep segments matching the gene family prefix
    segments <- unlist(strsplit(ct, "[._]"))
    segments[grepl(paste0("^", gene_family), segments, ignore.case = TRUE)]
  })))
  length(all_genes)
})

ir_plot_height <- function(facet_mode = c("none", "grid", "wrap")) {
  facet_mode <- match.arg(facet_mode)
  n <- n_genes()
  ng <- n_groups()
  base_h <- max(450, min(n * 25, 2500))
  if (ng <= 1 || facet_mode == "none") {
    return(base_h)
  }
  if (facet_mode == "grid") {
    # facet_grid(Group ~ .): each group stacked vertically
    return(base_h * ng)
  }
  # facet_wrap: ggplot default ncol = ceiling(sqrt(n))
  ncol <- ceiling(sqrt(ng))
  nrow <- ceiling(ng / ncol)
  base_h * nrow
}
