## Pins the Cerebro object that exportFromSCE() and exportFromSeurat() build
## from the same inputs, so the two exporters can share their builders without
## drifting apart.

skip_if_not_installed("SingleCellExperiment")
skip_if_not_installed("ggplot2")

pin_example <- function(name) {
  path <- system.file("extdata/examples", name, package = "CerebroNexus")
  if (!nzchar(path)) {
    path <- testthat::test_path("../../inst/extdata/examples", name)
  }
  readRDS(path)
}

## Everything the exporters copy from `@metadata` / `@misc`.
pin_extras <- function() {
  list(
    experiment = list(date_of_analysis = "2020-01-01"),
    parameters = list(min_cells = 3L, label = "x"),
    technical_info = list(R = "4.0"),
    gene_lists = list(mt = c("MT-A", "MT-B"), ribo = "RPL1"),
    trees = list(sample = list(tree = "sample")),
    most_expressed_genes = list(sample = data.frame(sample = "A", gene = "G1")),
    marker_genes = list(
      cerebro_seurat = list(
        sample = data.frame(gene = "G1"),
        cluster = data.frame(gene = "G2")
      )
    ),
    enriched_pathways = list(enrichr = list(sample = data.frame(term = "T1"))),
    trajectories = list(
      monocle2 = list(t1 = list(meta = data.frame(pseudotime = 1))),
      slingshot = list(t2 = list())
    ),
    extra_material = list(
      tables = list(tab = data.frame(a = 1:2)),
      plots = list(plot = ggplot2::ggplot())
    )
  )
}

## Grouping columns with NA values in both supported shapes, a cell cycle
## column, and one extra column.
pin_meta <- function(meta) {
  n <- nrow(meta)
  sample <- as.character(meta$sample)
  sample[2] <- NA
  meta$sample <- sample
  meta$cluster <- factor(rep(c("b", "a"), length.out = n), levels = c("b", "a"))
  meta$cluster[3] <- NA
  meta$Phase <- rep(c("S", "G1", "G2M"), length.out = n)
  meta$score <- seq_len(n) / 10
  meta
}

pin_export <- function(exporter, ...) {
  file <- tempfile(fileext = ".crb")
  on.exit(unlink(file), add = TRUE)
  messages <- character()
  warnings <- character()
  withCallingHandlers(
    exporter(
      ...,
      file = file,
      experiment_name = "pin",
      organism = "hg",
      cell_cycle = "Phase",
      codec = "rds",
      viewer_binary = "never"
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(crb = readRDS(file), messages = messages, warnings = warnings)
}

expect_pinned_content <- function(crb, meta, groups, rest) {
  extras <- pin_extras()
  expect_s3_class(crb, "Cerebro")
  expect_identical(
    names(crb$experiment),
    c("experiment_name", "organism", "date_of_analysis", "date_of_export")
  )
  expect_identical(crb$experiment$experiment_name, "pin")
  expect_identical(crb$experiment$organism, "hg")
  expect_identical(crb$experiment$date_of_analysis, "2020-01-01")
  expect_s3_class(crb$experiment$date_of_export, "Date")
  expect_identical(crb$technical_info, extras$technical_info)
  expect_identical(crb$gene_lists, extras$gene_lists)

  md <- crb$getMetaData()
  expect_identical(
    colnames(md),
    c("cell_barcode", groups, "nUMI", "nGene", "Phase", rest)
  )
  expect_identical(md$cell_barcode, rownames(meta))
  sample_levels <- c(sort(unique(meta$sample)), "N/A")
  expect_identical(levels(md$sample), sample_levels)
  expect_identical(as.character(md$sample[2]), "N/A")
  expect_identical(levels(md$cluster), c("b", "a", "N/A"))
  expect_identical(as.character(md$cluster[3]), "N/A")
  expect_identical(levels(md$Phase), c("S", "G1", "G2M"))
  expect_identical(md$score, meta$score)
  expect_identical(crb$groups[["sample"]], sample_levels)
  expect_identical(crb$groups[["cluster"]], c("b", "a", "N/A"))
  expect_identical(crb$cell_cycle, "Phase")

  expect_identical(crb$trees, extras$trees)
  expect_identical(crb$most_expressed_genes, extras$most_expressed_genes)
  expect_identical(crb$marker_genes, extras$marker_genes)
  expect_identical(crb$enriched_pathways, extras$enriched_pathways)
  expect_identical(crb$trajectories, extras$trajectories["monocle2"])
  expect_identical(crb$extra_material$tables, extras$extra_material$tables)
  expect_identical(names(crb$extra_material$plots), "plot")
}

test_that("exportFromSCE builds the pinned Cerebro object", {
  sce <- pin_example("pbmc_SCE.rds")
  ## S4Vectors is not a declared dependency: coerce and use the slot.
  SummarizedExperiment::colData(sce) <- methods::as(
    pin_meta(as.data.frame(SummarizedExperiment::colData(sce))),
    "DataFrame"
  )
  sce@metadata <- pin_extras()
  meta <- as.data.frame(SummarizedExperiment::colData(sce))

  out <- pin_export(
    exportFromSCE,
    object = sce,
    groups = c("sample", "cluster")
  )
  crb <- out$crb

  expect_pinned_content(crb, meta, c("sample", "cluster"), "score")
  expect_identical(crb$parameters, pin_extras()$parameters)
  expect_identical(
    crb$expression,
    SummarizedExperiment::assay(sce, "logcounts")
  )
  expect_identical(crb$getExpressionBackend()$type, "embedded")
  expect_identical(crb$availableProjections(), "UMAP")
  expect_identical(
    crb$getProjection("UMAP"),
    as.data.frame(SingleCellExperiment::reducedDim(sce, "UMAP"))
  )
  expect_identical(
    out$warnings,
    paste0(
      "Warning: Skipping trajectories of method `slingshot`. At the moment, ",
      "only trajectories generated with Monocle 2 (`monocle2`) are supported."
    )
  )
  expect_true(any(grepl("Overview of Cerebro object:", out$messages)))
  expect_true(any(grepl("class: Cerebro", out$messages)))
  expect_true(any(grepl("] Done!", out$messages, fixed = TRUE)))
})

test_that("exportFromSeurat builds the pinned Cerebro object", {
  skip_if_not_installed("Seurat")
  seurat <- pin_example("pbmc_seurat.rds")
  seurat@meta.data <- pin_meta(seurat@meta.data)
  extras <- pin_extras()
  ## Seurat skips tables for groups that are not exported.
  extras$most_expressed_genes$unknown <- data.frame(gene = "G9")
  extras$mean_expression <- list(
    sample = data.frame(gene = "G1", expression = 1),
    unknown = data.frame(gene = "G9", expression = 9)
  )
  extras$enriched_pathways$enrichr$unknown <- data.frame(term = "T9")
  seurat@misc <- extras
  meta <- seurat@meta.data

  out <- pin_export(
    exportFromSeurat,
    object = seurat,
    groups = c("sample", "cluster"),
    main_group = "cluster",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA"
  )
  crb <- out$crb

  rest <- setdiff(
    names(meta),
    c("sample", "cluster", "nCount_RNA", "nFeature_RNA", "Phase")
  )
  expect_pinned_content(crb, meta, c("sample", "cluster"), rest)
  expect_identical(
    crb$parameters,
    c(pin_extras()$parameters, list(main_group = "cluster"))
  )
  expect_identical(crb$meta_data$nUMI, meta$nCount_RNA)
  expect_identical(crb$meta_data$nGene, meta$nFeature_RNA)
  expect_identical(crb$mean_expression, extras$mean_expression["sample"])
  expect_identical(
    unname(as.matrix(crb$expression)),
    unname(as.matrix(SeuratObject::LayerData(seurat, layer = "data")))
  )
  expect_identical(crb$availableProjections(), "umap")
  expect_identical(
    crb$getProjection("umap"),
    as.data.frame(seurat@reductions$umap@cell.embeddings)
  )
  expect_identical(
    out$warnings,
    paste0(
      "Warning: Skipping trajectories of method `slingshot`. At the moment, ",
      "only trajectories generated with Monocle 2 (`monocle2`) are supported."
    )
  )
  expect_true(any(grepl("Overview of Cerebro object:", out$messages)))
  expect_true(any(grepl("] Done!", out$messages, fixed = TRUE)))
})
