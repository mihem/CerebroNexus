.generated_app_fixture_runtime <- new.env(parent = emptyenv())

generated_app_fixture_source_runtime <- function() {
  target <- environment(generated_app_fixture_source_runtime)
  ## Builder test files intentionally source and mock many runtime functions in
  ## the shared test environment. A single inherited symbol is not proof that
  ## this fixture's complete runtime was loaded; in a full-suite run that made
  ## generated-App fixtures reuse a partially mocked Builder. Own an explicit
  ## sentinel and source the canonical runtime once for this fixture family.
  if (!isTRUE(.generated_app_fixture_runtime$loaded)) {
    builder_e2e_source_runtime(target)
    .generated_app_fixture_runtime$loaded <- TRUE
  }
  invisible(target)
}

generated_app_fixture_pages <- function(conditional = character()) {
  always <- c(
    "data_info",
    "coordinated_views",
    "groups",
    "gene_expression",
    "gene_id_conversion",
    "color_management",
    "about"
  )
  all_conditional <- c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "immune_repertoire",
    "trajectory",
    "hla_tcr_motifs"
  )
  list(
    visible = c(always, conditional),
    hidden = setdiff(all_conditional, conditional)
  )
}

.generated_app_fixture_gene_ids <- function(n_genes, organism = "hg") {
  prefix <- if (identical(organism, "hg")) {
    c("MT-CO1", "MT-ND1", "MT-CYB", "RPS3", "RPL4")
  } else {
    c("mt-CO1", "mt-ND1", "mt-CYB", "Rps3", "Rpl4")
  }
  c(prefix, sprintf("GENE%03d", seq_len(n_genes - length(prefix))))
}

.generated_app_fixture_expression_object <- function(
  prefix,
  n_cells,
  n_genes,
  cluster_levels,
  sample_levels,
  organism = "hg"
) {
  .builder_test_with_seed(4101L + n_cells + n_genes, {
    cells <- sprintf("%s_cell_%02d", prefix, seq_len(n_cells))
    genes <- .generated_app_fixture_gene_ids(n_genes, organism)
    row_index <- matrix(seq_len(n_genes), nrow = n_genes, ncol = n_cells)
    column_index <- matrix(
      rep(seq_len(n_cells), each = n_genes),
      nrow = n_genes,
      ncol = n_cells
    )
    dense <- (row_index * 3L + column_index * 5L) %% 11L
    dense[dense < 4L] <- 0L
    dense[1L, ] <- (seq_len(n_cells) %% 5L) + 1L
    dense[, 1L] <- (seq_len(n_genes) %% 7L) + 1L
    counts <- Matrix::Matrix(
      dense,
      sparse = TRUE,
      dimnames = list(genes, cells)
    )

    clusters <- factor(
      rep(cluster_levels, length.out = n_cells),
      levels = cluster_levels
    )
    samples <- factor(
      rep(sample_levels, each = ceiling(n_cells / length(sample_levels)))[
        seq_len(n_cells)
      ],
      levels = sample_levels
    )
    treatment <- factor(
      rep(c("control", "treated"), each = ceiling(n_cells / 2L))[
        seq_len(n_cells)
      ],
      levels = c("control", "treated")
    )
    metadata <- data.frame(
      seurat_clusters = clusters,
      sample = samples,
      treatment = treatment,
      percent.mt = round(seq(1.5, 9.5, length.out = n_cells), 3),
      qc_missing = round(seq(0.05, 0.95, length.out = n_cells), 3),
      row.names = cells,
      check.names = FALSE
    )
    metadata$qc_missing[c(2L, n_cells - 1L)] <- NA_real_

    object <- SeuratObject::CreateSeuratObject(
      counts = counts,
      assay = "RNA",
      project = paste0(prefix, "_fixture"),
      meta.data = metadata
    )
    object <- Seurat::NormalizeData(object, verbose = FALSE)

    pca <- cbind(
      PC_1 = seq(-2, 2, length.out = n_cells),
      PC_2 = seq(1.5, -1.5, length.out = n_cells),
      PC_3 = sin(seq(0, 2 * pi, length.out = n_cells))
    )
    umap <- cbind(
      UMAP_1 = cos(seq(0, 2 * pi, length.out = n_cells)),
      UMAP_2 = sin(seq(0, 2 * pi, length.out = n_cells))
    )
    tsne <- cbind(
      TSNE_1 = seq(-3, 3, length.out = n_cells),
      TSNE_2 = rep(c(-1, 1), length.out = n_cells)
    )
    rownames(pca) <- rownames(umap) <- rownames(tsne) <- cells
    object[["pca"]] <- SeuratObject::CreateDimReducObject(
      embeddings = pca,
      key = "PC_",
      assay = "RNA"
    )
    object[["umap"]] <- SeuratObject::CreateDimReducObject(
      embeddings = umap,
      key = "UMAP_",
      assay = "RNA"
    )
    object[["tsne"]] <- SeuratObject::CreateDimReducObject(
      embeddings = tsne,
      key = "TSNE_",
      assay = "RNA"
    )
    fixed_time <- as.POSIXct("2026-08-07 00:00:00", tz = "UTC")
    for (name in names(object@commands)) {
      object@commands[[name]]@time.stamp <- fixed_time
    }
    object
  })
}

.generated_app_fixture_projection_contract <- function(object) {
  stats::setNames(
    lapply(SeuratObject::Reductions(object), function(reduction) {
      coordinates <- SeuratObject::Embeddings(object, reduction)
      coordinates[c(1L, nrow(coordinates)), , drop = FALSE]
    }),
    SeuratObject::Reductions(object)
  )
}

.generated_app_fixture_counts <- function(object, groups) {
  stats::setNames(
    lapply(groups, function(group) {
      unclass(table(object@meta.data[[group]]))
    }),
    groups
  )
}

.generated_app_fixture_contract <- function(
  object,
  dataset_name,
  organism,
  groups,
  default_group,
  default_projection,
  palettes,
  projections = SeuratObject::Reductions(object),
  conditional_pages = character(),
  optional_payloads = character(),
  spatial_sections = character(),
  image_alignment = list(),
  output_file
) {
  pages <- generated_app_fixture_pages(conditional_pages)
  groups <- c(default_group, setdiff(groups, default_group))
  projections <- c(
    default_projection,
    setdiff(projections, default_projection)
  )
  app_settings <- list(
    initial_dataset = "e2e-basic",
    show_upload_ui = FALSE,
    welcome_message = "Generated App E2E",
    point_size = list(overview_projection_point_size = 6),
    variable_to_compare = FALSE
  )
  list(
    dataset_name = dataset_name,
    organism = organism,
    n_cells = as.integer(ncol(object)),
    n_genes = as.integer(nrow(object)),
    cell_ids = colnames(object),
    gene_ids = rownames(object),
    groups = groups,
    group_levels = stats::setNames(
      lapply(groups, function(group) {
        levels(object@meta.data[[group]])
      }),
      groups
    ),
    group_counts = .generated_app_fixture_counts(object, groups),
    projections = projections,
    projection_coordinates = .generated_app_fixture_projection_contract(object)[
      projections
    ],
    default_group = default_group,
    default_projection = default_projection,
    palettes = palettes,
    visible_pages = pages$visible,
    hidden_pages = pages$hidden,
    optional_payloads = optional_payloads,
    spatial_sections = spatial_sections,
    image_alignment = image_alignment,
    output_files = list(crb = output_file),
    app_settings = app_settings
  )
}

.generated_app_fixture_settings <- function(
  name,
  organism,
  groups,
  reductions,
  default_group,
  default_projection,
  color_overrides = list()
) {
  list(
    name = name,
    organism = organism,
    groups = groups,
    reductions = reductions,
    default_group = default_group,
    default_projection = default_projection,
    initial_projections = default_projection,
    overview_point_size = 6,
    expression_backend = "embedded",
    palette = "cerebro",
    color_overrides = color_overrides
  )
}

.generated_app_fixture_basic <- function() {
  object <- .generated_app_fixture_expression_object(
    prefix = "basic",
    n_cells = 36L,
    n_genes = 60L,
    cluster_levels = c("Alpha", "Beta", "Gamma"),
    sample_levels = c("sample_a", "sample_b"),
    organism = "hg"
  )
  colors <- c(
    Alpha = "#A63D14",
    Beta = "#D97706",
    Gamma = "#F2B84B"
  )
  groups <- c("seurat_clusters", "sample", "treatment")
  list(
    object = object,
    attachments = list(),
    builder_settings = .generated_app_fixture_settings(
      "Basic expression",
      "hg",
      groups,
      c("umap", "tsne"),
      "seurat_clusters",
      "umap",
      color_overrides = list(seurat_clusters = colors)
    ),
    expected = .generated_app_fixture_contract(
      object,
      "Basic expression",
      "hg",
      groups,
      "seurat_clusters",
      "umap",
      palettes = list(seurat_clusters = colors),
      projections = c("umap", "tsne"),
      output_file = "01-basic-expression-e2e-basic.crb"
    )
  )
}

.generated_app_fixture_analysis <- function() {
  object <- .generated_app_fixture_expression_object(
    prefix = "analysis",
    n_cells = 28L,
    n_genes = 52L,
    cluster_levels = c("Delta", "Epsilon"),
    sample_levels = c("analysis_a", "analysis_b"),
    organism = "hg"
  )
  object@misc$marker_genes <- list(
    cerebro_seurat = list(
      seurat_clusters = data.frame(
        seurat_clusters = c("Delta", "Epsilon", "Delta"),
        gene = c("GENE001", "GENE002", "GENE003"),
        p_val = c(0.001, 0.002, 0.003),
        avg_log2FC = c(2.4, 1.9, 1.2),
        pct.1 = c(0.9, 0.85, 0.7),
        pct.2 = c(0.2, 0.3, 0.4),
        p_val_adj = c(0.01, 0.02, 0.03),
        on_cell_surface = c(TRUE, FALSE, TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  )
  object@misc$most_expressed_genes <- list(
    seurat_clusters = data.frame(
      seurat_clusters = c("Delta", "Epsilon", "Delta"),
      gene = c("GENE004", "GENE005", "GENE006"),
      pct = c(95, 90, 82),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
  object@misc$mean_expression <- list(
    seurat_clusters = data.frame(
      seurat_clusters = c("Delta", "Epsilon", "Delta"),
      gene = c("GENE004", "GENE005", "GENE006"),
      mean_expr = c(3.5, 2.75, 1.5),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
  object@misc$enriched_pathways <- list(
    offline = list(
      seurat_clusters = data.frame(
        seurat_clusters = c("Delta", "Epsilon", "Delta"),
        Term = c("Pathway A", "Pathway B", "Pathway C"),
        Combined.Score = c(9.5, 8.25, 7.0),
        adjusted_p_value = c(0.01, 0.02, 0.03),
        gene = c("GENE001", "GENE002", "GENE003"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  )
  trajectory_cells <- colnames(object)[c(1L, 8L)]
  object@misc$trajectories <- list(
    monocle2 = list(
      analysis_lineage = list(
        meta = data.frame(
          DR_1 = c(0.1, 0.9),
          DR_2 = c(0.2, 0.8),
          pseudotime = c(0, 1),
          state = factor(c("start", "end"), levels = c("start", "end")),
          row.names = trajectory_cells,
          check.names = FALSE
        ),
        edges = data.frame(
          source = "start",
          target = "end",
          weight = 1,
          source_dim_1 = 0.1,
          source_dim_2 = 0.2,
          target_dim_1 = 0.9,
          target_dim_2 = 0.8,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    )
  )
  object@misc$extra_material <- list(
    tables = list(
      fixture_summary = data.frame(
        metric = c("cells", "genes", "source"),
        value = c("28", "52", "offline fixture"),
        stringsAsFactors = FALSE
      )
    )
  )
  groups <- c("seurat_clusters", "sample", "treatment")
  colors <- c(Delta = "#7C2D12", Epsilon = "#F59E0B")
  optional <- c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectory",
    "extra_material"
  )
  conditional <- c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "extra_material",
    "trajectory"
  )
  list(
    object = object,
    attachments = list(),
    builder_settings = utils::modifyList(
      .generated_app_fixture_settings(
        "Offline analysis",
        "hg",
        groups,
        c("umap", "tsne"),
        "seurat_clusters",
        "tsne",
        color_overrides = list(seurat_clusters = colors)
      ),
      list(
        included_trajectories = list(monocle2 = "analysis_lineage"),
        default_trajectory = list(
          method = "monocle2",
          name = "analysis_lineage"
        )
      )
    ),
    expected = .generated_app_fixture_contract(
      object,
      "Offline analysis",
      "hg",
      groups,
      "seurat_clusters",
      "tsne",
      palettes = list(seurat_clusters = colors),
      projections = c("umap", "tsne"),
      conditional_pages = conditional,
      optional_payloads = optional,
      output_file = "02-offline-analysis-e2e-analysis.crb"
    )
  )
}

.generated_app_fixture_static_record <- function(id) {
  generated_app_fixture_source_runtime()
  builder_example_catalog()[[id]]
}

.generated_app_fixture_spatial <- function() {
  generated_app_fixture_source_runtime()
  object <- .builder_test_with_seed(
    2026L,
    .builder_fixture_stabilize(.builder_fixture_spatial())
  )
  root <- dirname(
    .generated_app_fixture_static_record("complete_viewer_data")$serialized_path
  )
  alignment <- list(
    section_a = list(
      bounds = list(xmin = 10, xmax = 106, ymin = 20, ymax = 92),
      dimensions = c(height = 240L, width = 320L)
    ),
    section_b = list(
      bounds = list(xmin = 250, xmax = 330, ymin = 40, ymax = 104),
      dimensions = c(height = 220L, width = 360L)
    )
  )
  attachments <- list(
    section_a = c(
      list(path = file.path(root, "section_a_1_he.png")),
      alignment$section_a
    ),
    section_b = c(
      list(path = file.path(root, "section_b_1_he.png")),
      alignment$section_b
    )
  )
  groups <- c("cell_type", "condition")
  colors <- c(control = "#B45309", treated = "#FBBF24")
  list(
    object = object,
    attachments = attachments,
    builder_settings = .generated_app_fixture_settings(
      "Spatial sections",
      "mm",
      groups,
      c("umap", "tsne"),
      "condition",
      "tsne",
      color_overrides = list(condition = colors)
    ),
    expected = .generated_app_fixture_contract(
      object,
      "Spatial sections",
      "mm",
      groups,
      "condition",
      "tsne",
      palettes = list(condition = colors),
      conditional_pages = character(),
      optional_payloads = "spatial",
      spatial_sections = c("section_a", "section_b"),
      image_alignment = alignment,
      output_file = "03-spatial-sections-e2e-spatial.crb"
    )
  )
}

.generated_app_fixture_immune <- function(id, name, output_file) {
  is_tcr <- identical(id, "immune_tcr_hla")
  mode <- if (is_tcr) "tcr_hla" else "bcr_only"
  generated_app_fixture_source_runtime()
  object <- .builder_test_with_seed(
    2026L,
    .builder_fixture_stabilize(.builder_fixture_immune(mode))
  )
  if (is_tcr) {
    umap <- SeuratObject::Embeddings(object, "umap")
    tsne <- cbind(TSNE_1 = umap[, 2L] + 0.25, TSNE_2 = -umap[, 1L])
    object[["tsne"]] <- SeuratObject::CreateDimReducObject(
      embeddings = tsne,
      key = "TSNE_",
      assay = SeuratObject::DefaultAssay(object)
    )
  }
  projections <- if (is_tcr) c("umap", "tsne") else "umap"
  conditional <- if (is_tcr) {
    c("immune_repertoire", "hla_tcr_motifs")
  } else {
    "immune_repertoire"
  }
  optional <- if (is_tcr) {
    c("immune_repertoire", "hla")
  } else {
    "immune_repertoire"
  }
  groups <- c("sample", "cell_type", "condition")
  colors <- c(donor1 = "#92400E", donor2 = "#F59E0B")
  list(
    object = object,
    attachments = list(),
    builder_settings = .generated_app_fixture_settings(
      name,
      "mm",
      groups,
      projections,
      "sample",
      "umap",
      color_overrides = list(sample = colors)
    ),
    expected = .generated_app_fixture_contract(
      object,
      name,
      "mm",
      groups,
      "sample",
      "umap",
      palettes = list(sample = colors),
      projections = projections,
      conditional_pages = conditional,
      optional_payloads = optional,
      output_file = output_file
    )
  )
}

.generated_app_fixture_trekker <- function() {
  record <- .generated_app_fixture_static_record("complete_viewer_data")
  object <- record$make()$object
  trekker <- object@misc$trekker
  object@misc <- list(trekker = trekker)
  object@images <- list()
  groups <- c("patient_id", "cell_type", "region")
  colors <- c(
    epithelial_zone = "#A16207",
    stroma = "#FACC15",
    immune_zone = "#CA8A04"
  )
  list(
    object = object,
    attachments = list(),
    builder_settings = .generated_app_fixture_settings(
      "Trekker map",
      "hg",
      groups,
      "umap",
      "region",
      "umap",
      color_overrides = list(region = colors)
    ),
    expected = .generated_app_fixture_contract(
      object,
      "Trekker map",
      "hg",
      groups,
      "region",
      "umap",
      palettes = list(region = colors),
      projections = "umap",
      conditional_pages = character(),
      optional_payloads = "trekker",
      output_file = "06-trekker-map-e2e-trekker.crb"
    )
  )
}

generated_app_fixture_matrix <- function() {
  fixtures <- list(
    basic = .generated_app_fixture_basic(),
    analysis = .generated_app_fixture_analysis(),
    spatial = .generated_app_fixture_spatial(),
    immune_tcr_hla = .generated_app_fixture_immune(
      "immune_tcr_hla",
      "Immune TCR HLA",
      "04-immune-tcr-hla-e2e-immune-tcr-hla.crb"
    ),
    immune_bcr = .generated_app_fixture_immune(
      "immune_bcr_only",
      "Immune BCR",
      "05-immune-bcr-e2e-immune-bcr.crb"
    ),
    trekker = .generated_app_fixture_trekker()
  )
  fixtures
}
