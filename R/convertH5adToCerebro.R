#' Pick the first candidate column that exists in obs, else the fallback name
#' (which \code{convertSCEToCerebro()} then computes from the counts).
#' @keywords internal
#' @noRd
.h5adQcColumn <- function(available, candidates, fallback) {
  hit <- candidates[candidates %in% available]
  if (length(hit)) hit[[1L]] else fallback
}

#' Convert an AnnData (h5ad) file to a Cerebro file
#'
#' @description
#' One-step route from a \code{.h5ad} file, the container of the Python scverse
#' ecosystem (AnnData, scanpy), to a \code{.crb} file that
#' \code{\link{createShinyApp}} can serve. The file is read natively from HDF5
#' with \code{\link{readH5adAsSCE}}, so neither Python nor
#' \pkg{zellkonverter} is needed, and the resulting
#' \code{SingleCellExperiment} goes through \code{\link{convertSCEToCerebro}},
#' so every option of that route applies here too.
#'
#' @param h5ad_file Path to a \code{.h5ad} file (any anndata version, including
#'   the pre-0.7 layout), or to a MuData \code{.h5mu} file together with
#'   \code{modality}.
#' @param result_dir Directory the \code{.crb} is written to (created if
#'   missing). The file is named \code{cerebro_<basename>.crb}.
#' @param assay Which matrix to export: \code{"X"} (default) or the name of an
#'   entry of \code{layers}, e.g. \code{"counts"}. With \code{use_raw = TRUE}
#'   the matrix is \code{raw/X} and this must stay \code{"X"}.
#' @param use_raw Use \code{raw/X} (and \code{raw/var}) as the expression
#'   matrix; see \code{\link{readH5adAsSCE}}.
#' @param gene_names \code{var} column to use as gene names (default: the index,
#'   or a symbol column when the index is Ensembl IDs); see
#'   \code{\link{readH5adAsSCE}}.
#' @param modality For a MuData file, the modality to convert (e.g.
#'   \code{"gex"}); see \code{\link{readH5adAsSCE}}.
#' @param airr Immune receptor (TCR/BCR) data to import: \code{"auto"}
#'   (default) uses the MuData modality named \code{airr}, or
#'   \code{obsm["airr"]} of a plain h5ad, if the file has one, and imports
#'   nothing otherwise. Give a name to use another modality (or \code{obsm}
#'   entry), or \code{FALSE} to skip receptor data. See
#'   \code{\link{readH5adAsSCE}}.
#' @param airr_sample \code{obs} column naming the sample of each cell, which
#'   keys the immune repertoire. By default \code{orig.ident}, \code{sample},
#'   \code{Sample} or \code{sample_id} is used if present, else one sample.
#' @param nUMI,nGene \code{obs} columns holding transcripts and expressed genes
#'   per cell. By default the usual scanpy/Seurat names are looked for
#'   (\code{total_counts}, \code{n_counts}, \code{nCount_RNA}, \code{nUMI} and
#'   \code{n_genes_by_counts}, \code{n_genes}, \code{nFeature_RNA},
#'   \code{nGene}); when none exists the values are computed from the
#'   \code{counts} layer (or from \code{assay}).
#' @param groups Character vector of \code{obs} columns to use as grouping
#'   variables. Required; an error lists the usable columns when omitted.
#' @param extra_modalities For an \code{.h5mu} file, other modalities to add to
#'   the expression data as extra features, for example \code{"prot"}. Their
#'   features are named \code{<modality>:<feature>}. See
#'   \code{\link{convertSCEToCerebro}}, argument \code{extra_features}.
#' @param experiment_name,organism,groups_naming,max_group_levels,add_all_meta_data,use_delayed_array,expression_matrix_mode,codec,viewer_binary,viewer_binary_threshold,verbose,cell_cycle,main_group,projections,add_most_expressed_genes,most_expressed_genes
#'   As in \code{\link{convertSCEToCerebro}}. \code{experiment_name} defaults to
#'   the file's base name.
#' @param spatial_split_by \code{obs} column dividing \code{obsm["spatial"]} into
#'   one section per level (e.g. the tissue sections of a MERFISH data set); see
#'   \code{\link{exportFromSCE}}.
#' @param spatial_images Which Visium tissue images to embed (\code{"auto"},
#'   a vector of labels, or \code{FALSE}); see \code{\link{readH5adAsSCE}}.
#' @param marker_file,marker_method Optional marker table, as in
#'   \code{\link{convertSCEToCerebro}}. When given, it replaces the marker
#'   results taken from scanpy.
#'
#' @return The path of the written \code{.crb}, invisibly.
#'
#' @details
#' Category colours (\code{uns["<column>_colors"]}) are kept as the groups' default
#' colours in the Viewer. Visium tissue images are embedded and drawn behind the
#' spots; when a file has the images of several libraries (\code{obs["library_id"]})
#' each library becomes one section with its own image. The Viewer's Spatial plot
#' has the y axis pointing up, so a Visium image, stored with row 0 on top, needs
#' the vertical flip to line up with the spots: build the app with
#' \code{createShinyApp(..., spatial_image_settings = list(<dataset label> =
#' list(spatial = list(hires = list(flip_y = TRUE)))))}. As for Seurat Visium
#' objects, the coordinates are the file's, so the whole picture is then the
#' mirror image (top to bottom) of the slide as drawn by scanpy.
#'
#' scanpy's \code{rank_genes_groups} results, if present, are exported as
#' marker genes. Embeddings in \code{obsm} (\code{X_umap}, \code{X_tsne},
#' \code{X_pca}, ...) become the projections; PCA is dropped when a non-PCA
#' embedding exists, as on the other routes. Receptor chains written by scirpy
#' (\code{obsm["airr"]}) fill the Immune repertoire page.
#'
#' @examplesIf requireNamespace("rhdf5") && requireNamespace("SingleCellExperiment")
#' \donttest{
#' h5ad <- file.path(tempdir(), "example.h5ad")
#' exportToH5ad(
#'   system.file("extdata/examples/example.crb", package = "CerebroNexus"),
#'   h5ad
#' )
#' convertH5adToCerebro(
#'   h5ad_file = h5ad,
#'   result_dir = file.path(tempdir(), "output"),
#'   groups = c("seurat_clusters", "sample"),
#'   expression_matrix_mode = "h5"
#' )
#' }
#'
#' @seealso \code{\link{readH5adAsSCE}}, \code{\link{convertSCEToCerebro}}
#'
#' @export
convertH5adToCerebro <- function(
  h5ad_file,
  result_dir,
  assay = "X",
  use_raw = FALSE,
  modality = NULL,
  gene_names = NULL,
  airr = "auto",
  airr_sample = NULL,
  extra_modalities = NULL,
  experiment_name = NULL,
  organism = "Human",
  groups = NULL,
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = NULL,
  nGene = NULL,
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = TRUE,
  cell_cycle = NULL,
  main_group = NULL,
  projections = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  spatial_split_by = NULL,
  spatial_images = "auto"
) {
  expression_matrix_mode <- match.arg(expression_matrix_mode)
  codec <- match.arg(codec)
  viewer_binary <- match.arg(viewer_binary)
  if (!is.character(h5ad_file) || length(h5ad_file) != 1L) {
    stop("`h5ad_file` must be one file path.", call. = FALSE)
  }
  if (.zarrIsStore(h5ad_file) && length(.sdTables(h5ad_file))) {
    stop(
      "`",
      h5ad_file,
      "` is a SpatialData store; use convertSpatialDataToCerebro().",
      call. = FALSE
    )
  }
  if (
    !tolower(tools::file_ext(h5ad_file)) %in% c("h5ad", "h5mu") &&
      !.zarrIsStore(h5ad_file)
  ) {
    stop(
      "`h5ad_file` must be a .h5ad (or MuData .h5mu) file, or an AnnData .zarr store: ",
      h5ad_file,
      call. = FALSE
    )
  }

  sce <- readH5adAsSCE(
    h5ad_file,
    use_raw = use_raw,
    modality = modality,
    gene_names = gene_names,
    airr = airr,
    airr_sample = airr_sample,
    spatial_images = spatial_images,
    extra_modalities = extra_modalities,
    verbose = verbose
  )

  obs_columns <- names(SummarizedExperiment::colData(sce))
  if (is.null(nUMI)) {
    nUMI <- .h5adQcColumn(
      obs_columns,
      c("total_counts", "n_counts", "nCount_RNA", "nUMI"),
      "nUMI"
    )
  }
  if (is.null(nGene)) {
    nGene <- .h5adQcColumn(
      obs_columns,
      c("n_genes_by_counts", "n_genes", "nFeature_RNA", "nGene"),
      "nGene"
    )
  }

  convertSCEToCerebro(
    sce_file = sce,
    result_dir = result_dir,
    assay = assay,
    extra_features = if (length(extra_modalities)) TRUE else NULL,
    experiment_name = experiment_name,
    organism = organism,
    groups = groups,
    groups_naming = groups_naming,
    max_group_levels = max_group_levels,
    nUMI = nUMI,
    nGene = nGene,
    add_all_meta_data = add_all_meta_data,
    use_delayed_array = use_delayed_array,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    viewer_binary = viewer_binary,
    viewer_binary_threshold = viewer_binary_threshold,
    verbose = verbose,
    cell_cycle = cell_cycle,
    main_group = main_group,
    projections = projections,
    marker_file = marker_file,
    marker_method = marker_method,
    add_most_expressed_genes = add_most_expressed_genes,
    most_expressed_genes = most_expressed_genes,
    spatial_split_by = spatial_split_by,
    .source_name = h5ad_file
  )
}
