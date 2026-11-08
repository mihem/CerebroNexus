.builder_repo_source_runtime <- new.env(parent = baseenv())
.builder_repo_prerequisite <- testthat::test_path(
  "..",
  "..",
  "inst",
  "builder",
  "prerequisite.R"
)
if (!file.exists(.builder_repo_prerequisite)) {
  .builder_repo_prerequisite <- system.file(
    file.path("builder", "prerequisite.R"),
    package = "CerebroNexus"
  )
}
base::source(
  .builder_repo_prerequisite,
  local = .builder_repo_source_runtime,
  encoding = "UTF-8"
)
.builder_repo_source_utf8 <- .builder_repo_source_runtime$builder_source_utf8
rm(.builder_repo_source_runtime, .builder_repo_prerequisite)

builder_repo_source <- function(file, local = parent.frame()) {
  path <- testthat::test_path("..", "..", "inst", "builder", file)
  if (!file.exists(path)) {
    path <- system.file(file.path("builder", file), package = "CerebroNexus")
  }
  .builder_repo_source_utf8(path, envir = local, chdir = TRUE)
}

builder_minimal_entry <- function(id = "ds1", name = "PBMC") {
  list(
    id = id,
    profile = list(nUMI = "nCount_RNA", nGene = "nFeature_RNA"),
    levels = list(cluster = c("A", "B")),
    settings = list(
      name = name,
      organism = "hg",
      assay = "RNA",
      layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      groups = "cluster",
      reductions = "umap",
      analyses = character(),
      tables = list(),
      images = list(),
      palette = "cerebro",
      color_overrides = list()
    )
  )
}
