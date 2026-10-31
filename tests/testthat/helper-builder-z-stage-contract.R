builder_stage_contract_source_runtime <- function(local = parent.frame()) {
  for (name in getNamespaceExports("shiny")) {
    if (!exists(name, envir = local, inherits = FALSE)) {
      assign(name, getExportedValue("shiny", name), envir = local)
    }
  }
  builder_repo_source("app_bundle.R", local = local)
  for (file in c(
    "io.R",
    "profile.R",
    "state.R",
    "marker_import.R",
    "review.R",
    "ui/inspect_stage.R",
    "preview.R",
    "ui/core_stage.R",
    "ui/marker_import.R",
    "ui/enhance_stage.R",
    "ui/review_stage.R",
    "ui/build_status.R",
    "extras.R"
  )) {
    builder_repo_source(file, local = local)
  }
  invisible(local)
}

builder_stage_html <- function(value) {
  htmltools::renderTags(value)$html
}


builder_stage_review_identity <- function(
  ids = c("dataset-b", "dataset-a"),
  revisions = seq_along(ids)
) {
  checks <- paste0("configuration:", ids, ":revision:", revisions)
  names(checks) <- ids
  list(schema_version = 1L, dataset_order = ids, checks = checks)
}

builder_stage_frozen_plan <- function(make_app = TRUE) {
  review_identity <- builder_stage_review_identity()
  structure(
    list(
      revision = 17L,
      review_identity = review_identity,
      readiness = "ready",
      dataset_order = c("dataset-b", "dataset-a"),
      make_app = make_app,
      app_contract_version = if (make_app) 1L else NULL,
      items = list(
        list(
          id = "dataset-b",
          name = "Dataset B",
          filename = "01-dataset-b.crb",
          organism = "hg",
          assay = "RNA",
          layer = "data",
          groups = c("cluster", "sample"),
          included_groups = c("cluster", "sample"),
          reductions = c("umap", "pca"),
          included_projections = "umap",
          analyses = "marker_genes",
          analysis_dependency_graph = list(
            marker_genes = list(id = "marker_genes", dependencies = character())
          ),
          artifact_identity = list(
            schema_version = 2L,
            cells = c("cell-a", "cell-b"),
            features = c("gene-a", "gene-b", "gene-c"),
            group_levels = list(cluster = c("A", "B")),
            spatial_sections = "section-a"
          ),
          cell_count = 2L,
          gene_count = 3L,
          histology_coverage = list(
            sections = "section-a",
            with_histology = "section-a",
            missing_histology = character()
          ),
          estimated_runtime = "minutes",
          estimated_disk_bytes = 4096,
          tables = list(differential_expression = list()),
          images = list(section_a = list()),
          colors = list(cluster = c(A = "#111111")),
          color_custom_count = 1L,
          nUMI = "nCount_RNA",
          nGene = "nFeature_RNA",
          default_group = "cluster",
          default_projection = "umap",
          viewer_page_expectations = list(
            always = builder_viewer_page_catalog()$always,
            conditional = builder_viewer_page_catalog()$conditional,
            visible_conditional = c("marker_genes", "extra_material"),
            hidden_conditional = c("spatial", "trajectory")
          ),
          expression_backend = "h5",
          sidecars = "01-dataset-b.h5",
          metadata_policy = list(
            included = c("cluster", "sample"),
            excluded = "patient_name"
          ),
          nomenclature = "HGNC",
          readiness = "ready",
          acknowledgements = "Marker genes replace the existing method"
        ),
        list(
          id = "dataset-a",
          name = "Dataset A",
          filename = "02-dataset-a.crb",
          organism = "mm",
          groups = "cell_type",
          reductions = "umap",
          analyses = character(),
          colors = list(cell_type = c(T = "#222222")),
          expression_backend = "embedded",
          sidecars = character(),
          metadata_policy = list(included = "cell_type")
        )
      ),
      manifest = list(immune_repertoire = list(status = "valid")),
      viewer_page_expectations = list(
        list(pages = "overview"),
        list(pages = "overview")
      ),
      viewer_bundle_assets = c("spatial-assets/image.png"),
      private_assets = c("private-data/01-dataset-b.crb"),
      acknowledgements = list("Filtered orphan repertoire rows"),
      app_options = list(
        enabled = make_app,
        show_upload_ui = FALSE,
        initial_dataset = "dataset-b",
        initial_dataset_mode = "automatic",
        welcome_message = "Welcome, lab team!",
        point_size = list(overview_projection_point_size = 4),
        variable_to_compare = TRUE,
        host = "127.0.0.1",
        port = 8080L,
        max_request_size = 8000,
        display_mode = "normal",
        launch_browser = TRUE
      ),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      ),
      output_release = list(
        directory = "/private/host/output",
        overwrite = FALSE,
        replacement_policy = "preserve_existing",
        estimated_runtime = "minutes",
        estimated_disk_bytes = 4096,
        targets = c(
          "/private/host/output/01-dataset-b.crb",
          "/private/host/output/01-dataset-b.h5",
          "/private/host/output/02-dataset-a.crb",
          "/private/host/output/cerebro_app"
        )
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_stage_review_snapshot <- function(
  plan = builder_stage_frozen_plan()
) {
  structure(
    list(
      revision = as.character(plan$revision),
      identity = plan$review_identity,
      readiness = "ready",
      error = NULL,
      dataset_order = plan$review_identity$dataset_order,
      items = plan$items,
      make_app = isTRUE(plan$make_app),
      viewer_page_expectations = lapply(
        plan$items,
        function(item) item$viewer_page_expectations %||% list()
      ),
      output_release = list(
        estimated_runtime = plan$output_release$estimated_runtime %||% NULL,
        estimated_disk_bytes = plan$output_release$estimated_disk_bytes %||% 0
      ),
      warnings = plan$warnings %||% list()
    ),
    class = c("builder_review_snapshot", "list")
  )
}
