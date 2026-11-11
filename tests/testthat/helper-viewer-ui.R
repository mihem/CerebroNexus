# Load the real UI constructors without starting the full dashboard or reading
# datasets. Parsing here isolates definitions; assertions inspect their output.
viewer_ui_test_env <- function() {
  scope <- new.env(parent = asNamespace("shiny"))
  scope$tabItem <- shinydashboard::tabItem
  scope$box <- shinydashboard::box
  scope$Cerebro.options <- list(cerebro_root = viewer_app_test_path())
  for (expression in parse(viewer_test_path("shiny_UI.R"))) {
    if (
      is.call(expression) &&
        as.character(expression[[1L]])[1] %in% c("<-", "=") &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    ) {
      eval(expression, scope)
    }
  }
  ## The script of the Spatial page, which shiny_UI.R makes outside the page
  ## files (it also defines the server's shinyjs functions).
  scope$viewer_spatial_page_script <- NULL
  scope
}

viewer_ui_test_pages <- function() {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("overview", "UI_projection.R"), scope)
  pages <- list(overview = scope$overviewProjectionUI())
  for (page in c(
    "gene_expression",
    "spatial",
    "trajectory",
    "trekker",
    "immune_repertoire",
    "hla_tcr_motifs",
    "coordinated_views"
  )) {
    sys.source(viewer_test_path(page, "UI.R"), scope)
    pages[[page]] <- scope[[paste0("tab_", page)]]
  }
  pages
}
