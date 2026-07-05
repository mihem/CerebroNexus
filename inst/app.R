##----------------------------------------------------------------------------##
## load packages
##----------------------------------------------------------------------------##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)

##----------------------------------------------------------------------------##
## set options
##----------------------------------------------------------------------------##
custom_welcome_message <- "Welcome to Cerebro! This is a custom welcome message. You can change it in the app options."
Cerebro.options <<- list(
  "mode" = "closed",
  ## This bundled app ships three distinct demo data sets so the sidebar
  ## "Select dataset:" switcher is visible out of the box: switching changes
  ## the UMAP, the cell-type composition, and the conditional tabs (Immune
  ## Repertoire on all three PBMC sets). They are embedded-backend .crb
  ## files, so no h5 matrix is configured. The richest data set (Full, T+B)
  ## is listed first and loaded by default (crb_pick_smallest_file = FALSE)
  ## so the app opens on its fullest state. The full T+B set additionally
  ## carries a monocle2 B-cell trajectory, which surfaces the Trajectory tab
  ## (dynamically inserted by insertConditionalTab).
  "crb_file_to_load" = c(
    "PBMC - Full (T+B)" = "extdata/v1.4/demo_full_tcr_bcr.crb",
    "PBMC - Healthy (T/NK)" = "extdata/v1.4/demo_healthy_t.crb",
    "PBMC - B-cell rich" = "extdata/v1.4/demo_bcell_rich.crb",
    "Xenium - Spatial" = "extdata/v1.4/demo_spatial.crb"
  ),
  "crb_pick_smallest_file" = FALSE,
  ## Synthetic (NOT real) tissue-like placeholder that demonstrates the Spatial
  ## tab's histology-background overlay. The name matches the crb_file_to_load
  ## key so the Spatial tab pairs it with the Xenium demo dataset.
  "spatial_images" = c(
    "Xenium - Spatial" = "extdata/v1.4/demo_spatial_histology.png"
  ),
  "cerebro_root" = ".",
  "welcome_message" = custom_welcome_message,
  "overview_default_point_size" = 1,
  "gene_expression_default_point_size" = 2,
  "overview_default_point_opacity" = 0.3,
  "gene_expression_default_point_opacity" = 0.5,
  "overview_default_percentage_cells_to_show" = 100,
  "gene_expression_default_percentage_cells_to_show" = 20,
  "projections_show_hover_info" = FALSE
)

options(shiny.maxRequestSize = 800 * 1024^2)

##----------------------------------------------------------------------------##
## load server and UI functions
##----------------------------------------------------------------------------##
source("shiny/v1.4/shiny_UI.R", local = TRUE)
source("shiny/v1.4/shiny_server.R", local = TRUE)

##----------------------------------------------------------------------------##
## launch app
##----------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)
