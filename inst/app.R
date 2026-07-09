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
  ## This bundled app ships several distinct demo data sets so the sidebar
  ## "Select dataset:" switcher is visible out of the box: switching changes
  ## the UMAP, the cell-type composition, and the conditional tabs (Immune
  ## Repertoire / Trajectory on the PBMC set, Spatial on the spatial sets).
  ## They are embedded-backend .crb files, so no h5 matrix is configured. The
  ## PBMC set (Full, T+B) is listed first and loaded by default
  ## (crb_pick_smallest_file = FALSE); it carries TCR + BCR and a monocle2
  ## B-cell trajectory, so it surfaces both the Immune Repertoire and Trajectory
  ## tabs (dynamically inserted by insertConditionalTab).
  "crb_file_to_load" = c(
    "PBMC - Full (T+B)" = "extdata/v1.4/demo_full_tcr_bcr.crb",
    ## REAL public spatial data, one per technology (down-sampled). The bracketed
    ## label states the platform. All four flow through the same platform-
    ## agnostic .getSpatialData extraction, spanning spot / bead / in-situ-imaging
    ## capture and Seurat v4 vs v5 objects: Slide-seq v2 is a
    ## Seurat v4 object, the others are v5. The demos deliberately show BOTH
    ## background-image paths: MERFISH and Xenium EMBED their genuine histology
    ## image (DAPI) inside the .crb, while Visium loads its H&E from an EXTERNAL
    ## file (demo_spatial_visium_he.png) via `spatial_images` below — a live
    ## example of that path, which also keeps the Visium .crb smaller. Slide-seq
    ## has no tissue photo by design (bead scatter is the complete spatial view).
    ## Rebuild with data-raw/build_spatial_demos.R.
    "Mouse brain (Visium)" = "extdata/v1.4/demo_spatial_visium.crb",
    "Mouse hippocampus (Slide-seq v2)" = "extdata/v1.4/demo_spatial_slideseq.crb",
    "Mouse ileum (MERFISH)" = "extdata/v1.4/demo_spatial_merfish.crb",
    "Mouse brain (Xenium)" = "extdata/v1.4/demo_spatial_xenium.crb"
  ),
  "crb_pick_smallest_file" = FALSE,
  ## Visium loads its real H&E background from an EXTERNAL image file (rather than
  ## embedding it in the .crb) — this exercises the `spatial_images` code path.
  ## The key must match the dropdown label above. The other image demos embed
  ## their image inside the .crb.
  ## Images default to NO flip; the Spatial tab's "Flip vertically/horizontally"
  ## checkboxes let the user align it if a given dataset needs it (for this Visium
  ## H&E that is a vertical flip, matching Seurat's own SpatialPlot).
  "spatial_images" = c(
    "Mouse brain (Visium)" = "extdata/v1.4/demo_spatial_visium_he.png"
  ),
  ## Default alignment of the Visium H&E overlay, found by eye in the Spatial
  ## tab and captured here so the demo opens pre-aligned. The user can still
  ## adjust or Reset (which returns to these values, not to identity).
  "spatial_images_offset_x" = c("Mouse brain (Visium)" = 600),
  "spatial_images_offset_y" = c(
    "Mouse brain (Visium)" = -750,
    "Mouse ileum (MERFISH)" = -0,
    "Mouse brain (Xenium)" = -10
  ),
  "spatial_images_scale_x" = c("Mouse brain (Visium)" = 1.55),
  "spatial_images_scale_y" = c("Mouse brain (Visium)" = 1.55),
  "spatial_images_flip_y" = c(
    "Mouse brain (Visium)" = TRUE,
    "Mouse brain (Xenium)" = TRUE
  ),
  "cerebro_root" = ".",
  "welcome_message" = custom_welcome_message,
  "overview_default_point_size" = 1,
  "gene_expression_default_point_size" = 2,
  ## Larger default spatial points so cell-type layering reads clearly against
  ## the histology background in the demo.
  "point_size" = list("spatial_projection_point_size" = 5),
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
