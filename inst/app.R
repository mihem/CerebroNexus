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
custom_welcome_message <- "Welcome to CerebroNexus! This is a custom welcome message. You can change it in the app options."
Cerebro.options <<- list(
  "mode" = "closed",
  ## Keep the source demo runnable directly from inst/ without requiring an
  ## installed CerebroNexus package. Exported apps receive this value in
  ## cerebro_config.rds when createShinyApp() builds them.
  "cerebro_version" = "4.3.6",
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
    "PBMC - Full (T+B)" = "extdata/examples/demo_full_tcr_bcr.crb",
    ## REAL public spatial data, one per technology (down-sampled). The bracketed
    ## label states the platform. All four flow through the same platform-
    ## agnostic .getSpatialData extraction, spanning spot / bead / in-situ-imaging
    ## capture and Seurat v4 vs v5 objects: Slide-seq v2 is a
    ## Seurat v4 object, the others are v5. The demos deliberately show BOTH
    ## background-image paths: MERFISH embeds its DAPI in the .crb; Visium and
    ## Xenium load external files via `spatial_images` below. Slide-seq has no
    ## tissue photo by design (bead scatter is the complete spatial view).
    ## Rebuild with data-raw/build_spatial_demos.R.
    "Mouse brain (Visium)" = "extdata/examples/demo_spatial_visium.crb",
    "Mouse hippocampus (Slide-seq v2)" = "extdata/examples/demo_spatial_slideseq.crb",
    "Mouse ileum (MERFISH)" = "extdata/examples/demo_spatial_merfish.crb",
    "Mouse brain (Xenium)" = "extdata/examples/demo_spatial_xenium.crb",
    ## REAL Trekker single-cell spatial-mapping output (Curio / Takara), down-
    ## sampled from the smallest official bundle (Mouse_Brain_TrekkerU_C). Unlike
    ## the spatial demos above it drives the bespoke **Trekker** tab, not the
    ## generic Spatial tab: real single nuclei x whole transcriptome, positions
    ## inferred from bead spatial barcodes, no histology image. Carries a
    ## `trekker` slot (three coordinate orientations, positioning QC, upstream
    ## Moran's I, embedded per-nucleus positioning-evidence images).
    ## Rebuild with data-raw/build_trekker_demo.R (see data-raw/trekker.md).
    "Mouse brain (Trekker)" = "extdata/examples/demo_trekker.crb",
    ## The HLA & TCR demo: REAL single cells with REAL paired TCR, from 10x's
    ## dextramer cohort. The repertoire is ANTIGEN-SELECTED (cells were sorted
    ## for binding a pMHC dextramer), which is precisely why its motif network is
    ## legible where an unselected repertoire's is not -- 12,000 cells give 169
    ## TRB nodes in 39 motifs on measured sequences. Donor genotypes are the
    ## PUBLISHED ones (table S1 of the source paper), measured independently of
    ## these cells, so the carrier contrasts are real; the repertoire is still
    ## antigen-selected and says so, since the reagent panel decided which
    ## receptors are present. Class I only (sorted CD8+ T cells), so the
    ## Class I x Class II pair scope stays hidden here -- it appears when a data
    ## set carries Class II typing and a lineage column.
    ## The per-cell dextramer_* columns are 10x's RAW BINDER CALLS for a reagent,
    ## NOT validated peptide specificity: staining is heavily cross-reactive
    ## here, which is what the restriction_in_genotype column makes visible.
    ## Nothing on the Associations tab uses them.
    ## Rebuild with data-raw/build_hla_tcr_dextramer_demo.R.
    "HLA & TCR" = "extdata/examples/demo_hla_tcr_dextramer.crb"
  ),
  "crb_pick_smallest_file" = FALSE,
  ## Visium and Xenium load real backgrounds from external image files. The
  ## nested keys match dataset -> spatial data -> Background image dropdown.
  ## Images default to NO flip; the Spatial tab's "Flip vertically/horizontally"
  ## checkboxes let the user align it if a given dataset needs it (for this Visium
  ## H&E that is a vertical flip, matching Seurat's own SpatialPlot).
  "spatial_images" = list(
    "Mouse brain (Visium)" = list(
      "anterior1" = c(
        "Tissue background" = "extdata/examples/demo_spatial_visium_he.png"
      )
    ),
    "Mouse brain (Xenium)" = list(
      "fov" = list(
        "Tissue background" = list(
          path = "extdata/examples/spatial/xenium/dapi.png",
          bounds = c(xmin = 0, xmax = 5448.5, ymin = 0, ymax = 3538.55)
        )
      ),
      "fov_colour" = list(
        "Pink stain" = list(
          path = "extdata/examples/spatial/xenium/pink_stain_90.png",
          bounds = c(xmin = -3538.55, xmax = 0, ymin = 0, ymax = 5448.5)
        ),
        "Fluorescent yellow" = list(
          path = paste0(
            "extdata/examples/spatial/xenium/",
            "fluorescent_yellow_90.png"
          ),
          bounds = c(xmin = -3538.55, xmax = 0, ymin = 0, ymax = 5448.5)
        )
      )
    )
  ),
  ## Default alignment of the Visium H&E overlay, found by eye in the Spatial
  ## tab and captured here so the demo opens pre-aligned. The user can still
  ## adjust or Reset (which returns to these values, not to identity).
  "spatial_image_settings" = list(
    "Mouse brain (Visium)" = list(
      "anterior1" = list(
        "Tissue background" = list(
          offset_x = 600,
          offset_y = -750,
          scale_x = 1.55,
          scale_y = 1.55,
          flip_y = TRUE
        )
      )
    ),
    "Mouse ileum (MERFISH)" = list(
      "fov" = list(
        "Tissue background" = list(rotation = 90)
      )
    ),
    "Mouse brain (Xenium)" = list(
      "fov" = list(
        "Tissue background" = list(offset_y = -10, flip_y = TRUE)
      ),
      "fov_colour" = list(
        "Pink stain" = list(offset_x = 10, flip_x = FALSE, flip_y = TRUE),
        "Fluorescent yellow" = list(
          offset_x = 10,
          flip_x = FALSE,
          flip_y = TRUE
        )
      )
    )
  ),
  ## Plot rotation is independent of per-image alignment above and is resolved
  ## for one exact dataset + spatial entry.
  "spatial_plot_rotation" = list(
    "Mouse ileum (MERFISH)" = c("fov" = 90)
  ),
  "cerebro_root" = ".",
  "welcome_message" = custom_welcome_message,
  ## One appearance per dataset, shared by every cell scatter page.
  "point_size" = c(
    "PBMC - Full (T+B)" = 6,
    "Mouse brain (Visium)" = 5,
    "Mouse hippocampus (Slide-seq v2)" = 5,
    "Mouse ileum (MERFISH)" = 5,
    "Mouse brain (Xenium)" = 5,
    "Mouse brain (Trekker)" = 4.4,
    "HLA & TCR" = 1
  ),
  "point_opacity" = c(
    "PBMC - Full (T+B)" = 1,
    "Mouse brain (Visium)" = 1,
    "Mouse hippocampus (Slide-seq v2)" = 1,
    "Mouse ileum (MERFISH)" = 1,
    "Mouse brain (Xenium)" = 1,
    "Mouse brain (Trekker)" = 0.85,
    "HLA & TCR" = 0.3
  ),
  "percentage_cells_to_show" = 100,
  "projections_show_hover_info" = FALSE
)

options(shiny.maxRequestSize = 800 * 1024^2)

##----------------------------------------------------------------------------##
## load server and UI functions
##----------------------------------------------------------------------------##
source("viewer/shiny_UI.R", local = TRUE)
source("viewer/shiny_server.R", local = TRUE)

##----------------------------------------------------------------------------##
## launch app
##----------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)
