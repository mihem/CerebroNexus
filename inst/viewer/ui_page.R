##----------------------------------------------------------------------------##
## The page: the content of every tab and the dashboard around them.
##
## Evaluated by viewerUiBuild() in shiny_UI.R, which defines the helpers used
## here. Nothing the server needs is defined in this file or in the UI.R files
## it sources: when the finished page is taken from the cache (ui_cache.R),
## none of them is evaluated.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
for (ui_page in c(
  "load_data",
  "overview",
  "groups",
  "marker_genes",
  "gene_expression",
  "gene_id_conversion",
  "color_management",
  "about",
  "most_expressed_genes",
  "enriched_pathways",
  "extra_material",
  "immune_repertoire",
  "trajectory",
  "spatial",
  "trekker",
  "coordinated_views",
  "hla_tcr_motifs"
)) {
  source(
    file.path(Cerebro.options[["cerebro_root"]], "viewer", ui_page, "UI.R"),
    local = TRUE
  )
}

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
## Use the recorded version so standalone bundles need no installed package.
brand_version <- Cerebro.options[["cerebro_version"]]
brand_version_label <- NULL
if (
  length(brand_version) &&
    !is.na(brand_version[[1L]]) &&
    nzchar(as.character(brand_version[[1L]]))
) {
  brand_version_label <- tags$span(
    class = "cerebro-brand-version",
    paste0("v", as.character(brand_version[[1L]]))
  )
}
ui <- dashboardPage(
  title = "CerebroNexus",
  ## Header is collapsed to zero height by the theme (see www/custom.css); the
  ## brand now lives at the top of the sidebar. We keep an empty
  ## dashboardHeader() because shinydashboard requires one for layout.
  dashboardHeader(title = NULL),
  dashboardSidebar(
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    div(
      class = "cerebro-brand",
      ## Rounded-geometric wordmark: the letters are vector outlines (Fredoka,
      ## SIL OFL), so it renders identically without depending on any installed
      ## font. Cerebro in near-black, Nexus in the amber accent. Kept in its own
      ## file (www/cerebronexus.svg) rather than inlined as ~10KB of path data.
      HTML(
        paste(
          readLines(
            paste0(
              Cerebro.options[["cerebro_root"]],
              "/viewer/www/cerebronexus.svg"
            ),
            warn = FALSE
          ),
          collapse = ""
        )
      ),
      brand_version_label
    ),
    tags$button(
      type = "button",
      id = "cerebro-nav-close",
      class = "cerebro-nav-close",
      `aria-label` = "Close navigation",
      HTML("&times;")
    ),
    uiOutput("crb_file_selector_UI", class = "cerebro-dataset-switcher"),
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Data info",
        tabName = "loadData",
        icon = cerebroIcon("info"),
        selected = TRUE
      ),
      ## Shown by default; hidden for a data set without projections.
      shiny::tagAppendAttributes(
        menuItem(
          "Projection",
          tabName = "overview",
          icon = cerebroIcon("home")
        ),
        id = "sidebar_item_overview"
      ),
      conditionalSidebarItem(
        "Linked views",
        "coordinated_views",
        "link"
      ),
      menuItem(
        "Groups",
        tabName = "groups",
        icon = cerebroIcon("layer-group")
      ),
      ## Conditional items are present when shinydashboard binds the sidebar,
      ## then shown only for data sets that support them. Keeping their anchors
      ## in the initial DOM lets updateTabItems() select them reliably.
      conditionalSidebarItem("Marker genes", "markerGenes", "list-alt"),
      conditionalSidebarItem(
        "Most expressed genes",
        "mostExpressedGenes",
        "bullhorn"
      ),
      conditionalSidebarItem(
        "Enriched pathways",
        "enrichedPathways",
        "sitemap"
      ),
      conditionalSidebarItem("Extra material", "extra_material", "gift"),
      conditionalSidebarItem(
        "Immune repertoire",
        "immune_repertoire",
        "dna"
      ),
      conditionalSidebarItem("Trajectory", "trajectory", "route"),
      conditionalSidebarItem("Spatial", "spatial", "map-pin"),
      conditionalSidebarItem("Trekker", "trekker", "map-marked-alt"),
      conditionalSidebarItem(
        "HLA & TCR Motifs",
        "hla_tcr_motifs",
        "fingerprint"
      ),
      menuItem(
        "Gene expression",
        tabName = "geneExpression",
        icon = cerebroIcon("signal")
      ),
      menuItem(
        "Gene ID conversion",
        tabName = "geneIdConversion",
        icon = cerebroIcon("barcode")
      ),
      menuItem(
        "Colour management",
        tabName = "color_management",
        icon = cerebroIcon("palette")
      ),
      menuItem("About", tabName = "about", icon = cerebroIcon("at"))
    ),
    uiOutput("preferences_options", class = "cerebro-sidebar-preferences")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$button(
      type = "button",
      id = "cerebro-nav-scrim",
      `aria-label` = "Close navigation",
      `aria-hidden` = "true",
      tabindex = "-1"
    ),
    tags$div(
      id = "cerebro-update-status",
      class = "sr-only",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    ),
    tags$dialog(
      id = "session-timeout-warning",
      role = "dialog",
      `aria-modal` = "true",
      `aria-labelledby` = "session-timeout-warning-title",
      `aria-describedby` = "session-timeout-warning-message",
      tags$h2(id = "session-timeout-warning-title", "Session expiring"),
      tags$p(
        id = "session-timeout-warning-message",
        "Your session will end in ",
        tags$span(id = "session-timeout-countdown", "60"),
        " seconds unless you continue."
      ),
      tags$button(
        id = "session-timeout-continue",
        type = "button",
        class = "btn btn-primary",
        "Continue session"
      )
    ),
    ## App CSS/JS as cacheable static resources (served from the cerebro_www
    ## resource path registered above) instead of inlined into every page. The
    ## browser caches them across connections and downloads them in parallel;
    ## scripts are deferred so they run after the document parses (each is a
    ## self-contained IIFE with its own Shiny-readiness retry, so order-safe).
    ##  - custom.css      : Console design language; overrides AdminLTE 2 chrome.
    ##  - fill_height.js  : sizes any .cerebro-fill element to the live viewport.
    ##  - trekker.css     : Trekker insight/QC presentation shared with Linked views.
    ##  - hla_motifs.*    : HLA association table and the node-size slider.
    tags$head(
      tags$style(HTML(
        "html, body { background: #f7f7f9; } .cerebro-starting .wrapper { visibility: hidden; } #cerebro-startup { position: fixed; inset: 0; z-index: 2147483000; display: grid; place-items: center; background: #f7f7f9; color: #51545c; font: 16px Arial, sans-serif; transition: opacity .12s ease-out; } #cerebro-startup > div { text-align: center; } #cerebro-startup button { margin-top: 16px; } @media (prefers-reduced-motion: reduce) { #cerebro-startup { transition: none; } }"
      )),
      tags$script(HTML(
        "document.documentElement.classList.add('cerebro-starting');"
      )),
      ## Inline and first: it asks for the page's first view before any other
      ## file is requested (www/first-view.js).
      tags$script(HTML(cerebro_read_file(
        file.path(cerebro_www_dir, "first-view.js")
      ))),
      cerebro_js("page-lifecycle.js", defer = TRUE),
      cerebro_js("startup.js", defer = TRUE),
      cerebro_css("custom.css"),
      cerebro_css("trekker.css"),
      cerebro_css("hla_motifs.css"),
      cerebro_css("coordviews.css"),
      cerebro_js("fill_height.js", defer = TRUE),
      cerebro_js("cv-geom.js", defer = TRUE),
      cerebro_js("cell_views_state.js", defer = TRUE),
      cerebro_js("dataset-context.js"),
      cerebro_js("cell_points_gpu.js"),
      cerebro_js("cell_views_wire.js"),
      cerebro_js("hla_motifs.js", defer = TRUE),
      cerebro_js("coordviews-config.js", defer = TRUE),
      cerebro_js("viewer-shell.js", defer = TRUE),
      cerebro_js("gene-feedback.js", defer = TRUE),
      cerebro_js("view-feedback.js", defer = TRUE),
      cerebro_js("cerebro-charts.js", defer = TRUE),
      cerebro_js("selection-table.js", defer = TRUE),
      cerebro_js("multiselect.js", defer = TRUE),
      cerebro_js("gene-selector.js", defer = TRUE),
      cerebro_js("background-model.js"),
      cerebro_js("cell_views.js"),
      cerebro_js("settings_drawer.js", defer = TRUE),
      cerebro_js("specialist-view-state.js", defer = TRUE)
    ),
    tags$main(
      id = "main-content",
      tabindex = "-1",
      tags$div(
        id = "cerebro-page-loader-announcement",
        class = "sr-only",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true"
      ),
      tags$div(
        id = "cerebro-page-loader",
        class = "cerebro-page-loader",
        `aria-hidden` = "true",
        hidden = "hidden",
        tags$span(
          class = "cerebro-page-loader-spinner",
          `aria-hidden` = "true"
        ),
        tags$span(
          class = "cerebro-page-loader-copy",
          tags$span(
            id = "cerebro-page-loader-detail",
            "Preparing data and visualisation…"
          )
        )
      ),
      tabItems(
        tab_load_data,
        tab_overview,
        tab_coordinated_views,
        tab_groups,
        tab_marker_genes,
        tab_most_expressed_genes,
        tab_enriched_pathways,
        tab_extra_material,
        tab_immune_repertoire,
        tab_trajectory,
        tab_spatial,
        tab_trekker,
        tab_hla_tcr_motifs,
        tab_gene_expression,
        tab_gene_id_conversion,
        tab_color_management,
        tab_about
      )
    ),
    tags$script(HTML(inactivity))
  )
)

ui$children <- tagList(
  tags$div(
    id = "cerebro-startup",
    tags$div(
      tags$div(
        style = "width:200px;margin:0 auto 24px",
        `aria-hidden` = "true",
        HTML(paste(
          readLines(
            file.path(
              Cerebro.options[["cerebro_root"]],
              "viewer/www/cerebronexus.svg"
            ),
            warn = FALSE
          ),
          collapse = "\n"
        ))
      ),
      tags$div(
        role = "status",
        `aria-live` = "polite",
        `data-startup-message` = "",
        "Opening workspace\u2026"
      ),
      tags$button(
        type = "button",
        class = "btn btn-default",
        `data-startup-retry` = "",
        hidden = "hidden",
        "Reload"
      )
    )
  ),
  tags$a(
    class = "cerebro-skip-link",
    href = "#main-content",
    "Skip to main content"
  ),
  ui$children
)

source(
  file.path(Cerebro.options[["cerebro_root"]], "viewer/static_ui.R"),
  local = TRUE
)
ui <- viewerStaticUI(ui)
