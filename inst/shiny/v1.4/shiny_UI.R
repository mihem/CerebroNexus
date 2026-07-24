##----------------------------------------------------------------------------##
## Custom functions.
##----------------------------------------------------------------------------##
cerebroBox <- function(
  title,
  content,
  collapsible = TRUE,
  collapsed = FALSE
) {
  box(
    title = title,
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = collapsible,
    collapsed = collapsed,
    content
  )
}

cerebroInfoButton <- function(id, ...) {
  actionButton(
    inputId = id,
    label = "info",
    icon = NULL,
    class = "btn-xs cerebro-info-btn",
    title = "Show additional information for this panel.",
    ...
  )
}

boxTitle <- function(title) {
  p(title, style = "padding-right: 5px; display: inline")
}

## Read an entire file into a single string. Used to inline .js/.svg/.html
## assets into the UI. readChar reads `size` bytes (the file's byte count) and
## stops at EOF, which faithfully covers ASCII/UTF-8 assets. Defined here,
## before the per-tab UI.R files are sourced with local = TRUE, so it is in
## scope for every one of them.
cerebro_read_file <- function(path) {
  readChar(path, file.info(path)$size)
}

## Register the www/ directory as a cacheable static resource path, so the app's
## own CSS/JS are delivered as <link>/<script src> (browser-cached, downloaded in
## parallel, deferred) instead of being inlined into every page's HTML on every
## connection. Runs once when this file is sourced — by inst/app.R and by
## exported apps alike (both source shiny_UI.R with Cerebro.options already set).
## cerebro_asset() returns the served URL for a www file, or NULL when the path
## could not be registered (then the caller falls back to inlining).
local({
  www_dir <- file.path(
    Cerebro.options[["cerebro_root"]],
    "shiny/v1.4/www"
  )
  if (dir.exists(www_dir)) {
    tryCatch(
      shiny::addResourcePath("cerebro_www", normalizePath(www_dir)),
      error = function(e) NULL
    )
  }
})
cerebro_asset <- function(file) {
  paste0("cerebro_www/", file)
}

##----------------------------------------------------------------------------##
## timeout function
##----------------------------------------------------------------------------##

timeoutSeconds <- 600

inactivity <- sprintf(
  "function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();",
  timeoutSeconds * 1000,
  timeoutSeconds,
  timeoutSeconds * 1000
)


##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/load_data/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/overview/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/groups/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/marker_genes/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/gene_expression/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/gene_id_conversion/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/color_management/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/about/UI.R"),
  local = TRUE
)

## Enhanced module UIs.
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/most_expressed_genes/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/enriched_pathways/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/extra_material/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/trajectory/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/spatial/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/trekker/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/hla_tcr_motifs/UI.R"),
  local = TRUE
)

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
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
      HTML(
        paste0(
          '<svg class="cerebro-logo" xmlns="http://www.w3.org/2000/svg" ',
          'viewBox="0 0 210 38" role="img" aria-labelledby="cb-logo-title">',
          '<title id="cb-logo-title">CerebroNexus</title>',
          '<text x="0" y="27" fill="currentColor" ',
          'font-family="var(--font-sans),system-ui,sans-serif" ',
          'font-size="27" font-weight="650" letter-spacing="-0.6">Cerebro</text>',
          '<text x="104" y="27" fill="#337ab7" ',
          'font-family="var(--font-sans),system-ui,sans-serif" ',
          'font-size="27" font-weight="750" letter-spacing="-0.6">Nexus</text>',
          '</svg>'
        )
      )
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Data info",
        tabName = "loadData",
        icon = icon("info"),
        selected = TRUE
      ),
      menuItem("Projection", tabName = "overview", icon = icon("home")),
      menuItem("Groups", tabName = "groups", icon = icon("layer-group")),
      ## Marker genes and Most expressed genes are inserted conditionally (see
      ## insertConditionalTab in shiny_server.R): a data set that carries neither
      ## — e.g. the spatial demos — no longer shows a sidebar item that opens to
      ## an empty table. Their tab bodies stay registered in tabItems(); without
      ## a menuItem there is simply no way to navigate to them, matching how the
      ## enriched-pathways / trajectory / spatial tabs already behave.
      div(id = "sidebar_item_marker_genes_placeholder"),
      div(id = "sidebar_item_most_expressed_genes_placeholder"),
      div(id = "sidebar_item_enriched_pathways_placeholder"),
      div(id = "sidebar_item_extra_material_placeholder"),
      div(id = "sidebar_item_immune_repertoire_placeholder"),
      div(id = "sidebar_item_trajectory_placeholder"),
      div(id = "sidebar_item_spatial_placeholder"),
      div(id = "sidebar_item_trekker_placeholder"),
      div(id = "sidebar_item_hla_tcr_motifs_placeholder"),
      menuItem(
        "Gene expression",
        tabName = "geneExpression",
        icon = icon("signal")
      ),
      menuItem(
        "Gene ID conversion",
        tabName = "geneIdConversion",
        icon = icon("barcode")
      ),
      menuItem(
        "Color management",
        tabName = "color_management",
        icon = icon("palette")
      ),
      menuItem("About", tabName = "about", icon = icon("at"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    ## App CSS/JS as cacheable static resources (served from the cerebro_www
    ## resource path registered above) instead of inlined into every page. The
    ## browser caches them across connections and downloads them in parallel;
    ## scripts are deferred so they run after the document parses (each is a
    ## self-contained IIFE with its own Shiny-readiness retry, so order-safe).
    ##  - custom.css      : Console design language; overrides AdminLTE 2 chrome.
    ##  - fill_height.js  : sizes any .cerebro-fill element to the live viewport.
    ##  - trekker.*       : Trekker page assets (scoped under .trekker-page / tk-).
    ##  - hla_motifs.*    : modebar over the visNetwork motif network.
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = cerebro_asset("custom.css")
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = cerebro_asset("trekker.css")
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = cerebro_asset("hla_motifs.css")
      ),
      tags$script(defer = NA, src = cerebro_asset("fill_height.js")),
      tags$script(defer = NA, src = cerebro_asset("trekker.js")),
      tags$script(defer = NA, src = cerebro_asset("hla_motifs.js")),
      ## Shared projection-scatter engine, loaded ONCE here instead of being
      ## inlined into all five projection tabs' extendShinyjs() (~69KB x5). Both
      ## files expose only window globals (window.cerebroProjectionLayout /
      ## window.cerebroProjection); each tab's thin js_projection_update_plot.js
      ## (still inlined via extendShinyjs) calls those globals. These are NOT
      ## deferred so the globals exist before the tab scripts' registerPlot()
      ## runs; layouts before scatter since scatter builds on the layout helpers.
      tags$script(src = cerebro_asset("projection_layouts.js")),
      tags$script(src = cerebro_asset("projection_scatter.js"))
    ),
    tags$script(HTML('$("body").addClass("fixed");')),
    tabItems(
      tab_load_data,
      tab_overview,
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
    ),
    tags$script(inactivity)
  )
)
