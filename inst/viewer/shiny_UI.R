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

## Serve the www/ directory as cacheable static assets, so the app's own CSS/JS
## are delivered as <link>/<script src> (browser-cached, downloaded in parallel,
## deferred) instead of being inlined into every page's HTML on every connection.
## Runs once when this file is sourced — by inst/app.R and by exported apps alike
## (both source shiny_UI.R with Cerebro.options already set).
cerebro_www_dir <- normalizePath(
  file.path(Cerebro.options[["cerebro_root"]], "viewer/www"),
  mustWork = FALSE
)
## Register under a prefix UNIQUE to this directory. addResourcePath's namespace
## is process-global, so a fixed "cerebro_www" would let a second exported app in
## the same process silently replace the first app's mapping and serve assets
## from the wrong bundle. Reuse a prefix already pointing here (idempotent);
## otherwise allocate a fresh, unused one. NULL = registration unavailable, so
## callers inline the asset instead.
cerebro_www_prefix <- local({
  if (!dir.exists(cerebro_www_dir)) {
    return(NULL)
  }
  existing <- shiny::resourcePaths()
  same <- names(existing)[vapply(
    existing,
    function(p) normalizePath(p, mustWork = FALSE) == cerebro_www_dir,
    logical(1)
  )]
  if (length(same)) {
    return(same[[1]])
  }
  prefix <- "cerebro_www"
  i <- 1L
  while (prefix %in% names(existing)) {
    prefix <- paste0("cerebro_www_", i)
    i <- i + 1L
  }
  tryCatch(
    {
      shiny::addResourcePath(prefix, cerebro_www_dir)
      prefix
    },
    error = function(e) NULL
  )
})

## Content-hashed URL for a registered asset: append ?v=<md5> so a redeployed
## app at the same address cannot keep serving a stale cached file. The token
## changes exactly when the file's bytes change — which is when the browser must
## refetch. (The per-directory prefix only prevents same-process path
## collisions; the path is identical across versions, so it does nothing for
## cross-version caching.)
cerebro_asset_url <- function(file) {
  url <- paste0(cerebro_www_prefix, "/", file)
  ver <- tryCatch(
    unname(tools::md5sum(file.path(cerebro_www_dir, file))),
    error = function(e) NA_character_
  )
  if (is.na(ver)) url else paste0(url, "?v=", substr(ver, 1, 10))
}

## Emit a <link>/<script> for a www asset, or — when registration was
## unavailable — inline the file contents so the page still works instead of
## 404-ing. This is the fallback the previous cerebro_asset() described in a
## comment but never actually provided.
cerebro_css <- function(file) {
  if (!is.null(cerebro_www_prefix)) {
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = cerebro_asset_url(file)
    )
  } else {
    tags$style(HTML(cerebro_read_file(file.path(cerebro_www_dir, file))))
  }
}
cerebro_js <- function(file, defer = FALSE) {
  if (!is.null(cerebro_www_prefix)) {
    src <- cerebro_asset_url(file)
    if (defer) tags$script(defer = NA, src = src) else tags$script(src = src)
  } else {
    tags$script(HTML(cerebro_read_file(file.path(cerebro_www_dir, file))))
  }
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
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/load_data/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/overview/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/groups/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/marker_genes/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/gene_expression/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/gene_id_conversion/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/color_management/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/about/UI.R"),
  local = TRUE
)

## Enhanced module UIs.
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/most_expressed_genes/UI.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/enriched_pathways/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/extra_material/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/immune_repertoire/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/trajectory/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial/UI.R"),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/trekker/UI.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/coordinated_views/UI.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/hla_tcr_motifs/UI.R"),
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
      menuItem(
        "Linked views",
        tabName = "coordinated_views",
        icon = icon("project-diagram")
      ),
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
      cerebro_css("custom.css"),
      cerebro_css("trekker.css"),
      cerebro_css("hla_motifs.css"),
      cerebro_css("coordviews.css"),
      cerebro_js("fill_height.js", defer = TRUE),
      cerebro_js("cv-geom.js", defer = TRUE),
      cerebro_js("trekker.js", defer = TRUE),
      cerebro_js("hla_motifs.js", defer = TRUE),
      cerebro_js("coordviews.js", defer = TRUE),
      ## Shared projection-scatter engine, loaded ONCE here instead of being
      ## inlined into all five projection tabs' extendShinyjs() (~69KB x5). Both
      ## files expose only window globals (window.cerebroProjectionLayout /
      ## window.cerebroProjection); each tab's thin js_projection_update_plot.js
      ## (still inlined via extendShinyjs) calls those globals. These are NOT
      ## deferred so the globals exist before the tab scripts' registerPlot()
      ## runs; layouts before scatter since scatter builds on the layout helpers.
      cerebro_js("projection_layouts.js"),
      cerebro_js("projection_scatter.js")
    ),
    tags$script(HTML('$("body").addClass("fixed");')),
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
    ),
    tags$script(inactivity)
  )
)
