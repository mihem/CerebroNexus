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

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Cerebro",
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
          'viewBox="0 0 230 34" role="img" aria-labelledby="cb-logo-title">',
          '<title id="cb-logo-title">cerebro — single cell</title>',
          # Wordmark: rounded stroked geometric lowercase, font-independent.
          # Round bowls are drawn with generous, well-separated
          # geometry so glyphs never overlap. x-height band y=12..28, radius 7,
          # advance ~21px, stroke 4, round caps/joins.
          '<g fill="none" stroke="currentColor" stroke-width="4" ',
          'stroke-linecap="round" stroke-linejoin="round">',
          # c : open circle, gap on the right
          '<path d="M17.9 15.05 A7 7 0 1 0 17.9 24.95"/>',
          # e : bar across the middle + open arc (gap lower-right)
          '<path d="M31 20 H45 A7 7 0 1 0 43.1 24.95"/>',
          # r : stem + small shoulder
          '<path d="M54 13 V28 M54 19 A6 6 0 0 1 63 16.2"/>',
          # e
          '<path d="M69 20 H83 A7 7 0 1 0 81.1 24.95"/>',
          # b : tall stem + full round bowl (separate circle, no overlap)
          '<path d="M92 4 V28"/><circle cx="99" cy="21" r="7"/>',
          # r
          '<path d="M113 13 V28 M113 19 A6 6 0 0 1 122 16.2"/>',
          # o : full circle
          '<circle cx="135" cy="21" r="7"/>',
          '</g>',
          # Dark "single cell" chip — smaller than the wordmark so the wordmark
          # clearly reads as the primary mark, with the chip as a secondary tag.
          # Square corners (rx=0), tight to the text.
          '<g transform="translate(150,14)">',
          '<rect x="0" y="0" width="63" height="17" rx="0" fill="#16171a"></rect>',
          '<text x="31.5" y="12.2" text-anchor="middle" ',
          'font-family="var(--font-sans),system-ui,sans-serif" ',
          'font-size="10.5" font-weight="600" letter-spacing="0.2" ',
          'fill="#ffffff">single cell</text>',
          '</g></svg>'
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
    ## Console design language — see www/custom.css. Loaded here so the theme
    ## overrides AdminLTE 2 / shinydashboard chrome across every tab.
    includeCSS(
      file.path(Cerebro.options[["cerebro_root"]], "shiny/v1.4/www/custom.css")
    ),
    ## Shared viewport lifecycle for stable card-level sizing and reveal.
    includeScript(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "shiny/v1.4/www/viewport.js"
      )
    ),
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
      tab_gene_expression,
      tab_gene_id_conversion,
      tab_color_management,
      tab_about
    ),
    tags$script(inactivity)
  )
)
