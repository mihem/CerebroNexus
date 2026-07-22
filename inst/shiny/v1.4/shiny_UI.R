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
    ## Console design language — see www/custom.css. Loaded here so the theme
    ## overrides AdminLTE 2 / shinydashboard chrome across every tab.
    includeCSS(
      file.path(Cerebro.options[["cerebro_root"]], "shiny/v1.4/www/custom.css")
    ),
    ## Fill-to-viewport height, app-wide. Any element with class "cerebro-fill"
    ## is sized to (viewport - its live top offset - a bottom gap), so a plot
    ## fills the screen without a hardcoded height and re-measures itself when the
    ## chrome above it changes. See www/fill_height.js.
    includeScript(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "shiny/v1.4/www/fill_height.js"
      )
    ),
    ## Trekker page assets (scoped under .trekker-page / tk- ids so they do not
    ## affect any other tab). Loaded app-wide like the theme above so the tab —
    ## which is registered in tabItems() but conditionally shown — is styled and
    ## wired whenever a Trekker .crb is loaded.
    includeCSS(
      file.path(Cerebro.options[["cerebro_root"]], "shiny/v1.4/www/trekker.css")
    ),
    includeScript(
      file.path(Cerebro.options[["cerebro_root"]], "shiny/v1.4/www/trekker.js")
    ),
    ## HLA & TCR Motifs modebar (draws a plotly-style toolbar over the visNetwork
    ## motif network; visNetwork's own nav buttons are turned off for consistency).
    includeCSS(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "shiny/v1.4/www/hla_motifs.css"
      )
    ),
    includeScript(
      file.path(
        Cerebro.options[["cerebro_root"]],
        "shiny/v1.4/www/hla_motifs.js"
      )
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
