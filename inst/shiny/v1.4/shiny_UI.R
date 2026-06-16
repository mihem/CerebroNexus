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
    class = "btn-xs",
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

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Cerebro",
  dashboardHeader(
    title = span(
      "Cerebro ",
      style = "color: white; font-size: 28px; font-weight: bold"
    )
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    sidebarMenu(
      id = "sidebar",
      menuItem(
        "Data info",
        tabName = "loadData",
        icon = icon("info"),
        selected = TRUE
      ),
      menuItem("Main", tabName = "overview", icon = icon("home")),
      menuItem("Groups", tabName = "groups", icon = icon("layer-group")),
      menuItem(
        "Marker genes",
        tabName = "markerGenes",
        icon = icon("list-alt")
      ),
      menuItem(
        "Most expressed genes",
        tabName = "mostExpressedGenes",
        icon = icon("bullhorn")
      ),
      div(id = "sidebar_item_enriched_pathways_placeholder"),
      div(id = "sidebar_item_extra_material_placeholder"),
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
    tags$script(HTML('$("body").addClass("fixed");')),
    tabItems(
      tab_load_data,
      tab_overview,
      tab_groups,
      tab_marker_genes,
      tab_most_expressed_genes,
      tab_enriched_pathways,
      tab_extra_material,
      tab_gene_expression,
      tab_gene_id_conversion,
      tab_color_management,
      tab_about
    ),
    tags$script(inactivity)
  )
)
