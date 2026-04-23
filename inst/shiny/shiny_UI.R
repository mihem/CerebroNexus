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

inactivity <- sprintf("function idleTimer() {
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
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


##----------------------------------------------------------------------------##
## Load UI content for each tab.
##----------------------------------------------------------------------------##
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/load_data/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/overview/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/spatial/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/groups/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/marker_genes/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/gene_expression/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/gene_id_conversion/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/color_management/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/about/UI.R"), local = TRUE)

## Immune Repertoire tabs (BCR/TCR)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/immune_repertoire/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/most_expressed_genes/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/enriched_pathways/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/trajectory/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/extra_material/UI.R"), local = TRUE)
source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/analysis_info/UI.R"), local = TRUE)

##----------------------------------------------------------------------------##
## Create dashboard with different tabs.
##----------------------------------------------------------------------------##
ui <- dashboardPage(
  title = "Cerebro",
  dashboardHeader(
    title = span("Cerebro ", style = "color: white; font-size: 28px; font-weight: bold")
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    sidebarMenu(
      id = "sidebar",
      menuItem("Data info", tabName = "loadData", icon = icon("info"), selected = TRUE),
      menuItem("Main", tabName = "overview", icon = icon("home")),
      menuItemOutput("sidebar_item_spatial"),
      menuItem("Groups", tabName = "groups", icon = icon("layer-group")),
      menuItem("Most expressed genes", tabName = "mostExpressedGenes", icon = icon("bullhorn")),
      menuItemOutput("sidebar_item_marker_genes"),
      menuItemOutput("sidebar_item_enriched_pathways"),
      menuItem("Gene expression", tabName = "geneExpression", icon = icon("signal")),
      menuItemOutput("sidebar_item_bcr"),
      menuItemOutput("sidebar_item_tcr"),
      menuItemOutput("sidebar_item_trajectory"),
      menuItemOutput("sidebar_item_extra_material"),
      menuItem("Gene ID conversion", tabName = "geneIdConversion", icon = icon("barcode")),
      menuItem("Analysis info", tabName = "analysis_info", icon = icon("info")),
      menuItem("Color management", tabName = "color_management", icon = icon("palette")),
      menuItem("About", tabName = "about", icon = icon("at"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      includeCSS(file.path(Cerebro.options[["cerebro_root"]], "shiny/www/custom.css")),
      tags$style(HTML("
        .github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}
        @keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}
        @media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}
        .github-corner svg{fill:#151513; color:#fff; position: fixed; top: 0; border: 0; right: 0; z-index: 9999;}
      "))
    ),
    # 覆盖 AdminLTE 默认的 solid box 蓝色标题栏 - 放在 body 内确保在 AdminLTE 之后加载
    tags$style(HTML("
      .box.box-solid.box-primary {
        border: 0px solid #5b7c99;
      }
      .box.box-solid.box-primary>.box-header {
        background: linear-gradient(135deg, #5b7c99 0%, #3d5a73 100%);
        background-color: #5b7c99;
      }
      .box.box-solid.box-success {
        border: 1px solid #6b9080;
      }
      .box.box-solid.box-success>.box-header {
        background: linear-gradient(135deg, #6b9080 0%, #5a7a6d 100%);
        background-color: #6b9080;
      }
      .box.box-solid.box-warning {
        border: 1px solid #e9c46a;
      }
      .box.box-solid.box-warning>.box-header {
        background: linear-gradient(135deg, #e9c46a 0%, #d4a84a 100%);
        background-color: #e9c46a;
      }
      .box.box-solid.box-danger {
        border: 1px solid #e07a5f;
      }
      .box.box-solid.box-danger>.box-header {
        background: linear-gradient(135deg, #e07a5f 0%, #c96a4f 100%);
        background-color: #e07a5f;
      }
      .box.box-solid.box-info {
        border: 1px solid #8b9dc3;
      }
      .box.box-solid.box-info>.box-header {
        background: linear-gradient(135deg, #8b9dc3 0%, #7589b0 100%);
        background-color: #8b9dc3;
      }
      .bg-light-blue, .label-primary, .modal-primary .modal-body {
        background-color: #5b7c99 !important;
      }
    ")),
    # GitHub corner
    tags$div(
      class = "github-corner",
      HTML('<a href="https://github.com/duocang/cerebroApp" class="github-corner" aria-label="View source on GitHub" target="_blank">
        <svg width="50" height="50" viewBox="0 0 250 250" aria-hidden="true">
          <path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path>
          <path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" class="octo-arm"></path>
          <path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path>
        </svg>
      </a>')
    ),
    tags$script(HTML('$("body").addClass("fixed");')),
    tabItems(
      tab_load_data,
      tab_overview,
      tab_spatial,
      tab_groups,
      tab_marker_genes,
      tab_most_expressed_genes,
      tab_enriched_pathways,
      tab_gene_expression,
      createImmuneRepertoireTab("bcr"),
      createImmuneRepertoireTab("tcr"),
      tab_trajectory,
      tab_extra_material,
      tab_gene_id_conversion,
      tab_analysis_info,
      tab_color_management,
      tab_about
    ),
    tags$script(inactivity),
    # Footer
    fixedPanel(
      bottom = 10,
      right = 15,
      style = "z-index: 1000;",
      tags$span(
        style = "color: #999; font-size: 12px;",
        "Maintained by Xuesong Wang - © ",
        format(Sys.Date(), "%Y")
      )
    )
  )
)

