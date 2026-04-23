##----------------------------------------------------------------------------##
## Tab: Immune Repertoire (BCR/TCR)
##----------------------------------------------------------------------------##

createImmuneRepertoireTab <- function(repertoire_type = c("bcr", "tcr")) {
  repertoire_type <- match.arg(repertoire_type)
  
  tab_name <- repertoire_type
  title <- ifelse(repertoire_type == "bcr", "BCR settings", "TCR settings")
  viz_title <- ifelse(repertoire_type == "bcr", "BCR visualizations", "TCR visualizations")
  
  tabItem(
    tabName = tab_name,
    fluidRow(
      cerebroBox(
        title = boxTitle(title),
        content = uiOutput(paste0(repertoire_type, "_settings_UI"))
      )
    ),
    fluidRow(
      cerebroBox(
        title = boxTitle(viz_title),
        content = uiOutput(paste0(repertoire_type, "_visualizations_UI"))
      )
    )
  )
}
