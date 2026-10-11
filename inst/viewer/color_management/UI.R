##----------------------------------------------------------------------------##
## Tab: Color management
##----------------------------------------------------------------------------##

tab_color_management <- tabItem(
  tabName = "color_management",
  cerebroVizPageHeader(
    "Colour management",
    "color_assignments_info",
    "Set the group colours used throughout CerebroNexus."
  ),
  uiOutput("color_assignments_UI", class = "cerebro-result-slot")
)
