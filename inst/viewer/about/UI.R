##----------------------------------------------------------------------------##
## Tab: About.
##----------------------------------------------------------------------------##

tab_about <- tabItem(
  tabName = "about",
  tagList(
    fluidRow(
      column(12, titlePanel("About CerebroNexus")),
      column(
        12,
        htmlOutput("about")
      )
    ),
    fluidRow(
      htmlOutput("about_footer")
    )
  )
)
