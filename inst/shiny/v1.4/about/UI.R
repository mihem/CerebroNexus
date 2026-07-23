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
        htmlOutput("about"),
        #        uiOutput("preferences"),
        actionButton("browser", "browser"),
        tags$script("$('#browser').hide();")
      )
    ),
    fluidRow(
      htmlOutput("about_footer")
    )
  )
)
